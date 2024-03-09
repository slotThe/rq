//! Bidirectional type checking:
//!
//! - Jana Dunfield and Neelakantan R. Krishnaswami. 2013.
//!   "Complete and easy bidirectional typechecking for higher-rank polymorphism."
//!   In Proc. of the 18th ACM SIGPLAN int. conf. on Functional programming (ICFP '13).
//!   Association for Computing Machinery, New York, NY, USA, 429–442.
//!   <https://doi.org/10.1145/2500365.2500582>
//!
//! The article is readily available [on the arXiv](https://arxiv.org/abs/1306.6032).

use std::collections::{BTreeMap, BTreeSet, HashMap};

use super::{context::{Item, State}, error::TypeCheckError, Monotype, TypVar, Type};
use crate::expr::{self, app, Expr};

/// A type-checked expression.
#[derive(Debug)]
pub struct TCExpr {
  pub expr: Expr,
}

impl TCExpr {
  /// Apply an expression to a type-checked expression. Assumes that the added
  /// expression is well-formed—i.e., type-checks. This is essentially an
  /// optimisation, as type-checking a large JSON chunk may take a while.
  pub fn apply(self, json: Expr) -> TCExpr {
    TCExpr {
      expr: app(self.expr, json),
    }
  }
}

impl Expr {
  /// Type check an expression.
  pub fn type_check(
    &self,
    stdlib: &BTreeMap<String, Type>,
  ) -> Result<Type, TypeCheckError> {
    let state = State::new(stdlib.clone());
    let (state, mut typ) = self.synth(state)?;
    typ.finish_mut(&state.ctx);
    Ok(typ)
  }

  /// Construct a [TCExpr] out of an [Expr].
  pub fn to_tcexpr(
    &self,
    ctx: &BTreeMap<String, Type>,
  ) -> Result<TCExpr, TypeCheckError> {
    self.type_check(ctx)?;
    Ok(TCExpr { expr: self.clone() })
  }
}

impl Type {
  /// Clean up after type checking: normalise all type variables (make them
  /// start at 0) and replace existential (unsolved) type variables with
  /// universal quantification.
  fn finish_mut(&mut self, ctx: &[Item]) {
    // Enumerate all type variables
    fn enum_tvars(tvars: &mut BTreeSet<TypVar>, typ: &Type) {
      match typ {
        Type::JSON => {},
        Type::Var(v) | Type::Exist(v) => {
          tvars.insert(*v);
        },
        Type::Arr(t1, t2) => {
          enum_tvars(tvars, t1);
          enum_tvars(tvars, t2);
        },
        Type::Forall(α, t) => {
          tvars.insert(*α);
          enum_tvars(tvars, t);
        },
      }
    }

    // Scale all type variables down, so they start at 0.
    fn downscale(tvars: &HashMap<TypVar, usize>, typ: &mut Type) {
      match typ {
        Type::Var(tvar) | Type::Exist(tvar) => {
          tvar.0 = *tvars.get(tvar).unwrap();
        },
        Type::JSON => {},
        Type::Arr(t1, t2) => {
          downscale(tvars, t1);
          downscale(tvars, t2);
        },
        Type::Forall(α, t) => {
          α.0 = *tvars.get(α).unwrap();
          downscale(tvars, t);
        },
      }
    }

    // Eschew existential type variables and replace them with universal
    // quantification. This looks nicer.
    fn forallise(typ: Type, rule: &Item) -> Type {
      match rule {
        Item::Unsolved(α̂) if α̂.unsolved_in(&typ) => {
          Type::forall(*α̂, typ.subst(Type::Var(*α̂), *α̂))
        },
        _ => typ,
      }
    }

    *self = ctx.iter().rfold(self.apply_ctx(ctx), forallise);
    let mut tvars = BTreeSet::new();
    enum_tvars(&mut tvars, self);
    downscale(&tvars.into_iter().zip(0..).collect(), self);
  }
}

impl Type {
  ///                   A.well_formed_under(Γ)  ≡  Γ ⊢ A
  ///
  /// Under context Γ, type A is well-formed.
  fn well_formed_under(&self, ctx: &[Item]) -> Result<(), TypeCheckError> {
    match self {
      // UnitWF
      Type::JSON => Ok(()),
      // UvarWF
      Type::Var(α) => {
        if ctx.contains(&Item::Var(*α)) {
          Ok(())
        } else {
          Err(TypeCheckError::TypeVariableNotInScope(*α))
        }
      },
      // EvarWF and SolvedEvarWF
      Type::Exist(α̂) => {
        if ctx.iter().any(|it| match it {
          Item::Unsolved(β̂) => α̂ == β̂,
          Item::Solved(β̂, _) => α̂ == β̂,
          _ => false,
        }) {
          Ok(())
        } else {
          Err(TypeCheckError::MalformedType(self.clone()))
        }
      },
      // ForallWF
      Type::Forall(α, t) => t.well_formed_under(&[ctx, &[Item::Var(*α)]].concat()),
      // ArrowWF
      Type::Arr(t1, t2) => {
        t1.well_formed_under(ctx)?;
        t2.well_formed_under(ctx)
      },
    }
  }

  ///                   A.subtype_of(Γ, B)  ≡  Γ ⊢ A <: B ⊣ Δ
  ///
  /// Under input context Γ, type A is a subtype of B, with output context Δ.
  /// Figure 9.
  fn subtype_of(&self, mut state: State, b: &Type) -> Result<State, TypeCheckError> {
    // println!("subtype; ctx: {:?}  a: {:?}  b: {:?}", state.ctx, self, b);
    match (self, b) {
      // <:Var
      (Type::Var(α), Type::Var(β)) if α == β => {
        self.well_formed_under(&state.ctx)?;
        Ok(state)
      },
      // <:JSON ≡ <:Unit
      (Type::JSON, Type::JSON) => Ok(state),
      // <:→
      (Type::Arr(a1, a2), Type::Arr(b1, b2)) => {
        let θ = b1.subtype_of(state, a1)?;
        // θ ⊢ [θ]A₂ <: [θ]B₂ ⊣ Δ
        a2.apply_ctx(&θ.ctx)
          .subtype_of(θ.clone(), &b2.apply_ctx(&θ.ctx))
      },
      // <:∀R
      (t, Type::Forall(α, box s)) => {
        let fresh_α = TypVar(state.fresh_mut());
        state.ctx.push(Item::Var(fresh_α));
        let s = s.clone().subst(Type::Var(fresh_α), *α);
        t.subtype_of(state, &s)
          .map(|θ| θ.drop_after(&Item::Var(fresh_α)))
      },
      // <:∀l
      (Type::Forall(α, box t), s) => {
        let fresh_α̂ = TypVar(state.fresh_mut());
        state
          .ctx
          .extend_from_slice(&[Item::Marker(fresh_α̂), Item::Unsolved(fresh_α̂)]);
        let t = t.clone().subst(Type::Exist(fresh_α̂), *α);
        t.subtype_of(state, s)
          .map(|θ| θ.drop_after(&Item::Marker(fresh_α̂)))
      },
      // <:Exvar
      (Type::Exist(α̂), Type::Exist(β̂)) if α̂ == β̂ => Ok(state),
      // <:InstantiateL
      (Type::Exist(α̂), s) if !s.free_variables().contains(α̂) => {
        α̂.instantiate_l(state, s.clone())
      },
      // <:InstantiateR
      (t, Type::Exist(α̂)) if !t.free_variables().contains(α̂) => {
        α̂.instantiate_r(state, t.clone())
      },
      // ERROR
      _ => Err(TypeCheckError::NotASubtype(self.clone(), b.clone())),
    }
  }
}

impl TypVar {
  /// InstLSolve and InstRSolve: this is symmetric for both InstantiateL
  /// and InstantiateR.
  fn inst_solve(self, state: State, typ: &Type) -> Result<State, TypeCheckError> {
    match typ.clone().to_mono() {
      Some(τ) => {
        let (ctx_l, _) = state
          .ctx
          .split_once(|i| *i == Item::Unsolved(self))
          .unwrap();
        match typ.well_formed_under(ctx_l) {
          Err(_) => Err(TypeCheckError::InstSolveError),
          Ok(()) => Ok(state.insert_at(&Item::Unsolved(self), &[Item::Solved(self, τ)])),
        }
      },
      None => Err(TypeCheckError::InstSolveError),
    }
  }

  ///                   α̂.instantiateL(Γ, A) ≡ Γ ⊢ α̂ :=< A ⊣ Δ
  ///
  /// Under input context Γ, instantiate α̂ such that α̂ <: A, with output
  /// context Δ.
  fn instantiate_l(self, mut state: State, typ: Type) -> Result<State, TypeCheckError> {
    // println!("instantiateL; ctx: {:?}  α̂: {:?}  a: {:?}", state.ctx, self, typ);
    match self.inst_solve(state.clone(), &typ) {
      Ok(state) => Ok(state),
      Err(TypeCheckError::InstSolveError) => match typ {
        // InstLReach
        Type::Exist(β̂)
          if Item::Unsolved(self).left_of(&Item::Unsolved(β̂), &state.ctx) =>
        {
          Ok(state.insert_at(
            &Item::Unsolved(β̂),
            &[Item::Solved(β̂, Monotype::Exist(self))],
          ))
        },
        // InstLArr
        Type::Arr(box t, box s) => {
          let α̂1 = TypVar(state.fresh_mut());
          let α̂2 = TypVar(state.fresh_mut());
          let state = state.insert_at(
            &Item::Unsolved(self),
            &[
              Item::Unsolved(α̂2),
              Item::Unsolved(α̂1),
              Item::Solved(
                self,
                Monotype::arr(Monotype::Exist(α̂1), Monotype::Exist(α̂2)),
              ),
            ],
          );
          let θ = α̂1.instantiate_r(state, t)?;
          α̂2.instantiate_l(θ.clone(), s.apply_ctx(&θ.ctx))
        },
        // InstLAIIR
        Type::Forall(α, box t) => {
          let fresh_α = TypVar(state.fresh_mut());
          state.ctx.push(Item::Var(fresh_α));
          let t = t.subst(Type::Var(fresh_α), α);
          self
            .instantiate_l(state, t)
            .map(|θ| θ.drop_after(&Item::Var(fresh_α)))
        },
        _ => Err(TypeCheckError::InstantiationError(self, typ)),
      },
      Err(err) => Err(err),
    }
  }

  ///                   α̂.instantiate_r(Γ, A)  ≡  Γ ⊢ A =:< α̂ ⊣ Δ
  ///
  /// Under input context Γ, instantiate α̂ such that A <: α̂, with output
  /// context Δ.
  fn instantiate_r(self, mut state: State, typ: Type) -> Result<State, TypeCheckError> {
    // println!("instantiateR; ctx: {:?}  a: {:?}  α̂: {:?}", state.ctx, typ, self);
    match self.inst_solve(state.clone(), &typ) {
      Ok(state) => Ok(state),
      Err(TypeCheckError::InstSolveError) => match typ {
        // InstRReach
        Type::Exist(_) => self.instantiate_l(state, typ),
        // InstRArr
        Type::Arr(box t, box s) => {
          let α̂1 = TypVar(state.fresh_mut());
          let α̂2 = TypVar(state.fresh_mut());
          let state = state.insert_at(
            &Item::Unsolved(self),
            &[
              Item::Unsolved(α̂2),
              Item::Unsolved(α̂1),
              Item::Solved(
                self,
                Monotype::arr(Monotype::Exist(α̂1), Monotype::Exist(α̂2)),
              ),
            ],
          );
          let θ = α̂1.instantiate_l(state, t)?;
          α̂2.instantiate_r(θ.clone(), s.apply_ctx(&θ.ctx))
        },
        // InstRAIIL
        Type::Forall(α, box t) => {
          let fresh_α̂ = TypVar(state.fresh_mut());
          state
            .ctx
            .extend([Item::Marker(fresh_α̂), Item::Unsolved(fresh_α̂)]);
          let t = t.subst(Type::Exist(fresh_α̂), α);
          self
            .instantiate_r(state, t)
            .map(|θ| θ.drop_after(&Item::Marker(fresh_α̂)))
        },
        _ => Err(TypeCheckError::InstantiationError(self, typ)),
      },
      Err(err) => Err(err),
    }
  }
}

impl Expr {
  ///                   e.synth(Γ)  ≡  Γ ⊢ e ⇒ A ⊣ Δ
  ///
  /// Under input context Γ, e synthesizes (infers) output type A, with output
  /// context Δ.
  fn synth(&self, mut state: State) -> Result<(State, Type), TypeCheckError> {
    // println!("infer; ctx: {:?}  e: {:?}", state.ctx, self);
    match self {
      // 1l⇒
      Expr::Const(_) | Expr::Arr(_) | Expr::Obj(_) => Ok((state, Type::JSON)),
      Expr::Builtin(b) => expr::var(b.show()).synth(state),
      // Var
      Expr::Var(v) => match state
        .ctx
        .iter()
        .filter_map(|it| match it {
          Item::Ann(y, t) if *y == v.name => Some((y, t)),
          _ => None,
        })
        .rev()
        .nth(v.level as usize)
      {
        Some((_, t)) => Ok((state.clone(), t.clone())),
        None => match state.stdlib.get(&v.name) {
          Some(t) => Ok((state.clone(), t.clone())),
          None => Err(TypeCheckError::VariableNotInScope(v.clone())),
        },
      },
      // Anno
      Expr::Ann(e, t) => {
        t.well_formed_under(&state.ctx)?;
        let state = e.check(state, t)?;
        Ok((state, t.clone()))
      },
      // →I⇒
      Expr::Lam(x, e) => {
        let α̂ = TypVar(state.fresh_mut());
        let β̂ = TypVar(state.fresh_mut());
        let ann = Item::Ann(x.to_string(), Type::Exist(α̂));
        state
          .ctx
          .extend([Item::Unsolved(α̂), Item::Unsolved(β̂), ann.clone()]);
        Ok((
          e.check(state, &Type::Exist(β̂))
            .map(|θ| θ.drop_after(&ann))?,
          Type::arr(Type::Exist(α̂), Type::Exist(β̂)),
        ))
      },
      // →E
      Expr::App(f, x) => {
        let (state, t) = f.synth(state)?;
        x.apply_type(state.clone(), &t.apply_ctx(&state.ctx))
      },
      //
      Expr::IfThenElse(i, t, e) => {
        let state = i.check(state, &Type::JSON)?;
        let (state, then_type) = t.synth(state)?;
        let state = e.check(state, &then_type)?;
        Ok((state, then_type))
      },
    }
  }

  ///                   e.check(Γ, A)  ≡  Γ ⊢ e ⇐ A ⊣ Δ
  ///
  /// Under input context Γ, e checks against input type A, with output
  /// context Δ.
  fn check(&self, mut state: State, against: &Type) -> Result<State, TypeCheckError> {
    // println!("check; ctx: {:?}  e: {:?}  b: {:?}", state.ctx, self, against);
    match (self, against) {
      // 1I
      (Expr::Const(_), Type::JSON) => Ok(state),
      // ∀I
      (_, Type::Forall(α, box t)) => {
        let fresh_α = TypVar(state.fresh_mut());
        state.ctx.push(Item::Var(fresh_α));
        let t = t.clone().subst(Type::Var(fresh_α), *α);
        self
          .check(state, &t)
          .map(|θ| θ.drop_after(&Item::Var(fresh_α)))
      },
      // →I
      (Expr::Lam(x, e), Type::Arr(box t, box s)) => {
        state.ctx.push(Item::Ann(x.clone(), t.clone()));
        e.check(state, s)
      },
      // Sub
      _ => {
        let (state, typ) = self.synth(state)?;
        typ
          .apply_ctx(&state.ctx)
          .subtype_of(state.clone(), &against.apply_ctx(&state.ctx))
      },
    }
  }

  ///                   e.apply_type(Γ, A)  ≡  Γ ⊢ A ∙ e ⇒> C ⊣ Δ
  ///
  /// Under input context Γ, applying a function of type A to e synthesises
  /// type C, with output context Δ.
  fn apply_type(
    &self,
    mut state: State,
    typ: &Type,
  ) -> Result<(State, Type), TypeCheckError> {
    // println!("apply_type; ctx: {:?}  e: {:?}  b: {:?}", state.ctx, self, typ);
    match typ {
      // ∀App
      Type::Forall(α, box t) => {
        let α̂ = TypVar(state.fresh_mut());
        state.ctx.push(Item::Unsolved(α̂));
        self.apply_type(state, &t.clone().subst(Type::Exist(α̂), *α))
      },
      // α̂App
      Type::Exist(α̂) => {
        let α̂1 = TypVar(state.fresh_mut());
        let α̂2 = TypVar(state.fresh_mut());
        let state = state.insert_at(
          &Item::Unsolved(*α̂),
          &[
            Item::Unsolved(α̂2),
            Item::Unsolved(α̂1),
            Item::Solved(*α̂, Monotype::arr(Monotype::Exist(α̂1), Monotype::Exist(α̂2))),
          ],
        );
        let state = self.check(state, &Type::Exist(α̂1))?;
        Ok((state, Type::Exist(α̂2)))
      },
      // →App
      Type::Arr(t, box s) => Ok((self.check(state, t)?, s.clone())),
      // ERROR
      _ => Err(TypeCheckError::ApplicationError(self.clone(), typ.clone())),
    }
  }
}
