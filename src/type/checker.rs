//! Bidirectional type checking:
//!
//! - Jana Dunfield and Neelakantan R. Krishnaswami. 2013.
//!   "Complete and easy bidirectional typechecking for higher-rank polymorphism."
//!   In Proc. of the 18th ACM SIGPLAN int. conf. on Functional programming (ICFP '13).
//!   Association for Computing Machinery, New York, NY, USA, 429–442.
//!   <https://doi.org/10.1145/2500365.2500582>
//!
//! The article is readily available [on the arXiv](https://arxiv.org/abs/1306.6032).

use std::collections::BTreeMap;

use super::{context::{Item, State}, error::TypeCheckError, Exist, Monotype, Type};
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
    self.duplicate_type_vars()?;
    // We use a mutable state instead of manually passing the context
    // around. This is super scary to me, but I did want to learn Rust :)
    let mut state = State::new(stdlib.clone());
    let typ = self.synth(&mut state)?;
    Ok(typ.finish(&state.ctx))
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
  ///                   A.well_formed_under(Γ)  ≡  Γ ⊢ A
  ///
  /// Under context Γ, type A is well-formed.
  fn well_formed_under(&self, ctx: &[Item]) -> Result<(), TypeCheckError> {
    match self {
      // UnitWF
      Type::JSON => Ok(()),
      // UvarWF
      Type::Var(α) => {
        if ctx.contains(&Item::Var(α.clone())) {
          Ok(())
        } else {
          Err(TypeCheckError::TypeVariableNotInScope(α.clone()))
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
      Type::Forall(α, t) => {
        t.well_formed_under(&[ctx, &[Item::Var(α.clone())]].concat())
      },
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
  fn subtype_of(&self, state: &mut State, b: &Type) -> Result<(), TypeCheckError> {
    // println!("subtype; ctx: {:?}  a: {:?}  b: {:?}", state.ctx, self, b);
    match (self, b) {
      // <:Var
      (Type::Var(α), Type::Var(β)) if α == β => {
        self.well_formed_under(&state.ctx)?;
        Ok(())
      },
      // <:JSON ≡ <:Unit
      (Type::JSON, Type::JSON) => Ok(()),
      // <:→
      (Type::Arr(a1, a2), Type::Arr(b1, b2)) => {
        b1.subtype_of(state, a1)?;
        // See Note [Apply]
        a2.apply_ctx(&state.ctx)
          .subtype_of(state, &b2.apply_ctx(&state.ctx))
      },
      // <:∀R
      (t, Type::Forall(α, box s)) => {
        // Type variables are assumed to be unique, so introducing a fresh one
        // here is not necessary.
        state.ctx.push(Item::Var(α.clone()));
        t.subtype_of(state, s)?;
        state.drop_after(&Item::Var(α.clone()));
        Ok(())
      },
      // <:∀l
      (Type::Forall(α, box t), s) => {
        let fresh_α̂ = Exist(state.fresh_mut());
        state
          .ctx
          .extend_from_slice(&[Item::Marker(fresh_α̂), Item::Unsolved(fresh_α̂)]);
        t.clone()
          .subst(Type::Exist(fresh_α̂), α)
          .subtype_of(state, s)?;
        state.drop_after(&Item::Marker(fresh_α̂));
        Ok(())
      },
      // <:Exvar
      (Type::Exist(α̂), Type::Exist(β̂)) if α̂ == β̂ => Ok(()),
      // <:InstantiateL
      (Type::Exist(α̂), s) if !α̂.unsolved_in(s) => α̂.instantiate_l(state, s.clone()),
      // <:InstantiateR
      (t, Type::Exist(α̂)) if !α̂.unsolved_in(t) => α̂.instantiate_r(state, t.clone()),
      // ERROR
      _ => Err(TypeCheckError::NotASubtype(self.clone(), b.clone())),
    }
  }
}

impl Exist {
  /// InstLSolve and InstRSolve: this is symmetric for both InstantiateL
  /// and InstantiateR.
  fn inst_solve(self, state: &mut State, typ: &Type) -> Option<()> {
    typ.clone().to_mono().and_then(|τ| {
      let (ctx_l, _) = state
        .ctx
        .split_once(|i| *i == Item::Unsolved(self))
        .unwrap();
      typ.well_formed_under(ctx_l).ok().map(|_| {
        state.replace_with(&Item::Unsolved(self), &[Item::Solved(self, τ)]);
      })
    })
  }

  ///                   α̂.instantiateL(Γ, A) ≡ Γ ⊢ α̂ :=< A ⊣ Δ
  ///
  /// Under input context Γ, instantiate α̂ such that α̂ <: A, with output
  /// context Δ.
  fn instantiate_l(self, state: &mut State, typ: Type) -> Result<(), TypeCheckError> {
    // println!("instantiateL; ctx: {:?}  α̂: {:?}  a: {:?}", state.ctx, self, typ);
    match self.inst_solve(state, &typ) {
      Some(()) => Ok(()),
      None => match typ {
        // InstLReach
        Type::Exist(β̂)
          if Item::Unsolved(self).left_of(&Item::Unsolved(β̂), &state.ctx) =>
        {
          state.replace_with(
            &Item::Unsolved(β̂),
            &[Item::Solved(β̂, Monotype::Exist(self))],
          );
          Ok(())
        },
        // InstLArr
        Type::Arr(box t, box s) => {
          let α̂1 = Exist(state.fresh_mut());
          let α̂2 = Exist(state.fresh_mut());
          state.replace_with(
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
          α̂1.instantiate_r(state, t)?;
          α̂2.instantiate_l(state, s.apply_ctx(&state.ctx))
        },
        // InstLAIIR
        Type::Forall(α, box t) => {
          // Type variables are assumed to be unique, so introducing a fresh one
          // here is not necessary.
          state.ctx.push(Item::Var(α.clone()));
          self.instantiate_l(state, t)?;
          state.drop_after(&Item::Var(α));
          Ok(())
        },
        _ => Err(TypeCheckError::InstantiationError(self, typ)),
      },
    }
  }

  ///                   α̂.instantiate_r(Γ, A)  ≡  Γ ⊢ A =:< α̂ ⊣ Δ
  ///
  /// Under input context Γ, instantiate α̂ such that A <: α̂, with output
  /// context Δ.
  fn instantiate_r(self, state: &mut State, typ: Type) -> Result<(), TypeCheckError> {
    // println!("instantiateR; ctx: {:?}  a: {:?}  α̂: {:?}", state.ctx, typ, self);
    match self.inst_solve(state, &typ) {
      Some(()) => Ok(()),
      None => match typ {
        // InstRReach
        Type::Exist(_) => self.instantiate_l(state, typ),
        // InstRArr
        Type::Arr(box t, box s) => {
          let α̂1 = Exist(state.fresh_mut());
          let α̂2 = Exist(state.fresh_mut());
          state.replace_with(
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
          α̂1.instantiate_l(state, t)?;
          α̂2.instantiate_r(state, s.apply_ctx(&state.ctx))
        },
        // InstRAIIL
        Type::Forall(α, box t) => {
          let fresh_α̂ = Exist(state.fresh_mut());
          state
            .ctx
            .extend([Item::Marker(fresh_α̂), Item::Unsolved(fresh_α̂)]);
          self.instantiate_r(state, t.subst(Type::Exist(fresh_α̂), &α))?;
          state.drop_after(&Item::Marker(fresh_α̂));
          Ok(())
        },
        _ => Err(TypeCheckError::InstantiationError(self, typ)),
      },
    }
  }
}

impl Expr {
  ///                   e.synth(Γ)  ≡  Γ ⊢ e ⇒ A ⊣ Δ
  ///
  /// Under input context Γ, e synthesizes (infers) output type A, with output
  /// context Δ.
  fn synth(&self, state: &mut State) -> Result<Type, TypeCheckError> {
    // println!("infer; ctx: {:?}  e: {:?}", state.ctx, self);
    match self {
      // 1l⇒
      Expr::Const(_) | Expr::Arr(_) | Expr::Obj(_) => Ok(Type::JSON),
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
        Some((_, t)) => Ok(t.clone()),
        None => match state.stdlib.get(&v.name) {
          Some(t) => Ok(t.clone()),
          None => Err(TypeCheckError::VariableNotInScope(v.clone())),
        },
      },
      // Anno
      Expr::Ann(e, t) => {
        t.well_formed_under(&state.ctx)?;
        e.check(state, t)?;
        Ok(t.clone())
      },
      // →I⇒
      Expr::Lam(x, e) => {
        let α̂ = Exist(state.fresh_mut());
        let β̂ = Exist(state.fresh_mut());
        let ann = Item::Ann(x.to_string(), Type::Exist(α̂));
        state
          .ctx
          .extend([Item::Unsolved(α̂), Item::Unsolved(β̂), ann.clone()]);
        e.check(state, &Type::Exist(β̂))?;
        state.drop_after(&ann);
        Ok(Type::arr(Type::Exist(α̂), Type::Exist(β̂)))
      },
      // →E
      Expr::App(f, x) => {
        let t = f.synth(state)?.apply_ctx(&state.ctx);
        x.apply_type(state, &t)
      },
      //
      Expr::IfThenElse(i, t, e) => {
        i.check(state, &Type::JSON)?;
        let then_type = t.synth(state)?;
        e.check(state, &then_type)?;
        Ok(then_type)
      },
    }
  }

  ///                   e.check(Γ, A)  ≡  Γ ⊢ e ⇐ A ⊣ Δ
  ///
  /// Under input context Γ, e checks against input type A, with output
  /// context Δ.
  fn check(&self, state: &mut State, against: &Type) -> Result<(), TypeCheckError> {
    // println!("check; ctx: {:?}  e: {:?}  b: {:?}", state.ctx, self, against);
    match (self, against) {
      // 1I
      (Expr::Const(_), Type::JSON) => Ok(()),
      // ∀I
      (_, Type::Forall(α, box t)) => {
        // Type variables are assumed to be unique, so introducing a fresh one
        // here is not necessary.
        state.ctx.push(Item::Var(α.clone()));
        self.check(state, t)?;
        state.drop_after(&Item::Var(α.clone()));
        Ok(())
      },
      // →I
      (Expr::Lam(x, e), Type::Arr(box t, box s)) => {
        state.ctx.push(Item::Ann(x.clone(), t.clone()));
        e.check(state, s)
      },
      // Sub
      _ => self.synth(state)?  // Γ ⊢ e ⇒ A ⊣ θ
        // See Note [Apply]
        .apply_ctx(&state.ctx)
        .subtype_of(state, &against.apply_ctx(&state.ctx)),
    }
  }

  ///                   e.apply_type(Γ, A)  ≡  Γ ⊢ A ∙ e ⇒> C ⊣ Δ
  ///
  /// Under input context Γ, applying a function of type A to e synthesises
  /// type C, with output context Δ.
  fn apply_type(&self, state: &mut State, typ: &Type) -> Result<Type, TypeCheckError> {
    // println!("apply_type; ctx: {:?}  e: {:?}  b: {:?}", state.ctx, self, typ);
    match typ {
      // ∀App
      Type::Forall(α, box t) => {
        let α̂ = Exist(state.fresh_mut());
        state.ctx.push(Item::Unsolved(α̂));
        self.apply_type(state, &t.clone().subst(Type::Exist(α̂), α))
      },
      // α̂App
      Type::Exist(α̂) => {
        let α̂1 = Exist(state.fresh_mut());
        let α̂2 = Exist(state.fresh_mut());
        state.replace_with(
          &Item::Unsolved(*α̂),
          &[
            Item::Unsolved(α̂2),
            Item::Unsolved(α̂1),
            Item::Solved(*α̂, Monotype::arr(Monotype::Exist(α̂1), Monotype::Exist(α̂2))),
          ],
        );
        self.check(state, &Type::Exist(α̂1))?;
        Ok(Type::Exist(α̂2))
      },
      // →App
      Type::Arr(t, box s) => {
        self.check(state, t)?;
        Ok(s.clone())
      },
      // ERROR
      _ => Err(TypeCheckError::ApplicationError(self.clone(), typ.clone())),
    }
  }
}

/* Note [Apply]

This is slightly difficult to read, but the alternative is just so much
more verbose that it does not seem worth it. Shortly, a call like

    a2.apply_ctx(&state.ctx)
      .subtype_of(state, &b2.apply_ctx(&state.ctx))

implements this kind of judgement:

    θ ⊢ [θ]A₂ <: [θ]B₂ ⊣ Δ

The point is that the argument of a function are evaluated before the
function call, so while `subtype_of` does mutate the state afterwards,
both substitutions—[θ]A₂ and [θ]B₂—happen with the same state.

 */
