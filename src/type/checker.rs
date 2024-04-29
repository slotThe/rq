//! Bidirectional type checking:
//!
//! - Jana Dunfield and Neelakantan R. Krishnaswami. 2013.
//!   "Complete and easy bidirectional typechecking for higher-rank polymorphism."
//!   In Proc. of the 18th ACM SIGPLAN int. conf. on Functional programming (ICFP '13).
//!   Association for Computing Machinery, New York, NY, USA, 429–442.
//!   <https://doi.org/10.1145/2500365.2500582>
//!
//! The article is readily available [on the arXiv](https://arxiv.org/abs/1306.6032).

use super::{context::{Item, State}, error::{TResult, TypeCheckError}, Exist, Monotype, Type};
use crate::expr::{Const, Expr, {self}};

impl Type {
  ///                   A.well_formed_under(Γ)  ≡  Γ ⊢ A
  ///
  /// Under context Γ, type A is well-formed.
  fn well_formed_under(&self, ctx: &[Item]) -> TResult<()> {
    match self {
      // UnitWF
      Type::Num => Ok(()),
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
      // ListWF
      Type::List(t) => t.well_formed_under(ctx),
    }
  }

  ///                   A.subtype_of(Γ, B)  ≡  Γ ⊢ A <: B ⊣ Δ
  ///
  /// Under input context Γ, type A is a subtype of B, with output context Δ.
  /// Figure 9.
  fn subtype_of(&self, state: &mut State, b: &Type) -> TResult<()> {
    // println!("subtype; ctx: {:?}  a: {:?}  b: {:?}", state.ctx, self, b);
    match (self, b) {
      // <:Var
      (Type::Var(α), Type::Var(β)) if α == β => {
        self.well_formed_under(&state.ctx)?;
        Ok(())
      },
      // <:JSON ≡ <:Unit
      (Type::JSON, Type::JSON) => Ok(()),
      // A number is a subtype of a JSON object.
      (Type::Num, Type::Num | Type::JSON) => Ok(()),
      // Lists are covariant.
      (Type::List(t1), Type::List(t2)) => t1.subtype_of(state, t2),
      (Type::List(t1), t2) => t1.subtype_of(state, t2),
      // <:→
      (Type::Arr(a1, a2), Type::Arr(b1, b2)) => {
        b1.subtype_of(state, a1)?; /* Contravariant! */
        // See Note [Apply]
        a2.apply_ctx(&state.ctx)
          .subtype_of(state, &b2.apply_ctx(&state.ctx))
      },
      // <:∀R
      (t, Type::Forall(α, box s)) => {
        // Type variables are assumed to be unique, so introducing a fresh one
        // here is not necessary.
        state.scoped_around(&[Item::Var(α.clone())], 0, |state| t.subtype_of(state, s))
      },
      // <:∀l
      (Type::Forall(α, box t), s) => {
        let fresh_α̂ = Exist(state.fresh_mut());
        state.scoped_around(
          &[Item::Marker(fresh_α̂), Item::Unsolved(fresh_α̂)],
          0,
          |state| {
            t.clone()
              .subst(Type::Exist(fresh_α̂), α)
              .subtype_of(state, s)
          },
        )
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
  fn instantiate_l(self, state: &mut State, typ: Type) -> TResult<()> {
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
          state.scoped_around(&[Item::Var(α.clone())], 0, |state| {
            self.instantiate_l(state, t.clone())
          })
        },
        // Lists behave like InstLArr.
        Type::List(box t) => {
          let α̂ = Exist(state.fresh_mut());
          state.replace_with(
            &Item::Unsolved(self),
            &[
              Item::Unsolved(α̂),
              Item::Solved(self, Monotype::list(Monotype::Exist(α̂))),
            ],
          );
          α̂.instantiate_l(state, t.apply_ctx(&state.ctx))
        },
        _ => Err(TypeCheckError::InstantiationError(self, typ)),
      },
    }
  }

  ///                   α̂.instantiate_r(Γ, A)  ≡  Γ ⊢ A =:< α̂ ⊣ Δ
  ///
  /// Under input context Γ, instantiate α̂ such that A <: α̂, with output
  /// context Δ.
  fn instantiate_r(self, state: &mut State, typ: Type) -> TResult<()> {
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
        // Lists behave like InstRArr.
        Type::List(box t) => {
          let α̂ = Exist(state.fresh_mut());
          state.replace_with(
            &Item::Unsolved(self),
            &[
              Item::Unsolved(α̂),
              Item::Solved(self, Monotype::list(Monotype::Exist(α̂))),
            ],
          );
          α̂.instantiate_l(state, t.apply_ctx(&state.ctx))
        },
        // InstRAIIL
        Type::Forall(α, box t) => {
          let fresh_α̂ = Exist(state.fresh_mut());
          state.scoped_around(
            &[Item::Marker(fresh_α̂), Item::Unsolved(fresh_α̂)],
            0,
            |state| self.instantiate_r(state, t.clone().subst(Type::Exist(fresh_α̂), &α)),
          )
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
  pub fn synth(&self, state: &mut State) -> TResult<Type> {
    // println!("infer; ctx: {:?}  e: {:?}", state.ctx, self);
    match self {
      // 1l⇒
      Expr::Const(Const::Num(_)) => Ok(Type::Num),
      Expr::Const(_) | Expr::Obj(_) => Ok(Type::JSON),
      Expr::Builtin(b) => expr::var(&format!("{b}")).synth(state),
      // List
      Expr::Arr(xs) => {
        if xs.is_empty() {
          // An empty lists means it can have any type.
          let α̂ = Exist(state.fresh_mut());
          state.ctx.push(Item::Unsolved(α̂));
          Ok(Type::list(Type::Exist(α̂)))
        } else {
          // A non-empty list means we infer the first type, and check every
          // element in the list against it.
          let type_head = xs[0].synth(state)?;
          for el in &xs[1..] {
            el.check(state, &type_head.apply_ctx(&state.ctx))?;
          }
          Ok(Type::list(type_head))
        }
      },
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
        None => match state.stdlib.get(v.name.as_str()) {
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
        state.scoped_around(
          &[
            Item::Unsolved(α̂),
            Item::Unsolved(β̂),
            Item::Ann(x.to_string(), Type::Exist(α̂)),
          ],
          2,
          |state| e.check(state, &Type::Exist(β̂)),
        )?;
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
        let then_type = t.synth(state)?.apply_ctx(&state.ctx);
        e.check(state, &then_type)?;
        Ok(then_type)
      },
    }
  }

  ///                   e.check(Γ, A)  ≡  Γ ⊢ e ⇐ A ⊣ Δ
  ///
  /// Under input context Γ, e checks against input type A, with output
  /// context Δ.
  fn check(&self, state: &mut State, against: &Type) -> TResult<()> {
    // println!("check; ctx: {:?}  e: {:?}  b: {:?}", state.ctx, self, against);
    match (self, against) {
      // 1I
      (Expr::Const(Const::Num(_)), Type::JSON | Type::Num) => Ok(()),
      (Expr::Const(_), Type::JSON) => Ok(()),
      // As in `synth`, lists are just treated as JSON lists for now.
      (Expr::Arr(xs), Type::List(t)) => {
        for el in xs {
          el.check(state, t)?;
        }
        Ok(())
      },
      // ∀I
      (_, Type::Forall(α, box t)) => {
        // Type variables are assumed to be unique, so introducing a fresh one
        // here is not necessary.
        state.scoped_around(&[Item::Var(α.clone())], 0, |state| self.check(state, t))
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
  fn apply_type(&self, state: &mut State, typ: &Type) -> TResult<Type> {
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
