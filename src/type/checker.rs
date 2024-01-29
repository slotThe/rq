use std::collections::HashMap;

use thiserror::Error;

use super::{arr, t_or, TVar, Type};
use crate::expr::{Const, Expr};

/// A type-checked expression.
#[derive(Debug)]
pub struct TCExpr {
  pub expr: Expr,
}

impl Expr {
  // Verify that the given expression has a valid type.
  pub fn check(&self, ctx: &HashMap<String, Type>) -> Result<TCExpr, TypeCheckError> {
    self.infer(ctx)?;
    Ok(TCExpr { expr: self.clone() })
  }

  /// Infer the type of an expression. Returns the desugared expression with
  /// its associated type.
  pub fn infer(&self, ctx: &HashMap<String, Type>) -> Result<Type, TypeCheckError> {
    let mut state = State {
      ctx:  ctx.clone(),
      tvar: TVar(0),
    };
    let (raw_type, mut constrs) = gather_constraints(&mut state, self)?;
    Ok(raw_type.refine(&constrs.unify()?))
  }
}

#[derive(Debug, Clone, Error, PartialEq)]
pub enum TypeCheckError {
  #[error("variable not in scope: {0}")]
  VariableNotInScope(String),
  #[error("can't unify {0} with {1}")]
  UnificationError(Type, Type),
  #[error("Occurs check: can't construct infinite type: {0} ≡ {1}")]
  OccursCheck(Type, Type),
}

/// A single substitution comprises a type variable and its refined type.
type Substitutions = HashMap<TVar, Type>;

impl Type {
  /// Refine a type variable according to a context.
  fn refine(
    &self,                // Type to refine according to that context.
    subs: &Substitutions, // Substitutions; i.e., possible refinements.
  ) -> Type {
    match self {
      Type::JSON | Type::Str | Type::Num | Type::Array | Type::Obj => self.clone(),
      Type::Var(tv) => subs.get(tv).unwrap_or(self).clone(),
      Type::Arr(t1, t2) => arr(t1.refine(subs), t2.refine(subs)),
      Type::Or(t1, t2) => t_or(t1.refine(subs), t2.refine(subs)),
    }
  }
}

/// An association list of type constraints. An element (x, y) represents
/// a constraint of the form x ≡ y.
#[derive(Debug, Clone)]
struct Constraints(Vec<(Type, Type)>);

impl FromIterator<(Type, Type)> for Constraints {
  fn from_iter<T: IntoIterator<Item = (Type, Type)>>(iter: T) -> Self {
    Constraints(iter.into_iter().collect::<Vec<_>>())
  }
}

impl Type {
  /// Check if the two given types unify.
  fn unifies_with(&self, t2: &Type) -> bool {
    self == t2
      || matches!(
        (self, t2),
        // This is left biased so we get a hacky version of subtyping: e.g.,
        // it's always fine to treat Num like a JSON, but not the other way
        // around.
        (Type::Num, Type::JSON)
          | (Type::Str, Type::JSON)
          | (Type::Array, Type::JSON)
          | (Type::Obj, Type::JSON)
      )
  }
}

impl Constraints {
  /// Create a new constraint mapping.
  fn new() -> Constraints { Constraints(Vec::new()) }

  /// Unify the given constraints and produce a refinement mapping.
  fn unify(&mut self) -> Result<Substitutions, TypeCheckError> {
    match self.0.pop() {
      None => Ok(HashMap::new()),
      Some((t1, t2)) => {
        if t1.unifies_with(&t2) {
          self.unify()
        } else {
          let unify_error = Err(TypeCheckError::UnificationError(t1.clone(), t2.clone()));
          // Unify a coproduct given constraints for the left and right side.
          let mut unify_or = |l, r| -> Result<Substitutions, TypeCheckError> {
            let mut constraints_l = self.clone();
            constraints_l.0.push(l);
            self.0.push(r);
            self
              .unify()
              .or_else(|_| constraints_l.unify().or(unify_error.clone()))
          };
          match (t1.clone(), t2.clone()) {
            (Type::Var(v), _) => self.substitute(v, &t2), // refine
            (_, Type::Var(v)) => self.substitute(v, &t1), // refine
            (Type::Arr(t1, t2), Type::Arr(t3, t4)) => {
              self.0.extend_from_slice(&[(*t1, *t3), (*t2, *t4)]); // add new constraints
              self.unify()
            },
            (Type::Or(t3, t4), _) => unify_or((*t3, t2.clone()), (*t4, t2.clone())),
            (_, Type::Or(t3, t4)) => unify_or((t2.clone(), *t3), (t2.clone(), *t4)),
            _ => unify_error,
          }
        }
      },
    }
  }

  /// Substitute the given type variable by its refined type.
  fn substitute(
    &mut self,  // Constraints possibly containing the type variable
    var: TVar,  // The type variable to refine
    typ: &Type, // Its refined type
  ) -> Result<Substitutions, TypeCheckError> {
    if var.occurs_in(typ) {
      Err(TypeCheckError::OccursCheck(Type::Var(var), typ.clone()))
    } else {
      let refinement: &Substitutions = &HashMap::from([(var, typ.clone())]);
      let mut unified: Substitutions = self
        .0
        .iter()
        .map(|(ty1, ty2)| (ty1.refine(refinement), ty2.refine(refinement)))
        .collect::<Constraints>()
        .unify()?;
      unified.entry(var).or_insert_with(|| typ.clone());
      Ok(unified)
    }
  }
}

#[derive(Clone)]
struct State {
  /// Typing context containing resolved constraints of the form
  /// variable → its type.
  ctx:  HashMap<String, Type>,
  /// Number of type variables in use.
  tvar: TVar,
}

impl State {
  /// Create a fresh new type variable, and increment the respective
  /// counter in the state.
  fn fresh_mut(&mut self) -> TVar {
    self.tvar.0 += 1;
    self.tvar
  }
}

impl Expr {
  fn to_json_type(&self) -> Type {
    match self {
      Expr::Const(Const::Num(_)) => Type::Num,
      Expr::Const(Const::String(_)) => Type::Str,
      _ => Type::JSON,
    }
  }
}

/// Infer the type of an expression and gather constraints on it.
fn gather_constraints(
  state: &mut State,
  expr: &Expr, // Gather constraints on the type of this.
) -> Result<(Type, Constraints), TypeCheckError> {
  match expr {
    Expr::Const(_) => Ok((expr.to_json_type(), Constraints::new())),
    Expr::Var(v) => match state.ctx.get(v) {
      None => Err(TypeCheckError::VariableNotInScope(v.clone())),
      Some(v) => Ok((v.clone(), Constraints::new())),
    },
    Expr::Arr(exprs) => Ok((
      Type::Array,
      Constraints(exprs.iter().try_fold(Vec::new(), |mut acc, e| {
        let (t_e, mut con_e) = gather_constraints(state, e)?;
        acc.append(&mut con_e.0);
        acc.push((t_e, e.to_json_type()));
        Ok(acc)
      })?),
    )),
    Expr::Obj(hm) => {
      let cons = hm.iter().try_fold(Vec::new(), |mut acc, (_, v)| {
        let (type_v, mut con_v) = gather_constraints(state, v)?;
        acc.append(&mut con_v.0);
        acc.push((type_v, v.to_json_type()));
        Ok(acc)
      })?;
      Ok((Type::Obj, Constraints(cons)))
    },
    Expr::Lam(var, body) => {
      let tv = Type::Var(state.fresh_mut());
      state.ctx.insert(var.clone(), tv.clone());
      let (ret_type, constrs) = gather_constraints(state, body)?;
      Ok((arr(tv, ret_type), constrs))
    },
    Expr::App(f, x) => {
      let (type_f, con_f) = gather_constraints(state, f)?;
      let (type_x, con_x) = gather_constraints(state, x)?;
      let ret_type = Type::Var(state.fresh_mut());
      let mut constraints = Constraints([con_f.0, con_x.0].concat());
      constraints.0.push((type_f, arr(type_x, ret_type.clone())));
      Ok((ret_type, constraints))
    },
  }
}
