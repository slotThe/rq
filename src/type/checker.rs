use std::{collections::{BTreeMap, BTreeSet, HashMap}, convert::identity};

use thiserror::Error;

use super::{arr, TVar, Type};
use crate::{expr::{de_bruijn::{DBEnv, DBVar}, var, Expr}, util::style};

/// A type-checked expression.
#[derive(Debug)]
pub struct TCExpr {
  pub expr: Expr,
}

impl Expr {
  /// Verify that the given expression has a valid type.
  pub fn check(&self, ctx: &BTreeMap<String, Type>) -> Result<TCExpr, TypeCheckError> {
    self.infer(ctx)?;
    Ok(TCExpr { expr: self.clone() })
  }

  /// Infer the type of an expression. Returns the desugared expression with
  /// its associated type.
  pub fn infer(&self, ctx: &BTreeMap<String, Type>) -> Result<Type, TypeCheckError> {
    let mut state = State {
      ctx:  DBEnv::from_iter_with(ctx.clone(), identity),
      tvar: TVar(0),
    };
    let (raw_type, mut constrs) = gather_constraints(&mut state, self)?;
    let mut typ = raw_type.refine(&constrs.unify()?);
    typ.normalise_mut();
    Ok(typ)
  }
}

impl Type {
  /// Normalise all type variables; i.e., make them start at 0.
  fn normalise_mut(&mut self) {
    fn enum_tvars(tvars: &mut BTreeSet<TVar>, typ: &Type) {
      match typ {
        Type::JSON => {},
        Type::Var(tvar) => {
          tvars.insert(*tvar);
        },
        Type::Arr(t1, t2) => {
          enum_tvars(tvars, t1);
          enum_tvars(tvars, t2);
        },
      }
    }
    fn downscale(tvars: &HashMap<TVar, usize>, typ: &mut Type) {
      match typ {
        Type::Var(tvar) => {
          tvar.0 = *tvars.get(tvar).unwrap();
        },
        Type::JSON => {},
        Type::Arr(t1, t2) => {
          downscale(tvars, t1);
          downscale(tvars, t2);
        },
      }
    }
    let mut tvars = BTreeSet::new();
    enum_tvars(&mut tvars, self);
    downscale(&tvars.into_iter().zip(0..).collect(), self);
  }
}

#[derive(Debug, Clone, Error, PartialEq)]
pub enum TypeCheckError {
  #[error("Variable not in scope: {0}")]
  VariableNotInScope(DBVar),
  #[error("Can't unify {} with {} in expression {}", style(.0), style(.1), style(.2))]
  UnificationError(Type, Type, Expr),
  #[error(
    "Occurs check: can't construct infinite type {} ≡ {} in expression {}",
    style(.0), style(.1), style(.2)
  )]
  OccursCheck(Type, Type, Expr),
}

/// Normalise a type checking error and emit it.
/// type_error: TCE::Ident → Type → Type → Result<Type, TCE>
macro_rules! type_error {
  (VariableNotInScope, $v:expr $(,)?) => {
    Err(TypeCheckError::VariableNotInScope($v.clone()))
  };
  ($error_typ:ident, $t1:expr, $t2:expr, $e:expr $(,)?) => {{
    let mut t1n = $t1.clone();
    t1n.normalise_mut();
    let mut t2n = $t2.clone();
    t2n.normalise_mut();
    Err(TypeCheckError::$error_typ(t1n, t2n, $e))
  }};
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
      Type::JSON => self.clone(),
      Type::Var(tv) => subs.get(tv).unwrap_or(self).clone(),
      Type::Arr(t1, t2) => arr(t1.refine(subs), t2.refine(subs)),
    }
  }
}

/// An association list of type constraints. An element (x, y, e) represents a
/// constraint of the form x ≡ y in the expression e. The expression is merely
/// used to improve error messages, and does not weigh on the constraints in
/// any way.
#[derive(Debug, Clone)]
struct Constraints(Vec<(Type, Type, Expr)>);

impl FromIterator<(Type, Type, Expr)> for Constraints {
  fn from_iter<T: IntoIterator<Item = (Type, Type, Expr)>>(iter: T) -> Self {
    Constraints(iter.into_iter().collect::<Vec<_>>())
  }
}

impl Constraints {
  /// Create a new constraint mapping.
  fn new() -> Constraints { Constraints(Vec::new()) }

  /// Unify the given constraints and produce a refinement mapping.
  fn unify(&mut self) -> Result<Substitutions, TypeCheckError> {
    match self.0.pop() {
      None => Ok(HashMap::new()),
      Some((t1, t2, e)) => {
        if t1 == t2 {
          self.unify()
        } else {
          match (t1.clone(), t2.clone()) {
            (Type::Var(v), _) => self.substitute(v, &t2, e), // refine
            (_, Type::Var(v)) => self.substitute(v, &t1, e), // refine
            (Type::Arr(t1, t2), Type::Arr(t3, t4)) => {
              self
                .0
                .extend_from_slice(&[(*t1, *t3, e.clone()), (*t2, *t4, e.clone())]); // add new constraints
              self.unify()
            },
            _ => type_error!(UnificationError, t1, t2, e),
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
    expr: Expr, // The expression in which the substitution takes place—for context.
  ) -> Result<Substitutions, TypeCheckError> {
    if var.occurs_in(typ) {
      type_error!(OccursCheck, &Type::Var(var), typ, expr)
    } else {
      let refinement: &Substitutions = &HashMap::from([(var, typ.clone())]);
      let mut unified: Substitutions = self
        .0
        .iter()
        .map(|(ty1, ty2, e)| (ty1.refine(refinement), ty2.refine(refinement), e.clone()))
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
  ctx:  DBEnv<Type>,
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

/// Infer the type of an expression and gather constraints on it.
fn gather_constraints(
  state: &mut State,
  expr: &Expr, // Gather constraints on the type of this.
) -> Result<(Type, Constraints), TypeCheckError> {
  match expr {
    Expr::Const(_) => Ok((Type::JSON, Constraints::new())),
    Expr::Var(v) => match state.ctx.lookup_var(v) {
      None => type_error!(VariableNotInScope, v),
      Some(t) => Ok((t.clone(), Constraints::new())),
    },
    Expr::Arr(exprs) => Ok((
      Type::JSON,
      Constraints(exprs.iter().try_fold(Vec::new(), |mut acc, e| {
        let (t_e, mut con_e) = gather_constraints(state, e)?;
        acc.append(&mut con_e.0);
        acc.push((t_e, Type::JSON, e.clone()));
        Ok(acc)
      })?),
    )),
    Expr::Obj(hm) => {
      let cons = hm.iter().try_fold(Vec::new(), |mut acc, (k, v)| {
        let (type_k, con_k) = gather_constraints(state, k)?;
        let (type_v, con_v) = gather_constraints(state, v)?;
        acc.extend_from_slice(&[con_k.0, con_v.0].concat());
        acc.extend_from_slice(&[
          (type_k, Type::JSON, k.clone()),
          (type_v, Type::JSON, v.clone()),
        ]);
        Ok(acc)
      })?;
      Ok((Type::JSON, Constraints(cons)))
    },
    Expr::Lam(var, body) => {
      let tv = Type::Var(state.fresh_mut());
      state.ctx.add_mut(var, &tv);
      let (ret_type, constrs) = gather_constraints(state, body)?;
      Ok((arr(tv, ret_type), constrs))
    },
    Expr::App(f, x) => {
      let (type_f, con_f) = gather_constraints(state, f)?;
      let (type_x, con_x) = gather_constraints(state, x)?;
      let ret_type = Type::Var(state.fresh_mut());
      let mut constraints = Constraints([con_f.0, con_x.0].concat());
      constraints
        .0
        .push((type_f, arr(type_x, ret_type.clone()), expr.clone()));
      Ok((ret_type, constraints))
    },
    Expr::IfThenElse(i, t, e) => {
      let (type_i, con_i) = gather_constraints(state, i)?;
      let (type_t, con_t) = gather_constraints(state, t)?;
      let (type_e, con_e) = gather_constraints(state, e)?;
      let mut constraints = Constraints([con_i.0, con_t.0, con_e.0].concat());
      constraints.0.extend_from_slice(&[
        (type_t.clone(), type_e, expr.clone()),
        (Type::JSON, type_i, *i.clone()),
      ]);
      Ok((type_t, constraints))
    },
    Expr::Builtin(b) => gather_constraints(state, &var(b.show())),
  }
}
