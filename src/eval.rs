//! The evaluator, based on normalisation by evaluation.
//!
//! Thanks to the following people for providing helpful material in that
//! direction:
//!
//!   - [Normalisation by Evaluation] by David Christiansen
//!   - [Building Fast Functional Languages Fast] by Edward Kmett
//!   - Gabriella Gonzalez's fantastic [Fall-from-Grace] project
//!
//! [Normalisation by Evaluation]: https://www.youtube.com/watch?v=CpADWJa-f28&pp=ygUbbm9ybWFsaXNhdGlvbiBieSBldmFsdWF0aW9u
//! [Building Fast Functional Languages Fast]: https://www.youtube.com/watch?v=gbmURWs_SaU&pp=ygUiYnVpbGRpbmcgZnVuY3Rpb25hbCBsYW5ndWFnZXMgZmFzdA%3D%3D
//! [Fall-From-Grace]: https://github.com/Gabriella439/grace

use std::collections::BTreeMap;

use ordered_float::OrderedFloat;
use thiserror::Error;

use self::stdlib::Builtin;
use crate::{expr::{app, de_bruijn::{DBEnv, DBVar}, if_then_else, λ, Const, Expr}, r#type::checker::TCExpr};

pub mod stdlib;
#[cfg(test)]
pub mod test;

impl TCExpr {
  /// Evaluate a type-checked expression into its normal form.
  pub fn eval(&self, env: &BTreeMap<&str, Builtin>) -> Result<Expr, EvalError> {
    self
      .expr
      .to_sem(&DBEnv::from_iter_with(env.clone(), Sem::SBuiltin))?
      .reify()
  }
}

#[derive(Debug, Clone, Error, PartialEq)]
pub enum EvalError {
  #[error("Wrong index: {0} not found in {1}")]
  WrongIndex(String, Expr),
}

fn num_vars(names: &[String], var: &str) -> isize {
  names.iter().filter(|&v| v == var).count() as isize
}

/// Summon a new variable from the void.
fn fresh(var: &str, names: &[String]) -> Sem {
  Sem::Var(DBVar::from_pair(var, num_vars(names, var)))
}

/// A semantic representation of a term.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Sem {
  Var(DBVar),
  SConst(Const),
  Closure(DBEnv<Sem>, String, Box<Expr>),
  App(Box<Sem>, Box<Sem>),
  Arr(Vec<Sem>),
  Obj(BTreeMap<Sem, Sem>),
  IfThenElse(Box<Sem>, Box<Sem>, Box<Sem>),
  SBuiltin(Builtin),
}

impl Sem {
  fn app(&self, s2: &Sem) -> Sem {
    Sem::App(Box::new(self.clone()), Box::new(s2.clone()))
  }

  fn is_truthy(&self) -> bool {
    !matches!(
      self,
      Sem::SConst(Const::Bool(false)) | Sem::SConst(Const::Null)
    )
  }
}

impl Expr {
  /// Convert an expression into a semantic version of itself; i.e., evaluate
  /// the expression, but leave it in the semantic representation.
  fn to_sem(&self, env: &DBEnv<Sem>) -> Result<Sem, EvalError> {
    match self {
      Expr::Const(c) => Ok(Sem::SConst(c.clone())),
      Expr::Lam(h, b) => Ok(Sem::Closure(env.clone(), h.to_string(), b.clone())),
      Expr::App(f, x) => f.to_sem(env)?.apply(&x.to_sem(env)?),
      Expr::Arr(xs) => Ok(Sem::Arr(xs.iter().flat_map(|x| x.to_sem(env)).collect())),
      Expr::Obj(ob) => Ok(Sem::Obj(
        ob.iter()
          .flat_map(|(k, v)| -> Result<(Sem, Sem), EvalError> {
            try { (k.to_sem(env)?, v.to_sem(env)?) }
          })
          .collect(),
      )),
      Expr::Builtin(f) => Ok(Sem::SBuiltin(*f)),
      Expr::Var(v) => Ok(env.lookup_var(v).expect(
        "Variable {v} not in scope—you've hit a bug in the type checker! Please report \
         this to the appropriate places.",
      )),
      Expr::IfThenElse(i, t, e) => match i.to_sem(env) {
        Err(_) | Ok(Sem::SConst(Const::Bool(false))) | Ok(Sem::SConst(Const::Null)) => {
          e.to_sem(env)
        },
        Ok(Sem::SConst(_)) => t.to_sem(env),
        Ok(isem) => Ok(Sem::IfThenElse(
          Box::new(isem), // Evaluate further
          Box::new(t.to_sem(env)?),
          Box::new(e.to_sem(env)?),
        )),
      },
    }
  }
}

impl Sem {
  /// Apply self to x. This includes evaluating closures, as well as builtin
  /// functions.
  fn apply(&self, x: &Sem) -> Result<Sem, EvalError> {
    use Builtin::*;
    use Sem::*;
    match (self, x) {
      // Closure
      (Closure(env, v, b), _) => {
        env.add_mut(v, x); // Associate bound variable v to x.
        b.to_sem(env)
      },
      // Small builtin
      (SBuiltin(Id), _) => Ok(x.clone()),
      (SBuiltin(_), _) => Ok(self.app(x)),
      (App(box SBuiltin(BConst), this), _) => Ok(*this.clone()),
      // Get
      (App(box SBuiltin(Get), box SConst(Const::Num(OrderedFloat(i)))), Arr(xs)) => xs
        .get(*i as usize)
        .ok_or(EvalError::WrongIndex(i.to_string(), x.reify()?))
        .cloned(),
      (App(box SBuiltin(Get), box SConst(Const::String(s))), Obj(ob)) => ob
        .get(&SConst(Const::String(s.to_string())))
        .ok_or(EvalError::WrongIndex(s.to_string(), x.reify()?))
        .cloned(),
      // Map
      (App(box SBuiltin(Map), closure), Arr(xs)) => {
        Ok(Arr(xs.iter().flat_map(|x| closure.apply(x)).collect()))
      },
      (App(box SBuiltin(Map), closure), Obj(ob)) => Ok(Obj(
        ob.iter()
          .flat_map(|(k, v)| -> Result<(Sem, Sem), EvalError> {
            try { (k.clone(), closure.apply(v)?) }
          })
          .collect(),
      )),
      // Filter
      (App(box SBuiltin(Filter), closure), Arr(xs)) => Ok(Arr(
        xs.iter()
          .filter_map(|x| {
            if closure.apply(x).ok()?.is_truthy() {
              Some(x.clone())
            } else {
              None
            }
          })
          .collect(),
      )),
      (App(box SBuiltin(Filter), closure), Obj(ob)) => Ok(Obj(
        ob.iter()
          .filter_map(|(k, v)| {
            if closure.apply(v).ok()?.is_truthy() {
              Some((k.clone(), v.clone()))
            } else {
              None
            }
          })
          .collect(),
      )),
      // Foldl
      (App(box App(box SBuiltin(Foldl), closure), init), Arr(xs)) => xs
        .iter()
        .try_fold(*init.clone(), |acc, x| closure.apply(&acc)?.apply(x)),
      (App(box App(box SBuiltin(Foldl), closure), init), Obj(ob)) => ob
        .iter()
        .try_fold(*init.clone(), |acc, (_, v)| closure.apply(&acc)?.apply(v)),
      // Binary operators
      (App(box SBuiltin(Add), box SConst(Const::Num(n))), SConst(Const::Num(m))) => {
        Ok(SConst(Const::Num(*n + *m)))
      },
      (
        App(box SBuiltin(Add), box SConst(Const::String(s))),
        SConst(Const::String(t)),
      ) => Ok(SConst(Const::String(s.clone() + t.as_str()))),
      (App(box SBuiltin(Sub), box SConst(Const::Num(n))), SConst(Const::Num(m))) => {
        Ok(SConst(Const::Num(*n - *m)))
      },
      (App(box SBuiltin(Mul), box SConst(Const::Num(n))), SConst(Const::Num(m))) => {
        Ok(SConst(Const::Num(*n * *m)))
      },
      (App(box SBuiltin(Div), box SConst(Const::Num(n))), SConst(Const::Num(m))) => {
        Ok(SConst(Const::Num(*n / *m)))
      },
      (App(box SBuiltin(Eq), a), b) => Ok(SConst(Const::Bool(**a == *b))),
      (App(box SBuiltin(Neq), a), b) => Ok(SConst(Const::Bool(**a != *b))),
      (App(box SBuiltin(Le), a), b) => Ok(SConst(Const::Bool(**a < *b))),
      (App(box SBuiltin(Leq), a), b) => Ok(SConst(Const::Bool(**a <= *b))),
      (App(box SBuiltin(Ge), a), b) => Ok(SConst(Const::Bool(**a > *b))),
      (App(box SBuiltin(Geq), a), b) => Ok(SConst(Const::Bool(**a >= *b))),
      // Otherwise
      _ => Ok(self.app(x)),
    }
  }

  /// Reify a semantic expression.
  fn reify(&self) -> Result<Expr, EvalError> {
    fn go(names: &mut Vec<String>, sem: &Sem) -> Result<Expr, EvalError> {
      match sem {
        Sem::Var(DBVar { name, level }) => Ok(Expr::Var(
          // Since we are type-checked, this is never smaller than 0.
          // Also see [Note closure var counts]
          DBVar::from_pair(name.as_str(), num_vars(names, name) - level),
        )),
        Sem::SConst(c) => Ok(Expr::Const(c.clone())),
        Sem::Closure(env, v, b) => {
          names.push(v.clone());
          env.add_mut(v, &fresh(v, names)); // See [Note closure var counts]
          Ok(λ(v, go(names, &b.to_sem(env)?)?))
        },
        Sem::App(f, x) => Ok(app(go(names, f)?, go(names, x)?)),
        Sem::Arr(xs) => Ok(Expr::Arr(xs.iter().flat_map(|x| go(names, x)).collect())),
        Sem::Obj(ob) => Ok(Expr::Obj(
          ob.iter()
            .flat_map(|(k, v)| -> Result<(Expr, Expr), EvalError> {
              try { (go(names, k)?, go(names, v)?) }
            })
            .collect(),
        )),
        Sem::IfThenElse(i, t, e) => {
          Ok(if_then_else(go(names, i)?, go(names, t)?, go(names, e)?))
        },
        Sem::SBuiltin(f) => Ok(Expr::Builtin(*f)),
      }
    }
    go(&mut Vec::new(), self)
  }
}

/* [Note closure var counts]

Given the expression

    names.push(v.clone());
    env.add_mut(v, &fresh(v, names));

in a closure semantic expression, we already count the name of the inner
bound variable towards its index in the environment. This means that the
accompanying retransformation for the variable looks like

    num_vars(names, name) - level

If we instead turned this around and had

    let freshv = fresh(v, names);
    names.push(v.clone());
    env.add_mut(v, &freshv);

then we do *not* do that and would need a different retransformation:

    num_vars(names, name) - level - 1

Note: I hope I never run into the kinds of troubles where this information
is necessary.

*/
