use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{desugarer::DExpr, Const};
use crate::{r#type::checker::TCExpr, stdlib::Builtin};

impl TCExpr {
  /// Evaluate a type-checked expression into its normal form.
  pub fn eval(&self, env: &HashMap<&str, Builtin>) -> DExpr {
    self.expr.desugar()
      // Normalisation by evaluation.
      .to_sem(&Rc::new(RefCell::new(
        env.iter()
          .map(|(v, e)| (v.to_string(), Sem::SBuiltin(*e)))
          .collect(),
      )))
      .reify()
  }
}

// XXX: This might be better as a Vec<String, Sem>, because values with the
// same name might occur deeper inside of expressions. However, I've
// completely ignored DeBruijn indices for now, so that would need to be
// addressed first.
type Env = Rc<RefCell<HashMap<String, Sem>>>;

/// A semantic representation of a term.
#[derive(Debug, Clone)]
enum Sem {
  Var(String),
  SConst(Const),
  Closure(Env, String, Box<DExpr>),
  App(Box<Sem>, Box<Sem>),
  Arr(Vec<Sem>),
  Obj(HashMap<String, Sem>),
  SBuiltin(Builtin),
}

fn app(s1: &Sem, s2: &Sem) -> Sem { Sem::App(Box::new(s1.clone()), Box::new(s2.clone())) }

/// Convert an expression into a semantic version of itself.
impl DExpr {
  fn to_sem(&self, env: &Env) -> Sem {
    match self {
      DExpr::Const(c) => Sem::SConst(c.clone()),
      DExpr::Lam(h, b) => Sem::Closure(env.clone(), h.to_string(), b.clone()),
      DExpr::App(f, x) => f.to_sem(env).apply(&x.to_sem(env)),
      DExpr::Arr(xs) => Sem::Arr(xs.iter().map(|x| x.to_sem(env)).collect()),
      DExpr::Obj(ob) => {
        Sem::Obj(ob.iter().map(|(k, v)| (k.clone(), v.to_sem(env))).collect())
      },
      DExpr::Builtin(f) => Sem::SBuiltin(*f),
      DExpr::Var(v) => env
        .borrow()
        .get(v)
        .unwrap_or_else(|| {
          panic!(
            "Variable {v} not in scope—you've hit a bug in the type checker! Please \
             report this to the appropriate places."
          )
        })
        .clone(),
    }
  }
}

impl Sem {
  /// Apply self to x.
  fn apply(&self, x: &Sem) -> Sem {
    use Builtin::*;
    use Sem::*;
    match (self, x) {
      (Closure(env, v, b), _) => {
        env.borrow_mut().insert(v.clone(), x.clone()); // Associate the bound variable to x
        b.to_sem(env)
      },
      (SBuiltin(Id), _) => x.clone(),
      (SBuiltin(_), _) => app(self, x),
      (App(box SBuiltin(BConst), this), _) => *this.clone(),
      (App(box SBuiltin(Get), box SConst(Const::Num(i))), Arr(xs)) => {
        xs[*i as usize].clone()
      },
      (App(box SBuiltin(Get), box SConst(Const::String(s))), Obj(ob)) => {
        ob.get(s).unwrap().clone()
      },
      _ => app(self, x),
    }
  }

  /// Reify a semantic expression.
  fn reify(&self) -> DExpr {
    fn go(names: &mut Vec<String>, sem: &Sem) -> DExpr {
      match sem {
        Sem::Var(v) => DExpr::Var(v.clone()),
        Sem::SConst(c) => DExpr::Const(c.clone()),
        Sem::Closure(env, v, b) => {
          let fresh_v = v.clone() + "'"; // TODO: find something better.
          names.push(fresh_v.clone());
          env
            .borrow_mut()
            .insert(v.clone(), Sem::Var(fresh_v.clone()));
          DExpr::Lam(fresh_v, Box::new(go(names, &b.to_sem(env))))
        },
        Sem::App(f, x) => DExpr::App(Box::new(go(names, f)), Box::new(go(names, x))),
        Sem::Arr(xs) => DExpr::Arr(xs.iter().map(|x| go(names, x)).collect()),
        Sem::Obj(ob) => {
          DExpr::Obj(ob.iter().map(|(k, v)| (k.clone(), go(names, v))).collect())
        },
        Sem::SBuiltin(f) => DExpr::Builtin(*f),
      }
    }
    go(&mut Vec::new(), self)
  }
}
