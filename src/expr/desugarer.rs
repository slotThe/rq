use std::{collections::HashMap, fmt::{self, Display}};

use super::{fmt_array, fmt_object, Const, Expr, Var};

/// A desugared expression.
#[derive(Debug, Clone, PartialEq)]
pub enum DExpr {
  Const(Const),
  Var(Var),
  Lam(Var, Box<DExpr>),
  App(Box<DExpr>, Box<DExpr>),
  Arr(Vec<DExpr>),
  Obj(HashMap<String, DExpr>),
}

impl Display for DExpr {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      DExpr::Const(c) => write!(f, "{c}"),
      DExpr::Var(v) => write!(f, "{v}"),
      DExpr::Lam(v, b) => write!(f, "λ{v}. {b}"),
      DExpr::App(g, x) => write!(f, "({g})[{x}]"),
      DExpr::Arr(xs) => fmt_array(xs, f),
      DExpr::Obj(hm) => fmt_object(hm, f),
    }
  }
}

impl Expr {
  // Desugar the given expression.
  pub fn desugar(self) -> DExpr {
    match self {
      Expr::Const(c) => DExpr::Const(c),
      Expr::Var(v) => DExpr::Var(v),
      Expr::Lam(v, e) => DExpr::Lam(v, Box::new(e.desugar())),
      Expr::App(f, x) => DExpr::App(Box::new(f.desugar()), Box::new(x.desugar())),
      Expr::Arr(xs) => DExpr::Arr(xs.into_iter().map(|x| x.desugar()).collect()),
      Expr::Obj(hm) => {
        DExpr::Obj(hm.into_iter().map(|(k, v)| (k, v.desugar())).collect())
      },
    }
  }
}
