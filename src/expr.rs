use std::{collections::BTreeMap, fmt::{self, Display}};

use self::de_bruijn::DBVar;
use crate::{eval::stdlib::Builtin, r#type::Type, util::{fmt_array, fmt_object, ord_f64::OrdF64}};

pub mod de_bruijn;
pub mod parser;
#[cfg(test)]
pub mod test;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Const {
  Null,
  Bool(bool),
  Num(OrdF64),
  String(String),
}

impl Display for Const {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Const::Num(n) => write!(f, "{n}"),
      Const::Bool(b) => write!(f, "{b}"),
      Const::Null => write!(f, "null"),
      Const::String(s) => write!(f, "\"{s}\""),
    }
  }
}

/// An expression as the user entered it (containing syntactic sugar).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
  Const(Const),
  /// A variable with its associated De Bruijn level.
  Var(DBVar),
  Lam(String, Box<Expr>),
  App(Box<Expr>, Box<Expr>),
  Arr(Vec<Expr>),
  Obj(BTreeMap<Expr, Expr>),
  IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
  Builtin(Builtin),
  Ann(Box<Expr>, Type),
}

impl Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    fn try_parens(expr: &Expr) -> String {
      if matches!(
        expr,
        Expr::Lam(_, _) | Expr::App(_, _) | Expr::IfThenElse(_, _, _) | Expr::Ann(_, _)
      ) {
        format!("({expr})")
      } else {
        format!("{expr}")
      }
    }
    match self {
      Expr::Const(c) => write!(f, "{c}"),
      Expr::Var(v) => write!(f, "{v}"),
      Expr::Lam(v, b) => write!(f, "λ{v}. {b}"),
      Expr::App(box Expr::App(box Expr::App(g, w), x), y) => write!(
        f,
        "{g} {} {} {}",
        try_parens(w),
        try_parens(x),
        try_parens(y)
      ),
      Expr::App(box Expr::App(g, x), y) => {
        write!(f, "{g} {} {}", try_parens(x), try_parens(y))
      },
      Expr::App(g, x) => write!(f, "{} {}", try_parens(g), try_parens(x)),
      Expr::Arr(xs) => fmt_array(xs, f),
      Expr::Obj(hm) => fmt_object(hm, f),
      Expr::IfThenElse(i, t, e) => write!(f, "if {i} then {t} else {e}"),
      Expr::Builtin(b) => write!(f, "{b}"),
      Expr::Ann(e, t) => write!(f, "{e} ∷ {t}"),
    }
  }
}

// Constructing expressions.
pub fn app(e1: Expr, e2: Expr) -> Expr { Expr::App(Box::new(e1), Box::new(e2)) }
pub fn var(v: &str) -> Expr { Expr::Var(DBVar::from_pair(v, 0)) }
pub fn λ(h: &str, b: Expr) -> Expr { Expr::Lam(h.to_string(), Box::new(b)) }
pub fn if_then_else(i: Expr, t: Expr, e: Expr) -> Expr {
  Expr::IfThenElse(Box::new(i), Box::new(t), Box::new(e))
}
pub fn num(n: impl Into<OrdF64>) -> Expr { Expr::Const(Const::Num(n.into())) }
pub fn expr_str<S: ToString>(s: S) -> Expr { Expr::Const(Const::String(s.to_string())) }
#[cfg(test)]
pub fn arr(xs: &[Expr]) -> Expr { Expr::Arr(xs.to_vec()) }
#[cfg(test)]
pub fn obj(xs: &[(&str, Expr)]) -> Expr {
  Expr::Obj(xs.iter().map(|(k, v)| (expr_str(k), v.clone())).collect())
}
#[cfg(test)]
pub fn var_ix(v: &str, ix: isize) -> Expr { Expr::Var(DBVar::from_pair(v, ix)) }
#[cfg(test)]
pub fn ann(e: Expr, t: Type) -> Expr { Expr::Ann(Box::new(e), t) }
