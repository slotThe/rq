use std::{collections::BTreeMap, fmt::{self, Display}};

use ordered_float::OrderedFloat;

use crate::{eval::stdlib::Builtin, util::{fmt_array, fmt_object}};

pub mod json;
pub mod parser;
#[cfg(test)]
pub mod test;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Const {
  Null,
  Bool(bool),
  Num(OrderedFloat<f64>),
  String(String),
}

impl Display for Const {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Const::Num(n) => write!(f, "{n}"),
      Const::Bool(b) => write!(f, "{b}"),
      Const::Null => write!(f, "null"),
      Const::String(s) => write!(f, "{s}"),
    }
  }
}

/// An expression as the user entered it (containing syntactic sugar).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
  Const(Const),
  Var(String),
  Lam(String, Box<Expr>),
  App(Box<Expr>, Box<Expr>),
  Arr(Vec<Expr>),
  Obj(BTreeMap<Expr, Expr>),
  IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
  Builtin(Builtin),
}

impl Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Expr::Const(c) => write!(f, "{c}"),
      Expr::Var(v) => write!(f, "{v}"),
      Expr::Lam(v, b) => write!(f, "λ{v}. {b}"),
      Expr::App(box Expr::App(g, x), y) => write!(f, "({g} {x} {y})"),
      Expr::App(g, x) => write!(f, "({g} {x})"),
      Expr::Arr(xs) => fmt_array(xs, f),
      Expr::Obj(hm) => fmt_object(hm, f),
      Expr::IfThenElse(i, t, e) => write!(f, "if {i} then {t} else {e}"),
      Expr::Builtin(b) => write!(f, "{b}"),
    }
  }
}

// Constructing expressions.
pub fn app(e1: Expr, e2: Expr) -> Expr { Expr::App(Box::new(e1), Box::new(e2)) }
pub fn var(v: &str) -> Expr { Expr::Var(v.to_string()) }
pub fn lam(h: &str, b: Expr) -> Expr { Expr::Lam(h.to_string(), Box::new(b)) }
pub fn if_then_else(i: Expr, t: Expr, e: Expr) -> Expr {
  Expr::IfThenElse(Box::new(i), Box::new(t), Box::new(e))
}
pub fn num(n: f64) -> Expr { Expr::Const(Const::Num(OrderedFloat(n))) }
pub fn expr_str<S: ToString>(s: S) -> Expr { Expr::Const(Const::String(s.to_string())) }
#[cfg(test)]
pub fn arr(xs: &[Expr]) -> Expr { Expr::Arr(xs.to_vec()) }
#[cfg(test)]
pub fn obj(xs: &[(&str, Expr)]) -> Expr {
  Expr::Obj(xs.iter().map(|(k, v)| (expr_str(k), v.clone())).collect())
}
