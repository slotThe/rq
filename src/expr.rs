use std::{collections::HashMap, fmt::{self, Display}};

pub mod desugarer;
pub mod evaluator;
pub mod json;
pub mod parser;
#[cfg(test)]
pub mod test;

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
  Num(f64),
  Bool(bool),
  Null,
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
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
  Const(Const),
  Var(String),
  Lam(String, Box<Expr>),
  App(Box<Expr>, Box<Expr>),
  Arr(Vec<Expr>),
  Obj(HashMap<String, Expr>),
}

impl Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Expr::Const(c) => write!(f, "{c}"),
      Expr::Var(v) => write!(f, "{v}"),
      Expr::Lam(v, b) => write!(f, "λ{v}. {b}"),
      Expr::App(g, x) => write!(f, "({g})⟨{x}⟩"),
      Expr::Arr(xs) => fmt_array(xs, f),
      Expr::Obj(hm) => fmt_object(hm, f),
    }
  }
}

fn fmt_object<T: Display>(
  hm: &HashMap<String, T>,
  f: &mut fmt::Formatter,
) -> fmt::Result {
  write!(
    f,
    "{{ {} }}",
    hm.iter()
      .map(|(k, v)| k.to_owned() + ": " + &v.to_string())
      .intersperse(", ".to_string())
      .collect::<String>(),
  )
}

fn fmt_array<T: Display>(xs: &[T], f: &mut fmt::Formatter) -> fmt::Result {
  write!(
    f,
    "[ {} ]",
    xs.iter()
      .map(|x| x.to_string())
      .intersperse(", ".to_string())
      .collect::<String>()
  )
}

// Constructing expressions.

pub fn app(e1: Expr, e2: Expr) -> Expr { Expr::App(Box::new(e1), Box::new(e2)) }
#[cfg(test)]
pub fn lam(h: &str, b: Expr) -> Expr { Expr::Lam(h.to_string(), Box::new(b)) }
#[cfg(test)]
pub fn var(v: &str) -> Expr { Expr::Var(v.to_string()) }
#[cfg(test)]
pub fn num(n: f64) -> Expr { Expr::Const(Const::Num(n)) }
#[cfg(test)]
pub fn arr(xs: &[Expr]) -> Expr { Expr::Arr(xs.to_vec()) }
#[cfg(test)]
pub fn obj(xs: &[(&str, Expr)]) -> Expr {
  Expr::Obj(xs.iter().map(|(k, v)| (k.to_string(), v.clone())).collect())
}
