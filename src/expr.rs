use std::{collections::HashMap, fmt::{self, Display}};

pub mod desugarer;
pub mod json;
pub mod parser;
pub mod test;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Const {
  Num(f64),
  Bool(bool),
  Null,
  // TODO: String
}

impl Display for Const {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Const::Num(n) => write!(f, "{n}"),
      Const::Bool(b) => write!(f, "{b}"),
      Const::Null => write!(f, "null"),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var(pub String);

impl Display for Var {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.0) }
}

/// An expression as the user entered it (containing syntactic sugar).
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
  Const(Const),
  Var(Var),
  Lam(Var, Box<Expr>),
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
      Expr::App(g, x) => write!(f, "({g})[{x}]"),
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
    "{{{}}}",
    hm.iter()
      .map(|(k, v)| k.to_owned() + ": " + &v.to_string())
      .intersperse(", ".to_string())
      .collect::<String>(),
  )
}

fn fmt_array<T: Display>(xs: &[T], f: &mut fmt::Formatter) -> fmt::Result {
  write!(
    f,
    "[{}]",
    xs.iter()
      .map(|x| x.to_string())
      .intersperse(", ".to_string())
      .collect::<String>()
  )
}
