use core::fmt;
use std::fmt::Display;

use super::ord_f64::OrdF64;

/// A constant value.
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
