//! Type check errors.

use std::{error::Error, fmt::Display};

use super::{TypVar, Type};
use crate::{expr::{de_bruijn::DBVar, Expr}, util::style};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeCheckError {
  TypeVariableNotInScope(String),
  MalformedType(Type),
  NotASubtype(Type, Type),
  InstantiationError(TypVar, Type),
  VariableNotInScope(DBVar),
  ApplicationError(Expr, Type),
  /// Internal error, does not abort the type checker.
  InstSolveError,
  // XXX: Might we think of something better here?
}

impl Error for TypeCheckError {}

impl Display for TypeCheckError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      TypeCheckError::TypeVariableNotInScope(v) => {
        write!(f, "Type variable not in scope: {}", style(v))
      },
      TypeCheckError::MalformedType(t) => write!(f, "Malformed type: {}", style(t)),
      TypeCheckError::NotASubtype(t1, t2) => {
        write!(f, "{} is not a subtype of {}", style(t1), style(t2))
      },
      TypeCheckError::InstantiationError(tv, t) => {
        write!(
          f,
          "Can't instantiate type variable {} to type {}",
          style(tv),
          style(t)
        )
      },
      TypeCheckError::VariableNotInScope(v) => {
        write!(f, "Variable not in scope: {}", style(v))
      },
      TypeCheckError::ApplicationError(e, t) => {
        write!(
          f,
          "Not a function type: {} with type {}",
          style(e),
          style(t)
        )
      },
      TypeCheckError::InstSolveError => unreachable!(),
    }
  }
}
