//! Type checking errors.

use std::{collections::HashSet, error::Error, fmt::Display};

use super::{Exist, Type};
use crate::{expr::{de_bruijn::DBVar, Expr}, util::style};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeCheckError {
  DuplicateTypeVariable(String),
  TypeVariableNotInScope(String),
  MalformedType(Type),
  NotASubtype(Type, Type),
  InstantiationError(Exist, Type),
  VariableNotInScope(DBVar),
  ApplicationError(Expr, Type),
}

/// A value, or a type checking error.
pub type TResult<A> = Result<A, TypeCheckError>;

impl Error for TypeCheckError {}

impl Display for TypeCheckError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      TypeCheckError::DuplicateTypeVariable(α) => {
        write!(f, "Conflicting definitions for type variable {}", style(α))
      },
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
    }
  }
}

impl Expr {
  /// Check whether the expression contains duplicate type variables (in a
  /// single annotation).
  pub fn duplicate_type_vars(&self) -> TResult<()> {
    fn duplicates_in_type(typ: &Type, seen: &mut HashSet<String>) -> TResult<()> {
      match typ {
        Type::Forall(α, t) => {
          if seen.contains(α) {
            Err(TypeCheckError::DuplicateTypeVariable(α.clone()))
          } else {
            seen.insert(α.clone());
            duplicates_in_type(t, seen)
          }
        },
        Type::Arr(t, s) => {
          duplicates_in_type(t, seen)?;
          duplicates_in_type(s, seen)
        },
        _ => Ok(()),
      }
    }

    match self {
      Expr::Lam(_, e) => e.duplicate_type_vars(),
      Expr::App(f, x) => {
        f.duplicate_type_vars()?;
        x.duplicate_type_vars()
      },
      Expr::IfThenElse(i, t, e) => {
        i.duplicate_type_vars()?;
        t.duplicate_type_vars()?;
        e.duplicate_type_vars()
      },
      Expr::Ann(x, t) => {
        x.duplicate_type_vars()?;
        duplicates_in_type(t, &mut HashSet::new())
      },
      _ => Ok(()),
    }
  }
}
