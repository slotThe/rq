use std::io::Write;

use crate::{expr::{de_bruijn::DBVar, Expr}, r#type::Type, util::style};

#[derive(Debug, Clone, thiserror::Error, miette::Diagnostic, PartialEq)]
pub enum EvalError {
  #[error("Wrong index: {0} not found in {1}")]
  #[diagnostic(help = "check the index you wanted to use")]
  WrongIndex(String, Expr),
}

#[derive(Debug, Clone, thiserror::Error, miette::Diagnostic, PartialEq)]
pub enum TypeCheckError {
  #[error("Variable not in scope: {0}")]
  #[diagnostic(help = "check the name of the variable for typos")]
  VariableNotInScope(DBVar),
  #[error("Can't unify {} with {} in expression {}", style(.0), style(.1), style(.2))]
  #[diagnostic(help = "check that function arguments for type errors")]
  UnificationError(Type, Type, Expr),
  #[error(
    "Occurs check: can't construct infinite type {} ≡ {} in expression {}",
    style(.0), style(.1), style(.2)
  )]
  #[diagnostic(help = "check the logic of your error for infinite recursion")]
  OccursCheck(Type, Type, Expr),
}

pub fn print_err(
  out_handle: &mut std::io::StdoutLock<'_>,
  err: impl Into<miette::Report>,
) -> anyhow::Result<()> {
  let diagnostic = err.into();
  let mut error_str = String::new();

  miette::GraphicalReportHandler::new()
    .render_report(&mut error_str, diagnostic.as_ref())?;

  writeln!(out_handle, "{error_str}")?;
  Ok(())
}
