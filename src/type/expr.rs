use std::collections::BTreeMap;

use super::{context::State, error::TResult, Type};
use crate::expr::{app, Expr};

/// A type-checked expression.
#[derive(Debug)]
pub struct TCExpr {
  pub expr: Expr,
}

impl TCExpr {
  /// Construct a [TCExpr] out of an [Expr].
  pub fn new(expr: Expr, ctx: &BTreeMap<String, Type>) -> TResult<Self> {
    expr.type_check(ctx)?;
    Ok(TCExpr { expr })
  }

  /// Apply an expression to a type-checked expression. Assumes that the added
  /// expression is well-formed—i.e., type-checks. This is essentially an
  /// optimisation, as type-checking a large JSON chunk may take a while.
  pub fn apply(self, json: Expr) -> Self {
    TCExpr {
      expr: app(self.expr, json),
    }
  }
}

impl Expr {
  /// Type check an expression.
  pub fn type_check(&self, stdlib: &BTreeMap<String, Type>) -> TResult<Type> {
    self.duplicate_type_vars()?;
    // We use a mutable state instead of manually passing the context
    // around. This is super scary to me, but I did want to learn Rust :)
    let mut state = State::new(stdlib.clone());
    let typ = self.synth(&mut state)?;
    Ok(typ.finish(&state.ctx))
  }
}
