use std::collections::BTreeMap;

use crate::{eval::stdlib::STDLIB_TYPES, expr::{de_bruijn::DBVar, parser::parse}, r#type::{arr, checker::TypeCheckError, TVar, Type}};

macro_rules! check_eq {
  ($left:expr, $right:expr $(,)?) => {
    assert_eq!(
      &parse($left).unwrap().infer(&STDLIB_TYPES),
      &Ok($right)
    )
  };
  ($left:expr, $right:expr, $($arg:tt)+) => {
    assert_eq!(
      &parse($left).unwrap().infer(&STDLIB_TYPES),
      &Ok($right),
      $($arg)+
    )
  };
}

#[test]
#[allow(non_snake_case)]
fn infer_type_of_S_combinator() {
  use Type::*;
  check_eq!(
    "λf -> λg → |x| f x (g x)",
    arr(
      arr(Var(TVar(0)), arr(Var(TVar(1)), Var(TVar(2)))),
      arr(
        arr(Var(TVar(0)), Var(TVar(1))),
        arr(Var(TVar(0)), Var(TVar(2))),
      )
    )
  );
}

#[test]
fn de_bruijn() {
  let expr = parse("|x| x@1").unwrap();
  assert_eq!(
    expr.infer(&BTreeMap::new()),
    Err(TypeCheckError::VariableNotInScope(DBVar::from_pair("x", 1))),
    "|x| x@1 should not be in scope"
  );
}
