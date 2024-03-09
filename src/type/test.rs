use std::collections::BTreeMap;

use crate::{eval::stdlib::STDLIB_TYPES, expr::{de_bruijn::DBVar, parser::parse}, r#type::{error::TypeCheckError, TypVar, Type}};

macro_rules! check_eq {
  ($left:expr, $right:expr $(,)?) => {
    assert_eq!(
      &parse($left).unwrap().type_check(&STDLIB_TYPES),
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
  check_eq!(
    "λf -> λg → |x| f x (g x)",
    Type::forall(
      TypVar(2),
      Type::forall(
        TypVar(1),
        Type::forall(
          TypVar(0),
          Type::arr(
            Type::arr(Type::var(0), Type::arr(Type::var(1), Type::var(2))),
            Type::arr(
              Type::arr(Type::var(0), Type::var(1)),
              Type::arr(Type::var(0), Type::var(2))
            )
          )
        )
      )
    )
  );
}

#[test]
fn de_bruijn() {
  let expr = parse("|x| x@1").unwrap();
  assert_eq!(
    expr.type_check(&BTreeMap::new()),
    Err(TypeCheckError::VariableNotInScope(DBVar::from_pair("x", 1))),
    "|x| x@1 should not be in scope"
  );
}
