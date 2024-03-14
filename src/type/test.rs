use std::collections::BTreeMap;

use crate::{eval::stdlib::STDLIB_TYPES, expr::{de_bruijn::DBVar, parser::parse}, r#type::{error::TypeCheckError, Type}};

macro_rules! check_eq {
  ($left:expr, $right:expr $(,)?) => {
    assert_eq!(
      &parse($left).type_check(&STDLIB_TYPES),
      &Ok($right)
    )
  };
  ($left:expr, $right:expr, $($arg:tt)+) => {
    assert_eq!(
      &parse($left).infer(&STDLIB_TYPES),
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
      "j",
      Type::forall(
        "i",
        Type::forall(
          "g",
          Type::arr(
            Type::arr(Type::var("g"), Type::arr(Type::var("i"), Type::var("j"))),
            Type::arr(
              Type::arr(Type::var("g"), Type::var("i")),
              Type::arr(Type::var("g"), Type::var("j"))
            )
          )
        )
      )
    )
  );
}

#[test]
fn de_bruijn() {
  let expr = parse("|x| x@1");
  assert_eq!(
    expr.type_check(&BTreeMap::new()),
    Err(TypeCheckError::VariableNotInScope(DBVar::from_pair("x", 1))),
    "|x| x@1 should not be in scope"
  );
}
