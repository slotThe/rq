use std::collections::BTreeMap;

use crate::{expr::parser::parse, r#type::{arr, TVar, Type}};

#[test]
#[allow(non_snake_case)]
fn infer_type_of_S_combinator() {
  use Type::*;
  let expr = parse("λf -> λg → |x| f x (g x)").unwrap();
  let typ = expr.infer(&BTreeMap::new());
  assert_eq!(
    typ,
    Ok(arr(
      arr(Var(TVar(0)), arr(Var(TVar(1)), Var(TVar(2)))),
      arr(
        arr(Var(TVar(0)), Var(TVar(1))),
        arr(Var(TVar(0)), Var(TVar(2))),
      )
    ))
  );
}
