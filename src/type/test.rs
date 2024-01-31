use std::collections::HashMap;

use anyhow::{anyhow, Result};

use crate::{expr::parser::parse, r#type::{arr, TVar, Type}};

#[test]
#[allow(non_snake_case)]
fn infer_type_of_S_combinator() -> Result<()> {
  use Type::*;
  let expr = parse("λf -> λg → |x| f x (g x)").unwrap();
  let typ = expr.infer(&HashMap::new());
  assert_eq!(
    typ,
    Ok(arr(
      arr(Var(TVar(3)), arr(Var(TVar(5)), Var(TVar(6)))),
      arr(
        arr(Var(TVar(3)), Var(TVar(5))),
        arr(Var(TVar(3)), Var(TVar(6))),
      )
    ))
  );
  Ok(())
}
