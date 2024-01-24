#[cfg(test)]
use anyhow::Result;

#[cfg(test)]
use crate::{expr::parser::parse, r#type::{TVar, Type}};

#[test]
#[allow(non_snake_case)]
fn infers_type_of_S_combinator() -> Result<()> {
  use Type::*;
  let expr = parse("λf -> λg → \\x → f x (g x)")?;
  let typ = expr.check();
  assert_eq!(
    typ,
    Ok(Arr(
      Box::new(Arr(
        Box::new(Var(TVar(3))),
        Box::new(Arr(Box::new(Var(TVar(5))), Box::new(Var(TVar(6)))))
      )),
      Box::new(Arr(
        Box::new(Arr(Box::new(Var(TVar(3))), Box::new(Var(TVar(5))))),
        Box::new(Arr(Box::new(Var(TVar(3))), Box::new(Var(TVar(6))))),
      ))
    ))
  );
  Ok(())
}
