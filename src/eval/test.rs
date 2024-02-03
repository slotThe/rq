#[rustfmt::skip]
mod evaluator {
  use crate::{eval::stdlib::{STDLIB_CTX, STDLIB_TYPES}, expr::{lam, num, obj, parser::parse, var, arr, Expr}};

  macro_rules! eval_eq {
  ($left:expr, $right:expr $(,)?) => {
    assert_eq!(
      &parse($left).unwrap().check(&STDLIB_TYPES).unwrap().eval(&STDLIB_CTX),
      &$right)
  };
  ($left:expr, $right:expr, $($arg:tt)+) => {
    assert_eq!(
      &parse($left).unwrap().check(&STDLIB_TYPES).unwrap().eval(&STDLIB_CTX),
      &$right,
      $($arg)+)
  };
}

  #[test]
  fn app_const_id() {
    eval_eq!("(\\x -> \\y -> x) (\\x -> x)", lam("y'", lam("x'", var("x'"))));
  }

  #[test]
  fn eval_if() {
    eval_eq!("if ((\\x -> x.this) {this: 3}) then 1 else 4", num(1.0));
  }

  #[test]
  fn eval_obj_key() {
    eval_eq!(
      "(\\x -> { x.name: x.id }) { name: \"a\", id: 0 }",
      obj(&[("a", num(0.0))]),
      "Simple get");
    eval_eq!(
      "(map (\\x -> { if x.name then 3 else 1: x.id })) [{ name: \"3\", id: 0 }]",
      arr(&[Expr::Obj([(num(3.0), num(0.0))].into_iter().collect())]),
      "Nested get");
  }
}
