#[rustfmt::skip]
mod evaluator {
  use std::assert_matches::assert_matches;

  use crate::{eval::stdlib::{STDLIB_CTX, STDLIB_TYPES}, expr::{arr, expr_str, num, obj, parser::parse, var, λ, Expr}, TCExpr};

  macro_rules! eval_eq {
    ($left:expr, $right:expr $(,)?) => {
      assert_eq!(
        &TCExpr::new(parse($left), &STDLIB_TYPES).unwrap().eval(&STDLIB_CTX),
        &Ok($right)
      )
    };
    ($left:expr, $right:expr, $($arg:tt)+) => {
      assert_eq!(
        &TCExpr::new(parse($left), &STDLIB_TYPES).unwrap().eval(&STDLIB_CTX),
        &Ok($right),
        $($arg)+
      )
    };
  }

  macro_rules! eval_err {
    ($left:expr) => {
      assert_matches!(
        &TCExpr::new(parse($left), &STDLIB_TYPES).unwrap().eval(&STDLIB_CTX),
        &Err(_)
      )
    };
  }

  #[test]
  fn app_const_id() {
    eval_eq!("(\\x -> \\y -> x) (\\x -> x)", λ("y", λ("x", var("x"))));
  }

  #[test]
  fn eval_if() {
    eval_eq!("if ((\\x -> x.this) {this: 3}) then 1 else 4", num(1));
  }

  #[test]
  fn eval_obj_key() {
    eval_eq!(
      "(\\x -> { x.name: x.id }) { name: \"a\", id: 0 }",
      obj(&[("a", num(0))]),
      "Simple get"
    );
    eval_eq!(
      "(map (\\x -> { if x.name then 3 else 1: x.id })) [{ name: \"3\", id: 0 }]",
      arr(&[Expr::Obj([(num(3), num(0))].into_iter().collect())]),
      "Nested get"
    );
  }

  #[test]
  fn index_errors() {
    eval_err!("get 1 [0]");
    eval_err!("get \"this\" (get 0 [{that:3}])");
  }

  #[test]
  fn eval_bin_ops() {
    eval_eq!("2 * (if 2 < 2 + 1 then 2 else 1)", num(4));
    eval_eq!("2 * if 2 < 2 + 1 then 2 else 1", num(4));
    eval_eq!("0 * 2 + 3.2", num(3.2));
    eval_eq!("2 + ((|y| y 1) id) * ((λx → x 3) id)", num(5))
  }

  #[test]
  fn higher_order_functions() {
    eval_eq!("foldl (+) 0 [1, 2, 3, 4]", num(10));
    eval_eq!("map (- 1) [1, 2, 3, 4]", arr(&[num(0), num(1), num(2), num(3)]));
    eval_eq!("map (1 +) [1, 2, 3, 4]", arr(&[num(2), num(3), num(4), num(5)]));
    eval_eq!("(filter (get \"age\" | (>= 42)) | map .age | foldl (+) 0) [{name: \"A\", age: 43},{name:\"B\"},{name:\"C\", age:42}]", num(85));
    eval_eq!("set 1 2 [ 1, 10, 3, 4 ]", arr(&[num(1), num(2), num(3), num(4)]));
    eval_eq!("(λx → set 1 x.name [ 1, 10, 3, 4 ]) { name: \"bob\" }", arr(&[num(1), expr_str("bob"), num(3), num(4)]));
  }

}
