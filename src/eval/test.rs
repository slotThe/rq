#[rustfmt::skip]
mod evaluator {
  use std::assert_matches::assert_matches;

  use crate::{eval::stdlib::{STDLIB_CTX, STDLIB_TYPES}, expr::{arr, lam, num, obj, parser::parse, var, Expr}};

  macro_rules! eval_eq {
    ($left:expr, $right:expr $(,)?) => {
      assert_eq!(
        &parse($left).unwrap().check(&STDLIB_TYPES).unwrap().eval(&STDLIB_CTX),
        &Ok($right)
      )
    };
    ($left:expr, $right:expr, $($arg:tt)+) => {
      assert_eq!(
        &parse($left).unwrap().check(&STDLIB_TYPES).unwrap().eval(&STDLIB_CTX),
        &Ok($right),
        $($arg)+
      )
    };
  }

  macro_rules! eval_err {
    ($left:expr) => {
      assert_matches!(
        &parse($left).unwrap().check(&STDLIB_TYPES).unwrap().eval(&STDLIB_CTX),
        &Err(_)
      )
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
      "Simple get"
    );
    eval_eq!(
      "(map (\\x -> { if x.name then 3 else 1: x.id })) [{ name: \"3\", id: 0 }]",
      arr(&[Expr::Obj([(num(3.0), num(0.0))].into_iter().collect())]),
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
    eval_eq!("2 * (if 2 < 2 + 1 then 2 else 1)", num(4.0));
    eval_eq!("2 * if 2 < 2 + 1 then 2 else 1", num(4.0));
    eval_eq!("0 * 2 + 3", num(3.0));
    eval_eq!("2 + ((|y| y 1) id) * ((λx → x 3) id)", num(5.0))
  }

  #[test]
  fn higher_order_functions() {
    eval_eq!("foldl (+) 0 [1, 2, 3, 4]", num(10.0));
    eval_eq!("map (- 1) [1, 2, 3, 4]", arr(&[num(0.0), num(1.0), num(2.0), num(3.0)]));
  }
}
