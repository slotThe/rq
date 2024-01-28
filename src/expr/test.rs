///////////////////////////////////////////////////////////////////////
///// Parser tests

mod parser {
  use crate::expr::{app, arr, lam, num, obj, parser::parse, var, Const::*, Expr::*};

  macro_rules! parse_eq {
  ($left:expr, $right:expr $(,)?) => {
    assert_eq!(&parse($left), &Ok($right))
  };
  ($left:expr, $right:expr, $($arg:tt)+) => {
    assert_eq!(&parse($left), &Ok($right), $($arg)+)
  };
}

  #[test]
  fn applications() {
    parse_eq!("(\\x -> x) 5", app(lam("x", var("x")), num(5.0)),);
    parse_eq!("λx → x 5", lam("x", app(var("x"), num(5.0))));
  }

  #[test]
  fn lambda_variants() {
    let lambda = lam("x", var("x"));
    parse_eq!("\\x -> x", lambda.clone());
    parse_eq!("λx → x", lambda.clone());
    parse_eq!("|x| x", lambda);
  }

  #[test]
  fn higher_order_application() {
    parse_eq!(
      "\\x -> \\y -> x y 2",
      lam("x", lam("y", app(app(var("x"), var("y")), num(2.0))))
    )
  }

  #[test]
  #[allow(non_snake_case)]
  fn parses_S_combinator() {
    parse_eq!(
      "λf -> λg → \\x → f x (g x)",
      lam(
        "f",
        lam(
          "g",
          lam("x", app(app(var("f"), var("x")), app(var("g"), var("x"))))
        )
      )
    )
  }

  #[test]
  fn parses_objects_and_arrays() {
    parse_eq!(
      "|f| f { \"name\": { \"first\": 42, \"second\": [ 10, null ] } }",
      lam(
        "f",
        app(
          var("f"),
          obj(&[(
            "name",
            obj(&[
              ("first", num(42.0)),
              ("second", arr(&[num(10.0), Const(Null)]))
            ])
          )])
        )
      )
    )
  }
}

mod evaluator {
  use anyhow::Result;

  use crate::expr::{app, lam, var};

  #[test]
  fn app_const_id() -> Result<()> {
    assert_eq!(
      lam("y'", lam("x'", var("x'"))).desugar(),
      app(lam("x", lam("y", var("x"))), lam("x", var("x")))
        .check()?
        .eval()
    );
    Ok(())
  }
}
