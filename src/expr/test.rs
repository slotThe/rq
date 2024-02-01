mod parser {
  use crate::expr::{app, arr, expr_str, lam, num, obj, parser::parse, var, Const::*, Expr::*};

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
    parse_eq!("(\\x -> x) 5", app(lam("x", var("x")), num(5.0)));
    parse_eq!("λx → x 5", lam("x", app(var("x"), num(5.0))));
    parse_eq!(
      "map (|x| x) [1]",
      app(app(var("map"), lam("x", var("x"))), arr(&[num(1.0)]))
    );
    parse_eq!(
      "|x| map (λy → [get 0 y, 2]) x",
      lam(
        "x",
        app(
          app(
            var("map"),
            lam(
              "y",
              arr(&[app(app(var("get"), num(0.0)), var("y")), num(2.0)])
            )
          ),
          var("x")
        )
      )
    );
    parse_eq!(
      "(get 0) [0]",
      app(app(var("get"), num(0.0)), arr(&[num(0.0)]))
    );
    parse_eq!(
      "   ( ( ((( get 0 ))   )) [0]   )  ",
      app(app(var("get"), num(0.0)), arr(&[num(0.0)]))
    );
    parse_eq!(
      "((((get  0)))) [ 0 ,1]",
      app(app(var("get"), num(0.0)), arr(&[num(0.0), num(1.0)]))
    );
  }

  #[test]
  fn whitespace() {
    parse_eq!("[ 1 ,   5]", arr(&[num(1.0), num(5.0)]));
    parse_eq!("[ 1,5    ]", arr(&[num(1.0), num(5.0)]));
    parse_eq!("[ 1, 5]", arr(&[num(1.0), num(5.0)]));
    parse_eq!("[ 1 ,5]", arr(&[num(1.0), num(5.0)]));
    parse_eq!("{\"a\":[1]}", obj(&[("a", arr(&[num(1.0)]))]));
    parse_eq!("{ \"a\"  :[1] }", obj(&[("a", arr(&[num(1.0)]))]));
    parse_eq!("{\"a\":  [1] }", obj(&[("a", arr(&[num(1.0)]))]));
    parse_eq!("(  \\x  -> x )", lam("x", var("x")));
    parse_eq!("(|x|x)", lam("x", var("x")));
    parse_eq!("(λ x→x )", lam("x", var("x")));
  }

  #[test]
  fn lambda_variants() {
    let lambda = lam("x", var("x"));
    parse_eq!("\\x -> x", lambda.clone());
    parse_eq!("λx → x", lambda.clone());
    parse_eq!("|  x  | x", lambda);
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
  fn objects_and_arrays() {
    parse_eq!(
      "|f| f { \"name\": { \"first\": 42, \"second\": [ 10, null ] } } 5",
      lam(
        "f",
        app(
          app(
            var("f"),
            obj(&[(
              "name",
              obj(&[
                ("second", arr(&[num(10.0), Const(Null)])),
                ("first", num(42.0)),
              ])
            )])
          ),
          num(5.0)
        )
      )
    )
  }

  #[test]
  fn dot_patterns() {
    parse_eq!(
      "\\x -> x.1.a.\"b\".c.4",
      lam(
        "x",
        app(
          app(var("get"), num(4.0)),
          app(
            app(var("get"), expr_str(r"c")),
            app(
              app(var("get"), expr_str("b")),
              app(
                app(var("get"), expr_str("a")),
                app(app(var("get"), num(1.0)), var("x"))
              )
            )
          )
        )
      )
    )
  }
}

mod evaluator {
  use std::collections::HashMap;

  use anyhow::Result;

  use crate::expr::{app, lam, var};

  #[test]
  fn app_const_id() -> Result<()> {
    assert_eq!(
      lam("y'", lam("x'", var("x'"))).desugar(),
      app(lam("x", lam("y", var("x"))), lam("x", var("x")))
        .check(&HashMap::new())?
        .eval(&HashMap::new())
    );
    Ok(())
  }
}
