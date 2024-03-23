#[rustfmt::skip]
mod parser {
  use crate::{expr::{ann, app, arr, expr_str, if_then_else, num, obj, parser::parse, var, var_ix, λ, Builtin::*, Const::*, Expr::*}, r#type::Type};

  macro_rules! parse_eq {
    ($left:expr, $right:expr $(,)?) => {
      assert_eq!(&parse($left), &$right)
    };
    ($left:expr, $right:expr, $($arg:tt)+) => {
      assert_eq!(&parse($left), &$right, $($arg)+)
    };
  }

  #[test]
  fn applications() {
    parse_eq!("(\\x -> x) 5", app(λ("x", var("x")), num(5)));
    parse_eq!("λx → x 5", λ("x", app(var("x"), num(5))));
    parse_eq!("map (|x| x) [1]", app(app(var("map"), λ("x", var("x"))), arr(&[num(1)])));
    parse_eq!("map (get 0 | (+ 1)) [[1],[2]]", app(app(var("map"), λ("x", app(λ("x", app(app(Builtin(Add), var("x")), num(1))), app(app(var("get"), num(0)), var("x"))))), arr(&[arr(&[num(1)]), arr(&[num(2)])])));
    parse_eq!("|x| map (λy → [get 0 y, 2]) x", λ("x", app(app(var("map"), λ("y", arr(&[app(app(var("get"), num(0)), var("y")), num(2)]))), var("x"))));
    parse_eq!("(get 0) [0]", app(app(var("get"), num(0)), arr(&[num(0)])));
    parse_eq!("   ( ( ((( get 0 ))   )) [0]   )  ", app(app(var("get"), num(0)), arr(&[num(0)])));
    parse_eq!("((((get  0)))) [ 0 ,1]", app(app(var("get"), num(0)), arr(&[num(0), num(1)])));
  }

  #[test]
  fn whitespace() {
    parse_eq!("[ 1 ,   5]", arr(&[num(1), num(5)]));
    parse_eq!("[ 1,5    ]", arr(&[num(1), num(5)]));
    parse_eq!("[ 1, 5]", arr(&[num(1), num(5)]));
    parse_eq!("[ 1 ,5]", arr(&[num(1), num(5)]));
    parse_eq!("{\"a\":[1]}", obj(&[("a", arr(&[num(1)]))]));
    parse_eq!("{ \"a\"  :[1] }", obj(&[("a", arr(&[num(1)]))]));
    parse_eq!("{\"a\":  [1] }", obj(&[("a", arr(&[num(1)]))]));
    parse_eq!("(  \\x  -> x )", λ("x", var("x")));
    parse_eq!("(|x|x)", λ("x", var("x")));
    parse_eq!("(λ x→x )", λ("x", var("x")));
    parse_eq!("(1+2)", app(app(Builtin(Add), num(1)), num(2)));
    parse_eq!("(  1 + 2  )", app(app(Builtin(Add), num(1)), num(2)));
    parse_eq!("1 < ( 2*4 )", app(app(Builtin(Le), num(1)), app(app(Builtin(Mul), num(2)), num(4))));
    parse_eq!("(  +    )", λ("x", λ("y", app(app(Builtin(Add), var("x")), var("y")))));
    parse_eq!("(  +  1  )", λ("x", app(app(Builtin(Add), var("x")), num(1))));
    parse_eq!("(+1)", λ("x", app(app(Builtin(Add), var("x")), num(1))));
    parse_eq!("(  1+   )", λ("y", app(app(Builtin(Add), num(1)), var("y"))));
    parse_eq!("(1+)", λ("y", app(app(Builtin(Add), num(1)), var("y"))));

  }

  #[test]
  fn lambda_variants() {
    let lambda = λ("x", var("x"));
    parse_eq!("\\x -> x", lambda.clone());
    parse_eq!("λx → x", lambda.clone());
    parse_eq!("|  x  | x", lambda);
    let lambda2 = λ("x", λ("y", var("x")));
    parse_eq!("λx y → x", lambda2.clone());
    parse_eq!("|x,y|x", lambda2.clone());
    parse_eq!("|  x ,   y | x", lambda2.clone());
  }

  #[test]
  fn higher_order_application() {
    parse_eq!("\\x -> \\y -> x y 2", λ("x", λ("y", app(app(var("x"), var("y")), num(2)))))
  }

  #[test]
  #[allow(non_snake_case)]
  fn parses_S_combinator() {
    parse_eq!("λf -> λg → \\x → f x (g x)", λ("f", λ("g", λ("x", app(app(var("f"), var("x")), app(var("g"), var("x")))))))}

  #[test]
  fn objects_and_arrays() {
    parse_eq!("|f| f { \"name\": { \"first\": 42, \"second\": [ 10, null ] } } 5", λ("f", app(app(var("f"), obj(&[("name", obj(&[("second", arr(&[num(10), Const(Null)])), ("first", num(42)),]))])), num(5))))
  }

  #[test]
  fn dot_patterns() {
    parse_eq!("\\x -> x.1.a.\"b\".c.4", λ("x", app(app(var("get"), num(4)), app(app(var("get"), expr_str("c")), app(app(var("get"), expr_str("b")), app(app(var("get"), expr_str("a")), app(app(var("get"), num(1)), var("x"))))))));
    parse_eq!(".0", λ("ω", app(app(var("get"), num(0)), var("ω"))));
    parse_eq!("map .0 [1]", app(app(var("map"), λ("ω", app(app(var("get"), num(0)), var("ω")))), arr(&[num(1)])));
    parse_eq!("filter (.0 | (= 1)) [[1],[2]]", app(app(var("filter"), λ("x", app(λ("x", app(app(Builtin(Eq), var("x")), num(1))), app(λ("ω", app(app(var("get"), num(0)), var("ω"))), var("x"))))), arr(&[arr(&[num(1)]), arr(&[num(2)])])));
    parse_eq!("|f, x| f x.1", λ("f", λ("x", app(var("f"), app(app(var("get"), num(1)), var("x"))))));
  }

  #[test]
  fn pipes() {
    parse_eq!("get 0 | get \"phones\" | get 0", λ("x", app(app(var("get"), num(0)), app(λ("x", app(app(var("get"), expr_str("phones")), app(app(var("get"), num(0)), var("x")))), var("x")))));
    parse_eq!("map (\\x -> x.id) | get 0", λ("x", app(app(var("get"), num(0)), app(app(var("map"), λ("x", app(app(var("get"), expr_str("id")), var("x")))), var("x")))));
    parse_eq!("get 0 | \\x -> { name: x.name }", λ("x", app(λ("x", obj(&[("name", app(app(var("get"), expr_str("name")), var("x")))])), app(app(var("get"), num(0)), var("x")))));
    parse_eq!("(get 0 | \\x -> { name: x.name }) [{name: 0}]", app(λ("x", app(λ("x", obj(&[("name", app(app(var("get"), expr_str("name")), var("x")))])), app(app(var("get"), num(0)), var("x")))), arr(&[obj(&[("name", num(0))])])));
    parse_eq!("get 0 | foldl (+) 0", λ("x", app(app(app(var("foldl"), λ("x", λ("y", app(app(Builtin(Add), var("x")), var("y"))))), num(0)), app(app(var("get"), num(0)), var("x")))));
    parse_eq!("(get 0 | foldl (+) 0) [[1]]", app(λ("x", app(app(app(var("foldl"), λ("x", λ("y", app(app(Builtin(Add), var("x")), var("y"))))), num(0)), app(app(var("get"), num(0)), var("x")))), arr(&[arr(&[num(1)])])));
  }

  #[test]
  fn parse_if_then_else() {
    parse_eq!("if null then 2 else 5", if_then_else(Const(Null), num(2), num(5)));
    parse_eq!("if (get \"this\" { this: 3 }) then 1 else 4", if_then_else(app(app(var("get"), expr_str("this")), obj(&[("this", num(3))])), num(1), num(4)));
    parse_eq!("if get \"this\" then 1 else 4", if_then_else(app(var("get"), expr_str("this")), num(1), num(4)));
    parse_eq!("if \\x -> map (\\y -> get 0 y) x then 1 else 4", if_then_else(λ("x", app(app(var("map"), λ("y", app(app(var("get"), num(0)), var("y")))), var("x"))), num(1), num(4)))
  }

  #[test]
  fn bin_ops_and_precedence() {
    parse_eq!("1 + 2 + 3",
              app(app(Builtin(Add), app(app(Builtin(Add), num(1)), num(2))), num(3)),
              "(1 + 2) + 3");
    parse_eq!("1 + 2 * 3",
              app(app(Builtin(Add), num(1)), app(app(Builtin(Mul), num(2)), num(3))),
              "1 + (2 * 3)");
    parse_eq!("\\x -> 1 * get 4 x + (  5   =  5  )",
              λ("x", app(app(Builtin(Add), app(app(Builtin(Mul), num(1)), app(app(var("get"), num(4)), var("x")))), app(app(Builtin(Eq), num(5)), num(5)))),
              "λx. (1 * get 4 x) + (5 = 5)");
    parse_eq!("1 < 4 + 5 = 5",
              app(app(Builtin(Eq), app(app(Builtin(Le), num(1)), app(app(Builtin(Add), num(4)), num(5)))), num(5)),
              "(1 < (4 + 5)) = 5");
  }

  #[test]
  fn operator_sections() {
    parse_eq!("(+)", λ("x", λ("y", app(app(Builtin(Add), var("x")), var("y")))));
    parse_eq!("(=)", λ("x", λ("y", app(app(Builtin(Eq), var("x")), var("y")))));
    parse_eq!("(*)", λ("x", λ("y", app(app(Builtin(Mul), var("x")), var("y")))));
    parse_eq!("(+ 1)", λ("x", app(app(Builtin(Add), var("x")), num(1))));
    parse_eq!("(1 +)", λ("y", app(app(Builtin(Add), num(1)), var("y"))));
    parse_eq!("foldl (+) 0 [1,2,3,4]", app(app(app(var("foldl"), λ("x", λ("y", app(app(Builtin(Add), var("x")), var("y"))))), num(0)), arr(&[num(1), num(2), num(3), num(4)])));
    parse_eq!("map (- 1) [1,2,3,4]", app(app(var("map"), λ("x", app(app(Builtin(Sub), var("x")), num(1)))), arr(&[num(1), num(2), num(3), num(4)])));
  }

  #[test]
  fn de_bruijn_indices() {
    parse_eq!("\\x -> \\x -> x@1", λ("x", λ("x", var_ix("x", 1))));
    parse_eq!("\\x -> \\x -> x@0", λ("x", λ("x", var("x"))));
    parse_eq!("\\x -> \\x -> x", λ("x", λ("x", var("x"))));
  }

  #[test]
  fn annotations() {
    use Type::*;
    parse_eq!("1 :: JSON -> JSON -> JSON",
              ann(num(1), Type::arr(JSON, Type::arr(JSON, JSON))),
              "right associative by default");
    parse_eq!("1 :: (JSON -> JSON) -> JSON", ann(num(1), Type::arr(Type::arr(JSON, JSON), JSON)));
    parse_eq!("1 :: ((JSON -> JSON) -> JSON) -> JSON", ann(num(1), Type::arr(Type::arr(Type::arr(JSON, JSON), JSON), JSON)));
    parse_eq!("1 :: JSON -> JSON -> JSON -> JSON", ann(num(1), Type::arr(JSON, Type::arr(JSON, Type::arr(JSON, JSON)))));
    parse_eq!("1 :: JSON -> (JSON -> JSON) -> JSON", ann(num(1), Type::arr(JSON, Type::arr(Type::arr(JSON, JSON), JSON))));
    parse_eq!("1 ∷ ((JSON -> JSON) -> (JSON -> JSON))", ann(num(1), Type::arr(Type::arr(JSON, JSON), Type::arr(JSON, JSON))));
    parse_eq!("1 ∷ JSON -> JSON -> (JSON -> JSON)", ann(num(1), Type::arr(JSON, Type::arr(JSON, Type::arr(JSON, JSON)))));
  }
}
