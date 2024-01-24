#[cfg(test)]
use crate::expr::{parser::parse, Const::*, Expr::{self, *}, Var};

///////////////////////////////////////////////////////////////////////
///// Parser tests

#[test]
fn simple_app() {
  let x = "x".to_string();
  assert_eq!(
    parse(&format!("(\\{x} -> {x}) 5")),
    Ok(App(
      Box::new(Lam(Var(x.clone()), Box::new(Expr::Var(Var(x))))),
      Box::new(Const(Num(5.0)))
    ))
  )
}

#[test]
fn app_inside_lambda() {
  let x = "x".to_string();
  assert_eq!(
    parse(&format!("λ{x} → {x} 5")),
    Ok(Lam(
      Var(x.clone()),
      Box::new(App(Box::new(Expr::Var(Var(x))), Box::new(Const(Num(5.0)))))
    ))
  )
}

#[test]
fn lambda_variants() {
  let haskell_lam = parse("\\x -> x");
  let unicode_lam = parse("λx → x");
  let rust_lam = parse("|x| x");
  let lam = Ok(Lam(
    Var("x".to_string()),
    Box::new(Expr::Var(Var("x".to_string()))),
  ));
  assert_eq!(haskell_lam, lam);
  assert_eq!(unicode_lam, lam);
  assert_eq!(rust_lam, lam);
}

#[test]
fn higher_order_application() {
  assert_eq!(
    parse("\\x -> \\y -> x y 2"),
    Ok(Lam(
      Var("x".to_string()),
      Box::new(Lam(
        Var("y".to_string()),
        Box::new(App(
          Box::new(App(
            Box::new(Expr::Var(Var("x".to_string()))),
            Box::new(Expr::Var(Var("y".to_string()))),
          )),
          Box::new(Const(Num(2.0)))
        ))
      ))
    ))
  )
}

#[test]
#[allow(non_snake_case)]
fn parses_S_combinator() {
  assert_eq!(
    parse("λf -> λg → \\x → f x (g x)"),
    Ok(Lam(
      Var("f".to_string()),
      Box::new(Lam(
        Var("g".to_string()),
        Box::new(Lam(
          Var("x".to_string()),
          Box::new(App(
            Box::new(App(
              Box::new(Expr::Var(Var("f".to_string()))),
              Box::new(Expr::Var(Var("x".to_string())))
            )),
            Box::new(App(
              Box::new(Expr::Var(Var("g".to_string()))),
              Box::new(Expr::Var(Var("x".to_string())))
            ))
          ))
        ))
      ))
    ))
  )
}
