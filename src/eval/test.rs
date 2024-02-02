use crate::{eval::stdlib::{STDLIB_CTX, STDLIB_TYPES}, expr::{num, parser::parse}};

#[test]
fn eval_if() {
  let expr1 = parse("if ((\\x -> x.this) {this: 3}) then 1 else 4")
    .unwrap()
    .check(&STDLIB_TYPES)
    .unwrap();
  assert_eq!(num(1.0).desugar(), expr1.eval(&STDLIB_CTX));
}
