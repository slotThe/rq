use serde_json::Value;

use super::{expr_str, num, Const, Expr};

pub fn json_to_expr(json: &Value) -> Expr {
  match json {
    Value::Null => Expr::Const(Const::Null),
    Value::Bool(b) => Expr::Const(Const::Bool(*b)),
    Value::Number(n) => num(n.as_f64().expect("Number does not fit inside f64")), // FIXME
    Value::String(s) => expr_str(s),
    Value::Array(xs) => Expr::Arr(xs.iter().map(json_to_expr).collect()),
    Value::Object(hm) => Expr::Obj(
      hm.iter()
        .map(|(k, v)| (expr_str(k), json_to_expr(v)))
        .collect(),
    ),
  }
}
