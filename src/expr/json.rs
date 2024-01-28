use serde_json::Value;

use super::{Const, Expr};

pub fn json_to_expr(json: &Value) -> Expr {
  match json {
    Value::Null => Expr::Const(Const::Null),
    Value::Bool(b) => Expr::Const(Const::Bool(*b)),
    Value::Number(n) => Expr::Const(Const::Num(
      n.as_f64().expect("Number does not fit inside f64"), // FIXME
    )),
    Value::String(s) => Expr::Const(Const::String(s.to_owned())),
    Value::Array(xs) => Expr::Arr(xs.iter().map(json_to_expr).collect()),
    Value::Object(hm) => Expr::Obj(
      hm.iter()
        .map(|(k, v)| (k.to_string(), json_to_expr(v)))
        .collect(),
    ),
  }
}
