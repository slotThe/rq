use serde_json::Value;

use super::{desugarer::DExpr, Const};

pub fn json_to_dexpr(json: &Value) -> DExpr {
  match json {
    Value::Null => DExpr::Const(Const::Null),
    Value::Bool(b) => DExpr::Const(Const::Bool(*b)),
    Value::Number(n) => DExpr::Const(Const::Num(
      n.as_f64().expect("Number does not fit inside f64"),
    )),
    Value::String(_) => todo!(),
    Value::Array(xs) => DExpr::Arr(xs.iter().map(json_to_dexpr).collect()),
    Value::Object(hm) => DExpr::Obj(
      hm.iter()
        .map(|(k, v)| (k.to_string(), json_to_dexpr(v)))
        .collect(),
    ),
  }
}
