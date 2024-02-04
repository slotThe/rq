use serde_json::{Number, Value};

use super::{expr_str, num, Const, Expr};

pub fn json_to_expr(json: &Value) -> Expr {
  match json {
    Value::Null => Expr::Const(Const::Null),
    Value::Bool(b) => Expr::Const(Const::Bool(*b)),
    Value::Number(n) => num(n.as_f64().expect("Number does not fit inside f64")), // FIXME
    Value::String(s) => expr_str(s),
    Value::Array(xs) => Expr::Arr(xs.iter().map(json_to_expr).collect()),
    Value::Object(ob) => Expr::Obj(
      ob.iter()
        .map(|(k, v)| (expr_str(k), json_to_expr(v)))
        .collect(),
    ),
  }
}

impl Expr {
  pub fn to_json(&self) -> Value {
    match self {
      Expr::Const(Const::Bool(b)) => Value::Bool(*b),
      Expr::Const(Const::String(s)) => Value::String(s.to_string()),
      Expr::Const(Const::Null) => Value::Null,
      Expr::Const(Const::Num(n)) => {
        let n = n.into_inner();
        Value::Number(if n.fract() == 0.0 {
          (n as i64).into()
        } else {
          Number::from_f64(n).unwrap()
        })
      },
      Expr::Arr(xs) => Value::Array(xs.iter().map(|x| x.to_json()).collect()),
      Expr::Obj(ob) => Value::Object(
        ob.iter()
          .map(|(k, v)| (k.to_string(), v.to_json()))
          .collect(),
      ),
      // We could also convert the result to a string, but something like this
      // will almost certainly be a user error.
      _ => panic!("Can't convert {self} back into JSON!"),
    }
  }
}
