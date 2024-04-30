use crate::expr::Expr;

/// Flatten the given JSON expression into a vector.
pub fn flatten(json: &Expr) -> Vec<String> {
  let mut res = Vec::new();
  flatten_worker(json, "json".to_string(), &mut res);
  res
}

fn flatten_worker(json: &Expr, mut prefix: String, res: &mut Vec<String>) {
  match json {
    Expr::Const(c) => {
      prefix.push_str(&format!(" = {c}"));
      res.push(prefix);
    },
    Expr::Arr(xs) => {
      res.push(format!("{prefix} = []"));
      for (ix, expr) in xs.iter().enumerate() {
        flatten_worker(expr, format!("{prefix}.[{ix}]"), res);
      }
    },
    Expr::Obj(ob) => {
      res.push(format!("{prefix} = {{}}"));
      for (key, val) in ob {
        flatten_worker(val, format!("{prefix}.{key}"), res);
      }
    },
    _ => unimplemented!(),
  }
}
