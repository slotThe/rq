use std::{collections::HashSet, sync::LazyLock};

use crate::expr::{Const, Expr};

/// Flatten the given JSON expression into a vector.
pub fn flatten(json: &Expr) -> Vec<String> {
  let mut res = Vec::new();
  flatten_worker(json, "json".to_string(), &mut res);
  res
}

fn flatten_worker(json: &Expr, mut prefix: String, res: &mut Vec<String>) {
  match json {
    Expr::Const(c) => {
      prefix.push_str(&format!(" = {c};"));
      res.push(prefix);
    },
    Expr::Arr(xs) => {
      res.push(format!("{prefix} = [];"));
      for (ix, expr) in xs.iter().enumerate() {
        flatten_worker(expr, format!("{prefix}[{ix}]"), res);
      }
    },
    Expr::Obj(ob) => {
      res.push(format!("{prefix} = {{}};"));
      for (key, val) in ob {
        match key {
          Expr::Const(Const::String(s)) => {
            if valid_ident(s) {
              flatten_worker(val, format!("{prefix}.{s}"), res);
            } else {
              flatten_worker(val, format!("{prefix}[\"{s}\"]"), res);
            }
          },
          _ => unimplemented!(),
        }
      }
    },
    _ => unimplemented!(),
  }
}

/// Check if the given string is a valid identified; this is important if
/// we—like gron—want to produce valid JavaScript.
fn valid_ident(s: &str) -> bool {
  if s.is_empty() || JS_RESERVED.contains(s) {
    false
  } else {
    let mut s_iter = s.chars();
    valid_first_letter(s_iter.next().unwrap()) && s_iter.all(valid_letter)
  }
}

fn valid_first_letter(c: char) -> bool { c.is_alphabetic() || "$_".contains(c) }

fn valid_letter(c: char) -> bool { !c.is_whitespace() }

#[rustfmt::skip]
static JS_RESERVED: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
  HashSet::from([
    "break", "case", "catch", "class", "const", "continue", "debugger", "default",
    "delete", "do", "else", "export", "extends", "false", "finally", "for", "function",
    "if", "import", "in", "instanceof", "new", "null", "return", "super", "switch",
    "this", "throw", "true", "try", "typeof", "var", "void", "while", "with", "yield",
  ])
});

#[cfg(test)]
mod tests {
  use crate::util::flatten::valid_ident;

  #[test]
  fn test_valid_ident() {
    assert!(!valid_ident("class"));
    assert!(!valid_ident("instanceof"));
    assert!(!valid_ident("default"));
    assert!(!valid_ident(""));
    assert!(!valid_ident("blah blah"));
    assert!(!valid_ident("1blah"));
    assert!(!valid_ident("1"));
    assert!(valid_ident("blah"));
  }
}
