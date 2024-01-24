#![feature(extend_one)]
#![feature(iter_intersperse)]

pub mod expr;
pub mod r#type;

use std::{env, io::{self, BufRead, Read, Write}};

use anyhow::Result;
use expr::{parser::parse, Const, Expr};
use serde_json::Value;

const NULL: Expr = Expr::Const(Const::Null);

fn json_to_val(data: &str) -> serde_json::Result<Value> { serde_json::from_str(data) }

fn repl() -> Result<()> {
  let mut buffer = String::new();
  let stdin = io::stdin();
  let mut in_handle = stdin.lock();

  let stdout = io::stdout();
  let mut out_handle = stdout.lock();

  write!(out_handle, "λ> ")?;
  out_handle.flush()?; // Write to stdout.

  while in_handle.read_line(&mut buffer).is_ok() {
    match buffer.strip_prefix(":t ") {
      None => match buffer.strip_prefix(":d ") {
        // Normal expression
        None => writeln!(out_handle, "{}", parse(&buffer).unwrap_or(NULL))?,
        // Desugared expression
        Some(buffer) => {
          writeln!(out_handle, "{}", parse(buffer).unwrap_or(NULL).desugar())?
        },
      },
      // Type
      Some(buffer) => {
        let inferred_type = &parse(buffer).unwrap_or(NULL).check()?;
        writeln!(out_handle, "{inferred_type}")?;
      },
    }
    write!(out_handle, "λ> ")?;
    out_handle.flush()?;
    buffer.clear(); // Clear stdin buffer.
  }
  Ok(())
}

fn main() -> Result<()> {
  let mut input = String::new();
  io::stdin().read_to_string(&mut input)?;

  let json = json_to_val(&input)?;
  println!("Json: {json}");

  let input_expr = &env::args().collect::<Vec<_>>()[1];
  let expr = parse(input_expr).unwrap_or(NULL);
  println!("Expression: {expr}");
  let typ = expr.clone().check()?;
  println!("Inferred type: {typ}");
  let dexpr = expr.desugar();
  println!("Desugared expression: {dexpr}");

  Ok(())
}
