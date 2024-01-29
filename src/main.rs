#![feature(extend_one)]
#![feature(iter_intersperse)]
#![feature(box_patterns)]

#[macro_use]
extern crate lazy_static;

mod expr;
mod stdlib;
mod r#type;

use std::{env, io::{self, BufRead, Read, Write}};

use anyhow::Result;
use expr::{parser::parse, Const, Expr};
use serde_json::Value;

use crate::{expr::{app, json::json_to_expr}, stdlib::{STDLIB_CTX, STDLIB_TYPES}};

const NULL: Expr = Expr::Const(Const::Null);

fn main() -> Result<()> {
  let arg = env::args().nth(1);
  if arg.is_none() || arg == Some("repl".to_string()) {
    repl()
  } else {
    oneshot()
  }
}

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
        None => {
          let tce = parse(&buffer).unwrap_or(NULL).check(&STDLIB_TYPES)?;
          writeln!(out_handle, "{}", tce.eval(&STDLIB_CTX))?;
        },
        // Desugared expression
        Some(buffer) => {
          writeln!(out_handle, "{}", parse(buffer).unwrap_or(NULL).desugar())?
        },
      },
      // Type
      Some(buffer) => {
        let inferred_type = &parse(buffer).unwrap_or(NULL).infer(&STDLIB_TYPES)?;
        writeln!(out_handle, "{inferred_type}")?;
      },
    }
    write!(out_handle, "λ> ")?;
    out_handle.flush()?;
    buffer.clear(); // Clear stdin buffer.
  }
  Ok(())
}

fn oneshot() -> Result<()> {
  let mut input = String::new();
  io::stdin().read_to_string(&mut input)?;

  let json: Value = serde_json::from_str(&input)?;
  println!("Json: {json}");

  let input_expr = &env::args().collect::<Vec<_>>()[1];
  let expr = parse(input_expr).unwrap_or(NULL);
  println!("Expression: {expr}");
  let typ = expr.clone().infer(&STDLIB_TYPES)?;
  println!("Inferred type: {typ}");
  let dexpr = expr.desugar();
  println!("Desugared expression: {dexpr}");

  let eval = app(expr, json_to_expr(&json))
    .check(&STDLIB_TYPES)?
    .eval(&STDLIB_CTX);
  println!("Evaluated expression: {}", eval);

  Ok(())
}
