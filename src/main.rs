#![feature(assert_matches)]
#![feature(box_patterns)]
#![feature(iter_intersperse)]
#![feature(stmt_expr_attributes)]
#![feature(trait_alias)]
#![feature(try_blocks)]

#[macro_use]
extern crate lazy_static;

mod error;
mod eval;
mod expr;
mod r#type;
mod util;

use std::{env, io::{self, BufRead, Read, Write}};

use expr::{parser::parse_main, Expr};

use crate::{error::print_err, eval::stdlib::{STDLIB_CTX, STDLIB_TYPES}, expr::{app, json::json_to_expr}};

fn main() -> miette::Result<()> {
  miette::set_panic_hook();

  let arg = env::args().nth(1);
  if arg.is_none() || arg == Some("repl".to_string()) {
    repl().map_err(|e| miette::miette!("{e:?}"))
  } else {
    oneshot()
  }
}

fn repl() -> anyhow::Result<()> {
  let mut buffer = String::new();
  let stdin = io::stdin();
  let mut in_handle = stdin.lock();

  let stdout = io::stdout();
  let mut out_handle = stdout.lock();

  write!(out_handle, "λ> ")?;
  out_handle.flush()?; // Write to stdout.

  while in_handle.read_line(&mut buffer).is_ok() {
    match &buffer {
      _ if buffer.starts_with(":e ") => {
        parse_main(&buffer[3..]).map(|e| writeln!(out_handle, "{}", e));
      },
      _ if buffer.starts_with(":d ") => {
        parse_main(&buffer[3..]).map(|e| writeln!(out_handle, "{:?}", e));
      },
      _ if buffer.starts_with(":dp ") => {
        parse_main(&buffer[4..]).map(|e| writeln!(out_handle, "{:#?}", e));
      },
      _ if buffer.starts_with(":t ") => {
        if let Some(expr) = parse_main(&buffer[3..]) {
          match expr.infer(&STDLIB_TYPES) {
            Ok(typ) => writeln!(out_handle, "{typ}")?,
            Err(err) => writeln!(out_handle, "{err}")?,
          };
        }
      },
      _ => {
        if let Some(expr) = parse_main(&buffer) {
          match expr.check(&STDLIB_TYPES) {
            Ok(expr) => match expr.eval(&STDLIB_CTX) {
              Ok(expr) => writeln!(out_handle, "{expr}")?,
              Err(err) => print_err(&mut out_handle, err)?,
            },
            Err(err) => print_err(&mut out_handle, err)?,
          };
        }
      },
    }
    write!(out_handle, "λ> ")?;
    out_handle.flush()?;
    buffer.clear(); // Clear stdin buffer.
  }
  Ok(())
}

fn oneshot() -> miette::Result<()> {
  let mut input = String::new();
  io::stdin()
    .read_to_string(&mut input)
    .map_err(|e| miette::miette!("{e:?}"))?;
  if let Some(expr) = parse_main(&env::args().collect::<Vec<_>>()[1]) {
    println!(
      "{}",
      app(
        expr,
        json_to_expr(
          &serde_json::from_str(&input).map_err(|e| miette::miette!("{e:?}"))?
        )
      )
      .check(&STDLIB_TYPES)?
      .eval(&STDLIB_CTX)?
      .to_json()
    )
  }
  Ok(())
}
