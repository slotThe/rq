//! A tiny functional language to filter and manipulate JSON.

#![allow(confusable_idents)]
#![feature(assert_matches)]
#![feature(box_patterns)]
#![feature(iter_intersperse)]
#![feature(lazy_cell)]
#![feature(slice_split_once)]
#![feature(try_blocks)]
#![feature(type_changing_struct_update)]
#![allow(rustdoc::invalid_rust_codeblocks)]

mod eval;
mod expr;
mod r#type;
mod util;

use std::{collections::BTreeMap, env, io::{self, BufRead, Read, Write}};

use anyhow::Result;
use eval::stdlib::STDLIB_HELP;
use expr::{parser::{parse_expr, parse_json}, Expr};
use r#type::expr::TCExpr;

use crate::{eval::stdlib::{STDLIB_CTX, STDLIB_TYPES}, expr::app};

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
    match &buffer {
      _ if buffer.starts_with(":e ") => {
        parse_expr(&buffer[3..]).map(|e| writeln!(out_handle, "{}", e));
      },
      _ if buffer.starts_with(":d ") => {
        parse_expr(&buffer[3..]).map(|e| writeln!(out_handle, "{:?}", e));
      },
      _ if buffer.starts_with(":i ") => {
        match STDLIB_HELP.get(&buffer[3..].trim()) {
          Some(help) => writeln!(out_handle, "{}", help.wrap("", 50))?,
          None => writeln!(
            out_handle,
            "Error: Found no builtin function with that name."
          )?,
        };
      },
      _ if buffer.starts_with(":l") => STDLIB_HELP
        .iter()
        // Deduping; XXX: this should probably be handled more gracefully.
        .map(|(name, help)| (help, name))
        .collect::<BTreeMap<_, _>>()
        .iter()
        .for_each(|(help, name)| {
          let _ = writeln!(out_handle, "{name}	{}", help.wrap("	", 50));
        }),
      _ if buffer.starts_with(":dp ") => {
        parse_expr(&buffer[4..]).map(|e| writeln!(out_handle, "{:#?}", e));
      },
      _ if buffer.starts_with(":t ") => {
        if let Some(expr) = parse_expr(&buffer[3..]) {
          match expr.type_check(&STDLIB_TYPES) {
            Ok(typ) => writeln!(out_handle, "{typ}")?,
            Err(err) => writeln!(out_handle, "{err}")?,
          };
        }
      },
      _ => {
        if let Some(expr) = parse_expr(&buffer) {
          match TCExpr::new(expr, &STDLIB_TYPES) {
            Ok(expr) => match expr.eval(&STDLIB_CTX) {
              Ok(expr) => writeln!(out_handle, "{expr}")?,
              Err(err) => writeln!(out_handle, "{err}")?,
            },
            Err(err) => writeln!(out_handle, "{err}")?,
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

fn oneshot() -> Result<()> {
  let mut input = String::new();
  io::stdin().read_to_string(&mut input)?;
  if let Some(expr) = parse_expr(&env::args().collect::<Vec<_>>()[1]) {
    if let Some(json) = parse_json(&input) {
      println!(
        "{}",
        TCExpr::new(app(expr, json), &STDLIB_TYPES)?.eval(&STDLIB_CTX)?
      )
    }
  }
  Ok(())
}
