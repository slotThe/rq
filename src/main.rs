//! A tiny functional language to filter and manipulate JSON.

#![allow(confusable_idents)]
#![feature(assert_matches)]
#![feature(box_patterns)]
#![feature(iter_intersperse)]
#![feature(slice_split_once)]
#![feature(try_blocks)]
#![allow(rustdoc::invalid_rust_codeblocks)]

mod cli;
mod eval;
mod expr;
mod r#type;
mod util;

use std::{collections::BTreeMap, env, io::{self, BufRead, Read, Write}};

use anyhow::Result;
use cli::{Fatten, Flatten, Help};
use eval::stdlib::STDLIB_HELP;
use expr::{parser::{parse_expr, parse_json}, Expr};
use r#type::expr::TCExpr;
use util::fatten::fatten;

use crate::{cli::HELP, eval::stdlib::{STDLIB_CTX, STDLIB_TYPES}, expr::app, util::flatten::flatten};

fn main() -> Result<()> {
  let args = env::args().skip(1).collect::<Vec<String>>();
  match args.iter().map(|s| s as &str).collect::<Vec<_>>()[..] {
    ["repl", ..] => repl(),
    [Help!(), ..] => {
      println!("{}", *HELP);
      Ok(())
    },
    [Flatten!()] => {
      let mut input = String::new();
      io::stdin().read_to_string(&mut input)?;
      flatten_io(&input);
      Ok(())
    },
    [Flatten!(), f] => {
      let mut input = String::new();
      read_file(&mut input, f)?;
      flatten_io(&input);
      Ok(())
    },
    [Fatten!()] => {
      let mut input = String::new();
      io::stdin().read_to_string(&mut input)?;
      println!("{}", fatten(&input));
      Ok(())
    },
    [Fatten!(), f] => {
      let mut input = String::new();
      read_file(&mut input, f)?;
      println!("{}", fatten(&input));
      Ok(())
    },
    [x] => {
      let mut input = String::new();
      io::stdin().read_to_string(&mut input)?;
      oneshot(&input, x)
    },
    [x, f] => {
      let mut input = String::new();
      read_file(&mut input, f)?;
      oneshot(&input, x)
    },
    _ => {
      println!("{}", *HELP);
      Ok(())
    },
  }
}

fn flatten_io(input: &str) {
  if let Some(json) = parse_json(input) {
    for line in flatten(&json) {
      println!("{}", line)
    }
  }
}

fn read_file(input: &mut String, f: &str) -> Result<()> {
  if let Ok(mut file) = std::fs::File::open(f) {
    file.read_to_string(input)?;
  }
  Ok(())
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

/// Try to read from the given file, or read from stdin if that fails.
fn oneshot(input: &str, expr: &str) -> Result<()> {
  if let Some(expr) = parse_expr(expr) {
    if let Some(json) = parse_json(input) {
      println!(
        "{}",
        TCExpr::new(app(expr, json), &STDLIB_TYPES)?.eval(&STDLIB_CTX)?
      )
    }
  }
  Ok(())
}

#[cfg(test)]
mod test {
  use std::{io::Read, process::{Command, Stdio}};

  use anyhow::Result;

  const SIMPLE: &str = "[{\"name\": \"John Doe\", \"age\": 43, \"phones\": [\"+44 \
                        1234567\", \"+44 2345678\"]}, {\"name\":\"Alice\"}, \
                        {\"name\":\"Bob\", \"age\":42}]";

  macro_rules! cli_test {
    ($inp:expr, $rq_expr:literal, $comp:literal $(,)?) => {{
      let inp = $inp.stdout(Stdio::piped()).spawn()?;
      let output = Command::new("cargo")
        .args(["run", "-q", $rq_expr])
        .stdin(inp.stdout.unwrap())
        .stdout(Stdio::piped())
        .spawn()?;
      let mut buf = String::new();
      output.stdout.unwrap().read_to_string(&mut buf)?;
      assert_eq!($comp, buf.trim());
      Ok(())
    }};
  }

  #[test]
  fn test_cargo_metadata() -> Result<()> {
    cli_test!(
      Command::new("cargo").args(["metadata", "--format-version=1"]),
      ".packages | map .name",
      "[\"ahash\", \"aho-corasick\", \"allocator-api2\", \"anyhow\", \"ariadne\", \
       \"cc\", \"cfg-if\", \"chumsky\", \"hashbrown\", \"libc\", \"memchr\", \
       \"once_cell\", \"proc-macro2\", \"psm\", \"quote\", \"regex-automata\", \
       \"regex-syntax\", \"rq\", \"serde\", \"serde_derive\", \"stacker\", \"syn\", \
       \"unicode-ident\", \"unicode-width\", \"version_check\", \"winapi\", \
       \"winapi-i686-pc-windows-gnu\", \"winapi-x86_64-pc-windows-gnu\", \"yansi\", \
       \"zerocopy\", \"zerocopy-derive\"]"
    )
  }

  #[test]
  fn simple_filter() -> Result<()> {
    cli_test!(
      Command::new("echo").arg(SIMPLE),
      "filter (get \"age\" | (>= 42)) | map (|x| { x.name: x.age })",
      "[{\"John Doe\": 43}, {\"Bob\": 42}]"
    )
  }

  #[test]
  fn simple_fold() -> Result<()> {
    cli_test!(
      Command::new("echo").arg(SIMPLE),
      "map .age | foldl (+) 0",
      "85"
    )
  }
}
