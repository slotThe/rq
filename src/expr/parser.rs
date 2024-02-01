use std::collections::HashMap;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::prelude::*;

use super::{app, lam, var, Const};
use crate::Expr;

// Lots stolen from https://github.com/zesterer/chumsky/blob/0.9/examples/json.rs
//
// This is an absolutely unreadable mess, and the compulsively imperative
// nature of chumsky really makes my head spin a bit. And yet, all this pain
// seems worth it for those error messages. Still, I yearn for the days when
// I can return to nom.
fn p_expr() -> impl Parser<char, Expr, Error = Simple<char>> {
  recursive(|p_expr| {
    let p_var = text::ident();

    let p_num = {
      let frac = just('.').chain(text::digits(10));
      let exp = just('e')
        .or(just('E'))
        .chain(just('+').or(just('-')).or_not())
        .chain::<char, _, _>(text::digits(10));
      just('-')
        .or_not()
        .chain::<char, _, _>(text::int(10))
        .chain::<char, _, _>(frac.or_not().flatten())
        .chain::<char, _, _>(exp.or_not().flatten())
        .collect::<String>()
        .from_str()
        .unwrapped()
        .labelled("number")
        .map(Const::Num)
    };

    let p_str = {
      let escape = just('\\').ignore_then(
        just('\\')
          .or(just('/'))
          .or(just('"'))
          .or(just('b').to('\x08'))
          .or(just('f').to('\x0C'))
          .or(just('n').to('\n'))
          .or(just('r').to('\r'))
          .or(just('t').to('\t'))
          .or(
            just('u').ignore_then(
              filter(|c: &char| c.is_ascii_hexdigit())
                .repeated()
                .exactly(4)
                .collect::<String>()
                .validate(|digits, span, emit| {
                  char::from_u32(u32::from_str_radix(&digits, 16).unwrap())
                    .unwrap_or_else(|| {
                      emit(Simple::custom(span, "invalid unicode character"));
                      '\u{FFFD}' // unicode replacement character
                    })
                }),
            ),
          ),
      );
      just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .labelled("string")
    };

    let p_const = just("true")
      .to(true)
      .or(just("false").to(false))
      .map(Const::Bool)
      .or(just("null").to(Const::Null))
      .or(p_num)
      .or(p_str.map(Const::String))
      .map(Expr::Const);

    let p_array = just('[')
      .padded()
      .ignore_then(
        p_expr
          .clone()
          .chain(just(',').padded().ignore_then(p_expr.clone()).repeated())
          .or_not()
          .flatten()
          .padded()
          .then_ignore(just(']'))
          .map(Expr::Arr),
      )
      .labelled("array");

    let p_obj = {
      let p_kv = p_var
        .or(p_str)
        .then_ignore(just(':').padded())
        .then(p_expr.clone());
      just('{')
        .padded()
        .ignore_then(
          p_kv
            .clone()
            .chain(just(',').padded().ignore_then(p_kv.clone()).repeated())
            .or_not()
            .flatten()
            .padded()
            .then_ignore(just('}').padded()),
        )
        .collect::<HashMap<String, Expr>>()
        .map(Expr::Obj)
        .labelled("object")
    };

    let p_lam = {
      let haskell_head = (just("\\").or(just("λ")).padded())
        .ignore_then(p_var)
        .then_ignore(just("->").or(just("→")).padded());
      let rust_head = p_var.delimited_by(just('|'), just('|')).padded();
      haskell_head
        .or(rust_head)
        .padded()
        .then(p_expr)
        .map(|(h, b)| lam(h.as_str(), b))
        .labelled("lambda")
    };

    let p_app = recursive(|p_app| {
      let fun = {
        // Only variable and parenthesised lambdas can represent functions.
        let head = p_lam
          .clone()
          .delimited_by(just('('), just(')'))
          .or(p_var.map(Expr::Var))
          .padded();
        // Check for dot syntax: x.1, x.blah, …
        head
          .then(
            just('.')
              .ignore_then(
                text::int(10)
                  .map(|i: String| Const::Num(i.parse::<f64>().unwrap()))
                  .or(p_var.or(p_str).map(Const::String)),
              )
              .map(Expr::Const)
              .repeated(),
          )
          .foldl(|acc, c| app(app(var("get"), c), acc))
      };
      let go = fun
        .then(
          p_var
            .map(Expr::Var)
            .or(p_array.clone())
            .or(p_obj.clone())
            .or(p_lam.clone().delimited_by(just('('), just(')')))
            .or(p_app)
            .or(p_const.clone())
            .padded()
            .repeated(),
        )
        .foldl(app);
      go.clone()
        .delimited_by(just('('), just(')'))
        .or(go)
        .padded()
        .labelled("application")
    });

    p_obj
      .or(p_array)
      .or(p_const)
      .or(p_app)
      .or(p_lam)
      .or(p_var.map(Expr::Var))
  })
}

/// Parse an expression.
pub fn parse(inp: &str) -> Result<Expr, Vec<Simple<char>>> {
  p_expr().then_ignore(end()).parse(inp.trim())
}

/// Parse an expression.
///
/// Lots stolen from https://github.com/zesterer/chumsky/blob/0.9/examples/json.rs
pub fn parse_main(inp: &str) -> Option<Expr> {
  let (expr, errs) = p_expr().then_ignore(end()).parse_recovery(inp.trim());
  match expr {
    Some(e) => Some(e),
    None => {
      errs.into_iter().for_each(|e| {
        let msg = if let chumsky::error::SimpleReason::Custom(msg) = e.reason() {
          msg.clone()
        } else {
          format!(
            "{}{}, expected {}",
            if e.found().is_some() {
              "Unexpected token"
            } else {
              "Unexpected end of input"
            },
            if let Some(label) = e.label() {
              format!(" while parsing {}", label)
            } else {
              String::new()
            },
            if e.expected().len() == 0 {
              "something else".to_string()
            } else {
              e.expected()
                .map(|expected| match expected {
                  Some(expected) => expected.to_string(),
                  None => "end of input".to_string(),
                })
                .collect::<Vec<_>>()
                .join(", ")
            },
          )
        };

        let report = Report::build(ReportKind::Error, (), e.span().start)
          .with_code(3)
          .with_message(msg)
          .with_label(
            Label::new(e.span())
              .with_message(match e.reason() {
                chumsky::error::SimpleReason::Custom(msg) => msg.clone(),
                _ => format!(
                  "Unexpected {}",
                  e.found()
                    .map(|c| format!("token {}", c.fg(Color::Red)))
                    .unwrap_or_else(|| "end of input".to_string())
                ),
              })
              .with_color(Color::Red),
          );

        let report = match e.reason() {
          chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
            .with_label(
              Label::new(span.clone())
                .with_message(format!(
                  "Unclosed delimiter {}",
                  delimiter.fg(Color::Yellow)
                ))
                .with_color(Color::Yellow),
            ),
          chumsky::error::SimpleReason::Unexpected => report,
          chumsky::error::SimpleReason::Custom(_) => report,
        };

        report.finish().print(Source::from(&inp)).unwrap();
      });
      None
    },
  }
}
