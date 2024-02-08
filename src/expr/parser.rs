//! Lots stolen from chumsky's [json] and [nano_rust] examples.
//!
//! [json]: https://github.com/zesterer/chumsky/blob/0.9/examples/json.rs
//! [nano_rust]: https://github.com/zesterer/chumsky/blob/0.9/examples/nano_rust.rs

use std::collections::BTreeMap;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::prelude::*;

use super::{app, de_bruijn::DBVar, expr_str, if_then_else, lam, num, var, Const};
use crate::{eval::stdlib::Builtin, Expr};

// This is an absolutely unreadable mess, and the compulsively imperative
// nature of chumsky really makes my head spin a bit. Plus, compile times went
// up by a lot, and I essentially have to comment out all of the parser when
// working on another component so things stay responsive. Lots of fun. And
// yet, all this pain seems worth it for those error messages. Still, I yearn
// for the days when I can return to nom.
#[rustfmt::skip]
fn p_expr() -> impl Parser<char, Expr, Error = Simple<char>> {
  // You might think that I need a lexer; and you would be right.
  let p_varlike = text::ident().try_map(
    move |s: <char as chumsky::text::Character>::Collection, span| {
      if ["if", "then", "else"].contains(&s.as_str()) {
        Err(Simple::expected_input_found(span, None, None))
      } else {
        Ok(s)
      }
    },
  );

  let p_var = p_varlike
    .then(just("@").ignore_then(text::int(10).from_str().unwrapped()).or_not())
    .map(|(a, b)| Expr::Var(DBVar{ name: a.clone(), level: b.unwrap_or(0) }));

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
                char::from_u32(u32::from_str_radix(&digits, 16).unwrap()).unwrap_or_else(
                  || {
                    emit(Simple::custom(span, "invalid unicode character"));
                    '\u{FFFD}' // unicode replacement character
                  },
                )
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

  let p_const = choice((
    just("true")
      .to(true)
      .or(just("false").to(false))
      .map(Const::Bool),
    just("null").to(Const::Null),
    p_num,
    p_str.map(Const::String),
  ))
  .map(Expr::Const);

  let p_expr = recursive(|p_expr| {
    let p_array = just('[')
      .padded()
      .ignore_then(
        p_expr.clone()
          .padded()
          .chain(just(',').padded().ignore_then(p_expr.clone()).repeated())
          .or_not()
          .flatten()
          .padded()
          .then_ignore(just(']'))
          .map(Expr::Arr),
      )
      .labelled("array");

    let p_obj = {
      let p_kv = (p_varlike.or(p_str).map(|s| Expr::Const(Const::String(s))))
        .then_ignore(just(':').padded())
        .or(p_expr.clone().then_ignore(just(':').padded()))
        .then(p_expr.clone());
      just('{')
        .padded()
        .ignore_then(
          p_kv.clone()
            .chain(just(',').padded().ignore_then(p_kv.clone()).repeated())
            .or_not()
            .flatten()
            .padded()
            .then_ignore(just('}').padded()),
        )
        .collect::<BTreeMap<Expr, Expr>>()
        .map(Expr::Obj)
        .labelled("object")
    };

    let p_lam = {
      let haskell_head = (just("\\").or(just("λ")).padded())
        .ignore_then(p_varlike)
        .then_ignore(just("->").or(just("→")).padded());
      let rust_head = p_varlike.padded().delimited_by(just('|'), just('|'));
      haskell_head.or(rust_head)
        .padded()
        .then(p_expr.clone())
        .map(|(h, b)| lam(h.as_str(), b))
        .labelled("lambda")
    };

    let p_if_then_else = (just("if").padded())
      .ignore_then(p_expr.clone())
      .then_ignore(just("then").padded())
      .then(p_expr.clone())
      .then_ignore(just("else").padded())
      .then(p_expr.clone())
      .map(|((i, t), e)| if_then_else(i, t, e));

    let p_app = recursive(|p_app| {
      let fun = {
        // The f in f x.
        let head = p_lam.clone().padded().delimited_by(just('('), just(')'))
          .or(p_var)
          .or(p_app.clone().padded().delimited_by(just('('), just(')')));
        // Check for dot syntax: x.1, x.blah, …
        head
          .then(
            just('.')
              .ignore_then(
                text::int(10)
                  .map(|i: String| num(i.parse::<f64>().unwrap()))
                  .or(p_varlike.or(p_str).map(expr_str)),
              )
              .repeated(),
          )
          .foldl(|acc, c| app(app(var("get"), c), acc))
      };
      let go = fun // the f in f x
        .then(
          // the x in f x
          choice((
            p_var,
            p_if_then_else.clone(),
            p_const.clone(),
            p_array.clone(),
            p_obj.clone(),
            p_lam.clone().padded().delimited_by(just('('), just(')')),
            p_app,
            p_expr.clone().padded(),
          ))
          .padded()
          .repeated(),
        )
        .foldl(app);
      go.clone()
        .or(go.clone().padded().delimited_by(just('('), just(')')))
        .padded()
        .labelled("application")
    });

    // The operator-part of the parser—here be dragons. All of this code
    // duplication probably indicates that something is seriously amiss, and I
    // don't understand chumsky enough. An XXX for another day!
    //
    // NOTE: boxed() does type erasure, which is quite crucial for the
    // operator parsers. Otherwise, chumsky compiles for… quite a while (as
    // in, probably hours, if not days).
    let all_exprs = choice((
      p_obj,
      p_array,
      p_const,
      p_if_then_else,
      p_app,
      p_lam,
      p_var,
      p_expr.clone().padded().delimited_by(just('('), just(')')),
    ))
    .boxed();

    let mul_sym = choice((
      just('*').or(just('·')).to(Expr::Builtin(Builtin::Mul)),
      just('/').or(just('÷')).to(Expr::Builtin(Builtin::Div)),
    )).padded();
    let p_ops_mul = choice((
      // Operator sections: (= 3), (3 =)
      mul_sym.clone()
        .padded()
        .then(all_exprs.clone().padded())
        .delimited_by(just('('), just(')'))
        .map(|(op, b)| (None, Vec::from([(op, Some(b))]))),
      all_exprs.clone()
        .padded()
        .then(mul_sym.clone())
        .delimited_by(just('('), just(')'))
        .map(|(a, op)| (Some(a), Vec::from([(op, None)]))),
      // A normal operator call: 3 = 4 = 5 = 7
      all_exprs.clone().map(Some)
        .then(mul_sym.clone().then(all_exprs.clone().map(Some)).repeated()),
      // An operator without anything: (=)
      mul_sym.clone()
        .delimited_by(just('('), just(')'))
        .map(|op| (None, Vec::from([(op, None)]))),
    ))
    .foldl(apply_op).map(|x| x.unwrap())
    .boxed();

    let sum_sym = choice((
      just('+').to(Expr::Builtin(Builtin::Add)),
      just('-').to(Expr::Builtin(Builtin::Sub)),
    )).padded();
    let p_ops_sum = choice((
      // Operator sections: (= 3), (3 =)
      sum_sym.clone()
        .padded()
        .then(p_ops_mul.clone().padded())
        .delimited_by(just('('), just(')'))
        .map(|(op, b)| (None, Vec::from([(op, Some(b))]))),
      p_ops_mul.clone()
        .padded()
        .then(sum_sym.clone())
        .delimited_by(just('('), just(')'))
        .map(|(a, op)| (Some(a), Vec::from([(op, None)]))),
      // A normal operator call: 3 = 4 = 5 = 7
      p_ops_mul.clone().map(Some)
        .then(sum_sym.clone().then(p_ops_mul.clone().map(Some)).repeated()),
      // An operator without anything: (=)
      sum_sym.clone()
        .delimited_by(just('('), just(')'))
        .map(|op| (None, Vec::from([(op, None)]))),
    ))
    .foldl(apply_op).map(|x| x.unwrap())
    .boxed();

    let comp_sym = choice((
      just("==").or(just("=")).to(Expr::Builtin(Builtin::Eq)),
      just("!=")
        .or(just("/="))
        .or(just("≠"))
        .to(Expr::Builtin(Builtin::Neq)),
      just("<=").or(just("≤")).to(Expr::Builtin(Builtin::Leq)),
      just(">=").or(just("≥")).to(Expr::Builtin(Builtin::Geq)),
      just('<').to(Expr::Builtin(Builtin::Le)),
      just('>').to(Expr::Builtin(Builtin::Ge)),
    )).padded();
    let p_ops_comp = choice((
      // Operator sections: (= 3), (3 =)
      comp_sym.clone()
        .padded()
        .then(p_ops_sum.clone().padded())
        .delimited_by(just('('), just(')'))
        .map(|(op, b)| (None, Vec::from([(op, Some(b))]))),
      p_ops_sum.clone()
        .padded()
        .then(comp_sym.clone())
        .delimited_by(just('('), just(')'))
        .map(|(a, op)| (Some(a), Vec::from([(op, None)]))),
      // A normal operator call: 3 = 4 = 5 = 7
      p_ops_sum.clone().map(Some)
        .then(comp_sym.clone().then(p_ops_sum.clone().map(Some)).repeated()),
      // An operator without anything: (=)
      comp_sym.clone()
        .delimited_by(just('('), just(')'))
        .map(|op| (None, Vec::from([(op, None)]))),
    ))
    .foldl(apply_op).map(|x| x.unwrap())
    .boxed();

    let p_many = p_ops_comp.clone()
      .then(just('|').padded().ignore_then(p_ops_comp.clone()).repeated());
    choice((
      // (get 0 | foldl (+) 0) [[1]]
      just('(').padded()
        .ignore_then(p_many.clone()
                     .foldl(|a, b| lam("x", app(b.clone(), app(a, var("x"))))))
        .then_ignore(just(')').padded())
        .then(p_ops_comp.clone().padded().repeated())
        .foldl(app),
      // get 0 | foldl (+) 0
      p_many.clone()
        .foldl(|a, b| lam("x", app(b.clone(), app(a, var("x"))))),
    ))
  });

  p_expr
}

/// Apply an operation to possibly non-existent operands. The presence or
/// absence of operators corresponds to what kind of section (if any) is being
/// applied: (+), (x +), (+ x), x + y.
fn apply_op(a: Option<Expr>, (op, b): (Expr, Option<Expr>)) -> Option<Expr> {
  Some(match (a, b) {
    (None, None) => lam("x", lam("y", app(app(op, var("x")), var("y")))),
    (Some(a), None) => lam("y", app(app(op, a), var("y"))),
    (None, Some(b)) => lam("x", app(app(op, var("x")), b)),
    (Some(a), Some(b)) => app(app(op, a), b),
  })
}

#[cfg(test)]
pub fn parse(inp: &str) -> Result<Expr, Vec<Simple<char>>> {
  p_expr().then_ignore(end()).parse(inp.trim())
}

/// Parse an expression.
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
