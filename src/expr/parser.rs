//! Lots stolen from chumsky's [json] and [nano_rust] examples.
//!
//! [json]: https://github.com/zesterer/chumsky/blob/0.9/examples/json.rs
//! [nano_rust]: https://github.com/zesterer/chumsky/blob/0.9/examples/nano_rust.rs

use std::collections::BTreeMap;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::prelude::*;

use super::{app, de_bruijn::DBVar, expr_str, if_then_else, num, var, λ, Const};
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
        .ignore_then(p_varlike.padded().repeated().at_least(1))
        .then_ignore(just("->").or(just("→")).padded());
      let rust_head = p_varlike.padded().separated_by(just(',')).at_least(1)
        .delimited_by(just('|'), just('|'));
      haskell_head.or(rust_head)
        .padded()
        .then(p_expr.clone())
        .map(|(hs, b)| hs.iter().rev().fold(b, |acc, h| λ(h.as_str(), acc)))
        .labelled("lambda")
    };

    let p_if_then_else = (just("if").padded())
      .ignore_then(p_expr.clone())
      .then_ignore(just("then").padded())
      .then(p_expr.clone())
      .then_ignore(just("else").padded())
      .then(p_expr.clone())
      .map(|((i, t), e)| if_then_else(i, t, e));

    // x.1, x.blah, .1, .blah, … See the docs of `mk_dot_syntax`.
    let p_dot_syntax = move |head: Option<Expr>| {
      just('.')
        .ignore_then(
          text::int(10).map(|i: String| num(i.parse::<f64>().unwrap()))
            .or(p_varlike.or(p_str).map(expr_str))
        )
        .repeated().at_least(1)
        .map(move |xs| mk_dot_syntax(head.clone(), xs))};

    let p_app = recursive(|p_app| {
      let fun = {
        // The f in f x.
        let head = p_lam.clone().padded().delimited_by(just('('), just(')'))
          .or(p_var)
          .or(p_app.clone().padded().delimited_by(just('('), just(')')));
        // Check for dot syntax: x.1, x.blah, …
        head.clone().then_with(move |x| p_dot_syntax(Some(x)))
          .or(head)
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
            p_app.padded().delimited_by(just('('), just(')')),
            p_dot_syntax(None).delimited_by(just('('), just(')')),
            p_dot_syntax(None),
            p_expr.clone().padded().delimited_by(just('('), just(')')),
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
    let p_symbol = |b: Builtin| {
      choice(b.names().into_iter().map(just).collect::<Vec<_>>())
        .to(Expr::Builtin(b))
    };

    let all_exprs = choice((
      p_obj,
      p_array,
      p_const,
      p_if_then_else,
      p_app,
      p_lam,
      p_var,
      p_dot_syntax(None),
      p_expr.clone().padded().delimited_by(just('('), just(')')),
    ))
    .boxed();

    let mul_sym = choice((
      p_symbol(Builtin::Mul),
      p_symbol(Builtin::Div),
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
      p_symbol(Builtin::Add),
      p_symbol(Builtin::Sub),
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
      p_symbol(Builtin::Eq),
      p_symbol(Builtin::Neq),
      p_symbol(Builtin::Leq),
      p_symbol(Builtin::Geq),
      p_symbol(Builtin::Le),
      p_symbol(Builtin::Ge),
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

    let parse = p_ops_comp.clone()
      .then(just('|').padded().ignore_then(p_ops_comp.clone()).repeated())
      .foldl(|a, b| λ("x", app(b.clone(), app(a, var("x")))));
    parse.clone().delimited_by(just('('), just(')')).or(parse.clone())
  });

  p_expr.clone()
    .then(p_expr.clone().padded().repeated())
    .foldl(app)
}

/// Apply an operation to possibly non-existent operands. The presence or
/// absence of operators corresponds to what kind of section (if any) is being
/// applied: (+), (x +), (+ x), x + y.
fn apply_op(a: Option<Expr>, (op, b): (Expr, Option<Expr>)) -> Option<Expr> {
  Some(match (a, b) {
    (None, None) => λ("x", λ("y", app(app(op, var("x")), var("y")))),
    (Some(a), None) => λ("y", app(app(op, a), var("y"))),
    (None, Some(b)) => λ("x", app(app(op, var("x")), b)),
    (Some(a), Some(b)) => app(app(op, a), b),
  })
}

/// Imbue the dot syntax with meaning.
///
/// * head: is `None` if the syntax is of the form `.a.b.c`, in which case it
///   will desugar to `λω. get a (get b (get c ω))`. A value of `Some(x)`
///   instead indicates expressions of the form `x.a.b.c`; in that case, the
///   desugared value will be `get a (get b (get c x))`.
fn mk_dot_syntax(head: Option<Expr>, mut xs: Vec<Expr>) -> Expr {
  fn go(xs: &[Expr], end: &Expr) -> Expr {
    match xs {
      [] => todo!(),
      [x] => app(app(var("get"), x.clone()), end.clone()),
      [x, ys @ ..] => app(app(var("get"), x.clone()), go(ys, end)),
    }
  }
  xs.reverse();
  match head {
    Some(h) => go(&xs, &h),
    None => λ("ω", go(&xs, &var("ω"))),
  }
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
