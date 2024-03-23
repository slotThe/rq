//! Lots stolen from chumsky's [json] and [nano_rust] examples.
//!
//! [json]: https://github.com/zesterer/chumsky/blob/0.9/examples/json.rs
//! [nano_rust]: https://github.com/zesterer/chumsky/blob/0.9/examples/nano_rust.rs

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::prelude::*;

use super::{app, de_bruijn::DBVar, expr_str, if_then_else, num, var, λ, Const};
use crate::{eval::stdlib::Builtin, r#type::Type, Expr};

// This is an absolutely unreadable mess, and the compulsively imperative
// nature of chumsky really makes my head spin a bit. Plus, compile times went
// up by a lot, and I essentially have to comment out all of the parser when
// working on another component so things stay responsive. Lots of fun. And
// yet, all this pain seems worth it for those error messages. Still, I yearn
// for the days when I can return to nom.
#[rustfmt::skip]
fn p_expr<'a>() -> impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> {
  // You might think that I need a lexer; and you would be right.
  let p_varlike = text::ident().try_map(
    move |s: &str, span| {
      if ["if", "then", "else"].contains(&s) || s.starts_with('λ') { // XXX: ???
        Err(Rich::custom(span, format!("{s}: Invalid variable name")))
      } else {
        Ok(s)
      }
    },
  );

  let p_var = p_varlike
    .then(just("@").ignore_then(text::int(10).from_str().unwrapped()).or_not())
    .map(|(a, b)| Expr::Var(DBVar{ name: a.to_string(), level: b.unwrap_or(0) }));

  let p_num = {
    let frac = just('.').then(text::digits(10));
    let exp = just('e').or(just('E'))
      .then(one_of("+-").or_not())
      .then(text::digits(10));
    just('-').or_not()
      .then(text::int(10))
      .then(frac.or_not())
      .then(exp.or_not())
      .to_slice()
      .map(|s: &str| Const::Num(s.parse().unwrap()))
      .boxed()
  };

  let escape = just('\\').then(
    choice((
      just('\\'),
      just('/'),
      just('"'),
      just('b').to('\x08'),
      just('f').to('\x0C'),
      just('n').to('\n'),
      just('r').to('\r'),
      just('t').to('\t'),
      just('u').ignore_then(text::digits(16).exactly(4).to_slice().validate(
        |digits, e, emitter| {
          char::from_u32(u32::from_str_radix(digits, 16).unwrap()).unwrap_or_else(
            || {
              emitter.emit(Rich::custom(e.span(), "invalid unicode character"));
              '\u{FFFD}' // unicode replacement character
            },
          )
        },
      )),
    )))
    .ignored()
    .boxed();

  let p_str = none_of("\\\"").ignored().or(escape)
    .repeated().to_slice()
    .delimited_by(just('"'), just('"'))
    .boxed();

  let p_const = choice((
    just("true").to(true)
      .or(just("false").to(false))
      .map(Const::Bool),
    just("null").to(Const::Null),
    p_num,
    p_str.clone().map(|s: &str| Const::String(s.to_string())),
  ))
  .map(Expr::Const);

  let p_expr = recursive(|p_expr| {
    let p_array = p_expr.clone().separated_by(just(',').padded())
      .collect().padded().delimited_by(just('['), just(']'))
      .map(Expr::Arr);

    let p_obj = {
      let p_kv = p_varlike.or(p_str.clone())
        .map(|s: &str| Expr::Const(Const::String(s.to_string())))
        .then_ignore(just(':').padded())
        .or(p_expr.clone().then_ignore(just(':').padded()))
        .then(p_expr.clone())
        .boxed();
      p_kv.clone().separated_by(just(',').padded())
        .collect().padded()
        .delimited_by(just('{'), just('}'))
        .map(Expr::Obj)
    };

    let p_lam = choice((
      // Haskell-like
      just("λ").or(just("\\")).padded()
        .ignore_then(
          p_varlike.padded().repeated().at_least(1)
            .foldr(
              just("→").or(just("->")).padded()
                .ignore_then(p_expr.clone()),
              λ
            )
        ),
      // Rust-like
      p_varlike.padded().separated_by(just(',')).at_least(1).collect::<Vec<_>>()
        .delimited_by(just('|'), just('|'))
        .then(p_expr.clone())
        .map(|(hs, e)| hs.iter().rfold(e, |acc, h| λ(h, acc))),
    ));

    let p_if_then_else = just("if").padded()
      .ignore_then(p_expr.clone())
      .then_ignore(just("then").padded())
      .then(p_expr.clone())
      .then_ignore(just("else").padded())
      .then(p_expr.clone())
      .map(|((i, t), e)| if_then_else(i, t, e));

    // chumsky 1.x removed then_with, and I have no idea how to use the
    // context-manipulating functions :/
    let p_dot_syntax_worker = just('.')
      .ignore_then(
        text::int(10).from_str::<f64>().unwrapped().map(num)
          .or(p_varlike.or(p_str.clone()).map(expr_str))
      )
      .repeated().at_least(1).collect();

    let p_dot_syntax_no_head = p_dot_syntax_worker.clone()
      .map(|xs| mk_dot_syntax(None, xs));

    let p_app = recursive(|p_app| {
      // The f in f x.
      let head = choice((
        p_lam.clone().padded().delimited_by(just('('), just(')')),
        p_var,
        p_app.clone().padded().delimited_by(just('('), just(')')),
      ));

      // x.1, x.blah, .1, .blah, … See the docs of `mk_dot_syntax`.
      let p_dot_syntax_head = head.clone()
        .then(p_dot_syntax_worker.clone())
        .map(|(head, xs)| mk_dot_syntax(Some(head), xs));

      let p_dot_syntax_try_head = head.clone().or_not()
        .then(p_dot_syntax_worker.clone())
        .map(|(mb_head, xs)| mk_dot_syntax(mb_head, xs));

      let go = p_dot_syntax_head.or(head.clone()) // the f in f x
        .foldl(
          // the x in f x
          choice((
            p_dot_syntax_try_head.clone(),
            p_var,
            p_if_then_else.clone(),
            p_const.clone(),
            p_array.clone(),
            p_obj.clone(),
            p_lam.clone().padded().delimited_by(just('('), just(')')),
            p_app.padded().delimited_by(just('('), just(')')),
            p_dot_syntax_try_head.clone().delimited_by(just('('), just(')')),
            p_expr.clone().padded().delimited_by(just('('), just(')')),
            p_expr.clone().padded(),
          )).padded().repeated(),
          app
        ).boxed();
      go.clone()
        .or(go.clone().padded().delimited_by(just('('), just(')')))
        .padded()
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
      p_dot_syntax_no_head,
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
        .then(mul_sym.clone()
              .then(all_exprs.clone().map(Some)).repeated().collect()),
      // An operator without anything: (=)
      mul_sym.clone()
        .delimited_by(just('('), just(')'))
        .map(|op| (None, Vec::from([(op, None)]))),
    ))
    .map(|(a, bs)| bs.iter().cloned().fold(a, apply_op).unwrap())
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
        .then(sum_sym.clone()
              .then(p_ops_mul.clone().map(Some)).repeated().collect()),
      // An operator without anything: (=)
      sum_sym.clone()
        .delimited_by(just('('), just(')'))
        .map(|op| (None, Vec::from([(op, None)]))),
    ))
    .map(|(a, bs)| bs.iter().cloned().fold(a, apply_op).unwrap())
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
        .then(comp_sym.clone()
              .then(p_ops_sum.clone().map(Some)).repeated().collect()),
      // An operator without anything: (=)
      comp_sym.clone()
        .delimited_by(just('('), just(')'))
        .map(|op| (None, Vec::from([(op, None)]))),
    ))
    .map(|(a, bs)| bs.iter().cloned().fold(a, apply_op).unwrap())
    .boxed();

    // Parse a type.
    let p_type = recursive(|p_type| {
      let p_forall = just("forall").or(just("∀")).padded()
        .ignore_then(text::ident().then_ignore(just(".").padded()))
        .then(p_type.clone())
        .map(|(α, t)| Type::forall(α, t));
      let inner = choice((
        p_forall.clone(),            // ∀α. A
        just("Num").to(Type::Num),   // Num
        text::ident().try_map(       // JSON
          move |s: &str, span| {
            if "json" == &s.to_lowercase() {
              Ok(Type::JSON)
            } else {
              Err(Rich::custom(span, format!("{s}: Invalid type name")))
            }
          },
        ),
        text::ident().map(|s: &str| Type::Var(s.to_string())),
        p_type.clone().padded().delimited_by(just('('),just(')'))
      ));
      inner.clone()
        .then_ignore(just("->").or(just("→")).padded())
        .repeated()
        .foldr(inner.clone(), Type::arr)
    });

    // Parse an expression with a possible type annotation: expr ∷ Type
    let p_ann = {
      let inner = p_ops_comp.clone()
        .then(just("::").or(just("∷")).padded().ignore_then(p_type.clone()).or_not())
        .map(|(e, t)| match t {
          None => e,
          Some(t) => Expr::Ann(Box::new(e), t),
        });
      choice((
        inner.clone().delimited_by(just('('),just(')')),
        inner.clone(),
        p_ops_comp.clone()
      ))
    };

    let parse = p_ann.clone()
      .foldl(
        just('|').padded().ignore_then(p_ann.clone()).repeated(),
        |a, b| λ("x", app(b.clone(), app(a, var("x"))))
      );
    parse.clone().delimited_by(just('('), just(')')).or(parse.clone())
  });

  p_expr.clone()
    .foldl(p_expr.clone().padded().repeated(), app)
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

/// Parse JSON into an [Expr]. Implementation taken from [chumsky's examples].
/// This is here mostly for efficiency, as the expression language is quite a
/// bit more complicated than JSON itself.
///
/// [chumsky's examples]: https://github.com/zesterer/chumsky/blob/e3b91d80da96a38d9798fccf6549bd28d00e52cc/examples/json.rs
#[rustfmt::skip]
fn p_json<'a>() -> impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> {
  recursive(|value| {
    let digits = text::digits(10).to_slice();

    let frac = just('.').then(digits);

    let exp = just('e').or(just('E'))
      .then(one_of("+-").or_not())
      .then(digits);

    let number = just('-').or_not()
      .then(text::int(10))
      .then(frac.or_not())
      .then(exp.or_not())
      .to_slice()
      .map(|s: &str| s.parse().unwrap())
      .boxed();

    let escape = just('\\')
      .then(choice((
        just('\\'),
        just('/'),
        just('"'),
        just('b').to('\x08'),
        just('f').to('\x0C'),
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
        just('u').ignore_then(text::digits(16).exactly(4).to_slice().validate(
          |digits, e, emitter| {
            char::from_u32(u32::from_str_radix(digits, 16).unwrap()).unwrap_or_else(
              || {
                emitter.emit(Rich::custom(e.span(), "invalid unicode character"));
                '\u{FFFD}' // unicode replacement character
              },
            )
          },
        )),
      )))
      .ignored()
      .boxed();

    let string = none_of("\\\"").ignored()
      .or(escape).repeated().to_slice()
      .map(ToString::to_string)
      .delimited_by(just('"'), just('"'))
      .boxed();

    let array = value.clone()
      .separated_by(just(',').padded())
      .allow_trailing()
      .collect()
      .padded()
      .delimited_by(just('['), just(']'))
      .boxed();

    let member = string.clone()
      .then_ignore(just(':').padded())
      .then(value)
      .map(|(s, v)| (Expr::Const(Const::String(s)), v));
    let object = member.clone()
      .separated_by(just(',').padded())
      .collect()
      .padded()
      .delimited_by(just('{'), just('}'))
      .boxed();

    choice((
      just("null").to(Expr::Const(Const::Null)),
      just("true").to(Expr::Const(Const::Bool(true))),
      just("false").to(Expr::Const(Const::Bool(false))),
      number.map(|n| Expr::Const(Const::Num(n))),
      string.map(|s| Expr::Const(Const::String(s))),
      array.map(Expr::Arr),
      object.map(Expr::Obj),
    ))
    .padded()
  })
}

#[cfg(test)]
pub fn parse(inp: &str) -> Expr { p_expr().then_ignore(end()).parse(inp.trim()).unwrap() }

/// Parse an [Expr].
pub fn parse_expr(inp: &str) -> Option<Expr> { parse_main(p_expr, inp) }

/// Parse JSON into an [Expr].
pub fn parse_json(inp: &str) -> Option<Expr> { parse_main(p_json, inp) }

/// Parse the input with the given parser.
fn parse_main<'a, T>(parser: impl Fn() -> T, inp: &'a str) -> Option<Expr>
where
  T: Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>>,
{
  let (expr, errs) = parser()
    .then_ignore(end())
    .parse(inp.trim())
    .into_output_errors();
  match expr {
    Some(e) => Some(e),
    None => {
      errs.into_iter().for_each(|e| {
        Report::build(ReportKind::Error, (), e.span().start)
          .with_message(e.to_string())
          .with_label(
            Label::new(e.span().into_range())
              .with_message(e.reason().to_string())
              .with_color(Color::Red),
          )
          .finish()
          .print(Source::from(&inp))
          .unwrap()
      });
      None
    },
  }
}
