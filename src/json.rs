use std::collections::BTreeMap;

use chumsky::prelude::*;

use crate::{expr::Expr, parser::parse_main, util::r#const::Const};

/// A type for ordinary JSON data.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Json {
  Const(Const),
  Arr(Vec<Json>),
  Obj(BTreeMap<Json, Json>),
}

impl From<Json> for Expr {
  fn from(json: Json) -> Self {
    match json {
      Json::Const(c) => Expr::Const(c),
      Json::Arr(a) => Expr::Arr(a.into_iter().map(Expr::from).collect()),
      Json::Obj(o) => {
        Expr::Obj(o.into_iter().map(|(k, v)| (k.into(), v.into())).collect())
      },
    }
  }
}

/// Parse JSON data into a [Json] type.
pub fn parse_json(inp: &str) -> Option<Json> { parse_main(p_json, inp) }

/// Parse JSON into a [Json]. Implementation taken from [chumsky's examples].
/// This is here mostly for efficiency, as the expression language is quite a
/// bit more complicated than JSON itself.
///
/// [chumsky's examples]: https://github.com/zesterer/chumsky/blob/e3b91d80da96a38d9798fccf6549bd28d00e52cc/examples/json.rs
#[rustfmt::skip]
fn p_json<'a>() -> impl Parser<'a, &'a str, Json, extra::Err<Rich<'a, char>>> {
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
      .map(|(s, v)| (Json::Const(Const::String(s)), v));
    let object = member.clone()
      .separated_by(just(',').padded())
      .collect()
      .padded()
      .delimited_by(just('{'), just('}'))
      .boxed();

    choice((
      just("null").to(Json::Const(Const::Null)),
      just("true").to(Json::Const(Const::Bool(true))),
      just("false").to(Json::Const(Const::Bool(false))),
      number.map(|n| Json::Const(Const::Num(n))),
      string.map(|s| Json::Const(Const::String(s))),
      array.map(Json::Arr),
      object.map(Json::Obj),
    ))
    .padded()
  })
}
