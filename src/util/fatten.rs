use std::collections::BTreeMap;

use chumsky::{prelude::*, text::newline};
use fatten_core::*;
use lhs::*;
use rhs::*;

pub fn fatten(inp: &str) -> JsonMap {
  parse::parse(inp)
    .into_iter()
    .map(create_map)
    .reduce(merge)
    .map(arrify_map)
    .and_then(|jm| match jm {
      JsonMap::Branch(m) => m.get(&Lhs::key("json")).map(|box m| m.clone()),
      _ => None,
    })
    .unwrap_or(JsonMap::Tip(NULL))
}

mod lhs {
  #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
  pub enum Lhs {
    Ix(u32),
    Key(String),
  }

  impl std::fmt::Display for Lhs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
        Lhs::Key(s) => write!(f, "\"{s}\""),
        Lhs::Ix(i) => write!(f, "{i}"),
      }
    }
  }

  impl Lhs {
    pub fn key(s: &str) -> Self { Self::Key(s.to_string()) }
  }
}

mod rhs {
  use crate::{expr::Const, util::ord_f64::OrdF64};

  /// Intermediate form when fattening a flattened JSON structure.
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub enum Rhs {
    Const(Const),
    Arr, // blah = [];
    Obj, // blah = {};
  }

  pub const NULL: Rhs = Rhs::Const(Const::Null);

  impl Rhs {
    pub fn num(n: impl Into<OrdF64>) -> Self { Self::Const(Const::Num(n.into())) }

    pub fn str(s: impl ToString) -> Self { Self::Const(Const::String(s.to_string())) }
  }
}

mod fatten_core {
  use super::*;

  /// A [JsonMap] is a subset of an [Expr] that better fits the problem of
  /// fattening.
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub enum JsonMap {
    Tip(Rhs),
    Branch(BTreeMap<Lhs, Box<JsonMap>>),
    Arr(Vec<JsonMap>),
  }

  impl std::fmt::Display for JsonMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
        JsonMap::Tip(Rhs::Obj) => write!(f, "{{}}"),
        JsonMap::Tip(Rhs::Arr) => write!(f, "[]"),
        JsonMap::Tip(Rhs::Const(c)) => write!(f, "{c}"),
        JsonMap::Arr(arr) => {
          write!(f, "[")?;
          for el in &arr[0..=arr.len() - 2] {
            write!(f, "{}, ", el)?;
          }
          write!(f, "{}", arr[arr.len() - 1])?;
          write!(f, "]")
        },
        JsonMap::Branch(map) => {
          write!(f, "{{")?;
          write!(
            f,
            "{}",
            map
              .iter()
              .map(|(k, v)| format!("{k}: {v}"))
              .intersperse(", ".to_string())
              .collect::<String>()
          )?;
          write!(f, "}}")
        },
      }
    }
  }

  impl JsonMap {
    fn branch<T: Into<Vec<(Lhs, Self)>>>(items: T) -> Self {
      let mut res = BTreeMap::new();
      items.into().into_iter().for_each(|(lhs, rhs)| {
        res.insert(lhs, Box::new(rhs));
      });
      JsonMap::Branch(res)
    }
  }

  // {1: ....} -> [.....]
  pub fn arrify_map(jm: JsonMap) -> JsonMap {
    match jm {
      JsonMap::Tip(_) => jm,
      JsonMap::Branch(mut map) => match map.first_entry() {
        Some(entry) => match entry.key() {
          Lhs::Key(_) => JsonMap::Branch(map),
          Lhs::Ix(_) => {
            JsonMap::Arr(map.iter().fold(Vec::new(), |mut acc, (_, box k)| {
              acc.push(arrify_map(k.clone()));
              acc
            }))
          },
        },
        None => JsonMap::Branch(
          map
            .into_iter()
            .map(|(lhs, box m)| (lhs, Box::new(arrify_map(m))))
            .collect(),
        ),
      },
      JsonMap::Arr(_) => todo!(),
    }
  }

  pub fn merge(jm1: JsonMap, jm2: JsonMap) -> JsonMap {
    match (&jm1, &jm2) {
      (JsonMap::Branch(m1), JsonMap::Branch(m2)) => {
        if m1.is_empty() {
          jm2
        } else if m2.is_empty() {
          jm1
        } else {
          JsonMap::Branch(m2.iter().fold(m1.clone(), |mut acc, (lhs, rhs)| {
            match acc.get(lhs) {
              None | Some(box JsonMap::Tip(_)) => {
                acc.insert(lhs.clone(), rhs.clone());
              },
              Some(box JsonMap::Branch(m)) => {
                acc.insert(
                  lhs.clone(),
                  Box::new(merge(JsonMap::Branch(m.clone()), *rhs.clone())),
                );
              },
              _ => todo!(),
            };
            acc
          }))
        }
      },
      _ => todo!(),
    }
  }

  /// Create a nested map from a single line.
  pub fn create_map((lhs, rhs): (Vec<Lhs>, Rhs)) -> JsonMap {
    lhs
      .into_iter()
      .rfold(JsonMap::Tip(rhs), |map, lhs| JsonMap::branch([(lhs, map)]))
    // ^ The right fold is important for the order here.
  }

  #[cfg(test)]
  mod tests {
    use parse::parse;
    use JsonMap as JM;
    use JsonMap::*;
    use Lhs as L;
    use Rhs as R;

    use super::*;

    #[test]
    fn create_map_test() {
      let line = parse("furble = {};")[0].clone();
      assert_eq!(
        JM::branch([(L::key("furble"), Tip(R::Obj))]),
        create_map(line)
      );

      let line = parse("x = 1;")[0].clone();
      assert_eq!(
        JM::branch([(L::key("x"), Tip(R::num(1)))]),
        create_map(line)
      );

      let line = parse("x.y.z = \"w\";")[0].clone();
      assert_eq!(
        JM::branch([(
          L::key("x"),
          JM::branch([(L::key("y"), JM::branch([(L::key("z"), Tip(R::str("w")))]))])
        )]),
        create_map(line)
      );
    }

    #[test]
    fn merge_test() {
      let line1 = create_map(parse("x.y = \"w\";")[0].clone());
      let line2 = create_map(parse("x.z = 1;")[0].clone());
      assert_eq!(
        JM::branch([(
          L::key("x"),
          JM::branch([
            (L::key("y"), Tip(R::str("w"))),
            (L::key("z"), Tip(R::num(1)))
          ])
        )]),
        merge(line1, line2)
      );

      let line1 = create_map(parse("x.y.z = \"w\";")[0].clone());
      let line2 = create_map(parse("x.y.w = 1;")[0].clone());
      assert_eq!(
        JM::branch([(
          L::key("x"),
          JM::branch([(
            L::key("y"),
            JM::branch([
              (L::key("z"), Tip(R::str("w"))),
              (L::key("w"), Tip(R::num(1)))
            ])
          )])
        )]),
        merge(line1, line2)
      );
    }
  }
}

mod parse {
  use super::*;

  pub fn parse(inp: &str) -> Vec<(Vec<Lhs>, Rhs)> {
    p_fatten().then_ignore(end()).parse(inp.trim()).unwrap()
  }

  fn p_fatten<'a>(
  ) -> impl Parser<'a, &'a str, Vec<(Vec<Lhs>, Rhs)>, extra::Err<Rich<'a, char>>> {
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

    let string = none_of("\\\"")
      .ignored()
      .or(escape)
      .repeated()
      .to_slice()
      .map(ToString::to_string)
      .delimited_by(just('"'), just('"'))
      .boxed();

    let ident =
    // a[1]
    (text::ident()
      .then(text::int(10).delimited_by(just('['), just(']')))
      .map(|(s, n): (&str, &str)| vec![Lhs::key(s), Lhs::Ix(n.parse::<u32>().unwrap())]))
    // a["b"]
    .or(
      text::ident()
        .then(string.clone().delimited_by(just('['), just(']')))
        .map(|(s1, s2): (&str, String)| vec![Lhs::key(s1), Lhs::key(&s2)]))
    // a
    .or(text::ident().map(|s: &str| vec![Lhs::key(s)]));

    let digits = text::digits(10).to_slice();

    let frac = just('.').then(digits);

    let exp = just('e')
      .or(just('E'))
      .then(one_of("+-").or_not())
      .then(digits);

    let number = just('-')
      .or_not()
      .then(text::int(10))
      .then(frac.or_not())
      .then(exp.or_not())
      .to_slice()
      .map(|s: &str| s.parse::<i32>().unwrap())
      .boxed();

    let lhs = ident
      .clone()
      .foldl(just('.').ignore_then(ident).repeated(), |mut acc, i| {
        acc.extend(i);
        acc
      })
      .boxed();

    let rhs = just("=")
      .padded()
      .ignore_then(choice((
        just("[]").to(Rhs::Arr),
        just("{}").to(Rhs::Obj),
        just("null").to(NULL),
        number.map(Rhs::num),
        string.map(Rhs::str),
      )))
      .padded()
      .then_ignore(just(';'))
      .boxed();

    lhs.then(rhs).separated_by(newline()).collect()
  }

  #[cfg(test)]
  mod tests {
    use Lhs as L;
    use Rhs as R;

    use super::*;
    use crate::expr::Const;

    #[test]
    fn parse_test() {
      assert_eq!(
        parse("a[\"1\"] = 2;"),
        vec![(vec![L::key("a"), L::key("1")], R::num(2))]
      );
      assert_eq!(
        parse("a.b = \"z\";"),
        vec![(vec![L::key("a"), L::key("b")], R::str("z"))]
      );
      assert_eq!(
        parse("a[1] = null;"),
        vec![(vec![L::key("a"), L::Ix(1)], R::Const(Const::Null))]
      );
    }
  }
}
