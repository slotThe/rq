use std::{collections::BTreeMap, fmt::{self, Display}};

use self::pretty::Blocks;
use crate::r#type::{arr, Type};

pub mod pretty;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Builtin {
  Id,
  BConst,
  Get,
  Map,
  Filter,
  Foldl,
  Add,
  Sub,
  Mul,
  Div,
  Eq,
  Neq,
  Le,
  Leq,
  Ge,
  Geq,
}

impl Builtin {
  pub fn show(&self) -> &'static str {
    match self {
      Builtin::Id => "id",
      Builtin::BConst => "const",
      Builtin::Get => "get",
      Builtin::Map => "map",
      Builtin::Filter => "filter",
      Builtin::Foldl => "foldl",
      Builtin::Add => "+",
      Builtin::Sub => "-",
      Builtin::Mul => "*",
      Builtin::Div => "÷",
      Builtin::Eq => "=",
      Builtin::Neq => "≠",
      Builtin::Le => "<",
      Builtin::Leq => "≤",
      Builtin::Ge => ">",
      Builtin::Geq => "≥",
    }
  }
}

impl Display for Builtin {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.show()) }
}

#[derive(Clone)]
pub struct StdFun {
  name:      &'static str,
  aliases:   Vec<&'static str>,
  builtin:   Builtin,
  expr_type: Type,
  help:      Blocks,
}

impl Builtin {
  pub fn names(&self) -> Vec<&'static str> {
    let entry = STDLIB.get(self).unwrap();
    let mut res = vec![entry.name];
    res.extend_from_slice(&entry.aliases);
    res
  }
}

lazy_static! {
  pub static ref STDLIB_HELP: BTreeMap<&'static str, Blocks> =
    STDLIB.clone().into_values()
    .flat_map(|f| {
      [
        [(f.name, f.help.clone())].to_vec(),
        f.aliases.into_iter().map(|a| (a, f.help.clone())).collect(),
      ]
      .concat()
    })
    .collect();

  pub static ref STDLIB_CTX: BTreeMap<&'static str, Builtin> =
    STDLIB.clone().values().map(|f| (f.name, f.builtin)).collect();

  pub static ref STDLIB_TYPES: BTreeMap<String, Type> =
    STDLIB.clone().into_values().map(|f| (f.name.to_string(), f.expr_type)).collect();

  pub static ref STDLIB: BTreeMap<Builtin, StdFun> = BTreeMap::from([
    (
      Builtin::Id,
      StdFun {
        name:      Builtin::Id.show(),
        aliases:   vec![],
        builtin:   Builtin::Id,
        expr_type: arr(Type::JSON, Type::JSON),
        help:      Blocks::to_plain("Return the argument."),
      },
    ),
    (
      Builtin::BConst,
      StdFun {
        name:      Builtin::BConst.show(),
        aliases:   vec![],
        builtin:   Builtin::BConst,
        expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
        help:      Blocks::new().fancy("const a b").plain("returns").fancy("a."),
      },
    ),
    (
      Builtin::Get,
      StdFun {
        name:      Builtin::Get.show(),
        aliases:   vec![],
        builtin:   Builtin::Get,
        expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
        help:      Blocks::new().fancy("get i x").plain("gets the").fancy("i'th")
          .plain("thing out of").fancy("x,").plain("where").fancy("i")
          .plain("should be (evaluate to) a number or a string, and").fancy("x")
          .plain("should evaluate to an array or object."),
      },
    ),
    (
      Builtin::Map,
      StdFun {
        name:      Builtin::Map.show(),
        aliases:   vec![],
        builtin:   Builtin::Map,
        expr_type: arr(arr(Type::JSON, Type::JSON), arr(Type::JSON, Type::JSON)),
        help:      Blocks::new().fancy("map f xs").plain("applies").fancy("f")
          .plain("to every value in").fancy("xs,")
          .plain("which may be an array (in which case »value« means element) or an object (in \
                  which case it really means value)."),
      },
    ),
    (
      Builtin::Filter,
      StdFun {
        name:      Builtin::Filter.show(),
        aliases:   vec![],
        builtin:   Builtin::Filter,
        expr_type: arr(arr(Type::JSON, Type::JSON), arr(Type::JSON, Type::JSON)),
        help:      Blocks::new().fancy("filter p xs").plain("applies the predicate").fancy("p")
          .plain("to every value of").fancy("xs.")
          .plain("Keep the elements for which the predicate returns truthy."),
      },
    ),
    (
      Builtin::Foldl,
      StdFun {
        name:      Builtin::Foldl.show(),
        aliases:   vec![],
        builtin:   Builtin::Foldl,
        expr_type: arr(
          // (b -> a -> b) -> b -> [a] -> b
          arr(Type::JSON, arr(Type::JSON, Type::JSON)),
          arr(Type::JSON, arr(Type::JSON, Type::JSON)),
        ),
        help:      Blocks::new()
          .plain("Left-associative fold over an array or (values of an) object; e.g.,")
          .fancy("    foldl f α [x₁, x₂, …, xₙ]  ≡  f(f(…f(α, x₁), …), xₙ)."),
      },
    ),
    (
      Builtin::Add,
      StdFun {
        name:      Builtin::Add.show(),
        aliases:   vec![],
        builtin:   Builtin::Add,
        expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
        help:      Blocks::to_plain("Add two numbers or concatenate two strings."),
      },
    ),
    (
      Builtin::Sub,
      StdFun {
        name:      Builtin::Sub.show(),
        aliases:   vec![],
        builtin:   Builtin::Sub,
        expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
        help:      Blocks::to_plain("Subtract two numbers."),
      },
    ),
    (
      Builtin::Mul,
      StdFun {
        name:      Builtin::Mul.show(),
        aliases:   vec!["·"],
        builtin:   Builtin::Mul,
        expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
        help:      Blocks::to_plain("Multiply two numbers."),
      },
    ),
    (
      Builtin::Div,
      StdFun {
        name:      Builtin::Div.show(),
        aliases:   vec!["/"],
        builtin:   Builtin::Div,
        expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
        help:      Blocks::to_plain(
          "Divide two numbers. No guarantees if the denominator is zero—the world might \
           explode.",
        ),
      },
    ),
    (
      Builtin::Eq,
      StdFun {
        name:      Builtin::Eq.show(),
        aliases:   vec![],
        builtin:   Builtin::Eq,
        expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
        help:      Blocks::to_plain("Check two expressions for equality."),
      },
    ),
    (
      Builtin::Neq,
      StdFun {
        name:      Builtin::Neq.show(),
        aliases:   vec!["!=", "/="],
        builtin:   Builtin::Neq,
        expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
        help:      Blocks::to_plain("Check two expressions for non-equality."),
      },
    ),
    (
      Builtin::Le,
      StdFun {
        name:      Builtin::Le.show(),
        aliases:   vec![],
        builtin:   Builtin::Le,
        expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
        help:      Blocks::new().fancy("e < e'").plain("checks whether")
          .fancy("e").plain("is less than").fancy("e'."),
      },
    ),
    (
      Builtin::Leq,
      StdFun {
        name:      Builtin::Leq.show(),
        aliases:   vec!["<="],
        builtin:   Builtin::Leq,
        expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
        help:      Blocks::new().fancy("e ≤ e'").plain("checks whether")
          .fancy("e").plain("is less-or-equal-to").fancy("e'."),
      },
    ),
    (
      Builtin::Ge,
      StdFun {
        name:      Builtin::Ge.show(),
        aliases:   vec![],
        builtin:   Builtin::Ge,
        expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
        help:      Blocks::new().fancy("e > e'").plain("checks whether")
          .fancy("e").plain("is greater than").fancy("e'."),
      },
    ),
    (
      Builtin::Geq,
      StdFun {
        name:      Builtin::Geq.show(),
        aliases:   vec![">="],
        builtin:   Builtin::Geq,
        expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
        help:      Blocks::new().fancy("e ≥ e'").plain("checks whether")
          .fancy("e").plain("is greater-or-equal-to").fancy("e'."),
      },
    ),
  ]);
}
