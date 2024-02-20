//! The standard library.

use std::{collections::BTreeMap, fmt::{self, Display}, sync::LazyLock};

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

macro_rules! mk_fun {
  // No aliases
  ($name:expr, $type:expr, $help:expr $(,)?) => {
    (
      $name,
      StdFun {
        name:      $name.show(),
        aliases:   vec![],
        builtin:   $name,
        expr_type: $type,
        help:      $help,
      },
    )
  };
  // Aliases
  ($name:expr, $type:expr, $help:expr, $( $alias:expr ),+  $(,)?) => {{
    let aliases = vec![$($alias),+];
    (
      $name,
      StdFun {
        name:      $name.show(),
        aliases:   aliases.clone(),
        builtin:   $name,
        expr_type: $type,
        help:      $help.aliases([[$name.show()].to_vec(), aliases].concat()),
      },
    )
  }};
}

pub static STDLIB_CTX: LazyLock<BTreeMap<&'static str, Builtin>> = LazyLock::new(|| {
  STDLIB
    .clone()
    .values()
    .map(|f| (f.name, f.builtin))
    .collect::<BTreeMap<&'static str, Builtin>>()
});

pub static STDLIB_HELP: LazyLock<BTreeMap<&'static str, Blocks>> = LazyLock::new(|| {
  STDLIB
    .clone()
    .into_values()
    .flat_map(|f| {
      [
        [(f.name, f.help.clone())].to_vec(),
        f.aliases.into_iter().map(|a| (a, f.help.clone())).collect(),
      ]
      .concat()
    })
    .collect()
});

pub static STDLIB_TYPES: LazyLock<BTreeMap<String, Type>> = LazyLock::new(|| {
  STDLIB
    .clone()
    .into_values()
    .map(|f| (f.name.to_string(), f.expr_type))
    .collect()
});

pub static STDLIB: LazyLock<BTreeMap<Builtin, StdFun>> = LazyLock::new(|| {
  BTreeMap::from([
    mk_fun!(
      Builtin::Id,
      arr(Type::JSON, Type::JSON),
      Blocks::to_plain("Return the argument.")
    ),
    mk_fun!(
      Builtin::BConst,
      arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      Blocks::new()
        .fancy("const a b")
        .plain("returns")
        .fancy("a.")
    ),
    mk_fun!(
      Builtin::Get,
      arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      Blocks::new()
        .fancy("get i x")
        .plain("gets the")
        .fancy("i'th")
        .plain("thing out of")
        .fancy("x,")
        .plain("where")
        .fancy("i")
        .plain("should be (evaluate to) a number or a string, and")
        .fancy("x")
        .plain("should evaluate to an array or object.")
    ),
    mk_fun!(
      Builtin::Map,
      arr(arr(Type::JSON, Type::JSON), arr(Type::JSON, Type::JSON)),
      Blocks::new()
        .fancy("map f xs")
        .plain("applies")
        .fancy("f")
        .plain("to every value in")
        .fancy("xs,")
        .plain(
          "which may be an array (in which case »value« means element) or an object (in \
           which case it really means value)."
        ),
    ),
    mk_fun!(
      Builtin::Filter,
      arr(arr(Type::JSON, Type::JSON), arr(Type::JSON, Type::JSON)),
      Blocks::new()
        .fancy("filter p xs")
        .plain("applies the predicate")
        .fancy("p")
        .plain("to every value of")
        .fancy("xs.")
        .plain("Keep the elements for which the predicate returns truthy."),
    ),
    mk_fun!(
      Builtin::Foldl,
      arr(
        arr(Type::JSON, arr(Type::JSON, Type::JSON)),
        arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      ),
      Blocks::new()
        .plain("Left-associative fold over an array or (values of an) object; e.g.,")
        .fancy("    foldl f α [x₁, x₂, …, xₙ]  ≡  f(f(…f(α, x₁), …), xₙ)."),
    ),
    mk_fun!(
      Builtin::Add,
      arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      Blocks::to_plain("Add two numbers or concatenate two strings."),
    ),
    mk_fun!(
      Builtin::Sub,
      arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      Blocks::to_plain("Subtract two numbers."),
    ),
    mk_fun!(
      Builtin::Mul,
      arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      Blocks::to_plain("Multiply two numbers."),
      "·"
    ),
    mk_fun!(
      Builtin::Div,
      arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      Blocks::to_plain(
        "Divide two numbers. No guarantees if the denominator is zero—the world might \
         explode.",
      ),
      "/"
    ),
    mk_fun!(
      Builtin::Eq,
      arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      Blocks::to_plain("Check two expressions for equality."),
    ),
    mk_fun!(
      Builtin::Neq,
      arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      Blocks::to_plain("Check two expressions for non-equality."),
      "!=",
      "/=",
    ),
    mk_fun!(
      Builtin::Le,
      arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      Blocks::new()
        .fancy("e < e'")
        .plain("checks whether")
        .fancy("e")
        .plain("is less than")
        .fancy("e'."),
    ),
    mk_fun!(
      Builtin::Leq,
      arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      Blocks::new()
        .fancy("e ≤ e'")
        .plain("checks whether")
        .fancy("e")
        .plain("is less-or-equal-to")
        .fancy("e'."),
      "<="
    ),
    mk_fun!(
      Builtin::Ge,
      arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      Blocks::new()
        .fancy("e > e'")
        .plain("checks whether")
        .fancy("e")
        .plain("is greater than")
        .fancy("e'."),
    ),
    mk_fun!(
      Builtin::Geq,
      arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      Blocks::new()
        .fancy("e ≥ e'")
        .plain("checks whether")
        .fancy("e")
        .plain("is greater-or-equal-to")
        .fancy("e'."),
      ">="
    ),
  ])
});
