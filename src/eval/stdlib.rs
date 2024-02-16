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
      Builtin::Mul => "·",
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
  builtin:   Builtin,
  expr_type: Type,
  help:      Blocks,
}

lazy_static! {
  pub static ref STDLIB_HELP: BTreeMap<&'static str, Blocks> =
    BTreeMap::from(STDLIB.clone().map(|f| (f.name, f.help)));

  pub static ref STDLIB_CTX: BTreeMap<&'static str, Builtin> =
    BTreeMap::from(STDLIB.clone().map(|f| (f.name, f.builtin)));

  pub static ref STDLIB_TYPES: BTreeMap<String, Type> =
    BTreeMap::from(STDLIB.clone().map(|f| (f.name.to_string(), f.expr_type)));

  pub static ref STDLIB: [StdFun; 16] = [
    StdFun {
      name:      Builtin::Id.show(),
      builtin:   Builtin::Id,
      expr_type: arr(Type::JSON, Type::JSON),
      help:      Blocks::to_plain("Return the argument."),
    },
    StdFun {
      name:      Builtin::BConst.show(),
      builtin:   Builtin::BConst,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      help:      Blocks::to_plain("Return the first argument."),
    },
    StdFun {
      name:      Builtin::Get.show(),
      builtin:   Builtin::Get,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      help:      Blocks::new().fancy("get i x").plain("gets the").fancy("i'th")
        .plain("thing out of").fancy("x,").plain("where")
        .fancy("i").plain("should be (evaluate to) a number or a string, and")
        .fancy("x").plain("should evaluate to an array or object."),
    },
    StdFun {
      name:      Builtin::Map.show(),
      builtin:   Builtin::Map,
      expr_type: arr(arr(Type::JSON, Type::JSON), arr(Type::JSON, Type::JSON)),
      help:      Blocks::new().fancy("map f xs").plain("applies").fancy("f")
        .plain("to every value in").fancy("xs,")
        .plain("which may be an array (in which case »value« means element) \
                 or an object (in which case it really means value).")
    },
    StdFun {
      name:      Builtin::Filter.show(),
      builtin:   Builtin::Filter,
      expr_type: arr(arr(Type::JSON, Type::JSON), arr(Type::JSON, Type::JSON)),
      help:      Blocks::new().fancy("filter p xs").plain("applies the predicate")
        .fancy("p").plain("to every value of").fancy("xs.")
        .plain("Keep the elements for which the predicate returns truthy."),
    },
    StdFun {
      name:      Builtin::Foldl.show(),
      builtin:   Builtin::Foldl,
      expr_type: arr( // (b -> a -> b) -> b -> [a] -> b
        arr(Type::JSON, arr(Type::JSON, Type::JSON)),
        arr(Type::JSON, arr(Type::JSON, Type::JSON))
      ),
      help:      Blocks::new()
        .plain("Left-associative fold over an array or (values of an) object; e.g.,")
        .fancy("    foldl f α [x₁, x₂, …, xₙ]  ≡  f(f(…f(α, x₁), …), xₙ)."),
    },
    StdFun {
      name:      Builtin::Add.show(),
      builtin:   Builtin::Add,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      help:      Blocks::to_plain("Add two number, or concatenate two strings."),
    },
    StdFun {
      name:      Builtin::Sub.show(),
      builtin:   Builtin::Sub,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      help:      Blocks::to_plain("Subtract two numbers."),
    },
    StdFun {
      name:      Builtin::Mul.show(),
      builtin:   Builtin::Mul,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      help:      Blocks::to_plain("Multiply two numbers."),
    },
    StdFun {
      name:      Builtin::Div.show(),
      builtin:   Builtin::Div,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      help:      Blocks::to_plain("Divide two numbers. No guarantees if the denominator is zero—\
                  the world might explode."),
    },
    StdFun {
      name:      Builtin::Eq.show(),
      builtin:   Builtin::Eq,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      help:      Blocks::to_plain("Check two expressions for equality."),
    },
    StdFun {
      name:      Builtin::Neq.show(),
      builtin:   Builtin::Neq,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      help:      Blocks::to_plain("Check two expressions for non-equality."),
    },
    StdFun {
      name:      Builtin::Le.show(),
      builtin:   Builtin::Le,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      help:      Blocks::new().fancy("e < e'").plain("checks whether").fancy("e")
        .plain("is less than").fancy("e'"),
    },
    StdFun {
      name:      Builtin::Leq.show(),
      builtin:   Builtin::Leq,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      help:      Blocks::new().fancy("e ≤ e'").plain("checks whether").fancy("e")
        .plain("is less-or-equal-to").fancy("e'"),
    },
    StdFun {
      name:      Builtin::Ge.show(),
      builtin:   Builtin::Ge,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      help:      Blocks::new().fancy("e > e'").plain("checks whether").fancy("e")
        .plain("is greater than").fancy("e'"),
    },
    StdFun {
      name:      Builtin::Geq.show(),
      builtin:   Builtin::Geq,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
      help:      Blocks::new().fancy("e ≥ e'").plain("checks whether").fancy("e")
        .plain("is greater-or-equal-to").fancy("e'"),
    },
  ];
}
