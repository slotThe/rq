use std::{collections::BTreeMap, fmt::{self, Display}};

use crate::r#type::{arr, Type};

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
}

lazy_static! {
  pub static ref STDLIB_CTX: BTreeMap<&'static str, Builtin> =
    BTreeMap::from(STDLIB.clone().map(|f| (f.name, f.builtin)));
  pub static ref STDLIB_TYPES: BTreeMap<String, Type> =
    BTreeMap::from(STDLIB.clone().map(|f| (f.name.to_string(), f.expr_type)));
  pub static ref STDLIB: [StdFun; 16] = [
    StdFun {
      name:      Builtin::Id.show(),
      builtin:   Builtin::Id,
      expr_type: arr(Type::JSON, Type::JSON),
    },
    StdFun {
      name:      Builtin::BConst.show(),
      builtin:   Builtin::BConst,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
    },
    StdFun {
      name:      Builtin::Get.show(),
      builtin:   Builtin::Get,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
    },
    StdFun {
      name:      Builtin::Map.show(),
      builtin:   Builtin::Map,
      expr_type: arr(arr(Type::JSON, Type::JSON), arr(Type::JSON, Type::JSON)),
    },
    StdFun {
      name:      Builtin::Filter.show(),
      builtin:   Builtin::Filter,
      expr_type: arr(arr(Type::JSON, Type::JSON), arr(Type::JSON, Type::JSON)),
    },
    StdFun {
      name:      Builtin::Foldl.show(),
      builtin:   Builtin::Foldl,
      expr_type: arr( // (b -> a -> b) -> b -> [a] -> b
        arr(Type::JSON, arr(Type::JSON, Type::JSON)),
        arr(Type::JSON, arr(Type::JSON, Type::JSON))
      )
    },
    StdFun {
      name:      Builtin::Add.show(),
      builtin:   Builtin::Add,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
    },
    StdFun {
      name:      Builtin::Sub.show(),
      builtin:   Builtin::Sub,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
    },
    StdFun {
      name:      Builtin::Mul.show(),
      builtin:   Builtin::Mul,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
    },
    StdFun {
      name:      Builtin::Div.show(),
      builtin:   Builtin::Div,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
    },
    StdFun {
      name:      Builtin::Eq.show(),
      builtin:   Builtin::Eq,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
    },
    StdFun {
      name:      Builtin::Neq.show(),
      builtin:   Builtin::Neq,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
    },
    StdFun {
      name:      Builtin::Le.show(),
      builtin:   Builtin::Le,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
    },
    StdFun {
      name:      Builtin::Leq.show(),
      builtin:   Builtin::Leq,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
    },
    StdFun {
      name:      Builtin::Ge.show(),
      builtin:   Builtin::Ge,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
    },
    StdFun {
      name:      Builtin::Geq.show(),
      builtin:   Builtin::Geq,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
    },
  ];
}
