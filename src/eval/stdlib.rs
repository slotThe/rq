use std::{collections::BTreeMap, fmt::{self, Display}};

use crate::r#type::{arr, Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Builtin {
  Id,
  BConst,
  Get,
  Map,
}

impl Builtin {
  fn show(&self) -> &'static str {
    match self {
      Builtin::Id => "id",
      Builtin::BConst => "const",
      Builtin::Get => "get",
      Builtin::Map => "map",
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
  pub static ref STDLIB: [StdFun; 4] = [
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
  ];
}
