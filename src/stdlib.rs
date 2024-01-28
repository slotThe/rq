use std::{collections::HashMap, fmt::{self, Display}};

use crate::r#type::{arr, Type};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Builtin {
  Id,
  Const,
}

impl Builtin {
  fn show(&self) -> &'static str {
    match self {
      Builtin::Id => "id",
      Builtin::Const => "const",
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
  pub static ref STDLIB_CTX: HashMap<&'static str, Builtin> =
    HashMap::from(STDLIB.clone().map(|f| (f.name, f.builtin)));
  pub static ref STDLIB_TYPES: HashMap<String, Type> =
    HashMap::from(STDLIB.clone().map(|f| (f.name.to_string(), f.expr_type)));
  pub static ref STDLIB: [StdFun; 2] = [
    StdFun {
      name:      Builtin::Id.show(),
      builtin:   Builtin::Id,
      expr_type: arr(Type::JSON, Type::JSON),
    },
    StdFun {
      name:      Builtin::Const.show(),
      builtin:   Builtin::Const,
      expr_type: arr(Type::JSON, arr(Type::JSON, Type::JSON)),
    }
  ];
}
