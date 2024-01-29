use std::fmt::Display;

pub mod checker;
#[cfg(test)]
pub mod test;

/// The type of a type!
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms)]
pub enum Type {
  Var(TVar),                 // A type variable.
  Num,                       // A number.
  Str,                       // A string.
  JSON,                      // The JSON type: a black hole.
  Array,                     // A JSON array
  Obj,                       // A JSON object
  Arr(Box<Type>, Box<Type>), // A type arrow.
  Or(Box<Type>, Box<Type>),  // A type-level coproduct: either the left or the right type.
}

/// Construct a type arrow.
pub fn arr(t1: Type, t2: Type) -> Type { Type::Arr(Box::new(t1), Box::new(t2)) }

/// Construct a coproduct of types.
pub fn t_or(t1: Type, t2: Type) -> Type { Type::Or(Box::new(t1), Box::new(t2)) }

impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Type::JSON => write!(f, "JSON"),
      Type::Num => write!(f, "Num"),
      Type::Str => write!(f, "String"),
      Type::Array => write!(f, "[{}]", Type::JSON),
      Type::Obj => write!(f, "{{{}}}", Type::JSON),
      Type::Var(v) => write!(
        f,
        "{}",
        std::char::from_u32((v.0 % 26 + 97) as u32).unwrap() // FIXME: :>
      ),
      Type::Arr(box Type::Arr(t11, t12), t2) => write!(f, "({t11} → {t12}) → {t2}"),
      Type::Arr(t1, t2) => {
        let add_parens = |t: &Type| match t {
          Type::Or(_, _) => format!("({t})"),
          _ => format!("{t}"),
        };
        write!(f, "{} → {}", add_parens(t1), add_parens(t2))
      },
      Type::Or(t1, t2) => {
        let add_parens = |t: &Type| match t {
          Type::Arr(_, _) => format!("({t})"),
          _ => format!("{t}"),
        };
        write!(f, "{} + {}", add_parens(t1), add_parens(t2))
      },
    }
  }
}

/// A type variable
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct TVar(pub usize);

impl Display for TVar {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{}", self.0)
  }
}

impl TVar {
  fn occurs_in(&self, t: &Type) -> bool {
    match t {
      Type::Var(tv) => tv == self,
      Type::JSON | Type::Str | Type::Num | Type::Array | Type::Obj => false,
      Type::Arr(t1, t2) | Type::Or(t1, t2) => self.occurs_in(t1) || self.occurs_in(t2),
    }
  }
}
