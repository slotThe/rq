use std::fmt::Display;

pub mod checker;
#[cfg(test)]
pub mod test;

/// The type of a type!
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms)]
pub enum Type {
  /// A type variable.
  Var(TVar),
  /// The JSON type: a black hole.
  JSON,
  /// A type arrow.
  Arr(Box<Type>, Box<Type>),
}

/// Construct a type arrow.
pub fn arr(t1: Type, t2: Type) -> Type { Type::Arr(Box::new(t1), Box::new(t2)) }

impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Type::JSON => write!(f, "JSON"),
      Type::Var(v) => {
        let (quot, rem) = (v.0 / 26, v.0 % 26);
        write!(
          f,
          "{}{}",
          char::from_u32((rem + 97) as u32).unwrap(),
          if quot == 0 {
            "".to_string()
          } else {
            quot.to_string()
          }
        )
      },
      Type::Arr(box Type::Arr(t11, t12), t2) => write!(f, "({t11} → {t12}) → {t2}"),
      Type::Arr(t1, t2) => write!(f, "{t1} → {t2}"),
    }
  }
}

/// A type variable
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TVar(pub usize);

impl Display for TVar {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{}", self.0)
  }
}

impl TVar {
  /// Does `t` occur in `self`?
  fn occurs_in(&self, t: &Type) -> bool {
    match t {
      Type::Var(tv) => tv == self,
      Type::JSON => false,
      Type::Arr(t1, t2) => self.occurs_in(t1) || self.occurs_in(t2),
    }
  }
}
