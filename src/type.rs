use std::fmt::Display;

pub mod checker;
#[cfg(test)]
pub mod test;

/// The type of a type!
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
  Var(TVar),                 // A type variable.
  JSON,                      // The JSON type: a black hole.
  Arr(Box<Type>, Box<Type>), // A type arrow.
}

impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Type::JSON => write!(f, "JSON"),
      Type::Var(v) => write!(
        f,
        "{}",
        std::char::from_u32((v.0 % 26 + 97) as u32).unwrap() // FIXME: :>
      ),
      Type::Arr(t1, t2) => match *t1.clone() {
        Type::Arr(t11, t12) => write!(f, "({t11} → {t12}) → {t2}"),
        _ => write!(f, "{t1} → {t2}"),
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
      Type::JSON => false,
      Type::Arr(t1, t2) => self.occurs_in(t1) || self.occurs_in(t2),
    }
  }
}
