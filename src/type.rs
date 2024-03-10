use std::fmt::Display;

pub mod checker;
mod context;
pub mod error;
#[cfg(test)]
pub mod test;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct TypVar(pub usize);

impl Display for TypVar {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let (quot, rem) = (self.0 / 26, self.0 % 26);
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
  }
}

impl TypVar {
  /// Does the given type variable appear as unsolved in the type?
  pub fn unsolved_in(&self, typ: &Type) -> bool {
    match typ {
      Type::Exist(α̂) => α̂ == self,
      Type::Forall(_, t) => self.unsolved_in(t),
      Type::Arr(t, s) => self.unsolved_in(t) || self.unsolved_in(s),
      _ => false,
    }
  }
}

/// The type of a type!
#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
#[allow(clippy::upper_case_acronyms)]
pub enum Type {
  JSON,
  /// A type variable α
  Var(String),
  /// An existential type variable α̂
  Exist(TypVar),
  /// A universal quantifier: ∀α. A
  Forall(String, Box<Type>),
  /// A → B
  Arr(Box<Type>, Box<Type>),
}

impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Self::JSON => write!(f, "JSON"),
      Self::Var(α) => write!(f, "{α}"),
      Self::Arr(box Self::Arr(t11, t12), t2) => write!(f, "({t11} → {t12}) → {t2}"),
      Self::Arr(t1, t2) => write!(f, "{t1} → {t2}"),
      Self::Exist(α̂) => write!(f, "∃{α̂}"),
      Self::Forall(α, t) => write!(f, "∀{α}. {t}"),
    }
  }
}

impl Type {
  pub fn arr(t1: Self, t2: Self) -> Self { Self::Arr(Box::new(t1), Box::new(t2)) }

  pub fn forall(v: &str, t: Self) -> Self { Self::Forall(v.to_string(), Box::new(t)) }

  pub fn var(v: &str) -> Self { Self::Var(v.to_string()) }
}
impl Type {
  /// Substitute to with from (a string) in self.
  pub fn subst(self, to: Self, from: &str) -> Self {
    match self {
      Self::JSON | Self::Exist(_) => self.clone(),
      Self::Var(ref α) => {
        if α == from {
          to.clone()
        } else {
          self
        }
      },
      Self::Forall(α, box t) => {
        Self::forall(&α, if α == from { t } else { t.subst(to, from) })
      },
      Self::Arr(box t1, box t2) => {
        Self::arr(t1.subst(to.clone(), from), t2.subst(to, from))
      },
    }
  }

  pub fn subst_type(self, to: &Self, from: &Self) -> Self {
    match self {
      Self::JSON | Self::Var(_) | Self::Exist(_) => {
        if self == *from {
          to.clone()
        } else {
          self
        }
      },
      Self::Forall(α, box t) => Self::forall(&α, t.subst_type(to, from)),
      Self::Arr(box t1, box t2) => {
        Self::arr(t1.subst_type(to, from), t2.subst_type(to, from))
      },
    }
  }
}

/// A monotype: like [Type], but without universal quantification.
#[derive(PartialEq, Eq, Debug, Clone)]
#[allow(clippy::upper_case_acronyms)]
pub enum Monotype {
  JSON,
  /// A type variable α
  Var(String),
  /// An existential type variable α̂
  Exist(TypVar),
  /// τ → σ
  Arr(Box<Monotype>, Box<Monotype>),
}

impl Monotype {
  pub fn to_poly(&self) -> Type {
    match self {
      Self::JSON => Type::JSON,
      Self::Var(α) => Type::Var(α.clone()),
      Self::Exist(α̂) => Type::Exist(*α̂),
      Self::Arr(box τ, box σ) => Type::arr(τ.to_poly(), σ.to_poly()),
    }
  }

  fn arr(t1: Self, t2: Self) -> Self { Self::Arr(Box::new(t1), Box::new(t2)) }
}

impl Type {
  pub fn to_mono(&self) -> Option<Monotype> {
    match self {
      Self::Forall(_, _) => None,
      Self::JSON => Some(Monotype::JSON),
      Self::Var(α) => Some(Monotype::Var(α.clone())),
      Self::Exist(α̂) => Some(Monotype::Exist(*α̂)),
      Self::Arr(t, s) => Some(Monotype::arr(t.to_mono()?, s.to_mono()?)),
    }
  }
}
