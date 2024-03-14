//! The types of a type!
//!
//! Generally, a type can either be a polytype ([Type]) or a [Monotype]; the
//! latter is almost like the former, only it does not allow universal
//! quantification.

use std::fmt::Display;

use crate::r#type::context::Item;

mod checker;
mod context;
pub mod error;
pub mod expr;
#[cfg(test)]
pub mod test;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct Exist(pub usize);

impl Display for Exist {
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

impl Exist {
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
  /// A number
  Num,
  /// The JSON black hole
  JSON,
  /// A type variable α
  Var(String),
  /// An existential type variable α̂
  Exist(Exist),
  /// A universal quantifier: ∀α. A
  Forall(String, Box<Type>),
  /// A → B
  Arr(Box<Type>, Box<Type>),
}

impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Self::Num => write!(f, "Num"),
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
  ///           A.subst(B, α)  ≡  [B.α]A
  ///
  /// Substitute the type variable α with type B in A.
  pub fn subst(self, to: Self, from: &str) -> Self {
    match self {
      Self::Num | Self::JSON | Self::Exist(_) => self.clone(),
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

  ///           A.subst_type(C, B)  ≡  [B/C]A
  ///
  /// Substitute type C for B in A.
  pub fn subst_type(self, to: &Self, from: &Self) -> Self {
    match self {
      Self::Num | Self::JSON | Self::Var(_) | Self::Exist(_) => {
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

impl Type {
  /// Clean up after type checking: replace existential (unsolved) type
  /// variables with universal quantification.
  pub fn finish(self, ctx: &[Item]) -> Self {
    fn forallise(typ: Type, rule: &Item) -> Type {
      match rule {
        Item::Unsolved(α̂) if α̂.unsolved_in(&typ) => {
          let name = α̂.to_string();
          Type::forall(
            &name.clone(),
            typ.subst_type(&Type::Var(name), &Type::Exist(*α̂)),
          )
        },
        _ => typ,
      }
    }
    ctx.iter().rfold(self.apply_ctx(ctx), forallise)
  }
}

// XXX: This could be expressed extremely elegantly with GADTs—alas.

/// A monotype: like [Type], but without universal quantification.
#[derive(PartialEq, Eq, Debug, Clone)]
#[allow(clippy::upper_case_acronyms)]
pub enum Monotype {
  Num,
  JSON,
  /// A type variable α
  Var(String),
  /// An existential type variable α̂
  Exist(Exist),
  /// τ → σ
  Arr(Box<Monotype>, Box<Monotype>),
}

impl Monotype {
  /// Convert a [Monotype] to a (poly)[Type].
  pub fn to_poly(&self) -> Type {
    match self {
      Self::Num => Type::Num,
      Self::JSON => Type::JSON,
      Self::Var(α) => Type::Var(α.clone()),
      Self::Exist(α̂) => Type::Exist(*α̂),
      Self::Arr(box τ, box σ) => Type::arr(τ.to_poly(), σ.to_poly()),
    }
  }

  fn arr(t1: Self, t2: Self) -> Self { Self::Arr(Box::new(t1), Box::new(t2)) }
}

impl Type {
  /// Try to convert (poly)[Type] to a [Monotype]. Fails if the [Type]
  /// contains universal quantification.
  pub fn to_mono(&self) -> Option<Monotype> {
    match self {
      Self::Forall(_, _) => None,
      Self::Num => Some(Monotype::Num),
      Self::JSON => Some(Monotype::JSON),
      Self::Var(α) => Some(Monotype::Var(α.clone())),
      Self::Exist(α̂) => Some(Monotype::Exist(*α̂)),
      Self::Arr(t, s) => Some(Monotype::arr(t.to_mono()?, s.to_mono()?)),
    }
  }
}
