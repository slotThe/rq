//! Type checking context.

use std::collections::BTreeMap;

use super::{Monotype, TypVar, Type};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Item {
  /// A type variable: α
  Var(TypVar),
  /// An annotation: x : A
  Ann(String, Type),
  /// An unsolved existential: α̂
  Unsolved(TypVar),
  /// A solved existential: α̂ = τ
  Solved(TypVar, Monotype),
  /// An auxiliary marker type: ▸α̂
  Marker(TypVar),
}

impl Item {
  /// Does `self` occur to the left of `other` in `ctx`?
  pub fn left_of(&self, other: &Item, ctx: &[Item]) -> bool {
    ctx.iter().position(|it| it == self).unwrap()
      < ctx.iter().position(|it| it == other).unwrap()
  }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct State {
  /// Count for fresh variables.
  pub count:  usize,
  /// Context in the sense of the paper.
  pub ctx:    Vec<Item>,
  /// Our standard library.
  pub stdlib: BTreeMap<String, Type>,
}

impl State {
  /// Create a new [State] from just the standard library.
  pub fn new(stdlib: BTreeMap<String, Type>) -> Self {
    Self {
      count: 0,
      ctx: vec![],
      stdlib,
    }
  }

  pub fn fresh_mut(&mut self) -> usize {
    self.count += 1;
    self.count - 1
  }

  /// Drop the context after and including `this`.
  pub fn drop_after(self, this: &Item) -> Self {
    Self {
      ctx: self
        .ctx
        .into_iter()
        .take_while(|item| this != item)
        .collect(),
      ..self
    }
  }

  /// Insert `ins` in place if `at` in `self`.
  pub fn insert_at(mut self, at: &Item, ins: &[Item]) -> Self {
    let ix = match self.ctx.iter().position(|i| i == at) {
      Some(ix) => ix,
      None => panic!("Can't find {:?} in {:?}", at, self.ctx),
    };
    self.ctx.remove(ix);
    for (i, item) in ins.iter().enumerate() {
      self.ctx.insert(i + ix, item.clone())
    }
    self
  }
}

impl Type {
  /// Apply a context, as a substitution, to a type.
  pub fn apply_ctx(&self, ctx: &[Item]) -> Self {
    fn subst(α̂: &TypVar, τ: &Monotype, t: &Type) -> Type {
      match t {
        Type::Exist(β̂) => {
          if α̂ == β̂ {
            τ.to_poly()
          } else {
            t.clone()
          }
        },
        Type::Forall(α, t) => Type::forall(*α, subst(α̂, τ, t)),
        Type::Arr(t1, t2) => Type::arr(subst(α̂, τ, t1), subst(α̂, τ, t2)),
        _ => t.clone(),
      }
    }
    ctx.iter().rfold(self.clone(), |acc, c| match c {
      Item::Solved(α̂, τ) => subst(α̂, τ, &acc),
      _ => acc,
    })
  }
}