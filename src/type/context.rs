//! Type checking context.

use std::collections::BTreeMap;

use super::{error::TResult, Exist, Monotype, Type};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Item {
  /// A type variable: α
  Var(String),
  /// An annotation: x : A
  Ann(String, Type),
  /// An unsolved existential: α̂
  Unsolved(Exist),
  /// A solved existential: α̂ = τ
  Solved(Exist, Monotype),
  /// An auxiliary marker type: ▸α̂
  Marker(Exist),
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
  pub stdlib: BTreeMap<&'static str, Type>,
}

impl State {
  /// Create a new [State] from just the standard library.
  pub fn new(stdlib: BTreeMap<&'static str, Type>) -> Self {
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

  /// Insert `ins` in place if `at` in `self`.
  pub fn replace_with(&mut self, replace: &Item, with: &[Item]) {
    let ix = self.ctx.iter().position(|i| i == replace).unwrap();
    self.ctx.remove(ix);
    for (i, item) in with.iter().enumerate() {
      self.ctx.insert(i + ix, item.clone())
    }
  }

  /// `state.scoped_around(more, n, act)` extends the `state` with `more`,
  /// executes `act`, and then drops every after and including the `n`th item
  /// from `more`.
  pub fn scoped_around<F: FnOnce(&mut Self) -> TResult<()>>(
    &mut self,
    more: &[Item],
    n: usize,
    act: F,
  ) -> TResult<()> {
    self.ctx.extend_from_slice(more);
    act(self)?;
    self.ctx = self
      .ctx
      .iter()
      .cloned()
      .take_while(|item| &more[n] != item)
      .collect();
    Ok(())
  }
}

impl Type {
  /// Apply a context, as a substitution, to a type.
  pub fn apply_ctx(&self, ctx: &[Item]) -> Self {
    fn subst(α̂: &Exist, τ: &Monotype, t: &Type) -> Type {
      match t {
        Type::Exist(β̂) => {
          if α̂ == β̂ {
            τ.to_poly()
          } else {
            t.clone()
          }
        },
        Type::Forall(α, t) => Type::forall(α, subst(α̂, τ, t)),
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
