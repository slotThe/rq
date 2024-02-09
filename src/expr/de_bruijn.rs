//! "Namespace" De Bruijn indices, as well as an environment that can hold them.
//! See [1] and [this] blog post for more information.
//!
//! [1]: Mark-Oliver Stehr, CINNI – A Generic Calculus of Explicit
//!      Substitutions and its Application to λ- ς- and π-Calculi, Electronic
//!      Notes in Theoretical Computer Science, Volume 36, 2000, Pages 70-92.
//!
//! [this]: https://www.haskellforall.com/2021/08/namespaced-de-bruijn-indices.html

use std::{cell::RefCell, collections::BTreeMap, fmt::{self, Display}, rc::Rc};

// A [D]e [B]ruijn variable.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DBVar {
  pub name:  String,
  pub level: isize,
}

impl Display for DBVar {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if self.level == 0 {
      write!(f, "{}", self.name)
    } else {
      write!(f, "{}@{}", self.name, self.level)
    }
  }
}

impl<'a> DBVar {
  pub fn from_pair<N>(v: &'a str, n: N) -> Self
  where
    N: TryInto<isize>,
    <N as TryInto<isize>>::Error: std::fmt::Debug,
  {
    Self {
      name:  v.to_string(),
      level: n.try_into().unwrap(),
    }
  }
}

/// The evaluation environment: A map from a variable name to its different
/// expressions, each element corresponding to one De Bruijn level. I.e.,
/// given an environment like ["x", [sem₀, sem₁]], xᵢ would correspond to
/// semᵢ, for i ∈ {0, 1}.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DBEnv<T>(pub Rc<RefCell<BTreeMap<String, Vec<T>>>>);

impl<T: Clone> DBEnv<T> {
  pub fn from_iter_with<S: Clone, F: Fn(S) -> T>(
    m: impl IntoIterator<Item = (impl Into<String>, S)>,
    f: F,
  ) -> Self {
    Self(Rc::new(RefCell::new(
      m.into_iter().map(|(k, s)| (k.into(), vec![f(s)])).collect(),
    )))
  }

  pub fn lookup_var(&self, v: &DBVar) -> Option<T> {
    let env = self.0.borrow();
    // De Bruijn indices start at 0 for the innermost expression; since we
    // push to the back of the vector, we need to turn indexing around.
    env.get(&v.name).and_then(|vec| {
      vec
        .get((vec.len() as isize - 1 - v.level) as usize)
        .cloned()
    })
  }

  /// Associate the given variable with its evaluated expression.
  pub fn add_mut(&self, name: &str, tea: &T) {
    self
      .0
      .borrow_mut()
      .entry(name.to_string())
      .and_modify(|xs| xs.push(tea.clone()))
      .or_insert(vec![tea.clone()]);
  }
}
