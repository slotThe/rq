//! A wrapper for f64 that has semi-sensible instances of Eq and Ord.
//!
//! Code heavily inspired from the [ordered_float] crate, but without
//! incurring the additional dependency.
//!
//! [ordered_float]: https://tikv.github.io/doc/ordered_float/

use std::{cmp::Ordering, fmt::{self, Display}, ops::{Add, Deref, Div, Mul, Sub}, str::FromStr};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct OrdF64(f64);

impl OrdF64 {
  pub fn unpack(&self) -> f64 { self.0 }
}

impl Eq for OrdF64 {}

impl Display for OrdF64 {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.0.fmt(f) }
}

impl<T: Into<f64>> From<T> for OrdF64 {
  fn from(value: T) -> Self { OrdF64(value.into()) }
}

impl Deref for OrdF64 {
  type Target = f64;

  fn deref(&self) -> &Self::Target { &self.0 }
}

impl PartialOrd for OrdF64 {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
}

impl Ord for OrdF64 {
  fn cmp(&self, other: &Self) -> Ordering {
    let lhs = &self.0;
    let rhs = &other.0;
    match lhs.partial_cmp(rhs) {
      Some(ordering) => ordering,
      None => {
        if lhs.is_nan() {
          if rhs.is_nan() {
            Ordering::Equal
          } else {
            Ordering::Greater
          }
        } else {
          Ordering::Less
        }
      },
    }
  }
}

impl FromStr for OrdF64 {
  type Err = <f64 as FromStr>::Err;

  fn from_str(s: &str) -> Result<Self, Self::Err> { f64::from_str(s).map(OrdF64) }
}

macro_rules! num_instances {
  ($($inst:ident $name:ident),+) => {
    $(
      impl $inst for OrdF64 {
        type Output = Self;
        fn $name(self, rhs: Self) -> Self::Output { OrdF64(self.0.$name(rhs.0)) }
      }
    )+
  };
}

num_instances!(Add add, Sub sub, Mul mul, Div div);
