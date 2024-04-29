use std::fmt::{self, Display};

/// Pretty printing of things
pub mod pretty_print;

/// Ordered floats—scandalous!
pub mod ord_f64;

/// Pretty print an object of pretty-printable things.
pub fn fmt_object<S: Display, T: Display>(
  ob: impl IntoIterator<Item = (S, T)>,
  f: &mut fmt::Formatter,
) -> fmt::Result {
  write!(
    f,
    "{{{}}}",
    ob.into_iter()
      .map(|(k, v)| k.to_string() + ": " + &v.to_string())
      .intersperse(", ".to_string())
      .collect::<String>(),
  )
}

/// Pretty print an array of pretty-printable things.
pub fn fmt_array<T: Display>(xs: &[T], f: &mut fmt::Formatter) -> fmt::Result {
  write!(
    f,
    "[{}]",
    xs.iter()
      .map(|x| x.to_string())
      .intersperse(", ".to_string())
      .collect::<String>()
  )
}

/// Style the given item with an off-gold like colour.
pub fn style(t: impl Display) -> String { format!("\x1b[33m{t}\x1b[0m") }
