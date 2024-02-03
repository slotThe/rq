use std::{collections::BTreeMap, fmt::{self, Display}};

pub fn fmt_object<T: Display>(
  hm: &BTreeMap<T, T>,
  f: &mut fmt::Formatter,
) -> fmt::Result {
  write!(
    f,
    "{{ {} }}",
    hm.iter()
      .map(|(k, v)| k.to_string() + ": " + &v.to_string())
      .intersperse(", ".to_string())
      .collect::<String>(),
  )
}

pub fn fmt_array<T: Display>(xs: &[T], f: &mut fmt::Formatter) -> fmt::Result {
  write!(
    f,
    "[ {} ]",
    xs.iter()
      .map(|x| x.to_string())
      .intersperse(", ".to_string())
      .collect::<String>()
  )
}
