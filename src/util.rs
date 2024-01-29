use std::{collections::HashMap, fmt::{self, Display}};

pub fn fmt_object<T: Display>(
  hm: &HashMap<String, T>,
  f: &mut fmt::Formatter,
) -> fmt::Result {
  write!(
    f,
    "{{ {} }}",
    hm.iter()
      .map(|(k, v)| k.to_owned() + ": " + &v.to_string())
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
