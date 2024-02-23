use super::Builtin;
use crate::util::style;

impl Builtin {
  /// Pretty-print a 'Builtin' function.
  pub fn show(&self) -> &'static str {
    match self {
      Builtin::Id => "id",
      Builtin::BConst => "const",
      Builtin::Get => "get",
      Builtin::Map => "map",
      Builtin::Filter => "filter",
      Builtin::Foldl => "foldl",
      Builtin::Add => "+",
      Builtin::Sub => "-",
      Builtin::Mul => "*",
      Builtin::Div => "÷",
      Builtin::Eq => "=",
      Builtin::Neq => "≠",
      Builtin::Le => "<",
      Builtin::Leq => "≤",
      Builtin::Ge => ">",
      Builtin::Geq => "≥",
    }
  }
}

/// A bunch of [`Block`]s!
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Blocks(Vec<Block>);

/// A unit of text that should be rendered in some way, and not be broken up
/// across lines. The second part is relevant when calling functions like
/// [`wrap`](Blocks::wrap).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Block {
  Plain(String),
  Fancy(String),
}

impl Block {
  /// Pretty-print a single [`Block`].
  pub fn show(&self) -> String {
    match self {
      Block::Plain(s) => s.to_string(),
      Block::Fancy(s) => {
        if s.ends_with([',', ';', '.']) {
          let l = s.len();
          style(&s[..l - 1]) + &s[l - 1..l]
        } else {
          style(s)
        }
      },
    }
  }

  pub fn len(&self) -> usize {
    match self {
      Block::Plain(s) => s.len(),
      Block::Fancy(s) => s.len(),
    }
  }
}

impl Blocks {
  /// Wrap the given blocks at 'wrap_at' columns. 'align' is the alignment to
  /// apply after a line break.
  pub fn wrap(&self, align: &str, wrap_at: usize) -> String {
    // Completely render a single line.
    let render = |line: Vec<Block>| -> String {
      line
        .iter()
        .map(|b| b.show())
        .intersperse(" ".to_string())
        .collect()
    };

    let mut res: Vec<String> = vec![];
    let mut line: Vec<Block> = vec![]; // Current line
    let mut line_len = 0; // Length of current line

    for chunk in &self.0 {
      if wrap_at < line_len + chunk.len() + 1 {
        res.push(render(line));
        line_len = chunk.len();
        line = vec![chunk.clone()];
      } else {
        line_len += chunk.len();
        line.push(chunk.clone());
      }
    }

    res.push(render(line)); // Push last (= incomplete) line
    res
      .into_iter()
      .intersperse("\n".to_string() + align)
      .collect()
  }
}

// Building
//
// I'm sure this would be much nicer with a blocks! macro, but that feels like
// too much work for now. A TODO for another day!
impl Blocks {
  pub fn new() -> Blocks { Blocks(vec![]) }

  pub fn fancy(mut self, inp: &str) -> Blocks {
    self.0.push(Block::Fancy(inp.to_string()));
    self
  }

  pub fn plain(mut self, inp: &str) -> Blocks {
    self.0.extend_from_slice(&Blocks::to_plain(inp).0);
    self
  }

  pub fn to_plain(input: &str) -> Blocks {
    Blocks(
      input
        .split(' ')
        .map(|s| Block::Plain(s.to_string()))
        .collect(),
    )
  }

  pub fn aliases(mut self, als: Vec<&str>) -> Blocks {
    self.0.extend_from_slice(
      &[
        [Block::Plain("Aliases:".to_string())].to_vec(),
        als
          .iter()
          .map(|a| Block::Fancy(a.to_string()))
          .collect::<Vec<_>>(),
      ]
      .concat(),
    );
    self
  }
}
