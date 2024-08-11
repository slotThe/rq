use std::{collections::BTreeSet, sync::LazyLock};

// XXX: This really needs proc macros to have any semblance of code-reuse. I
// reckon that this isn't worth the effort; when the CLI gets more complicated
// one should just accept the additional transitive dependencies and switch to
// clap.

pub static HELP: LazyLock<String> = LazyLock::new(|| {
  [
    "rq: A tiny functional language to filter and manipulate JSON.",
    &USAGE,
    &OPTIONS,
    "POSITIONAL ARGUMENTS:
  «EXPR»	A function in rq's expression language.
  «JSON»        JSON! Can either—as indicated—be piped via stdin,
  		or given as a file argument.
  repl          The literal string \"repl\"; starts a REPL.",
  ]
  .into_iter()
  .intersperse("\n\n")
  .collect()
});

static USAGE: LazyLock<String> = LazyLock::new(|| {
  let flatten = CLI_OPTIONS.iter().find(|o| o.long == "--flatten").unwrap();
  let help = CLI_OPTIONS.iter().find(|o| o.long == "--help").unwrap();
  format!(
    "USAGE:
  rq [EXPR] < [JSON]
  rq {} < [JSON]
  rq {}
  rq repl",
    flatten.usage(),
    help.usage()
  )
});

static OPTIONS: LazyLock<String> = LazyLock::new(|| {
  let opts: String = CLI_OPTIONS
    .iter()
    .map(|opt| opt.help())
    .intersperse("\n".to_string())
    .collect();
  format!("OPTIONS:\n{opts}")
});

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct CliOption {
  long:  String,
  short: String,
  help:  String,
}

impl CliOption {
  fn usage(&self) -> String { format!("[{}|{}]", self.short, self.long) }

  fn help(&self) -> String { format!("  {},{}	{}", self.short, self.long, self.help) }
}

macro_rules! mk_option {
  ($long:literal, $short:literal, $help:literal $(,)?) => {{
    CliOption {
      long:  format!("--{}", $long),
      short: format!("-{}", $short),
      help:  $help.to_string(),
    }
  }};
}

macro_rules! mk_options {
  ($(($long:literal, $short:literal, $help:literal $(,)?)),+ $(,)?) => {
    static CLI_OPTIONS: LazyLock<BTreeSet<CliOption>> = LazyLock::new (|| {
      BTreeSet::from([
        $(mk_option!($long, $short, $help)),+
      ])});
  };
}

mk_options!(
  ("help", "h", "Show this help text."),
  ("flatten", "f", "Flatten the given JSON into a list.")
);

macro_rules! Help {
  () => {
    "--help" | "-h"
  };
}
pub(crate) use Help;

macro_rules! Flatten {
  () => {
    "--flatten" | "-f"
  };
}
pub(crate) use Flatten;

macro_rules! Fatten {
  () => {
    "--fatten" | "-F"
  };
}
pub(crate) use Fatten;
