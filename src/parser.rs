use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::prelude::*;

/// Parse some input with the given parser.
pub fn parse_main<'a, T, R>(parser: impl Fn() -> T, inp: &'a str) -> Option<R>
where
  T: Parser<'a, &'a str, R, extra::Err<Rich<'a, char>>>,
{
  let (expr, errs) = parser()
    .then_ignore(end())
    .parse(inp.trim())
    .into_output_errors();
  match expr {
    Some(e) => Some(e),
    None => {
      errs.into_iter().for_each(|e| {
        Report::build(ReportKind::Error, e.span().into_range())
          .with_message(e.to_string())
          .with_label(
            Label::new(e.span().into_range())
              .with_message(e.reason().to_string())
              .with_color(Color::Red),
          )
          .finish()
          .print(Source::from(&inp))
          .unwrap()
      });
      None
    },
  }
}
