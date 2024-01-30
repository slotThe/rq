use anyhow::Result;
use nom::{self, branch::{alt, Alt}, bytes::complete::{escaped, tag}, character::complete::{alpha1, alphanumeric0, alphanumeric1, char, digit1, multispace0, one_of}, combinator::{all_consuming, cut, map}, error::{context, convert_error, ContextError, Error, ParseError, VerboseError}, multi::{many0, separated_list1}, number::complete::double, sequence::{delimited, pair, preceded, separated_pair, terminated}, AsChar, Err, Finish, IResult, InputTakeAtPosition, Parser};

use super::{app, var};
use crate::expr::{Const, Expr};

pub trait MyErr<'a> = ParseError<&'a str> + ContextError<&'a str>;
type IExpr<'a, E> = IResult<&'a str, Expr, E>;

pub fn parse(inp: &str) -> Result<Expr, String> {
  let res: Result<Expr, nom::error::VerboseError<&str>> =
    all_consuming(p_expr)(inp).finish().map(|(_, r)| r);
  match res {
    Ok(expr) => Ok(expr),
    Err(e) => Err(convert_error(inp, e)),
  }
}

fn p_expr<'a, E: MyErr<'a>>(input: &'a str) -> IExpr<'a, E> {
  context(
    "expression",
    lalt((p_array, p_obj, p_const, p_app, p_lam, map(p_var, Expr::Var))),
  )(input)
}

/// Parse a constant expression.
fn p_const<'a, E: MyErr<'a>>(input: &'a str) -> IExpr<'a, E> {
  let p_bool = alt((map(tag("true"), |_| true), map(tag("false"), |_| false)));
  map(
    alt((
      context("null", map(tag("null"), |_| Const::Null)),
      context("boolean", map(p_bool, Const::Bool)),
      context("number", map(double, Const::Num)),
      context("string", map(p_str, |s| Const::String(s.to_string()))),
    )),
    Expr::Const,
  )(input)
}

/// Parse a variable. Variables must start with a letter, and may use
/// alphanumeric characters after that.
fn p_var<'a, E: MyErr<'a>>(input: &'a str) -> IResult<&'a str, String, E> {
  map(pair(alpha1, alphanumeric0), |(s1, s2): (&str, &str)| {
    s1.to_string() + s2
  })(input)
}

/// Parse an array.
fn p_array<'a, E: MyErr<'a>>(input: &'a str) -> IExpr<'a, E> {
  context(
    "array",
    preceded(
      char('['),
      cut(terminated(
        map(separated_list1(symbol(","), p_expr), Expr::Arr),
        char(']'),
      )),
    ),
  )(input)
}

/// Parse an object.
fn p_obj<'a, E: MyErr<'a>>(input: &'a str) -> IExpr<'a, E> {
  let p_kv = |input| -> IResult<&'a str, (&'a str, Expr), E> {
    separated_pair(lalt((alphanumeric1, p_str)), cut(symbol(":")), p_expr)(input)
  };
  context(
    "object",
    map(
      preceded(
        terminated(tag("{"), multispace0),
        cut(terminated(
          separated_list1(symbol(","), p_kv),
          preceded(multispace0, tag("}")),
        )),
      ),
      |o| Expr::Obj(o.into_iter().map(|(k, v)| (k.to_string(), v)).collect()),
    ),
  )(input)
}

/// Parse a string. FIXME: This is probably bad.
fn p_str<'a, E: MyErr<'a>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
  context(
    "string",
    preceded(
      tag("\""),
      cut(terminated(
        escaped(alphanumeric1, '\\', one_of(r#""n\"#)),
        tag("\""),
      )),
    ),
  )(input)
}

/// Parse a lambda.
fn p_lam<'a, E: MyErr<'a>>(input: &'a str) -> IExpr<'a, E> {
  let haskell_head = |input| {
    preceded(
      context("lambda head", lalt((tag("\\"), tag("λ")))),
      cut(terminated(
        p_var,
        context("lambda ->", lalt((tag("->"), tag("→")))),
      )),
    )(input)
  };
  let rust_head =
    |input| context("lambda head", delimited(symbol("|"), p_var, symbol("|")))(input);
  let res = context(
    "lambda",
    try_parens(|input| {
      let (input, head) = lalt((haskell_head, rust_head))(input)?;
      let (input, body) = p_expr(input)?;
      Ok((input, Expr::Lam(head, Box::new(body))))
    }),
  )(input);
  res
}

fn p_dotted<'a, 'b, E: MyErr<'a>>(head: &'b Expr, input: &'a str) -> IExpr<'a, E> {
  match tag::<&str, &str, E>(".")(input) {
    Err(_) => Ok((input, head.clone())),
    Ok((input, _)) => {
      let (input, v) = alt((
        map(digit1, |d: &str| Const::Num(d.parse::<f64>().unwrap())),
        map(alt((alphanumeric1, p_str)), |s| {
          Const::String(s.to_string())
        }),
      ))(input)?;
      p_dotted(&app(app(var("get"), Expr::Const(v)), head.clone()), input)
    },
  }
}

/// Parse a function application.
fn p_app<'a, E: MyErr<'a>>(input: &'a str) -> IExpr<'a, E> {
  let go = |input| {
    let (input, fun) = {
      // Only variable and parenthesised lambdas can represent functions.
      let (input, head) = lalt((lexeme(parens(p_lam)), map(p_var, Expr::Var)))(input)?;
      // Check for dot syntax: x.1, x.blah
      p_dotted::<E>(&head, input).or(Ok((input, head)))
    }?;
    let (input, targets) = many0(
      // p_var before p_app, so function application associates to the left.
      lalt((
        map(p_var, Expr::Var),
        p_array,
        p_obj,
        parens(p_lam),
        p_app,
        p_const,
      )),
    )(input)?;
    Ok((input, targets.into_iter().fold(fun, app)))
  };
  try_parens(go)(input)
}

///////////////////////////////////////////////////////////////////////
// Util

pub fn symbol<'a, E: MyErr<'a>>(
  s: &'a str,
) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, E> {
  lexeme(tag(s))
}

pub fn lexeme<I, O, E, P>(parser: P) -> impl FnMut(I) -> IResult<I, O, E>
where
  P: Parser<I, O, E>,
  I: Clone + InputTakeAtPosition,
  <I as InputTakeAtPosition>::Item: AsChar + Clone,
  E: ParseError<I>,
{
  delimited(multispace0, parser, multispace0)
}

pub fn lalt<'a, O, E, List>(l: List) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
  E: MyErr<'a>,
  List: Alt<&'a str, O, E>,
{
  lexeme(alt(l))
}

pub fn try_parens<'a, F, R, E: MyErr<'a>>(
  parser: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, R, E>
where
  F: FnMut(&'a str) -> IResult<&'a str, R, E> + Clone,
{
  lalt((parens(parser.clone()), parser))
}

pub fn parens<'a, F, R, E: MyErr<'a>>(
  parser: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, R, E>
where
  F: FnMut(&'a str) -> IResult<&'a str, R, E> + Clone,
{
  delimited(tag("("), parser, tag(")"))
}
