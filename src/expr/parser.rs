use anyhow::Result;
use nom::{self, branch::{alt, Alt}, bytes::complete::{escaped, tag}, character::complete::{alpha1, alphanumeric0, alphanumeric1, multispace0, multispace1, one_of}, combinator::{all_consuming, map}, error::ParseError, multi::separated_list1, number::complete::double, sequence::{delimited, pair, terminated}, AsChar, Finish, IResult, InputTakeAtPosition, Parser};

use crate::expr::{Const, Expr, Var};

pub fn parse(inp: &str) -> Result<Expr, nom::error::Error<&str>> {
  all_consuming(p_expr)(inp).finish().map(|(_, r)| r)
}

fn p_expr(input: &str) -> IResult<&str, Expr> {
  lalt((p_array, p_obj, p_app, p_lam, p_const, map(p_var, Expr::Var)))(input)
}

/// Parse a constant expression.
fn p_const(input: &str) -> IResult<&str, Expr> {
  let p_bool = alt((map(tag("true"), |_| true), map(tag("false"), |_| false)));
  map(
    alt((
      map(tag("null"), |_| Const::Null),
      map(p_bool, Const::Bool),
      map(double, Const::Num),
    )),
    Expr::Const,
  )(input)
}

/// Parse a variable. Variables must start with a letter, and may use
/// alphanumeric characters after that.
fn p_var(input: &str) -> IResult<&str, Var> {
  map(pair(alpha1, alphanumeric0), |(s1, s2): (&str, &str)| {
    Var(s1.to_string() + s2)
  })(input)
}

/// Parse an array.
fn p_array(input: &str) -> IResult<&str, Expr> {
  let (input, _) = lexeme(tag("["))(input)?;
  let (input, xs) = separated_list1(lexeme(tag(",")), p_expr)(input)?;
  let (input, _) = lexeme(tag("]"))(input)?;
  Ok((input, Expr::Arr(xs)))
}

/// Parse an object.
fn p_obj(input: &str) -> IResult<&str, Expr> {
  let p_kv = |input| -> IResult<&str, (String, Expr)> {
    let (input, k) = terminated(p_str, lexeme(tag(":")))(input)?;
    let (input, v) = p_expr(input)?;
    Ok((input, (k.to_string(), v)))
  };
  let (input, _) = lexeme(tag("{"))(input)?;
  let (input, o) = separated_list1(lexeme(tag(",")), p_kv)(input)?;
  let (input, _) = lexeme(tag("}"))(input)?;
  Ok((input, Expr::Obj(o.into_iter().collect())))
}

/// Parse a string. FIXME: This is probably bad.
fn p_str(input: &str) -> IResult<&str, &str> {
  delimited(
    tag("\""),
    escaped(alphanumeric1, '\\', one_of(r#""n\"#)),
    tag("\""),
  )(input)
}

/// Parse a lambda.
fn p_lam(input: &str) -> IResult<&str, Expr> {
  let haskell_head = |input| {
    let (input, _) = lalt((tag("\\"), tag("λ")))(input)?;
    let (input, head) = p_var(input)?;
    let (input, _) = lalt((tag("->"), tag("→")))(input)?;
    Ok((input, head))
  };
  let rust_head = |input| {
    let (input, _) = lexeme(tag("|"))(input)?;
    let (input, head) = p_var(input)?;
    let (input, _) = lexeme(tag("|"))(input)?;
    Ok((input, head))
  };
  let res = try_parens(|input| {
    let (input, head) = lalt((haskell_head, rust_head))(input)?;
    let (input, body) = p_expr(input)?;
    Ok((input, Expr::Lam(head, Box::new(body))))
  })(input);
  res
}

/// Parse a function application.
fn p_app(input: &str) -> IResult<&str, Expr> {
  let go = |input| {
    // Only variable and parenthesised lambdas can represent functions.
    let (input, fun) = lalt((map(p_var, Expr::Var), parens(p_lam)))(input)?;
    let (input, targets) = separated_list1(
      multispace1,
      // p_var before p_app, so function application associates to the left.
      alt((map(p_var, Expr::Var), p_array, p_obj, p_app, p_lam, p_const)),
    )(input)?;
    Ok((
      input,
      targets
        .into_iter()
        .fold(fun, |f, x| Expr::App(Box::new(f), Box::new(x))),
    ))
  };
  try_parens(go)(input)
}

///////////////////////////////////////////////////////////////////////
// Util

pub fn symbol<'a>(s: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
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

pub fn lalt<I, O, E, List>(l: List) -> impl FnMut(I) -> IResult<I, O, E>
where
  I: Clone + InputTakeAtPosition,
  <I as InputTakeAtPosition>::Item: AsChar + Clone,
  E: ParseError<I>,
  List: Alt<I, O, E>,
{
  lexeme(alt(l))
}

pub fn try_parens<'a, F, R>(parser: F) -> impl FnMut(&'a str) -> IResult<&'a str, R>
where
  F: FnMut(&'a str) -> IResult<&'a str, R> + Clone,
{
  lexeme(alt((parens(parser.clone()), parser)))
}

pub fn parens<'a, F, R>(parser: F) -> impl FnMut(&'a str) -> IResult<&'a str, R>
where
  F: FnMut(&'a str) -> IResult<&'a str, R> + Clone,
{
  lexeme(delimited(tag("("), parser, tag(")")))
}
