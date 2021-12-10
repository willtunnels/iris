use crate::ast::raw_ast::{Lit, Program};
use lalrpop_util::lalrpop_mod;
use std::str::FromStr;

lalrpop_mod!(parse_impl);

#[derive(Debug, Clone, thiserror::Error)]
pub enum UserError {
    #[error("the literal `{0}` does not fit into the type `{1}`")]
    LitOutOfRange(String, &'static str),
}

pub type Token<'a> = parse_impl::Token<'a>;

pub type ParseError<'a> = lalrpop_util::ParseError<usize, Token<'a>, UserError>;

// Used from LALRPOP
fn parse_arith_lit<T, F>(s: &str, ignore: usize, ctor: F) -> Result<Lit, ParseError>
where
    T: FromStr,
    F: FnOnce(T) -> Lit,
{
    s[..s.len() - ignore]
        .parse()
        .map(ctor)
        .map_err(|_| ParseError::User {
            error: UserError::LitOutOfRange(s.into(), std::any::type_name::<T>()),
        })
}

pub fn parse_prog(prog: &str) -> Result<Program, ParseError> {
    parse_impl::ProgramParser::new().parse(prog)
}
