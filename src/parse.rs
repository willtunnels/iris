use crate::ast::raw_ast::{Lit, Program};
use lalrpop_util::lalrpop_mod;
use std::str::FromStr;

lalrpop_mod!(parse_impl);

pub type Token<'a> = parse_impl::Token<'a>;
type ParseError<'a> = lalrpop_util::ParseError<usize, Token<'a>, UserError>;

#[derive(Debug, Clone, thiserror::Error)]
pub enum UserError {
    #[error("the literal `{0}` does not fit into the type `{1}`")]
    LitOutOfRange(String, &'static str),
}

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

#[cfg(test)]
mod tests {
    use super::*;

    fn find_line_starts(text: &str) -> Vec<usize> {
        let mut line_starts = vec![0];
        for (i, c) in text.char_indices() {
            if c == '\n' {
                line_starts.push(i + 1);
            }
        }
        line_starts
    }

    #[test]
    fn test_parse_raw() {
        let prog = r"
            extern type Foo = crate::foo::Foo; // This is a comment
            extern type Bar<T> = crate::bar::Bar;

            // This is also a comment
            extern fn f(x: Foo, y: Bar<Foo>) = crate::f;
            extern fn g(x: Foo, y: Bar<Foo>) -> (i32, i32) = crate::g;

            fn h(x: i32) -> fn() -> fn(i32) -> i32 {
                let y = true;
                let z: i32 = x;
                0i32
            }
        ";
        println!("{:?}", find_line_starts(prog));
        parse_prog(prog).unwrap();
    }
}
