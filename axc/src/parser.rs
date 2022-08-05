//! AlexScript parser.

use std::{error::Error, fmt::Display};

use chumsky::{
    prelude::{filter, Simple},
    Parser,
};

/// Adapter to make `chumsky`'s parser errors usable as standard Rust errors.
#[derive(Debug)]
pub struct ParserError(pub Vec<Simple<char>>);

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for e in &self.0 {
            write!(f, "{}", e)?;
        }
        Ok(())
    }
}

impl Error for ParserError {}

/// Parser for AlexScript code.
pub fn parser() -> impl Parser<char, crate::syntax::SyntaxTree, Error = Simple<char>> {
    filter(|c: &char| c.is_numeric()).map(|_| todo!())
}

fn parse_expression() -> impl Parser<char, crate::syntax::Expr, Error = Simple<char>> {
    parser().map(|_| todo!())
}

fn parse_type() -> impl Parser<char, crate::syntax::Type, Error = Simple<char>> {
    parser().map(|_| todo!())
}
