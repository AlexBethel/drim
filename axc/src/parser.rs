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
pub fn parser() -> impl Parser<char, crate::SyntaxTree, Error = Simple<char>> {
    filter(|c: &char| c.is_numeric()).map(|_| todo!())
}
