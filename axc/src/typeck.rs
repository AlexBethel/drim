//! Type checker.

use std::{error::Error, fmt::Display};

use crate::syntax::SyntaxTree;

/// A compile-time type error from the user's source code.
#[derive(Debug)]
pub struct TypeError;

impl Display for TypeError {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Error for TypeError {}

/// Type-checks the syntax tree.
pub fn typeck(_: &SyntaxTree) -> Result<(), TypeError> {
    todo!()
}
