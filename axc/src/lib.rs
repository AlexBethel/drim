//! AlexScript compiler.
//!
//! AlexScript is a based programming language, for based people.

#![deny(missing_docs)]

pub mod ast2ir;
pub mod backends;
pub mod ir;
pub mod parser;
pub mod syntax;
pub mod typeck;
