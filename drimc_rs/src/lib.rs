//! Drim compiler.
//!
//! Drim is a modern, purely-functional programming language with a powerful linear type system and
//! an emphasis on performance.

#![deny(missing_docs)]

pub mod ast2ir;
pub mod backends;
pub mod ir;
pub mod parser;
pub mod syntax;
pub mod typeck;
