//! Type checker.

use std::{collections::BTreeMap, error::Error, fmt::Display};

use num_bigint::BigInt;

use crate::syntax::{Identifier, SyntaxTree};

/// A compile-time type error from the user's source code.
#[derive(Debug)]
pub struct TypeError;

impl Display for TypeError {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Error for TypeError {}

/// A type known at compile time. While this resembles the AST `Type` structure, this enum is
/// optimized for unifying types against one another and representing compiler-generated types
/// rather than strictly representing named types.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// `Foo`
    Named(Identifier),

    /// `List Int`
    Application {
        /// The function being applied. This must be a generic type.
        function: Box<Type>,

        /// The type given as an argument to the type.
        expression: Box<Type>,
    },

    /// `(a, b)`
    Tuple(Vec<Type>),

    /// `{ a: x, b: y }`
    Record(BTreeMap<String, Type>),

    /// Compiler-internal type representing an arbitrary-precision integer whose value is known at
    /// compile time. This is the default type of integer literals. A `CompInt` can be converted to
    /// an actual integer type via implicit application of the `fromCompInt` generic function.
    CompInt(BigInt),

    /// Compiler-internal type representing a string literal. See `CompInt`.
    CompString(String),
}

/// Type-checks the syntax tree.
pub fn typeck(_: SyntaxTree) -> Result<SyntaxTree, TypeError> {
    todo!()
}
