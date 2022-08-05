//! Conversion of AST to intermediate representation.

use crate::ir::IR;
use crate::syntax::SyntaxTree;

/// Compiles an abstract syntax tree into intermediate representation; this assumes the code already
/// type-checks, and emits unoptimized IR.
pub fn ast2ir(_: SyntaxTree) -> IR {
    todo!()
}
