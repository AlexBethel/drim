//! Untyped intermediate representation.
//!
//! Untyped IR is immediately generated after the AST has been successfully parsed. It dramatically
//! simplifies the complexity of the syntax tree, and is used to perform type inference, at which
//! point it is translated into typed IR.

use crate::{syntax::Identifier, typeck::Type};

/// An instruction in untyped IR.
enum Instruction {
    /// Apply a single-argument function to a value, and store the result in another location.
    Apply {
        /// The target into which to store the result.
        target: Location,

        /// The function to call.
        func: Location,

        /// The argument to call the function with.
        argument: Location,
    },

    /// Collect zero or more values into a storage location as a tuple.
    Collect {
        /// The target into which to store the result.
        target: Location,

        /// The source locations to pull from. If there is exactly one of these, then the variable
        /// is simply moved (since we define that a tuple of one element is the same as just that
        /// element); otherwise, the variables are collected into a tuple and moved into `target`.
        /// Also this means that if `source` is an empty `Vec`, then `target` becomes the unit value
        /// `()`.
        source: Vec<Location>,
    },

    /// Branch depending on which variant of its type a particular storage location is.
    Branch {
        /// The storage location to inspect.
        target: Location,

        /// The variant to test `target` against. This should be the name of a constructor. It is
        /// *undefined behavior* for this constructor to be for a type other than that of `target`;
        /// the AST translator must never produce a `Branch` node without also emitting a `FixType`
        /// node that specifies the type here.
        constructor: Identifier,

        /// Code to execute if the `target` is of the `variant` variant.
        iftrue: Vec<Instruction>,

        /// Code to execute otherwise.
        iffalse: Vec<Instruction>,
    },

    /// Destructure an algebraic object into its component pieces.
    DestructureData {
        /// The object we're destructuring.
        source: Location,

        /// The constructor that the `target` was constructed with. It is *undefined behavior* if
        /// `target` is of a variant type and this is not the constructor `target` was constructed
        /// with; the AST translator must never produce a `DestructureData` node without a guarding
        /// `Branch` node if there exists more than one variant of the underlying type.
        constructor: Identifier,

        /// The list of locations to store the parameters to the constructor. It is a type error
        /// (caught at type checking time) for the number of targets here to be different from the
        /// number of parameters to the constructor.
        targets: Vec<Location>,
    },

    /// Destructure a tuple into its component pieces.
    DestructureTuple {
        /// The tuple we're destructuring.
        source: Location,

        /// The list of locations to store the elements of the tuple. It is a type error (caught at
        /// type checking time) for the number of targets here to be different from the number of
        /// elements in the tuple.
        targets: Vec<Location>,
    },

    /// Create a new lambda with one parameter, and store it in a location.
    DefLambda {
        /// The location to store the lambda in.
        target: Location,

        /// The name of the parameter to the lambda.
        param: Location,

        /// Code to execute when the lambda is called.
        body: Vec<Instruction>,
    },

    /// Return a value from a function. It is *undefined behavior* for a function body to not
    /// include a `Return` instruction, and no instructions should ever follow a `Return`
    /// instruction.
    Return {
        /// The location to return.
        target: Location,
    },

    /// Mark this location as unreachable. If this node is reachable at runtime, then the entire
    /// program is undefined behavior.
    Unreachable,

    /// Assert that a storage location is of a particular type. While this node does nothing on its
    /// own, `FixType` is used as the fundamental anchor for type inference: all types in the
    /// program are deduced relative to this node.
    FixType {
        /// The storage location being annotated.
        target: Location,

        /// The type that the storage location should have.
        typ: Type,
    },
}

/// A storage location in untyped IR.
enum Location {
    /// A named location. Identifiers can be read from to access globally-defined functions and
    /// constants; but identifiers that are bound to (e.g., the `x` in `let x = 5`) must not be
    /// namespaced.
    Named(Identifier),

    /// A compiler-generated temporary location.
    Temporary(u64),
}
