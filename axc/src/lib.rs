//! AlexScript compiler.
//!
//! AlexScript is a based programming language, for based people.

#![deny(missing_docs)]

pub mod ast2ir;
pub mod ir;
pub mod parser;
pub mod typeck;
pub mod backends;

use num_bigint::BigUint;

/// A concrete syntax tree. This represents the full content of an AlexScript program, including all
/// whitespace, comments, and tokens: the source code of the original program can be recovered
/// completely using the syntax tree.
pub struct SyntaxTree {}

/// Top-level statements, making up the overall program.
pub enum Statement {
    /// Declaration of an abstract data type.
    TypeDefinition {
        /// The type being defined. This is only allowed to be `Named` or `Application`.
        left: Type,

        /// The possible constructors of the data type.
        constructors: Vec<TypeConstructor>,
    },

    /// Declaration that a type implements a type class.
    InstanceDefinition {
        /// The name of the type class.
        class_name: String,

        /// The type that conforms to the type class.
        typ: Type,

        /// The list of declarations that dictate the type's behavior when treated as an instance of
        /// the type class.
        decls: Vec<ClassDeclaration>,
    },

    /// Other declarations.
    ClassDeclaration(ClassDeclaration),
}

/// Top-level statements that are also allowed to occur within a type class definition, and which
/// therefore have an optional rather than strictly-required right-hand side, e.g., `type X;` rather
/// than `type X = Y;`.
pub enum ClassDeclaration {
    /// Declaration of a function or constant.
    Function {
        /// Name of the function and its arguments.
        name: String,

        /// The function arguments.
        arguments: Vec<Pattern>,

        /// The definition of the function.
        definition: Option<Expr>,
    },

    /// Declaration of a type that is a literal alias for another type.
    TypeAlias {
        /// The type being defined. This is only allowed to be `Named` or `Application`.
        left: Type,

        /// The target type.
        right: Option<Type>,
    },

    /// Declaration of a type class.
    ClassDefinition {
        /// The name of the class.
        name: String,

        /// The type variable representing a type conforming to the class.
        var: String,

        /// The list of declarations (optionally filled-in) that are necessary for a type to conform
        /// to the type class.
        decls: Vec<ClassDeclaration>,
    },
}

/// A possible constructor for an abstract data type.
pub struct TypeConstructor {
    /// The name of the constructor.
    pub name: String,

    /// The arguments to the abstract data type.
    pub args: Vec<Type>,
}

/// Expressions.
pub enum Expr {
    /// Unary operators, e.g., `-5`.
    UnaryOp {
        /// The text of the operator.
        kind: String,

        /// The value being operated upon.
        val: Box<Expr>,
    },

    /// Binary operators, e.g., `5 + 5`.
    BinaryOp {
        /// The text of the operator.
        kind: String,

        /// The left side of the operator.
        left: Box<Expr>,

        /// The right side of the operator.
        right: Box<Expr>,
    },

    /// Function application, e.g., `sin x`.
    Application {
        /// The function being applied. For curried functions with multiple arguments (e.g., `atan2
        /// y x`), this is another expression of type `Application`.
        func: Box<Expr>,

        /// The argument to which the function is being applied.
        argument: Box<Expr>,
    },

    /// Defining of temporary variables, e.g., `let x = 5 in x + x`.
    Let {
        /// The pattern being bound.
        left: Pattern,

        /// The variable the pattern is matching.
        right: Box<Expr>,

        /// The expression the pattern is being substituted into.
        into: Box<Expr>,
    },

    /// Matching of multiple cases, e.g., `match x { 5 => 'a', 6 => 'b' }`.
    Match {
        /// The expression being matched upon.
        matcher: Box<Expr>,

        /// The possible cases of the `match` expression.
        cases: Vec<(Pattern, Expr)>,
    },

    /// Record initialization, e.g., `{ pointer: xyz, length: 12 }`.
    Record {
        /// The elements of the record.
        elements: Vec<(String, Expr)>,
    },

    /// Anonymous functions, e.g., `fn x -> x + 1`.
    Lambda {
        /// Arguments to the lambda; multiple of these are equivalent to stacking lambdas by
        /// currying.
        arguments: Vec<Pattern>,

        /// The result of the lambda.
        result: Box<Expr>,
    },

    /// Variable references, possibly namespaced, e.g., `foo::bar::baz`.
    VariableReference(Vec<String>),

    /// Dot subscripts, e.g., `foo.bar`.
    DotSubscript {
        /// The left side of the subscript.
        value: Box<Expr>,

        /// The right side of the subscript; this is only allowed to be a single word.
        subscript: String,
    },

    /// Bracket subscripts, e.g., `foo[bar]`.
    BracketSubscript {
        /// The left side of the subscript.
        value: Box<Expr>,

        /// The right side of the subscript.
        subscript: Box<Expr>,
    },

    /// Literal tokens, e.g., strings and numbers.
    Literal(Literal),
}

/// Type names.
pub enum Type {
    /// `Foo`
    Named(String),

    /// `List Int`
    Application {
        /// The function being applied. This must be a generic type.
        function: Box<Type>,

        /// The expression given as an argument to the type. This can be any expression, to allow
        /// const generics; in most cases, though, it should be just a normal type.
        expression: Box<Expr>,
    },

    /// `(a, b)`
    Tuple(Vec<Type>),

    /// `{ a: x, b: y }`
    Record(Vec<(String, Type)>),
}

/// Patterns for use in function arguments, lambda arguments, `let` statements, and `match`
/// statements.
pub enum Pattern {
    /// `(a, b)`
    Tuple(Vec<Pattern>),

    /// `a: String`
    TypeAnnotated {
        /// The pattern being annotated.
        pat: Box<Pattern>,

        /// The type that `pat` is being asserted to have.
        typ: Box<Type>,
    },

    /// `Foo`
    Exact(String),

    /// `Foo { a: x, b: y, ... }`
    Destructure(String, Record),

    /// `a`
    Capture(String),

    /// `_`
    Ignore,

    /// `"hello"`
    Literal(Literal),
}

/// Record syntax blocks, e.g., "{a: b, c: d, ...}".
pub struct Record {
    /// The named members of the record, in order of occurrence.
    pub members: Vec<(String, Expr)>,

    /// Whether the record ends with "..."; this allows ignoring blocks.
    pub inexhaustive: bool,
}

/// Literal values included in source code.
pub enum Literal {
    /// `"hello"`
    String(String),

    /// `123`
    Integer(BigUint),

    /// `123.456`
    Float(f64),
}
