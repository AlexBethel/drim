//! AlexScript compiler.
//!
//! AlexScript is a based programming language, for based people.

/// A concrete syntax tree. This represents the full content of an AlexScript program, including all
/// whitespace, comments, and tokens: the source code of the original program can be recovered
/// completely using the syntax tree.
pub struct SyntaxTree {}

/// Expressions.
pub enum Expr {
    /// Unary operators, e.g., "-5".
    UnaryOp { kind: UnaryOpKind, val: Box<Expr> },

    /// Binary operators, e.g., "5 + 5".
    BinaryOp {
        kind: BinaryOpKind,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    /// Function application, e.g., "sin x".
    Application {
        func: Box<Expr>,
        argument: Box<Expr>,
    },

    /// Matching of multiple cases, e.g., "match x { 5 => 'a', 6 => 'b' }".
    Match {
        matcher: Box<Expr>,
        cases: Vec<(Pattern, Expr)>,
    },

    /// Struct initialization, e.g., "Vector { pointer: xyz, length: 12 }".
    StructInit {
        name: String,
        elements: Vec<(String, Expr)>,
    },

    /// Anonymous functions.
    Lambda {
        arguments: Vec<Pattern>,
        result: Box<Expr>,
    },

    /// Variable references, possibly namespaced, e.g., "foo::bar::baz".
    VariableReference(Vec<String>),

    /// Dot subscripts, e.g., "foo.bar".
    DotSubscript { value: Box<Expr>, subscript: String },

    /// Bracket subscripts, e.g., "foo[bar]".
    BracketSubscript {
        value: Box<Expr>,
        subscript: Box<Expr>,
    },

    /// Literal tokens, e.g., strings and numbers.
    Literal(Literal),
}

/// Kinds of unary operators, that are placed before an expression.
pub enum UnaryOpKind {
    /// +x, equivalent to absolute value.
    Plus,

    /// -x, multiplication by -1.
    Minus,
}

/// Kinds of binary operations, that are placed between two expressions.
///
/// As a convention, all binary operations should be one character.
pub enum BinaryOpKind {
    /// a + b
    Add,

    /// a - b
    Sub,

    /// a * b
    Mul,

    /// a / b
    Div,

    /// a % b
    Modulo,

    /// a ^ b
    Exponent,

    /// a & b
    And,

    /// a | b
    Or,
}

/// 
pub enum Pattern {}

pub enum Literal {}

pub enum Token {}
