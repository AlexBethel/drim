//! AlexScript compiler.
//!
//! AlexScript is a based programming language, for based people.

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
    UnaryOp { kind: String, val: Box<Expr> },

    /// Binary operators, e.g., `5 + 5`.
    BinaryOp {
        kind: String,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    /// Function application, e.g., `sin x`.
    Application {
        func: Box<Expr>,
        argument: Box<Expr>,
    },

    /// Defining of temporary variables, e.g., `let x = 5 in x + x`.
    Let { left: Pattern, right: Box<Expr> },

    /// Matching of multiple cases, e.g., `match x { 5 => 'a', 6 => 'b' }`.
    Match {
        matcher: Box<Expr>,
        cases: Vec<(Pattern, Expr)>,
    },

    /// Syntax sugar for matching on booleans, e.g., `if foo then bar else baz`.
    If {
        subject: Box<Expr>,
        iftrue: Box<Expr>,
        iffalse: Box<Expr>,
    },

    /// Struct initialization, e.g., `Vector { pointer: xyz, length: 12 }`.
    StructInit {
        name: String,
        elements: Vec<(String, Expr)>,
    },

    /// Anonymous functions, e.g., `fn x -> x + 1`.
    Lambda {
        arguments: Vec<Pattern>,
        result: Box<Expr>,
    },

    /// Variable references, possibly namespaced, e.g., `foo::bar::baz`.
    VariableReference(Vec<String>),

    /// Dot subscripts, e.g., `foo.bar`.
    DotSubscript { value: Box<Expr>, subscript: String },

    /// Bracket subscripts, e.g., `foo[bar]`.
    BracketSubscript {
        value: Box<Expr>,
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
        // TODO: is this right?
        function: Box<Expr>,
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
        pat: Box<Pattern>,
        // Note that types are expressions, to simplify parsing.
        typ: Box<Expr>,
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
