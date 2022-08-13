//! Syntax tree for Drim code.

use num_bigint::BigUint;

use crate::typeck;

/// A concrete syntax tree. This represents the full content of a Drim program, including all
/// whitespace, comments, and tokens: the source code of the original program can be recovered
/// completely using the syntax tree.
#[derive(Clone, Debug)]
pub struct SyntaxTree(pub Vec<Statement>);

/// Top-level statements, making up the overall program.
#[derive(Clone, Debug)]
pub enum Statement {
    /// Declaration of an abstract data type.
    TypeDefinition {
        /// The type being defined. This is only allowed to be `Named` or `Application`.
        typ: Type,

        /// The possible constructors of the data type.
        constructors: Vec<TypeConstructor>,
    },

    /// Declaration that a type implements a type class.
    InstanceDefinition {
        /// The name of the type class.
        class_name: Identifier,

        /// The type that conforms to the type class.
        typ: Type,

        /// The list of declarations that dictate the type's behavior when treated as an instance of
        /// the type class.
        decls: Vec<ClassMember>,
    },

    /// Declaration of a type class.
    ClassDefinition {
        /// The name of the class.
        name: Identifier,

        /// The type variable representing a type conforming to the class.
        var: String,

        /// The list of declarations (optionally filled-in) that are necessary for a type to conform
        /// to the type class.
        decls: Vec<ClassMember>,
    },

    /// Other declarations.
    ClassMember(ClassMember),
}

/// Top-level statements that are also allowed to occur within a type class definition, and which
/// therefore have an optional rather than strictly-required right-hand side, e.g., `type X;` rather
/// than `type X = Y;`.
#[derive(Clone, Debug)]
pub enum ClassMember {
    /// Declaration of a function or constant.
    Function {
        /// Name of the function.
        name: Identifier,

        /// The function arguments.
        arguments: Vec<Pattern>,

        /// The definition of the function.
        definition: Option<Expr>,

        /// The type of the overall function; this is filled in by the typechecker, and is left
        /// blank by the parser.
        typ: Option<typeck::Type>,
    },

    /// Declaration of a type that is a literal alias for another type.
    TypeAlias {
        /// The type being defined. This is only allowed to be `Named` or `Application`.
        left: Type,

        /// The target type.
        right: Option<Type>,
    },
}

/// A possible constructor for an abstract data type.
#[derive(Clone, Debug)]
pub struct TypeConstructor {
    /// The name of the constructor.
    pub name: String,

    /// The arguments to the abstract data type.
    pub args: Vec<Type>,
}

/// An expression.
#[derive(Clone, Debug)]
pub struct Expr {
    /// The contents of the expression.
    pub kind: ExprKind,

    /// An optional type signature, left as `None` by the parser and added by the type checker.
    pub typ: Option<typeck::Type>,
}

/// The different kinds of expressions.
#[derive(Clone, Debug)]
pub enum ExprKind {
    /// Unary operators, e.g., `-5`.
    UnaryOp {
        /// The text of the operator.
        kind: String,

        /// The value being operated upon.
        val: Box<Expr>,

        /// The function that the unary operator translates to.
        translation: String,
    },

    /// Binary operators, e.g., `5 + 5`.
    BinaryOp {
        /// The text of the operator.
        kind: String,

        /// The left side of the operator.
        left: Box<Expr>,

        /// The right side of the operator.
        right: Box<Expr>,

        /// The function that the binary operator translates to.
        translation: String,
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
    Record(Vec<(String, Expr)>),

    /// Anonymous functions, e.g., `fn x -> x + 1`.
    Lambda {
        /// Arguments to the lambda; multiple of these are equivalent to stacking lambdas by
        /// currying.
        arguments: Vec<Pattern>,

        /// The result of the lambda.
        result: Box<Expr>,
    },

    /// Dot subscripts, e.g., `foo.bar`.
    DotSubscript {
        /// The left side of the subscript.
        value: Box<Expr>,

        /// The right side of the subscript; semantically, this is only allowed to be a single word.
        subscript: Box<Expr>,
    },

    /// Bracket subscripts, e.g., `foo[bar]`.
    BracketSubscript {
        /// The left side of the subscript.
        value: Box<Expr>,

        /// The right side of the subscript.
        subscript: Box<Expr>,
    },

    /// Tuple expressions, e.g., `(2, 3)`.
    Tuple(Vec<Expr>),

    /// Variable references, possibly namespaced, e.g., `foo::bar::baz`.
    VariableReference(Identifier),

    /// Literal tokens, e.g., strings and numbers.
    Literal(Literal),
}

/// Type names.
#[derive(Clone, Debug)]
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
    Record(Vec<(String, Type)>),
}

/// Patterns for use in function arguments, lambda arguments, `let` statements, and `match`
/// statements.
#[derive(Clone, Debug)]
pub enum Pattern {
    /// `a`
    Capture(String),

    /// `(a, b)`
    Tuple(Vec<Pattern>),

    /// `{a: x, b: y}`
    Record {
        /// The named members of the record, in order of occurrence.
        members: Vec<(String, Option<Pattern>)>,

        /// Whether the record ends with "...", for ignoring unlisted elements.
        inexhaustive: bool,
    },

    /// `a: String`
    TypeAnnotated {
        /// The pattern being annotated.
        pat: Box<Pattern>,

        /// The type that `pat` is being asserted to have.
        typ: Box<Type>,
    },

    /// `Foo { a: x, b: y, ... }`
    // Note that the left side here *must* be just one word, semantically; but we let it be a
    // Pattern to make parsing easier.
    Destructure(Box<Pattern>, Box<Pattern>),

    /// `_`
    Ignore,

    /// `"hello"`
    Literal(Literal),
}

/// Namespaced identifiers.
#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Identifier {
    /// The elements of the identifier; there must be at least one of these.
    pub elems: Vec<String>,
}

/// Literal values included in source code.
#[derive(Clone, Debug)]
pub enum Literal {
    /// `"hello"`
    String(String),

    /// `123`
    Integer(BigUint),

    /// `123.456`
    Float(f64),
}
