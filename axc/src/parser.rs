//! AlexScript parser.

use std::{error::Error, fmt::Display};

use chumsky::{
    prelude::{choice, end, just, todo, Simple},
    text::{ident, keyword},
    Parser,
};

use crate::syntax::{ClassMember, Expr, Pattern, Statement, SyntaxTree, Type, TypeConstructor};

/// Adapter to make `chumsky`'s parser errors usable as standard Rust errors.
#[derive(Debug)]
pub struct ParserError(pub Vec<Simple<char>>);

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for e in &self.0 {
            write!(f, "{}", e)?;
        }
        Ok(())
    }
}

impl Error for ParserError {}

/// Information required to be able to parse AlexScript code, beyond the code itself.
pub struct ParserMeta {
    // This struct is just a total hacky workaround for the fact that chumsky isn't capable of
    // parsing a context-sensitive grammar. I don't intend on ever fixing this: the stage-1 compiler
    // will have a hardcoded list of operators, and the stage-2 compiler will have fully
    // overloadable custom operators.
    /// The list of registered binary operators.
    operators: Vec<OperatorDef>,
}

/// Definition of an operator.
struct OperatorDef {
    /// The string of symbols that goes between two terms to invoke this operator.
    name: String,

    /// The precedence level; if X has lower precedence than Y, then a X b Y c is a X (b Y c);
    /// otherwise, it is (a X b) Y c.
    precedence: u32,

    /// The associativity; if this is Left, then a X b X c is (a X b) X c; for Right, it is a X (b X
    /// c); for None, it is a syntax error.
    assoc: Option<Associativity>,
}

/// The possible associativity directions of an operator.
enum Associativity {
    Left,
    Right,
}

impl Default for ParserMeta {
    fn default() -> Self {
        use Associativity::*;
        Self {
            // These are mostly stolen from Haskell.
            operators: vec![
                OperatorDef {
                    // Exponentiation.
                    name: "^".to_string(),
                    precedence: 8,
                    assoc: Some(Right),
                },
                OperatorDef {
                    name: "*".to_string(),
                    precedence: 7,
                    assoc: Some(Left),
                },
                OperatorDef {
                    // Division, which always returns an exact result and does not round to an
                    // integer (unlike C etc.).
                    name: "/".to_string(),
                    precedence: 7,
                    assoc: Some(Left),
                },
                OperatorDef {
                    // Modulo, defined as Euclidean remainder.
                    name: "%".to_string(),
                    precedence: 7,
                    assoc: Some(Left),
                },
                OperatorDef {
                    name: "+".to_string(),
                    precedence: 6,
                    assoc: Some(Left),
                },
                OperatorDef {
                    name: "-".to_string(),
                    precedence: 6,
                    assoc: Some(Left),
                },
                OperatorDef {
                    // Append to head of list. This might get removed since it's inefficient,
                    // depending.
                    name: "::".to_string(),
                    precedence: 5,
                    assoc: Some(Right),
                },
                OperatorDef {
                    // Append lists.
                    name: "++".to_string(),
                    precedence: 5,
                    assoc: Some(Right),
                },
                OperatorDef {
                    name: "==".to_string(),
                    precedence: 4,
                    assoc: None,
                },
                OperatorDef {
                    name: "!=".to_string(),
                    precedence: 4,
                    assoc: None,
                },
                OperatorDef {
                    name: "<".to_string(),
                    precedence: 4,
                    assoc: None,
                },
                OperatorDef {
                    name: "<=".to_string(),
                    precedence: 4,
                    assoc: None,
                },
                OperatorDef {
                    name: ">".to_string(),
                    precedence: 4,
                    assoc: None,
                },
                OperatorDef {
                    name: ">=".to_string(),
                    precedence: 4,
                    assoc: None,
                },
                OperatorDef {
                    // Functor map.
                    name: "<$>".to_string(),
                    precedence: 4,
                    assoc: Some(Left),
                },
                OperatorDef {
                    // Functor map to constant.
                    name: "<$".to_string(),
                    precedence: 4,
                    assoc: Some(Left),
                },
                OperatorDef {
                    // Flipped `<$`.
                    name: "$>".to_string(),
                    precedence: 4,
                    assoc: Some(Left),
                },
                OperatorDef {
                    // Sequential application of applicative actions.
                    name: "<*>".to_string(),
                    precedence: 4,
                    assoc: Some(Left),
                },
                OperatorDef {
                    // Sequence applicative actions, discarding the left value.
                    name: "*>".to_string(),
                    precedence: 4,
                    assoc: Some(Left),
                },
                OperatorDef {
                    // Sequence applicative actions, discarding the right value.
                    name: "<*".to_string(),
                    precedence: 4,
                    assoc: Some(Left),
                },
                OperatorDef {
                    // Binary and boolean `and`.
                    name: "&".to_string(),
                    precedence: 3,
                    assoc: Some(Right),
                },
                OperatorDef {
                    // Binary and boolean `or`.
                    name: "|".to_string(),
                    precedence: 2,
                    assoc: Some(Right),
                },
                OperatorDef {
                    // Monad sequence.
                    name: ">>".to_string(),
                    precedence: 1,
                    assoc: Some(Left),
                },
                OperatorDef {
                    // Monad bind.
                    name: ">>=".to_string(),
                    precedence: 1,
                    assoc: Some(Left),
                },
                OperatorDef {
                    // Function application.
                    name: "$".to_string(),
                    precedence: 1,
                    assoc: Some(Left),
                },
            ],
        }
    }
}

/// Parser for AlexScript code.
pub fn parser(m: &ParserMeta) -> impl Parser<char, SyntaxTree, Error = Simple<char>> {
    parse_statement(m)
        .repeated()
        .map(SyntaxTree)
        .then_ignore(end())
}

fn parse_statement(m: &ParserMeta) -> impl Parser<char, Statement, Error = Simple<char>> {
    choice((
        parse_type_def(m),
        parse_instance_def(m),
        parse_class_def(m),
        parse_class_decl_stmt(m),
    ))
}

fn parse_type_def(m: &ParserMeta) -> impl Parser<char, Statement, Error = Simple<char>> {
    keyword("data")
        .ignore_then(parse_type(m))
        .then_ignore(just('='))
        .then(parse_constructor(m).repeated())
        .then_ignore(just(';'))
        .map(|(typ, constructors)| Statement::TypeDefinition { typ, constructors })
}

fn parse_constructor(m: &ParserMeta) -> impl Parser<char, TypeConstructor, Error = Simple<char>> {
    ident()
        .then(parse_type(m).repeated())
        .map(|(name, args)| TypeConstructor { name, args })
}

fn parse_instance_def(m: &ParserMeta) -> impl Parser<char, Statement, Error = Simple<char>> {
    keyword("instance")
        .ignore_then(ident())
        .then(parse_type(m))
        .then(
            parse_class_member(m)
                .repeated()
                .delimited_by(just('{'), just('}')),
        )
        .map(|((classname, typ), decls)| Statement::InstanceDefinition {
            class_name: classname,
            typ,
            decls,
        })
}

fn parse_class_decl_stmt(m: &ParserMeta) -> impl Parser<char, Statement, Error = Simple<char>> {
    parse_class_member(m).map(Statement::ClassMember)
}

fn parse_class_member(m: &ParserMeta) -> impl Parser<char, ClassMember, Error = Simple<char>> {
    choice((parse_func_decl(m), parse_type_alias(m)))
}

fn parse_func_decl(m: &ParserMeta) -> impl Parser<char, ClassMember, Error = Simple<char>> {
    keyword("def")
        .ignore_then(ident())
        .then(parse_pattern(m).repeated())
        .then(just('=').ignore_then(parse_expression(m)).or_not())
        .then_ignore(just(';'))
        .map(|((name, arguments), definition)| ClassMember::Function {
            name,
            arguments,
            definition,
        })
}

fn parse_type_alias(m: &ParserMeta) -> impl Parser<char, ClassMember, Error = Simple<char>> {
    keyword("type")
        .ignore_then(parse_type(m))
        .then(just('=').ignore_then(parse_type(m)).or_not())
        .then_ignore(just(';'))
        .map(|(left, right)| ClassMember::TypeAlias { left, right })
}

fn parse_class_def(m: &ParserMeta) -> impl Parser<char, Statement, Error = Simple<char>> {
    keyword("class")
        .ignore_then(ident())
        .then(ident())
        .then(
            parse_class_member(m)
                .repeated()
                .delimited_by(just('{'), just('}')),
        )
        .map(|((name, var), decls)| Statement::ClassDefinition { name, var, decls })
}

fn parse_expression(m: &ParserMeta) -> impl Parser<char, Expr, Error = Simple<char>> {
    todo()
}

fn parse_type(m: &ParserMeta) -> impl Parser<char, Type, Error = Simple<char>> {
    todo()
}

fn parse_pattern(m: &ParserMeta) -> impl Parser<char, Pattern, Error = Simple<char>> {
    todo()
}
