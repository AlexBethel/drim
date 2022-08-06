//! AlexScript parser.

use std::{error::Error, fmt::Display};

use chumsky::{
    prelude::{choice, end, just, one_of, todo, Simple},
    text::{ident, keyword, whitespace},
    Parser,
};

use crate::syntax::{
    ClassMember, Expr, Literal, Pattern, Statement, SyntaxTree, Type, TypeConstructor,
};

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
#[derive(PartialEq, Eq, Clone, Copy)]
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
pub fn parser<'a>(m: &'a ParserMeta) -> impl Parser<char, SyntaxTree, Error = Simple<char>> + 'a {
    // parse_statement(m)
    //     .repeated()
    //     .map(SyntaxTree)
    //     .then_ignore(end())
    parse_expression(m)
        .then_ignore(just(";").then(whitespace()))
        .repeated()
        .then_ignore(end())
        .map(|exprs| {
            println!("{:#?}", exprs);
            todo!()
        })
}

fn parse_statement<'a>(
    m: &'a ParserMeta,
) -> impl Parser<char, Statement, Error = Simple<char>> + 'a {
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
        .then_ignore(just('=').then(whitespace()))
        .then(parse_constructor(m).repeated())
        .then_ignore(just(';').then(whitespace()))
        .map(|(typ, constructors)| Statement::TypeDefinition { typ, constructors })
}

fn parse_constructor(m: &ParserMeta) -> impl Parser<char, TypeConstructor, Error = Simple<char>> {
    ident()
        .then(parse_type(m).repeated())
        .map(|(name, args)| TypeConstructor { name, args })
}

fn parse_instance_def<'a>(
    m: &'a ParserMeta,
) -> impl Parser<char, Statement, Error = Simple<char>> + 'a {
    keyword("instance")
        .ignore_then(ident())
        .then(parse_type(m))
        .then(
            parse_class_member(m)
                .repeated()
                .delimited_by(just('{').then(whitespace()), just('}').then(whitespace())),
        )
        .map(|((classname, typ), decls)| Statement::InstanceDefinition {
            class_name: classname,
            typ,
            decls,
        })
}

fn parse_class_decl_stmt<'a>(
    m: &'a ParserMeta,
) -> impl Parser<char, Statement, Error = Simple<char>> + 'a {
    parse_class_member(m).map(Statement::ClassMember)
}

fn parse_class_member<'a>(
    m: &'a ParserMeta,
) -> impl Parser<char, ClassMember, Error = Simple<char>> + 'a {
    choice((parse_func_decl(m), parse_type_alias(m)))
}

fn parse_func_decl<'a>(
    m: &'a ParserMeta,
) -> impl Parser<char, ClassMember, Error = Simple<char>> + 'a {
    keyword("def")
        .ignore_then(ident())
        .then(parse_pattern(m).repeated())
        .then(
            just('=')
                .then(whitespace())
                .ignore_then(parse_expression(m))
                .or_not(),
        )
        .then_ignore(just(';').then(whitespace()))
        .map(|((name, arguments), definition)| ClassMember::Function {
            name,
            arguments,
            definition,
        })
}

fn parse_type_alias(m: &ParserMeta) -> impl Parser<char, ClassMember, Error = Simple<char>> {
    keyword("type")
        .ignore_then(parse_type(m))
        .then(
            just('=')
                .then(whitespace())
                .ignore_then(parse_type(m))
                .or_not(),
        )
        .then_ignore(just(';').then(whitespace()))
        .map(|(left, right)| ClassMember::TypeAlias { left, right })
}

fn parse_class_def<'a>(
    m: &'a ParserMeta,
) -> impl Parser<char, Statement, Error = Simple<char>> + 'a {
    keyword("class")
        .ignore_then(ident())
        .then(ident())
        .then(
            parse_class_member(m)
                .repeated()
                .delimited_by(just('{').then(whitespace()), just('}').then(whitespace())),
        )
        .map(|((name, var), decls)| Statement::ClassDefinition { name, var, decls })
}

fn parse_expression<'a>(m: &'a ParserMeta) -> impl Parser<char, Expr, Error = Simple<char>> + 'a {
    (0..=10)
        .rev()
        .fold(parse_unary(m, parse_literal(m)).boxed(), |p, precedence| {
            parse_binary(m, precedence, p).boxed()
        })
}

fn parse_unary(
    _m: &ParserMeta,
    base: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> + Clone {
    choice((just("-").to("-"), just("+").to("+")))
        .then_ignore(whitespace())
        .repeated()
        .then(base.clone())
        .map(|(ops, exp)| {
            ops.into_iter().fold(exp, |exp, op| Expr::UnaryOp {
                kind: op.to_string(),
                val: Box::new(exp),
            })
        })
}

fn parse_binary<'a>(
    m: &'a ParserMeta,
    prec: u32,
    base: impl Parser<char, Expr, Error = Simple<char>> + Clone + 'a,
) -> impl Parser<char, Expr, Error = Simple<char>> + 'a + Clone {
    let op_defs = m.operators.iter().filter(|def| def.precedence == prec);
    let op_parsers = op_defs.map(|def| {
        just(def.name.to_string())
            .then(whitespace())
            .ignore_then(base.clone())
            .map(|e| (&def.name, &def.assoc, e))
    });

    let zero = one_of([]).map(|_| unreachable!()).boxed();
    let any_op = op_parsers.fold(zero, |l, r| l.or(r).boxed());
    let ops = any_op.repeated();

    base.then(ops).map(|(first, others)| {
        let mut assocs = others.iter().map(|(_, assoc, _)| assoc);
        let first_assoc = assocs.next();
        if !first_assoc.map_or(true, |first| assocs.all(|a| a == first)) {
            // TODO: Crash the parser properly here, with error recovery etc.
            panic!("Precedence parsing error: conflicting associativities");
        }

        let all_assoc = first_assoc.map(|o| **o).flatten();

        if all_assoc == None && others.len() >= 2 {
            panic!("Precedence parsing error: non-associative operation applied associatively");
        }

        match all_assoc {
            None | Some(Associativity::Left) => {
                others
                    .into_iter()
                    .fold(first, |left, (op_name, _assoc, right)| Expr::BinaryOp {
                        kind: op_name.to_owned(),
                        left: Box::new(left),
                        right: Box::new(right),
                    })
            }
            Some(Associativity::Right) => {
                // Right now we have:
                // a ^ b X c ^ d
                // | \-/ \-/ \-/
                // |  \---+---/
                // .      |
                // first  .
                //     others

                // To parse right-associatively, we need:
                // a ^ b X c ^ d
                // \-/ \-/ \-/ |
                //  \---+---/  |
                //      |      |
                //      .    last
                //   others_l
                let others_l = std::iter::once(&first)
                    .chain(others.iter().map(|(_name, _assoc, expr)| expr))
                    .zip(others.iter().map(|(name, _assoc, _expr)| name))
                    .collect::<Vec<_>>();
                let last = others
                    .iter()
                    .last()
                    .map(|(_name, _assoc, expr)| expr)
                    .unwrap_or(&first);

                // And then we can fold as with left-associative operators.
                others_l
                    .into_iter()
                    .rev()
                    .fold(last.to_owned(), |r, (l, op)| Expr::BinaryOp {
                        kind: op.to_string(),
                        left: Box::new(l.to_owned()),
                        right: Box::new(r),
                    })
            }
        }
    })
}

fn parse_literal(_m: &ParserMeta) -> impl Parser<char, Expr, Error = Simple<char>> + Clone {
    // TODO: add all the literals.
    chumsky::text::int(10)
        .then_ignore(whitespace())
        .map(|s: String| Expr::Literal(Literal::Integer(s.parse().unwrap())))
}

fn parse_type(_m: &ParserMeta) -> impl Parser<char, Type, Error = Simple<char>> {
    todo()
}

fn parse_pattern(_m: &ParserMeta) -> impl Parser<char, Pattern, Error = Simple<char>> {
    todo()
}
