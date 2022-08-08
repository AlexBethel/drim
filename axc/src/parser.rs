//! AlexScript parser.

use std::{error::Error, fmt::Display};

use chumsky::{
    prelude::{choice, end, just, none_of, one_of, Simple},
    recursive::recursive,
    text::{ident, int, keyword, whitespace},
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
    whitespace_cmt().ignore_then(
        parse_statement(m)
            .repeated()
            .map(SyntaxTree)
            .then_ignore(end())
            .map(|exprs| {
                println!("{:#?}", exprs);
                todo!()
            }),
    )
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
    pad(keyword("data"))
        .ignore_then(parse_type(m))
        .then_ignore(pad(just('=')))
        .then(parse_constructor(m).repeated())
        .then_ignore(pad(just(';')))
        .map(|(typ, constructors)| Statement::TypeDefinition { typ, constructors })
}

fn parse_constructor(m: &ParserMeta) -> impl Parser<char, TypeConstructor, Error = Simple<char>> {
    pad(ident())
        .then(parse_type(m).repeated())
        .map(|(name, args)| TypeConstructor { name, args })
}

fn parse_instance_def<'a>(
    m: &'a ParserMeta,
) -> impl Parser<char, Statement, Error = Simple<char>> + 'a {
    pad(keyword("instance"))
        .ignore_then(pad(ident()))
        .then(parse_type(m))
        .then(
            parse_class_member(m)
                .repeated()
                .delimited_by(pad(just('{')), pad(just('}'))),
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
    pad(keyword("def"))
        .ignore_then(pad(ident()))
        .then(parse_pattern(m).repeated())
        .then(pad(just('=')).ignore_then(parse_expression(m)).or_not())
        .then_ignore(pad(just(';')))
        .map(|((name, arguments), definition)| ClassMember::Function {
            name,
            arguments,
            definition,
        })
}

fn parse_type_alias(m: &ParserMeta) -> impl Parser<char, ClassMember, Error = Simple<char>> {
    pad(keyword("type"))
        .ignore_then(parse_type(m))
        .then(pad(just('=')).ignore_then(parse_type(m)).or_not())
        .then_ignore(pad(just(';')))
        .map(|(left, right)| ClassMember::TypeAlias { left, right })
}

fn parse_class_def<'a>(
    m: &'a ParserMeta,
) -> impl Parser<char, Statement, Error = Simple<char>> + 'a {
    pad(keyword("class"))
        .ignore_then(pad(ident()))
        .then(pad(ident()))
        .then(
            parse_class_member(m)
                .repeated()
                .delimited_by(pad(just('{')), pad(just('}'))),
        )
        .map(|((name, var), decls)| Statement::ClassDefinition { name, var, decls })
}

fn parse_expression<'a>(m: &'a ParserMeta) -> impl Parser<char, Expr, Error = Simple<char>> + 'a {
    recursive(|full_expr| {
        let lambda = parse_lambda_expr(m, full_expr.clone());
        let let_ = parse_let_expr(m, full_expr.clone());
        let match_ = parse_match_expr(m, full_expr.clone());
        let record = parse_record_expr(m, full_expr.clone());

        let base = choice((parse_literal(m), parse_var_ref_expr(m)));
        let subscript = parse_subscript_expr(m, base);
        let term = choice((lambda, let_, match_, record, subscript));

        let unary = parse_unary(m, term);

        let binary = (0..=10).rev().fold(unary.boxed(), |p, precedence| {
            parse_binary(m, precedence, p).boxed()
        });

        binary
    })
}

fn parse_unary(
    _m: &ParserMeta,
    base: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> + Clone {
    pad(choice((just("-").to("-"), just("+").to("+"))))
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
        pad(just(def.name.to_string()))
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

fn parse_let_expr(
    m: &ParserMeta,
    base: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> + Clone {
    pad(keyword("let"))
        .ignore_then(parse_pattern(m))
        .then_ignore(pad(just('=')))
        .then(base.clone())
        .then_ignore(pad(keyword("in")))
        .then(base.clone())
        .map(|((left, right), into)| Expr::Let {
            left,
            right: Box::new(right),
            into: Box::new(into),
        })
}

fn parse_match_expr(
    m: &ParserMeta,
    base: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> + Clone {
    pad(keyword("match"))
        .ignore_then(base.clone())
        .then(
            parse_pattern(m)
                .then_ignore(pad(just("=>")))
                .then(base.clone())
                .separated_by(pad(just(",")))
                .allow_trailing()
                .delimited_by(pad(just('{')), pad(just('}'))),
        )
        .map(|(matcher, cases)| Expr::Match {
            matcher: Box::new(matcher),
            cases,
        })
}

fn parse_record_expr(
    _m: &ParserMeta,
    base: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> + Clone {
    pad(ident())
        .then_ignore(pad(just(':')))
        .then(base)
        .separated_by(pad(just(',')))
        .allow_trailing()
        .delimited_by(pad(just('{')), pad(just('}')))
        .map(Expr::Record)
}

fn parse_lambda_expr(
    m: &ParserMeta,
    base: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> + Clone {
    pad(keyword("fn"))
        .ignore_then(parse_pattern(m).repeated())
        .then_ignore(pad(just("->")))
        .then(base)
        .map(|(arguments, result)| Expr::Lambda {
            arguments,
            result: Box::new(result),
        })
}

fn parse_subscript_expr(
    _m: &ParserMeta,
    base: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> + Clone {
    enum SubscriptKind {
        Dot,
        Bracket,
    }

    base.clone()
        .then(
            choice((
                pad(just('.'))
                    .ignore_then(base.clone())
                    .map(|e| (SubscriptKind::Dot, e)),
                base.clone()
                    .delimited_by(pad(just('[')), pad(just(']')))
                    .map(|e| (SubscriptKind::Bracket, e)),
            ))
            .repeated(),
        )
        .map(|(l, subscripts): (Expr, Vec<(SubscriptKind, Expr)>)| {
            subscripts.into_iter().fold(l, |l, (kind, r)| match kind {
                SubscriptKind::Dot => Expr::DotSubscript {
                    value: Box::new(l),
                    subscript: Box::new(r),
                },
                SubscriptKind::Bracket => Expr::BracketSubscript {
                    value: Box::new(l),
                    subscript: Box::new(r),
                },
            })
        })
}

fn parse_var_ref_expr(_m: &ParserMeta) -> impl Parser<char, Expr, Error = Simple<char>> + Clone {
    pad(ident())
        .separated_by(pad(just("::")))
        .map(Expr::VariableReference)
}

fn parse_literal(_m: &ParserMeta) -> impl Parser<char, Expr, Error = Simple<char>> + Clone {
    let string_char = none_of("\"\\").or(just('\\').ignore_then(choice((
        just('n').to('\n'),
        just('t').to('\t'),
        just('r').to('\r'),
        just('0').to('\x00'),
        just('\'').to('\''),
        just('\"').to('\"'),
        just('x').ignore_then(
            one_of("0123456789abcdefABCDEF")
                .repeated()
                .exactly(2)
                .collect::<String>()
                .map(|s| u8::from_str_radix(&s, 16).unwrap().try_into().unwrap()),
        ),
        just('u').ignore_then(
            one_of("0123456789abcdefABCDEF")
                .repeated()
                .collect::<String>()
                .map(|s| u32::from_str_radix(&s, 16).unwrap().try_into().unwrap()),
        ),
    ))));

    let int = int(10)
        .map(|s: String| s.parse().unwrap())
        .map(Literal::Integer);

    let string = string_char
        .clone()
        .repeated()
        .collect()
        .delimited_by(just('\"'), just('\"'))
        .map(Literal::String);

    let float = one_of("0123456789")
        .repeated()
        .collect::<String>()
        .then_ignore(just('.'))
        .then(one_of("0123456789").repeated().collect::<String>())
        .map(|(l, r)| (l + "." + &r).parse().unwrap())
        .map(Literal::Float);

    pad(choice((int, float, string))).map(Expr::Literal)
}

fn parse_type(m: &ParserMeta) -> impl Parser<char, Type, Error = Simple<char>> {
    recursive(|rec| {
        choice((
            parse_named_type(m),
            parse_tuple_type(m, rec.clone()),
            parse_record_type(m, rec.clone()),
        ))
        .repeated()
        .at_least(1)
        .map(|types| {
            types
                .into_iter()
                .reduce(|l, r| Type::Application {
                    function: Box::new(l),
                    expression: Box::new(r),
                })
                .unwrap()
        })
    })
}

fn parse_named_type(_m: &ParserMeta) -> impl Parser<char, Type, Error = Simple<char>> {
    pad(ident()).map(Type::Named)
}

fn parse_tuple_type(
    _m: &ParserMeta,
    rec: impl Parser<char, Type, Error = Simple<char>>,
) -> impl Parser<char, Type, Error = Simple<char>> {
    rec.separated_by(pad(just(',')))
        .allow_trailing()
        .delimited_by(pad(just('(')), pad(just(')')))
        .map(|types| {
            if types.len() == 1 {
                // `(Int)` is the same as `Int`
                types.into_iter().next().unwrap()
            } else {
                Type::Tuple(types)
            }
        })
}

fn parse_record_type(
    _m: &ParserMeta,
    rec: impl Parser<char, Type, Error = Simple<char>>,
) -> impl Parser<char, Type, Error = Simple<char>> {
    pad(ident())
        .then_ignore(pad(just(':')))
        .then(rec)
        .separated_by(pad(just(',')))
        .allow_trailing()
        .delimited_by(pad(just('{')), pad(just('}')))
        .map(Type::Record)
}

fn parse_pattern(m: &ParserMeta) -> impl Parser<char, Pattern, Error = Simple<char>> + Clone {
    recursive(|rec| {
        choice((
            parse_ignore_pattern(m),
            parse_capture_pattern(m),
            parse_literal_pattern(m),
            parse_tuple_pattern(m, rec.clone()),
            parse_record_pattern(m, rec.clone()),
        ))
        .repeated()
        .at_least(1)
        .map(|pats| {
            pats.into_iter()
                .reduce(|l, r| Pattern::Destructure(Box::new(l), Box::new(r)))
                .unwrap()
        })
        .then(pad(just(':')).ignore_then(parse_type(m)).or_not())
        .map(|(pat, typ)| match typ {
            Some(typ) => Pattern::TypeAnnotated {
                pat: Box::new(pat),
                typ: Box::new(typ),
            },
            None => pat,
        })
    })
}

fn parse_ignore_pattern(_m: &ParserMeta) -> impl Parser<char, Pattern, Error = Simple<char>> {
    pad(keyword("_")).to(Pattern::Ignore)
}

fn parse_capture_pattern(_m: &ParserMeta) -> impl Parser<char, Pattern, Error = Simple<char>> {
    pad(ident()).map(Pattern::Capture)
}

fn parse_tuple_pattern(
    _m: &ParserMeta,
    rec: impl Parser<char, Pattern, Error = Simple<char>>,
) -> impl Parser<char, Pattern, Error = Simple<char>> {
    rec.separated_by(pad(just(',')))
        .allow_trailing()
        .delimited_by(pad(just('(')), pad(just(')')))
        .map(|pats| {
            if pats.len() == 1 {
                // `(Int)` is the same as `Int`
                pats.into_iter().next().unwrap()
            } else {
                Pattern::Tuple(pats)
            }
        })
}

fn parse_record_pattern(
    _m: &ParserMeta,
    rec: impl Parser<char, Pattern, Error = Simple<char>>,
) -> impl Parser<char, Pattern, Error = Simple<char>> {
    let item = pad(ident()).then(pad(just(':')).ignore_then(rec).or_not());
    let items = item.separated_by(pad(just(','))).allow_trailing();
    let ellipsis = pad(just("...")).or_not().map(|ellipsis| ellipsis.is_some());

    items
        .then(ellipsis)
        .delimited_by(pad(just('{')), pad(just('}')))
        .map(|(members, inexhaustive)| Pattern::Record {
            members,
            inexhaustive,
        })
}

fn parse_literal_pattern(m: &ParserMeta) -> impl Parser<char, Pattern, Error = Simple<char>> {
    // TODO: factor out literal parsing so we don't have to do this ugly `unreachable` stuff.
    parse_literal(m).map(|e| match e {
        Expr::Literal(lit) => Pattern::Literal(lit),
        _ => unreachable!(),
    })
}

fn whitespace_cmt() -> impl Parser<char, (), Error = Simple<char>> + Clone {
    whitespace().then_ignore(
        just("//")
            .then(none_of("\n").repeated())
            .then(just('\n'))
            .then(whitespace())
            .repeated(),
    )
}

fn pad<T>(
    p: impl Parser<char, T, Error = Simple<char>> + Clone,
) -> impl Parser<char, T, Error = Simple<char>> + Clone {
    p.then_ignore(whitespace_cmt())
}
