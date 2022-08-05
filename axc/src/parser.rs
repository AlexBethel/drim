//! AlexScript parser.

use std::{error::Error, fmt::Display};

use chumsky::{
    prelude::{choice, empty, end, just, todo, Simple},
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

/// Parser for AlexScript code.
pub fn parser() -> impl Parser<char, SyntaxTree, Error = Simple<char>> {
    parse_statement()
        .repeated()
        .map(SyntaxTree)
        .then_ignore(end())
}

fn parse_statement() -> impl Parser<char, Statement, Error = Simple<char>> {
    choice((
        parse_type_def(),
        parse_instance_def(),
        parse_class_def(),
        parse_class_decl_stmt(),
    ))
}

fn parse_type_def() -> impl Parser<char, Statement, Error = Simple<char>> {
    keyword("data")
        .ignore_then(parse_type())
        .then_ignore(just('='))
        .then(parse_constructor().repeated())
        .then_ignore(just(';'))
        .map(|(typ, constructors)| Statement::TypeDefinition { typ, constructors })
}

fn parse_constructor() -> impl Parser<char, TypeConstructor, Error = Simple<char>> {
    ident()
        .then(parse_type().repeated())
        .map(|(name, args)| TypeConstructor { name, args })
}

fn parse_instance_def() -> impl Parser<char, Statement, Error = Simple<char>> {
    keyword("instance")
        .ignore_then(ident())
        .then(parse_type())
        .then(
            parse_class_member()
                .repeated()
                .delimited_by(just('{'), just('}')),
        )
        .map(|((classname, typ), decls)| Statement::InstanceDefinition {
            class_name: classname,
            typ,
            decls,
        })
}

fn parse_class_decl_stmt() -> impl Parser<char, Statement, Error = Simple<char>> {
    parse_class_member().map(Statement::ClassMember)
}

fn parse_class_member() -> impl Parser<char, ClassMember, Error = Simple<char>> {
    choice((parse_func_decl(), parse_type_alias()))
}

fn parse_func_decl() -> impl Parser<char, ClassMember, Error = Simple<char>> {
    keyword("def")
        .ignore_then(ident())
        .then(parse_pattern().repeated())
        .then(choice((
            just('=').ignore_then(parse_expression()).map(Some),
            empty().to(None),
        )))
        .then_ignore(just(';'))
        .map(|((name, arguments), definition)| ClassMember::Function {
            name,
            arguments,
            definition,
        })
}

fn parse_type_alias() -> impl Parser<char, ClassMember, Error = Simple<char>> {
    keyword("type")
        .ignore_then(parse_type())
        .then(choice((
            just('=').ignore_then(parse_type()).map(Some),
            empty().to(None),
        )))
        .then_ignore(just(';'))
        .map(|(left, right)| ClassMember::TypeAlias { left, right })
}

fn parse_class_def() -> impl Parser<char, Statement, Error = Simple<char>> {
    keyword("class")
        .ignore_then(ident())
        .then(ident())
        .then(
            parse_class_member()
                .repeated()
                .delimited_by(just('{'), just('}')),
        )
        .map(|((name, var), decls)| Statement::ClassDefinition { name, var, decls })
}

fn parse_expression() -> impl Parser<char, Expr, Error = Simple<char>> {
    todo()
}

fn parse_type() -> impl Parser<char, Type, Error = Simple<char>> {
    todo()
}

fn parse_pattern() -> impl Parser<char, Pattern, Error = Simple<char>> {
    todo()
}
