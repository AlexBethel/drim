//! Untyped intermediate representation.
//!
//! Untyped IR is immediately generated after the AST has been successfully parsed. It dramatically
//! simplifies the complexity of the syntax tree, and is used to perform type inference, at which
//! point it is translated into typed IR.

use crate::{
    syntax::{self, Identifier, SyntaxTree},
    typeck::Type,
};

/// A program represented in untyped IR.
#[derive(Debug)]
pub struct Program {
    /// The list of top-level declarations. Each declaration has a fully-namespaced name, and a list
    /// of instructions that should return the name's value. E.g., for
    /// ```
    /// def x = 5;
    /// ```
    /// the name `x` becomes associated with a list of instructions that return the value 5. When
    /// the program starts up, each one of these top-level functions is immediately called to
    /// initialize global variables, and then `main` is invoked.
    defs: Vec<(Identifier, Vec<Instruction>)>,
}

/// An instruction in untyped IR.
#[derive(Debug)]
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
#[derive(Debug, Clone)]
enum Location {
    /// A named location. Identifiers can be read from to access globally-defined functions and
    /// constants; but identifiers that are bound to (e.g., the `x` in `let x = 5`) must not be
    /// namespaced.
    Named(Identifier),

    /// A compiler-generated temporary location.
    Temporary(u64),
}

/// Converts a program's abstract syntax tree into untyped IR code.
pub fn ast_to_untyped_ir(ast: SyntaxTree) -> Program {
    let mut defs = vec![];
    let mut counter = 0;
    for stmt in ast.0.into_iter() {
        match stmt {
            syntax::Statement::TypeDefinition {
                typ: _,
                constructors: _,
            } => todo!(),
            syntax::Statement::InstanceDefinition {
                class_name: _,
                typ: _,
                decls: _,
            } => todo!(),
            syntax::Statement::ClassDefinition {
                name: _,
                var: _,
                decls: _,
            } => todo!(),
            syntax::Statement::ClassMember(syntax::ClassMember::Function {
                name,
                arguments,
                definition,
            }) => {
                defs.push((
                    name,
                    convert_fn(
                        &mut counter,
                        arguments,
                        definition.expect("Empty functions unimplemented"),
                    ),
                ));
            }
            syntax::Statement::ClassMember(syntax::ClassMember::TypeAlias {
                left: _,
                right: _,
            }) => {
                todo!()
            }
        }
    }

    Program { defs }
}

/// Generates a new temporary location name that's guaranteed to be unique.
fn temporary(counter: &mut u64) -> Location {
    let n = *counter;
    *counter += 1;
    Location::Temporary(n)
}

/// Converts a function definition into a list of instructions.
fn convert_fn(
    counter: &mut u64,
    mut arguments: Vec<syntax::Pattern>,
    definition: syntax::Expr,
) -> Vec<Instruction> {
    if arguments.len() > 1 {
        let first = arguments.remove(0);

        let lambda_loc = temporary(counter);
        let arg_loc = temporary(counter);
        vec![
            Instruction::DefLambda {
                target: lambda_loc.clone(),
                param: arg_loc.clone(),
                body: bind_pattern(counter, first, &arg_loc)
                    .into_iter()
                    .chain(convert_fn(counter, arguments, definition))
                    .collect(),
            },
            Instruction::Return { target: lambda_loc },
        ]
    } else {
        let ret_loc = temporary(counter);
        eval_expr(counter, definition, &ret_loc)
            .into_iter()
            .chain(std::iter::once(Instruction::Return { target: ret_loc }))
            .collect()
    }
}

/// Emits instructions that bind the given pattern to the variable stored in location `l`.
fn bind_pattern(counter: &mut u64, p: syntax::Pattern, l: &Location) -> Vec<Instruction> {
    match p {
        syntax::Pattern::Capture(name) => {
            vec![Instruction::Collect {
                target: Location::Named(syntax::Identifier { elems: vec![name] }),
                source: vec![l.to_owned()],
            }]
        }
        syntax::Pattern::Tuple(pats) => {
            let pat_locs: Vec<_> = pats.iter().map(|_| temporary(counter)).collect();
            std::iter::once(Instruction::DestructureTuple {
                source: l.to_owned(),
                targets: pat_locs.clone(),
            })
            .chain(
                Iterator::zip(pats.into_iter(), pat_locs)
                    .map(|(pat, pat_loc)| bind_pattern(counter, pat, &pat_loc))
                    .flatten(),
            )
            .collect()
        }
        syntax::Pattern::Record {
            members,
            inexhaustive,
        } => todo!(),
        syntax::Pattern::TypeAnnotated { pat, typ } => std::iter::once(Instruction::FixType {
            target: l.to_owned(),
            // typ: *typ,
            typ: todo!(),
        })
        .chain(bind_pattern(counter, *pat, l))
        .collect(),
        syntax::Pattern::Destructure(_, _) => todo!(),
        syntax::Pattern::Ignore => Vec::new(),
        syntax::Pattern::Literal(_) => Vec::new(),
    }
}

/// Emits intructions that evaluate the expression `e`, then place the result in location `l`.
fn eval_expr(counter: &mut u64, e: syntax::Expr, l: &Location) -> Vec<Instruction> {
    match e {
        syntax::Expr::UnaryOp {
            kind,
            val,
            translation,
        } => todo!(),
        syntax::Expr::BinaryOp {
            kind,
            left,
            right,
            translation,
        } => todo!(),
        syntax::Expr::Application { func, argument } => {
            let func_e = temporary(counter);
            let arg_e = temporary(counter);
            Iterator::chain(
                eval_expr(counter, *func, &func_e).into_iter(),
                eval_expr(counter, *argument, &arg_e).into_iter(),
            )
            .chain(vec![Instruction::Apply {
                target: l.to_owned(),
                func: func_e,
                argument: arg_e,
            }])
            .collect()
        }
        syntax::Expr::Let { left, right, into } => todo!(),
        syntax::Expr::Match { matcher, cases } => todo!(),
        syntax::Expr::Record(_) => todo!(),
        syntax::Expr::Lambda { arguments, result } => todo!(),
        syntax::Expr::DotSubscript { value, subscript } => todo!(),
        syntax::Expr::BracketSubscript { value, subscript } => todo!(),
        syntax::Expr::Tuple(elems) => {
            let elem_locs: Vec<_> = elems.iter().map(|_| temporary(counter)).collect();
            elems
                .into_iter()
                .zip(elem_locs.iter())
                .map(|(elem, elem_loc)| eval_expr(counter, elem, elem_loc))
                .flatten()
                .chain(std::iter::once(Instruction::Collect {
                    target: l.to_owned(),
                    source: elem_locs.clone(),
                }))
                .collect()
        }
        syntax::Expr::VariableReference(_) => todo!(),
        syntax::Expr::Literal(_) => todo!(),
    }
}
