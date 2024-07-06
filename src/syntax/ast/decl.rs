//! This module contains the AST definitions for declarations in the `zen` language.

use crate::source::Span;

use super::{Expr, Type};

#[derive(Debug, PartialEq)]
/// Represents a declaration in the source code.
pub struct Function {
    /// The name of the function.
    pub name: Box<str>,
    /// The parameters of the function.
    pub params: Box<[Parameter]>,
    /// The return type of the function.
    pub return_type: Option<Type>,
    /// The body of the function.
    pub body: Block,
    /// The span of the function in the source code.
    pub span: Span,
}

#[derive(Debug, PartialEq)]
/// Represents a parameter in a function declaration.
pub struct Parameter {
    /// The name of the parameter.
    pub name: Box<str>,
    /// The type of the parameter.
    pub ty: Box<Type>,
    /// The span of the parameter in the source code.
    pub span: Span,
}

#[derive(Debug, PartialEq)]
/// Represents a block of code in the `zen` language.
pub struct Block {
    /// The expressions in the block.
    pub exprs: Box<[Expr]>,
    /// If the block has an implicit return.
    pub has_implicit_return: bool,
    /// The span of the block in the source code.
    pub span: Span,
}
