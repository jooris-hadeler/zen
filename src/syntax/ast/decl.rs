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
    /// The body of the function, which is always a block expression.
    pub body: Expr,
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
