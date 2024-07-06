//! This module contains the `zen` language AST (Abstract Syntax Tree).

pub mod decl;
pub mod expr;
pub mod r#type;

pub use decl::*;
pub use expr::*;
pub use r#type::*;

#[derive(Debug, PartialEq, Default)]
/// Represents a module in the `zen` language.
pub struct Module {
    /// The list of functions in the module.
    pub functions: Vec<Function>,
    /// The list of type definitions in the module.
    pub type_defs: Vec<TypeDef>,
}
