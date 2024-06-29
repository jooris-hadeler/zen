//! This module contains all the AST definitions for types in the `zen` language.

use crate::source::Span;

#[derive(Debug, PartialEq)]
/// Represents a type in the `zen` language.
pub enum Type {
    /// A primitive type, e.g. `i8`, `bool` or `str`.
    Primitive(PrimitiveType),
    /// A reference type, e.g. `&i32`, `&mut str` or `&T`.
    Reference(ReferenceType),
    /// A pointer type, e.g. `*i32`, `*mut str` or `*T`.
    Pointer(PointerType),
    /// An array type, e.g. `[i32]`, `[str]` or `[T]`.
    Array(ArrayType),
}

impl Type {
    /// Returns the span of the type in the source code.
    pub fn span(&self) -> Span {
        match self {
            Type::Primitive(ty) => ty.span,
            Type::Reference(ty) => ty.span,
            Type::Pointer(ty) => ty.span,
            Type::Array(ty) => ty.span,
        }
    }
}

#[derive(Debug, PartialEq)]
/// Represents a primitive type in the `zen` language.
pub struct PrimitiveType {
    /// The kind of primitive type.
    pub kind: PrimitiveKind,
    /// The span of the primitive type in the source code.
    pub span: Span,
}

#[derive(Debug, PartialEq)]
/// Represents the kind of a primitive type.
pub enum PrimitiveKind {
    /// A signed 8-bit integer, e.g. `i8`.
    I8,
    /// A signed 16-bit integer, e.g. `i16`.
    I16,
    /// A signed 32-bit integer, e.g. `i32`.
    I32,
    /// A signed 64-bit integer, e.g. `i64`.
    I64,
    /// An unsigned 8-bit integer, e.g. `u8`.
    U8,
    /// An unsigned 16-bit integer, e.g. `u16`.
    U16,
    /// An unsigned 32-bit integer, e.g. `u32`.
    U32,
    /// An unsigned 64-bit integer, e.g. `u64`.
    U64,
    /// A 32-bit floating point number, e.g. `f32`.
    F32,
    /// A 64-bit floating point number, e.g. `f64`.
    F64,
    /// A boolean, e.g. `bool`.
    Bool,
    /// A character, e.g. `char`.
    Char,
    /// A string, e.g. `str`.
    Str,
}

#[derive(Debug, PartialEq)]
/// Represents a reference type in the `zen` language.
pub struct ReferenceType {
    /// The type being referenced.
    pub ty: Box<Type>,
    /// Whether the reference is mutable.
    pub mutable: bool,
    /// The span of the reference type in the source code.
    pub span: Span,
}

#[derive(Debug, PartialEq)]
/// Represents a pointer type in the `zen` language.
pub struct PointerType {
    /// The type being pointed to.
    pub ty: Box<Type>,
    /// Whether the pointer is mutable.
    pub mutable: bool,
    /// The span of the pointer type in the source code.
    pub span: Span,
}

#[derive(Debug, PartialEq)]
/// Represents an array type in the `zen` language.
pub struct ArrayType {
    /// The type of the elements in the array.
    pub ty: Box<Type>,
    /// The size of the array.
    pub size: usize,
    /// The span of the array type in the source code.
    pub span: Span,
}
