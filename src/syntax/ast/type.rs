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
    /// An array type, e.g. `[i32; 5]`, `[str; 2]` or `[T; 32]`.
    Array(ArrayType),
    /// A slice type, e.g. `[i32]`, `[str]` or `[T]`.
    Slice(SliceType),
    /// A named type, e.g. `Foo`, `Bar` or `foo.Baz`.
    Named(NamedType),
}

impl Type {
    /// Returns the span of the type in the source code.
    pub fn span(&self) -> Span {
        match self {
            Type::Primitive(ty) => ty.span,
            Type::Reference(ty) => ty.span,
            Type::Pointer(ty) => ty.span,
            Type::Array(ty) => ty.span,
            Type::Slice(ty) => ty.span,
            Type::Named(ty) => ty.span,
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

#[derive(Debug, PartialEq)]
/// Represents a slice type in the `zen` language.
pub struct SliceType {
    /// The type of the elements in the slice.
    pub ty: Box<Type>,
    /// The span of the slice type in the source code.
    pub span: Span,
}

#[derive(Debug, PartialEq)]
/// Represents a named type in the `zen` language.
pub struct NamedType {
    /// The path of the named type.
    pub path: Box<[Box<str>]>,
    /// The generic parameters of this type.
    pub generics: Option<GenericParameterList>,
    /// The span of the named type in the source code.
    pub span: Span,
}

#[derive(Debug, PartialEq)]
/// Represents a list of generic parameters in the `zen` language.
pub struct GenericParameterList {
    /// The generic parameters of the type.
    pub params: Box<[GenericParameter]>,
    /// The span of the generic parameter list in the source code.
    pub span: Span,
}

#[derive(Debug, PartialEq)]
/// Represents a generic parameter in the `zen` language.
pub struct GenericParameter {
    /// The name of the generic parameter.
    pub ty: Box<Type>,
    /// The span of the generic parameter in the source code.
    pub span: Span,
}
