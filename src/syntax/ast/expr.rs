//! This module contains all AST definitions for expressions.

use crate::source::Span;

#[derive(Debug, PartialEq)]
/// Represents an expression in the `zen` language.
pub enum Expr {
    /// An atomic expression, e.g. `42` or `true`.
    Atom(AtomExpr),
    /// A literal expression, e.g. `.Point { x: 32, y: 44 }` or `(1, 2, 3)`.
    Literal(LiteralExpr),
    /// A unary expression, e.g. `-42`, `!true` or `value?`.
    Unary(UnaryExpr),
    /// A binary expression, e.g. `1 + 2`, `a == b` or `true && false`.
    Binary(BinaryExpr),
    /// A symbol expression, e.g. `x`, `y` or `z`.
    Symbol(SymbolExpr),
}

impl Expr {
    /// Returns the span of the expression.
    pub fn span(&self) -> Span {
        match self {
            Expr::Atom(expr) => expr.span,
            Expr::Literal(expr) => expr.span,
            Expr::Unary(expr) => expr.span,
            Expr::Binary(expr) => expr.span,
            Expr::Symbol(expr) => expr.span,
        }
    }
}

#[derive(Debug, PartialEq)]
/// Represents an atomic expression in the `zen` language.
/// Atomic expressions are one token long.
pub struct AtomExpr {
    /// The kind of atomic expression.
    pub kind: AtomKind,
    /// The span of the atomic expression in the source code.
    pub span: Span,
}

#[derive(Debug, PartialEq)]
/// Represents the kind of an atomic expression.
pub enum AtomKind {
    /// A string atom, e.g. `"hello"`.
    String(String),
    /// A character atom, e.g. `'a'`.
    Char(char),
    /// A float atom, e.g. `3.14`.
    Float(f64),
    /// A boolean atom, e.g. `true`.
    Bool(bool),
    /// An integer atom, e.g. `42`.
    Int(u64),
}

#[derive(Debug, PartialEq)]
/// Represents a literal expression in the `zen` language.
/// Literal expressions are one or more tokens long.
pub struct LiteralExpr {
    /// The kind of literal expression.
    pub kind: LiteralKind,
    /// The span of the literal expression in the source code.
    pub span: Span,
}

#[derive(Debug, PartialEq)]
/// Represents the kind of a literal expression.
pub enum LiteralKind {
    /// An enum literal, e.g. `.State::Loading`.
    Enum(EnumLiteral),
    /// A slice literal, e.g. `[1, 2, 3]`.
    Slice(SliceLiteral),
    /// A struct literal, e.g. `.Point { x: 32, y: 44 }`.
    Struct(StructLiteral),
}

#[derive(Debug, PartialEq)]
/// Represents an enum literal in the `zen` language.
pub struct EnumLiteral {
    // TODO: Implement the `EnumLiteral` struct.
}

#[derive(Debug, PartialEq)]
/// Represents a slice literal in the `zen` language.
pub struct SliceLiteral {
    /// The elements of the slice literal.
    pub elements: Box<[Expr]>,
}

#[derive(Debug, PartialEq)]
/// Represents a struct literal in the `zen` language.
pub struct StructLiteral {
    // TODO: Implement the `StructLiteral` struct.
}

#[derive(Debug, PartialEq)]
/// Represents a unary expression in the `zen` language.
/// Unary expressions have a single operand.
pub struct UnaryExpr {
    /// The operator of the unary expression.
    pub op: UnaryOp,
    /// The operand of the unary expression.
    pub operand: Box<Expr>,
    /// The span of the unary expression in the source code.
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq)]
/// Represents the operator of a unary expression.
pub enum UnaryOp {
    /// The boolean negation operator, e.g. `!true`.
    Not,
    /// The arithmetic negation operator, e.g. `-42`.
    Negate,
    /// The check value operator, e.g. `value?`.
    CheckValue,
}

#[derive(Debug, PartialEq)]
/// Represents a binary expression in the `zen` language.
/// Binary expressions have a left and right operand.
pub struct BinaryExpr {
    /// The left operand of the binary expression.
    pub left: Box<Expr>,
    /// The operator of the binary expression.
    pub op: BinaryOp,
    /// The right operand of the binary expression.
    pub right: Box<Expr>,
    /// The span of the binary expression in the source code.
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq)]
/// Represents the operator of a binary expression.
pub enum BinaryOp {
    /// Represents the addition operator, e.g. `1 + 2`.
    Add,
    /// Represents the subtraction operator, e.g. `1 - 2`.
    Subtract,
    /// Represents the multiplication operator, e.g. `1 * 2`.
    Multiply,
    /// Represents the division operator, e.g. `1 / 2`.
    Divide,
    /// Represents the modulo operator, e.g. `1 % 2`.
    Modulo,

    /// Represents the bitwise and operator, e.g. `1 & 2`.
    BitwiseAnd,
    /// Represents the bitwise or operator, e.g. `1 | 2`.
    BitwiseOr,
    /// Represents the bitwise xor operator, e.g. `1 ^ 2`.
    BitwiseXor,

    /// Represents the logical and operator, e.g. `true && false`.
    LogicalAnd,
    /// Represents the logical or operator, e.g. `true || false`.
    LogicalOr,

    /// Represents the equality operator, e.g. `a == b`.
    Equal,
    /// Represents the inequality operator, e.g. `a != b`.
    Unequal,
    /// Represents the less than operator, e.g. `a < b`.
    LessThan,
    /// Represents the less than or equal operator, e.g. `a <= b`.
    LessEqual,
    /// Represents the greater than operator, e.g. `a > b`.
    GreaterThan,
    /// Represents the greater than or equal operator, e.g. `a >= b`.
    GreaterEqual,

    /// Represents the assignment operator, e.g. `a = b`.
    Assign,
    /// Represents the member access operator, e.g. `a.b`.
    Member,
}

#[derive(Debug, PartialEq)]
/// Represents a symbol expression in the `zen` language.
pub struct SymbolExpr {
    /// The name of the symbol.
    pub name: Box<str>,
    /// The span of the symbol expression in the source code.
    pub span: Span,
}