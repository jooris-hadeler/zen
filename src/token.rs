//! This module contains the definitions of the tokens produced by the lexical analysis.

use std::fmt::Display;

use crate::source::Span;

#[derive(Debug, Clone)]
/// Represents a token in the source code.
pub struct Token {
    /// The kind of the token.
    pub kind: TokenKind,
    /// The source text of the token.
    pub text: String,
    /// The span of the token in the source file.
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq)]
/// Represents the kind of a token.
pub enum TokenKind {
    /// Represents an identifier.
    Symbol,
    /// Represents an integer literal.
    Integer,
    /// Represents a floating-point literal.
    Float,
    /// Represents a string literal.
    String,
    /// Represents a boolean literal.
    Boolean,
    /// Represents a character literal.
    Char,

    /// Represents the `i8` type.
    TyI8,
    /// Represents the `i16` type.
    TyI16,
    /// Represents the `i32` type.
    TyI32,
    /// Represents the `i64` type.
    TyI64,
    /// Represents the `u8` type.
    TyU8,
    /// Represents the `u16` type.
    TyU16,
    /// Represents the `u32` type.
    TyU32,
    /// Represents the `u64` type.
    TyU64,
    /// Represents the `f32` type.
    TyF32,
    /// Represents the `f64` type.
    TyF64,
    /// Represents the `bool` type.
    TyBool,
    /// Represents the `char` type.
    TyChar,
    /// Represents the `str` type.
    TyStr,

    /// Represents the `let` keyword.
    KwLet,
    /// Represents the `mut` keyword.
    KwMut,
    /// Represents the `fn` keyword.
    KwFn,
    /// Represents the `if` keyword.
    KwIf,
    /// Represents the `else` keyword.
    KwElse,
    /// Represents the `while` keyword.
    KwWhile,
    /// Represents the `for` keyword.
    KwFor,
    /// Represents the `in` keyword.
    KwIn,
    /// Represents the `return` keyword.
    KwReturn,
    /// Represents the `break` keyword.
    KwBreak,
    /// Represents the `continue` keyword.
    KwContinue,
    /// Represents the `extern` keyword.
    KwExtern,
    /// Represents the `const` keyword.
    KwConst,
    /// Represents the `struct` keyword.
    KwStruct,
    /// Represents the `enum` keyword.
    KwEnum,
    /// Represents the `type` keyword.
    KwType,
    /// Represents the `import` keyword.
    KwImport,
    /// Represents the `from` keyword.
    KwFrom,
    /// Represents the `as` keyword.
    KwAs,
    /// Represents the `self` keyword.
    KwSelf,

    /// Represents the `+` operator.
    Plus,
    /// Represents the `-` operator.
    Minus,
    /// Represents the `*` operator.
    Star,
    /// Represents the `/` operator.
    Slash,
    /// Represents the `%` operator.
    Percent,
    /// Represents the `&` operator.
    Amper,
    /// Represents the `|` operator.
    Pipe,
    /// Represents the `^` operator.
    Caret,

    /// Represents the `==` operator.
    Equals,
    /// Represents the `!=` operator.
    NotEquals,
    /// Represents the `<` operator.
    Less,
    /// Represents the `<=` operator.
    LessEquals,
    /// Represents the `>` operator.
    Greater,
    /// Represents the `>=` operator.
    GreaterEquals,

    /// Represents the `&&` operator.
    And,
    /// Represents the `||` operator.
    Or,
    /// Represents the `!` operator.
    Bang,
    /// Represents the `?` operator.
    Question,
    /// Represents the `=` operator.
    Assign,

    /// Represents the `(` token.
    LParen,
    /// Represents the `)` token.
    RParen,
    /// Represents the `{` token.
    LBrace,
    /// Represents the `}` token.
    RBrace,
    /// Represents the `[` token.
    LBracket,
    /// Represents the `]` token.
    RBracket,
    /// Represents the `:` token.
    Colon,
    /// Represents the `::` token.
    DoubleColon,
    /// Represents the `;` token.
    Semicolon,
    /// Represents the `.` token.
    Dot,
    /// Represents the `,` token.
    Comma,
    /// Represents the `->` token.
    Arrow,
    /// Represents the `=>` token.
    FatArrow,

    /// Represents garbage text.
    Garbage,

    /// Represents the end of the file.
    Eof,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenKind::Symbol => "<SYMBOL>",
                TokenKind::Integer => "<INTEGER>",
                TokenKind::Float => "<FLOAT>",
                TokenKind::String => "<STRING>",
                TokenKind::Boolean => "<BOOLEAN>",
                TokenKind::Char => "<CHAR>",

                TokenKind::TyI8 => "i8",
                TokenKind::TyI16 => "i16",
                TokenKind::TyI32 => "i32",
                TokenKind::TyI64 => "i64",
                TokenKind::TyU8 => "u8",
                TokenKind::TyU16 => "u16",
                TokenKind::TyU32 => "u32",
                TokenKind::TyU64 => "u64",
                TokenKind::TyF32 => "f32",
                TokenKind::TyF64 => "f64",
                TokenKind::TyBool => "bool",
                TokenKind::TyChar => "char",
                TokenKind::TyStr => "str",

                TokenKind::KwLet => "let",
                TokenKind::KwMut => "mut",
                TokenKind::KwFn => "fn",
                TokenKind::KwIf => "if",
                TokenKind::KwElse => "else",
                TokenKind::KwWhile => "while",
                TokenKind::KwFor => "for",
                TokenKind::KwIn => "in",
                TokenKind::KwReturn => "return",
                TokenKind::KwBreak => "break",
                TokenKind::KwContinue => "continue",
                TokenKind::KwExtern => "extern",
                TokenKind::KwConst => "const",
                TokenKind::KwStruct => "struct",
                TokenKind::KwEnum => "enum",
                TokenKind::KwType => "type",
                TokenKind::KwImport => "import",
                TokenKind::KwFrom => "from",
                TokenKind::KwAs => "as",
                TokenKind::KwSelf => "self",

                TokenKind::Plus => "+",
                TokenKind::Minus => "-",
                TokenKind::Star => "*",
                TokenKind::Slash => "/",
                TokenKind::Percent => "%",
                TokenKind::Amper => "&",
                TokenKind::Pipe => "|",
                TokenKind::Caret => "^",
                TokenKind::Equals => "==",
                TokenKind::NotEquals => "!=",
                TokenKind::Less => "<",
                TokenKind::LessEquals => "<=",
                TokenKind::Greater => ">",
                TokenKind::GreaterEquals => ">=",
                TokenKind::And => "&&",
                TokenKind::Or => "||",
                TokenKind::Bang => "!",
                TokenKind::Question => "?",
                TokenKind::Assign => "=",
                TokenKind::LParen => "(",
                TokenKind::RParen => ")",
                TokenKind::LBrace => "{",
                TokenKind::RBrace => "}",
                TokenKind::LBracket => "[",
                TokenKind::RBracket => "]",
                TokenKind::Colon => ":",
                TokenKind::DoubleColon => "::",
                TokenKind::Semicolon => ";",
                TokenKind::Dot => ".",
                TokenKind::Comma => ",",
                TokenKind::Arrow => "->",
                TokenKind::FatArrow => "=>",

                TokenKind::Garbage => "<GARBAGE>",
                TokenKind::Eof => "<EOF>",
            }
        )
    }
}
