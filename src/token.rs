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

    /// Represents the `let` keyword.
    Let,
    /// Represents the `mut` keyword.
    Mut,
    /// Represents the `fn` keyword.
    Fn,
    /// Represents the `if` keyword.
    If,
    /// Represents the `else` keyword.
    Else,
    /// Represents the `while` keyword.
    While,
    /// Represents the `for` keyword.
    For,
    /// Represents the `in` keyword.
    In,
    /// Represents the `return` keyword.
    Return,
    /// Represents the `break` keyword.
    Break,
    /// Represents the `continue` keyword.
    Continue,
    /// Represents the `extern` keyword.
    Extern,
    /// Represents the `const` keyword.
    Const,
    /// Represents the `struct` keyword.
    Struct,
    /// Represents the `enum` keyword.
    Enum,
    /// Represents the `type` keyword.
    Type,
    /// Represents the `import` keyword.
    Import,
    /// Represents the `from` keyword.
    From,
    /// Represents the `as` keyword.
    As,
    /// Represents the `self` keyword.
    Self_,

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
                TokenKind::Symbol => "SYMBOL",
                TokenKind::Integer => "INTEGER",
                TokenKind::Float => "FLOAT",
                TokenKind::String => "STRING",
                TokenKind::Boolean => "BOOLEAN",
                TokenKind::Char => "CHAR",
                TokenKind::Let => "let",
                TokenKind::Mut => "mut",
                TokenKind::Fn => "fn",
                TokenKind::If => "if",
                TokenKind::Else => "else",
                TokenKind::While => "while",
                TokenKind::For => "for",
                TokenKind::In => "in",
                TokenKind::Return => "return",
                TokenKind::Break => "break",
                TokenKind::Continue => "continue",
                TokenKind::Extern => "extern",
                TokenKind::Const => "const",
                TokenKind::Struct => "struct",
                TokenKind::Enum => "enum",
                TokenKind::Type => "type",
                TokenKind::Import => "import",
                TokenKind::From => "from",
                TokenKind::As => "as",
                TokenKind::Self_ => "self",
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
                TokenKind::Garbage => "GARBAGE",
                TokenKind::Eof => "EOF",
            }
        )
    }
}
