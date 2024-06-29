//! This module contains all the logic for lexical analysis of the source code.

use crate::{
    source::{Source, Span},
    token::{Token, TokenKind},
};

#[derive(Debug)]
/// The scanner struct is responsible for performing lexical analysis on the source code.
/// It reads the source code character by character and produces a stream of tokens.
pub struct Scanner<'src> {
    /// The source file to scan.
    source: &'src Source,
    /// The current byte offset in the source file.
    offset: usize,
}

impl<'src> Scanner<'src> {
    /// Creates a new scanner for the given source file.
    pub fn new(source: &'src Source) -> Self {
        Self { source, offset: 0 }
    }

    /// Returns the current character in the source file if there is one.
    fn current(&self) -> Option<char> {
        self.source.contents().get(self.offset..)?.chars().next()
    }

    /// Returns the next character in the source file if there is one.
    fn peek(&self, n: usize) -> Option<char> {
        self.source
            .contents()
            .get(self.offset + n..)?
            .chars()
            .next()
    }

    /// Advances the scanner to the next character.
    fn advance(&mut self) -> char {
        let ch = self.source.contents()[self.offset..]
            .chars()
            .next()
            .unwrap();
        self.offset += ch.len_utf8();
        ch
    }

    /// Skips any whitespace and comments in the source file.
    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.current() {
                Some(c) if c.is_whitespace() => {
                    self.advance();
                }
                Some('/') => {
                    if let Some('/') = self.peek(1) {
                        self.advance();
                        self.advance();
                        while let Some(c) = self.current() {
                            if c == '\n' {
                                break;
                            }
                            self.advance();
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    /// Scans the source file and produces the next token.
    pub fn scan_next(&mut self) -> Token {
        self.skip_whitespace_and_comments();

        match self.current() {
            // If we've reached the end of the file, return an EOF token.
            None => Token {
                kind: TokenKind::Eof,
                text: "<EOF>".into(),
                span: Span::new(self.offset, self.offset, self.source.id()),
            },

            // Otherwise, scan the next token.
            Some(c) => match c {
                '0'..='9' => self.scan_number(),
                'a'..='z' | 'A'..='Z' | '_' => self.scan_symbol_or_keyword(),
                '+' | '*' | '/' | '%' | '?' | '^' => self.scan_single_char_operator(),
                '|' | '&' => self.scan_double_char_operator(),
                '-' => self.scan_minus_or_arrow(),
                '=' => self.scan_equals_assign_or_fat_arrow(),
                '<' | '>' | '!' => self.scan_comparison_or_bang(),
                '(' | ')' | '[' | ']' | '{' | '}' => self.scan_paren_bracket_or_brace(),
                '.' | ':' | ',' | ';' => self.scan_punctuation(),
                '"' => self.scan_string(),
                '\'' => self.scan_char(),

                // If we encounter an unknown character, return a garbage token.
                _ => {
                    let start = self.offset;
                    self.advance();
                    Token {
                        kind: TokenKind::Garbage,
                        text: format!(
                            "unexpected character `{}` found",
                            &self.source.contents()[start..self.offset]
                        ),
                        span: Span::new(start, self.offset, self.source.id()),
                    }
                }
            },
        }
    }

    /// Scans a minus operator or an arrow in the source file.
    fn scan_minus_or_arrow(&mut self) -> Token {
        let start = self.offset;

        self.advance();

        let kind = if let Some('>') = self.current() {
            self.advance();
            TokenKind::Arrow
        } else {
            TokenKind::Minus
        };

        Token {
            kind,
            text: self.source.contents()[start..self.offset].to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }
    }

    /// Scans a double-character operator in the source file.
    fn scan_double_char_operator(&mut self) -> Token {
        let start = self.offset;

        let kind = match self.advance() {
            '|' => {
                if let Some('|') = self.current() {
                    self.advance();
                    TokenKind::Or
                } else {
                    TokenKind::Pipe
                }
            }
            '&' => {
                if let Some('&') = self.current() {
                    self.advance();
                    TokenKind::And
                } else {
                    TokenKind::Amper
                }
            }
            _ => unreachable!(),
        };

        Token {
            kind,
            text: self.source.contents()[start..self.offset].to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }
    }

    /// Scans a string literal in the source file.
    fn scan_string(&mut self) -> Token {
        let start = self.offset;

        // Skip the opening quote.
        self.advance();

        while let Some(c) = self.current() {
            match c {
                '"' => {
                    self.advance();
                    break;
                }
                '\\' => {
                    self.advance();
                    self.advance();
                }
                _ => {
                    self.advance();
                }
            }
        }

        let text = &self.source.contents()[start..self.offset];

        Token {
            kind: TokenKind::String,
            text: text.to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }
    }

    /// Scans a character literal in the source file.
    fn scan_char(&mut self) -> Token {
        let start = self.offset;

        // Skip the opening quote.
        self.advance();

        // Skip the character.
        self.advance();

        // Skip the closing quote.
        let ch = self.advance();

        if ch != '\'' {
            return Token {
                kind: TokenKind::Garbage,
                text: "expected `'` closing quote after character literal".into(),
                span: Span::new(start, self.offset, self.source.id()),
            };
        }

        Token {
            kind: TokenKind::Char,
            text: self.source.contents()[start..self.offset].to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }
    }

    /// Scans a punctuation character in the source file.
    fn scan_punctuation(&mut self) -> Token {
        let start = self.offset;

        let kind = match self.advance() {
            '.' => TokenKind::Dot,
            ':' => {
                if self.peek(0) == Some(':') {
                    self.advance();
                    TokenKind::DoubleColon
                } else {
                    TokenKind::Colon
                }
            }
            ',' => TokenKind::Comma,
            ';' => TokenKind::Semicolon,
            _ => unreachable!(),
        };

        Token {
            kind,
            text: self.source.contents()[start..self.offset].to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }
    }

    /// Scans a parenthesis, bracket, or brace in the source file.
    fn scan_paren_bracket_or_brace(&mut self) -> Token {
        let start = self.offset;

        let kind = match self.advance() {
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            _ => unreachable!(),
        };

        Token {
            kind,
            text: self.source.contents()[start..self.offset].to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }
    }

    /// Scans an equals (==) comparison, an assignment operator or a fat arrow in the source file.
    fn scan_equals_assign_or_fat_arrow(&mut self) -> Token {
        let start = self.offset;

        self.advance();

        let kind = if let Some('=') = self.current() {
            self.advance();
            TokenKind::Equals
        } else if let Some('>') = self.current() {
            self.advance();
            TokenKind::FatArrow
        } else {
            TokenKind::Assign
        };

        Token {
            kind,
            text: self.source.contents()[start..self.offset].to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }
    }

    /// Scans a less/greater (than) comparison or a bang operator in the source file.
    fn scan_comparison_or_bang(&mut self) -> Token {
        let start = self.offset;

        let kind = match self.advance() {
            '<' => {
                if let Some('=') = self.peek(1) {
                    self.advance();
                    TokenKind::LessEquals
                } else {
                    TokenKind::Less
                }
            }
            '>' => {
                if let Some('=') = self.peek(1) {
                    self.advance();
                    TokenKind::GreaterEquals
                } else {
                    TokenKind::Greater
                }
            }
            '!' => {
                if let Some('=') = self.peek(1) {
                    TokenKind::NotEquals
                } else {
                    TokenKind::Bang
                }
            }
            _ => unreachable!(),
        };

        Token {
            kind,
            text: self.source.contents()[start..self.offset].to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }
    }

    /// Scans a single-character operator in the source file.
    fn scan_single_char_operator(&mut self) -> Token {
        let start = self.offset;

        let kind = match self.advance() {
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Star,
            '/' => TokenKind::Slash,
            '%' => TokenKind::Percent,
            '?' => TokenKind::Question,
            '^' => TokenKind::Caret,
            _ => unreachable!(),
        };

        Token {
            kind,
            text: self.source.contents()[start..self.offset].to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }
    }

    /// Scans a symbol or keyword in the source file.
    fn scan_symbol_or_keyword(&mut self) -> Token {
        let start = self.offset;

        while let Some(c) = self.current() {
            match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                    self.advance();
                }
                _ => break,
            }
        }

        let text = &self.source.contents()[start..self.offset];

        // Determine the kind of the token based on the text, which can be a symbol or a keyword.
        let kind = match text {
            "i8" => TokenKind::TyI8,
            "i16" => TokenKind::TyI16,
            "i32" => TokenKind::TyI32,
            "i64" => TokenKind::TyI64,
            "u8" => TokenKind::TyU8,
            "u16" => TokenKind::TyU16,
            "u32" => TokenKind::TyU32,
            "u64" => TokenKind::TyU64,
            "f32" => TokenKind::TyF32,
            "f64" => TokenKind::TyF64,
            "bool" => TokenKind::TyBool,
            "char" => TokenKind::TyChar,
            "str" => TokenKind::TyStr,
            "let" => TokenKind::KwLet,
            "mut" => TokenKind::KwMut,
            "fn" => TokenKind::KwFn,
            "if" => TokenKind::KwIf,
            "else" => TokenKind::KwElse,
            "while" => TokenKind::KwWhile,
            "for" => TokenKind::KwFor,
            "in" => TokenKind::KwIn,
            "return" => TokenKind::KwReturn,
            "break" => TokenKind::KwBreak,
            "extern" => TokenKind::KwExtern,
            "continue" => TokenKind::KwContinue,
            "const" => TokenKind::KwConst,
            "struct" => TokenKind::KwStruct,
            "enum" => TokenKind::KwEnum,
            "type" => TokenKind::KwType,
            "import" => TokenKind::KwImport,
            "from" => TokenKind::KwFrom,
            "as" => TokenKind::KwAs,
            "self" => TokenKind::KwSelf,
            "true" | "false" => TokenKind::Boolean,
            _ => TokenKind::Symbol,
        };

        Token {
            kind,
            text: text.to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }
    }

    /// Scans a number literal in the source file.
    fn scan_number(&mut self) -> Token {
        let mut kind = TokenKind::Integer;

        let start = self.offset;

        // Scan the integer part of the number.
        while let Some(c) = self.current() {
            match c {
                '0'..='9' => {
                    self.advance();
                }
                '.' => {
                    kind = TokenKind::Float;
                    self.advance();
                    break;
                }
                _ => break,
            }
        }

        // Scan the fractional part of the number.
        if kind == TokenKind::Float {
            while let Some(c) = self.current() {
                match c {
                    '0'..='9' => {
                        self.advance();
                    }
                    '.' => {
                        self.advance();

                        // keep consuming numbers and digits until we reach a non-digit character
                        while let Some(c) = self.current() {
                            match c {
                                '0'..='9' | '.' => {
                                    self.advance();
                                }
                                _ => break,
                            }
                        }

                        return Token {
                            kind,
                            text: "multiple decimal points in number".into(),
                            span: Span::new(start, self.offset, self.source.id()),
                        };
                    }
                    _ => break,
                }
            }
        }

        let text = &self.source.contents()[start..self.offset];

        Token {
            kind,
            text: text.to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }
    }
}
