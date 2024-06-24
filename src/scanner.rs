//! This module contains all the logic for lexical analysis of the source code.

use std::{fmt::Display, path::PathBuf};

use console::style;

use crate::{
    source::{Source, Span},
    token::{Token, TokenKind},
};

/// Represents the result of scanning the source code.
type ScannerResult = Result<Option<Token>, ScannerError>;

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

    /// Collects all the tokens from the source file.
    /// Returns none if an error occurred during scanning.
    pub fn collect(mut self) -> Option<Vec<Token>> {
        let mut tokens = Vec::new();
        let mut errored = false;

        loop {
            match self.scan_next() {
                Ok(Some(token)) => tokens.push(token),
                Ok(None) => break,
                Err(err) => {
                    eprintln!("{}", err);
                    errored = true;
                }
            }
        }

        if !errored {
            Some(tokens)
        } else {
            None
        }
    }

    /// Scans the source file and produces the next token.
    pub fn scan_next(&mut self) -> ScannerResult {
        self.skip_whitespace_and_comments();

        match self.current() {
            None => Ok(None),

            Some(c) => match c {
                '0'..='9' => self.scan_number(),
                'a'..='z' | 'A'..='Z' | '_' => self.scan_symbol_or_keyword(),
                '+' | '*' | '/' | '%' => self.scan_single_char_operator(),
                '|' | '&' => self.scan_double_char_operator(),
                '-' => self.scan_minus_or_arrow(),
                '=' => self.scan_equals_assign_or_fat_arrow(),
                '<' | '>' | '!' => self.scan_comparison_or_bang(),
                '(' | ')' | '[' | ']' | '{' | '}' => self.scan_paren_bracket_or_brace(),
                '.' | ':' | ',' | ';' => self.scan_punctuation(),
                '"' => self.scan_string(),

                _ => unimplemented!(),
            },
        }
    }

    /// Scans a minus operator or an arrow in the source file.
    fn scan_minus_or_arrow(&mut self) -> ScannerResult {
        let start = self.offset;

        self.advance();

        let kind = if let Some('>') = self.current() {
            self.advance();
            TokenKind::Arrow
        } else {
            TokenKind::Minus
        };

        Ok(Some(Token {
            kind,
            text: self.source.contents()[start..self.offset].to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }))
    }

    /// Scans a double-character operator in the source file.
    fn scan_double_char_operator(&mut self) -> ScannerResult {
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

        Ok(Some(Token {
            kind,
            text: self.source.contents()[start..self.offset].to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }))
    }

    /// Scans a string literal in the source file.
    fn scan_string(&mut self) -> ScannerResult {
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

        Ok(Some(Token {
            kind: TokenKind::String,
            text: text.to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }))
    }

    /// Scans a punctuation character in the source file.
    fn scan_punctuation(&mut self) -> ScannerResult {
        let start = self.offset;

        let kind = match self.advance() {
            '.' => TokenKind::Dot,
            ':' => TokenKind::Colon,
            ',' => TokenKind::Comma,
            ';' => TokenKind::Semicolon,
            _ => unreachable!(),
        };

        Ok(Some(Token {
            kind,
            text: self.source.contents()[start..self.offset].to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }))
    }

    /// Scans a parenthesis, bracket, or brace in the source file.
    fn scan_paren_bracket_or_brace(&mut self) -> ScannerResult {
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

        Ok(Some(Token {
            kind,
            text: self.source.contents()[start..self.offset].to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }))
    }

    /// Scans an equals (==) comparison, an assignment operator or a fat arrow in the source file.
    fn scan_equals_assign_or_fat_arrow(&mut self) -> ScannerResult {
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

        Ok(Some(Token {
            kind,
            text: self.source.contents()[start..self.offset].to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }))
    }

    /// Scans a less/greater (than) comparison or a bang operator in the source file.
    fn scan_comparison_or_bang(&mut self) -> ScannerResult {
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

        Ok(Some(Token {
            kind,
            text: self.source.contents()[start..self.offset].to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }))
    }

    /// Scans a single-character operator in the source file.
    fn scan_single_char_operator(&mut self) -> ScannerResult {
        let start = self.offset;

        let kind = match self.advance() {
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Star,
            '/' => TokenKind::Slash,
            '%' => TokenKind::Percent,
            _ => unreachable!(),
        };

        Ok(Some(Token {
            kind,
            text: self.source.contents()[start..self.offset].to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }))
    }

    /// Scans a symbol or keyword in the source file.
    fn scan_symbol_or_keyword(&mut self) -> ScannerResult {
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
            "let" => TokenKind::Let,
            "mut" => TokenKind::Mut,
            "fn" => TokenKind::Fn,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "return" => TokenKind::Return,
            "break" => TokenKind::Break,
            "extern" => TokenKind::Extern,
            "continue" => TokenKind::Continue,
            "const" => TokenKind::Const,
            "struct" => TokenKind::Struct,
            "enum" => TokenKind::Enum,
            "type" => TokenKind::Type,
            "import" => TokenKind::Import,
            "from" => TokenKind::From,
            "as" => TokenKind::As,
            "true" | "false" => TokenKind::Boolean,
            _ => TokenKind::Symbol,
        };

        Ok(Some(Token {
            kind,
            text: text.to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }))
    }

    /// Scans a number literal in the source file.
    fn scan_number(&mut self) -> ScannerResult {
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

                        // Error: multiple decimal points in number.
                        let (line, column) = self.source.source_position(start);

                        // keep consuming numbers and digits until we reach a non-digit character
                        while let Some(c) = self.current() {
                            match c {
                                '0'..='9' | '.' => {
                                    self.advance();
                                }
                                _ => break,
                            }
                        }

                        return Err(ScannerError {
                            message: "multiple decimal points in number".to_string(),
                            path: self.source.path().clone(),
                            line,
                            column,
                        });
                    }
                    _ => break,
                }
            }
        }

        let text = &self.source.contents()[start..self.offset];

        Ok(Some(Token {
            kind,
            text: text.to_string(),
            span: Span::new(start, self.offset, self.source.id()),
        }))
    }
}

#[derive(Debug)]
/// Represents an error that occurred during scanning.
pub struct ScannerError {
    /// The error message.
    pub message: String,
    /// The path to the source file.
    pub path: PathBuf,
    /// The line where the error occurred.
    pub line: usize,
    /// The column where the error occurred.
    pub column: usize,
}

impl Display for ScannerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let path = style(format!(
            "{}:{}:{}:",
            self.path.display(),
            self.line,
            self.column
        ))
        .cyan()
        .bold();

        let error = style("Error:").red().bold();

        let message = style(self.message.as_str()).white();

        write!(f, "{} {} {}", path, error, message)
    }
}

#[cfg(test)]
mod tests {
    use crate::{scanner::Scanner, source::Source, token::TokenKind};

    #[test]
    fn test_scan() {
        let source = Source::from_string("<test>", "abc == 12.4 ! - true false fn let return");
        let expected = vec![
            ("abc", TokenKind::Symbol),
            ("==", TokenKind::Equals),
            ("12.4", TokenKind::Float),
            ("!", TokenKind::Bang),
            ("-", TokenKind::Minus),
            ("true", TokenKind::Boolean),
            ("false", TokenKind::Boolean),
            ("fn", TokenKind::Fn),
            ("let", TokenKind::Let),
            ("return", TokenKind::Return),
        ];

        let mut scanner = Scanner::new(&source);
        let mut index = 0;

        while let Ok(Some(token)) = scanner.scan_next() {
            let (text, kind) = expected[index];
            assert_eq!(token.text, text);
            assert_eq!(token.kind, kind);
            index += 1;
        }
    }

    #[test]
    fn test_scan_number() {
        let source = Source::from_string("<test>", "123 45.67 89.10.11");
        let mut scanner = Scanner::new(&source);

        let Ok(Some(token)) = scanner.scan_next() else {
            panic!("Expected a token, but got None or error");
        };

        assert_eq!(token.kind, TokenKind::Integer);
        assert_eq!(token.text, "123");

        let Ok(Some(token)) = scanner.scan_next() else {
            panic!("Expected a token, but got None or error");
        };

        assert_eq!(token.kind, TokenKind::Float);
        assert_eq!(token.text, "45.67");

        let Err(err) = scanner.scan_next() else {
            panic!("Expected a token, but got None or error");
        };
        assert_eq!(err.message, "multiple decimal points in number");
    }
}
