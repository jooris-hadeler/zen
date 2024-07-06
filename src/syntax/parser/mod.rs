//! This module contains the parser for the `zen` language.

use console::style;

use crate::{
    scanner::Scanner,
    source::Source,
    token::{Token, TokenKind},
};

pub mod decl;
pub mod expr;
pub mod r#type;

/// The parser for the `zen` language.
pub struct Parser<'src> {
    /// The source to parse.
    source: &'src Source,
    /// The scanner producing tokens from the source.
    scanner: Scanner<'src>,
    /// The current token in the source.
    /// This is `None` if the parser has not yet started.
    current: Option<Token>,
}

impl<'src> Parser<'src> {
    /// Creates a new parser for the given source.
    pub fn new(source: &'src Source) -> Self {
        let scanner = Scanner::new(source);

        Self {
            source,
            scanner,
            current: None,
        }
    }

    /// Prints an error message for the given token.
    fn error(&self, token: Token, message: &str) {
        let (line, column) = self.source.source_position(token.span.start);

        eprintln!(
            "{} {} {}",
            style(format!(
                "{}:{}:{}:",
                self.source.path().display(),
                line,
                column
            ))
            .cyan(),
            style("error:").red().bold(),
            style(message).white()
        );
    }

    /// Consumes the current token and returns it.
    fn consume(&mut self) -> Token {
        // If we haven't started parsing yet, advance to the first token.
        if self.current.is_none() {
            self.advance();
        }

        let token = self.current.take().unwrap();

        self.advance();

        token
    }

    /// Expects the current token to be of the given kind.
    /// If it is we consume it and return it, otherwise we return `None` and print an error.
    fn expect(&mut self, kind: TokenKind) -> Option<Token> {
        if self.peek().kind == kind {
            Some(self.consume())
        } else {
            let token = self.peek().clone();
            let message = format!("expected '{}', found '{}'", kind, token.kind);
            self.error(token, message.as_str());

            None
        }
    }

    /// Tries to eat the current token if it is of the given kind.
    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.peek().kind == kind {
            self.consume();
            true
        } else {
            false
        }
    }

    /// Peeks at the next token in the source.
    fn peek(&mut self) -> &Token {
        // If we haven't started parsing yet, advance to the first token.
        if self.current.is_none() {
            self.advance();
        }

        self.current.as_ref().unwrap()
    }

    /// Advances the parser by scanning the next token.
    /// If the scanned token is garbage, prints an error and advances again.
    fn advance(&mut self) {
        loop {
            let token = self.scanner.scan_next();

            if let Token {
                kind: TokenKind::Garbage,
                text,
                span,
            } = &token
            {
                let (line, column) = self.source.source_position(span.start);

                eprintln!(
                    "{} {} {}",
                    style(format!(
                        "{}:{}:{}: ",
                        self.source.path().display(),
                        line,
                        column
                    ))
                    .cyan()
                    .bold(),
                    style("error:").red().bold(),
                    style(text).white().bold()
                );
            } else {
                self.current = Some(token);
                break;
            }
        }
    }
}
