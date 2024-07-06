//! This module contains the parser for declarations in the `zen` language.

use crate::{
    syntax::ast::{Function, Parameter},
    token::TokenKind,
};

use super::Parser;

impl Parser<'_> {
    /// Parses a function definition.
    pub fn parse_function(&mut self) -> Option<Function> {
        // Consume the `fn`.
        let fn_token = self.expect(TokenKind::KwFn)?;

        // Consume the name of the function.
        let name_token = self.expect(TokenKind::Symbol)?;
        let name = name_token.text.into_boxed_str();

        self.expect(TokenKind::LParen)?;

        // Parse the parameters of the function.
        let params = self.parse_function_params()?;

        self.expect(TokenKind::RParen)?;

        // Parse the return type of the function.
        let return_type = if self.peek().kind == TokenKind::Arrow {
            self.consume();
            self.parse_type()
        } else {
            None
        };

        // Parse the body of the function.
        let body = self.parse_expr_block()?;

        // Create the function.
        Some(Function {
            name,
            params,
            return_type,
            body,
            span: fn_token.span,
        })
    }

    /// Parses a comma-separated list of function parameters.
    fn parse_function_params(&mut self) -> Option<Box<[Parameter]>> {
        let mut params = Vec::new();

        // Parse the first parameter.
        params.push(self.parse_function_param()?);

        // Parse the rest of the parameters.
        while self.peek().kind == TokenKind::Comma {
            // Consume the `,`.
            self.consume();

            // Parse the parameter.
            params.push(self.parse_function_param()?);
        }

        Some(params.into())
    }

    /// Parses a single function parameter.
    fn parse_function_param(&mut self) -> Option<Parameter> {
        // Consume the name of the parameter.
        let name_token = self.expect(TokenKind::Symbol)?;
        let name = name_token.text.into_boxed_str();
        let mut span = name_token.span;

        self.expect(TokenKind::Colon)?;

        // Parse the type of the parameter.
        let ty = Box::new(self.parse_type()?);
        span.end = ty.span().end;

        Some(Parameter { name, ty, span })
    }
}
