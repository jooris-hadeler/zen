//! This module contains the expression parsing logic for the `zen` language.

use crate::{
    source::Span,
    syntax::ast::{
        AtomExpr, AtomKind, BinaryExpr, BinaryOp, BlockExpr, BreakExpr, CallExpr, ContinueExpr,
        EnumLiteral, Expr, ForExpr, IfExpr, LetExpr, LiteralExpr, LiteralKind, ReturnExpr,
        SliceLiteral, StructLiteral, StructLiteralField, SymbolExpr, UnaryExpr, UnaryOp, WhileExpr,
    },
    token::TokenKind,
};

use super::Parser;

impl Parser<'_> {
    /// Parses an expression.
    pub fn parse_expr(&mut self) -> Option<Expr> {
        self.parse_expr_internal(false)
    }

    /// Parses an expression, but allows for disallowing expressions which make no sense in let expressions.
    fn parse_expr_internal(&mut self, inside_let_value: bool) -> Option<Expr> {
        match self.peek().kind {
            TokenKind::KwIf => self.parse_expr_if(),
            TokenKind::KwWhile => self.parse_expr_while(),
            TokenKind::KwFor => self.parse_expr_for(),

            // These expressions are not allowed inside the value of a let expression.
            TokenKind::KwLet if !inside_let_value => self.parse_expr_let(),
            TokenKind::KwBreak if !inside_let_value => self.parse_expr_break(),
            TokenKind::KwReturn if !inside_let_value => self.parse_expr_return(),
            TokenKind::KwContinue if !inside_let_value => self.parse_expr_continue(),

            _ => self.parse_expr_arithmetic(0),
        }
    }

    /// Parses a for expression.
    fn parse_expr_for(&mut self) -> Option<Expr> {
        let for_token = self.expect(TokenKind::KwFor)?;
        let mut span = for_token.span;

        // Parse the name of the variable.
        let name_token = self.expect(TokenKind::Symbol)?;
        let name = name_token.text.into();

        // Parse the `in` keyword.
        self.expect(TokenKind::KwIn)?;

        // Parse the expression.
        let iterable = self.parse_expr()?;

        // Parse the body.
        let body = self.parse_expr_block()?;
        span.end = body.span().end;

        Some(Expr::For(ForExpr {
            name,
            iterable: Box::new(iterable),
            body: Box::new(body),
            span,
        }))
    }

    /// Parses a continue expression.
    fn parse_expr_continue(&mut self) -> Option<Expr> {
        let continue_token = self.expect(TokenKind::KwContinue)?;
        let span = continue_token.span;

        Some(Expr::Continue(ContinueExpr { span }))
    }

    /// Parses a return expression.
    fn parse_expr_return(&mut self) -> Option<Expr> {
        let return_token = self.expect(TokenKind::KwReturn)?;
        let mut span = return_token.span;

        // Parse the optional expression.
        let value = if self.peek().kind != TokenKind::Semicolon {
            let expr = self.parse_expr()?;
            span.end = expr.span().end;

            Some(Box::new(expr))
        } else {
            None
        };

        Some(Expr::Return(ReturnExpr { value, span }))
    }

    /// Parses a break expression.
    fn parse_expr_break(&mut self) -> Option<Expr> {
        let break_token = self.expect(TokenKind::KwBreak)?;
        let mut span = break_token.span;

        // Parse the optional expression.
        let value = if self.peek().kind != TokenKind::Semicolon {
            let expr = self.parse_expr()?;
            span.end = expr.span().end;

            Some(Box::new(expr))
        } else {
            None
        };

        Some(Expr::Break(BreakExpr { value, span }))
    }

    /// Parses a while expression.
    fn parse_expr_while(&mut self) -> Option<Expr> {
        let while_token = self.expect(TokenKind::KwWhile)?;
        let mut span = while_token.span;

        // Parse the condition.
        let condition = self.parse_expr_arithmetic(0)?;

        // Parse the body.
        let body = self.parse_expr_block()?;
        span.end = body.span().end;

        Some(Expr::While(WhileExpr {
            condition: Box::new(condition),
            body: Box::new(body),
            span,
        }))
    }

    /// Parses a let expression.
    fn parse_expr_let(&mut self) -> Option<Expr> {
        let let_token = self.expect(TokenKind::KwLet)?;
        let mut span = let_token.span;

        // Parse the optional mutability annotation.
        let mutable = self.eat(TokenKind::KwMut);

        // Parse the name of the variable.
        let name_token = self.expect(TokenKind::Symbol)?;
        let name = name_token.text.into();

        // Parse the optional type annotation.
        let ty = if self.peek().kind == TokenKind::Colon {
            // Consume the colon.
            self.consume();

            // Parse the type.
            let ty = self.parse_type()?;
            span.end = ty.span().end;

            Some(ty)
        } else {
            None
        };

        // Parse the equals sign.
        self.expect(TokenKind::Assign)?;

        // Parse the value of the variable, this cannot be a let expression.
        let value = self.parse_expr_internal(true)?;
        span.end = value.span().end;

        Some(Expr::Let(LetExpr {
            name,
            ty,
            mutable,
            value: Box::new(value),
            span,
        }))
    }

    /// Parses an if expression.
    fn parse_expr_if(&mut self) -> Option<Expr> {
        let if_token = self.expect(TokenKind::KwIf)?;
        let mut span = if_token.span;

        // Parse the condition.
        let condition = self.parse_expr_arithmetic(0)?;

        // Parse the then block.
        let body = self.parse_expr_block()?;
        span.end = body.span().end;

        // Parse optional else block
        let else_body = if self.peek().kind == TokenKind::KwElse {
            // Consume the else token.
            self.consume();

            // Parse the else block.
            let block = self.parse_expr_block()?;
            span.end = block.span().end;

            Some(Box::new(block))
        } else {
            None
        };

        Some(Expr::If(IfExpr {
            condition: Box::new(condition),
            body: Box::new(body),
            else_body,
            span,
        }))
    }

    /// Parses an arithmetic expression with a given binding power.
    /// The `min_binding_power` parameter is the minimum binding power of the operators to parse.
    /// This function uses the Pratt parsing algorithm.
    fn parse_expr_arithmetic(&mut self, min_binding_power: u8) -> Option<Expr> {
        // Parse the left-hand side of the expression.
        // Try to parse a prefix operator, otherwise parse an operand or grouped expression.
        let mut lhs = if let Some(op) = self.try_parse_prefix_op() {
            let right_binding_power = self.get_prefix_binding_power(op);

            // Skip the operator.
            let op_token = self.consume();

            // Parse the right-hand side of the unary expression.
            let operand = self.parse_expr_arithmetic(right_binding_power)?;

            // Create the unary expression.
            let span = Span::new(op_token.span.start, operand.span().end, self.source.id());
            Expr::Unary(UnaryExpr {
                op,
                operand: Box::new(operand),
                span,
            })
        } else if self.peek().kind == TokenKind::LParen {
            // Consume the left parenthesis.
            self.consume();

            let expr = self.parse_expr_arithmetic(0)?;

            // Consume the right parenthesis.
            // TODO: if we add recovery, we should recover here.
            self.expect(TokenKind::RParen)?;

            expr
        } else {
            self.parse_expr_operand()?
        };

        loop {
            // Try to parse a postfix operator.
            if let Some(op) = self.try_parse_postfix_op() {
                let left_binding_power = self.get_postfix_binding_power(op);

                // If the operator has a lower binding power than the minimum binding power, break the loop.
                if left_binding_power < min_binding_power {
                    break;
                }

                // Consume the operator.
                let op_token = self.consume();

                // Create the unary expression.
                let span = Span::new(lhs.span().start, op_token.span.end, self.source.id());
                lhs = Expr::Unary(UnaryExpr {
                    op,
                    operand: Box::new(lhs),
                    span,
                });

                continue;
            }

            // Parse a function call, if the next token is a left parenthesis.
            if self.peek().kind == TokenKind::LParen {
                /// The binding power of the call operator.
                const EXPR_CALL_BINDING_POWER: u8 = 27;

                if EXPR_CALL_BINDING_POWER < min_binding_power {
                    break;
                }

                // Parse the function call, with the lhs being the function.
                lhs = self.parse_expr_call(lhs)?;

                continue;
            }

            // Try to parse an infix operator.
            if let Some(op) = self.try_parse_infix_op() {
                let (left_binding_power, right_binding_power) = self.get_infix_binding_power(op);

                // If the operator has a lower binding power than the minimum binding power, break the loop.
                if left_binding_power < min_binding_power {
                    break;
                }

                // Skip the operator.
                self.consume();

                // Parse the right-hand side of the binary expression.
                let rhs = self.parse_expr_arithmetic(right_binding_power)?;
                let span = Span::new(lhs.span().start, rhs.span().end, self.source.id());

                // Create the binary expression.
                lhs = Expr::Binary(BinaryExpr {
                    left: Box::new(lhs),
                    op,
                    right: Box::new(rhs),
                    span,
                });

                continue;
            };

            break;
        }

        Some(lhs)
    }

    /// Returns the binding power of a given prefix operator.
    fn get_prefix_binding_power(&self, op: UnaryOp) -> u8 {
        match op {
            UnaryOp::Negate | UnaryOp::Not => 27,

            // If the operator is not a prefix operator, panic.
            _ => unreachable!("this point should not be reached, since this method should only be called if we have a valid prefix operator!")
        }
    }

    /// Returns the binding power of a given postfix operator.
    fn get_postfix_binding_power(&self, op: UnaryOp) -> u8 {
        match op {
            UnaryOp::CheckValue => 30,

            // If the operator is not a postfix operator, panic.
            _ => unreachable!("this point should not be reached, since this method should only be called if we have a valid postfix operator!")
        }
    }

    /// Returns the binding power of the given binary operator.
    /// If the left binding power is lower than the right binding power, then the operator is left-associative.
    fn get_infix_binding_power(&self, op: BinaryOp) -> (u8, u8) {
        match op {
            BinaryOp::Assign => (3, 4),
            BinaryOp::LogicalOr => (5, 6),
            BinaryOp::LogicalAnd => (7, 8),
            BinaryOp::BitwiseOr => (9, 10),
            BinaryOp::BitwiseXor => (11, 12),
            BinaryOp::BitwiseAnd => (13, 14),
            BinaryOp::Equal | BinaryOp::Unequal => (15, 16),
            BinaryOp::LessThan
            | BinaryOp::LessEqual
            | BinaryOp::GreaterThan
            | BinaryOp::GreaterEqual => (17, 18),
            BinaryOp::Add | BinaryOp::Subtract => (19, 20),
            BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => (21, 22),
            BinaryOp::Member => (27, 28),
            // Handle all BinaryOperators here. If the operator is not an infix operator, panic.
        }
    }

    /// Tried to parse an operand. This method does not consume the token.
    /// Returns `Some(operand)` if the peeked token is an operand, `None` otherwise.
    fn try_parse_prefix_op(&mut self) -> Option<UnaryOp> {
        Some(match self.peek().kind {
            TokenKind::Minus => UnaryOp::Negate,
            TokenKind::Bang => UnaryOp::Not,

            // If the token is not a prefix operator, return `None`.
            _ => return None,
        })
    }

    /// Tries to parse an infix operator. This method does not consume the token.
    /// Returns `Some(op)` if the peeked token is an infix operator, `None` otherwise.
    fn try_parse_infix_op(&mut self) -> Option<BinaryOp> {
        Some(match self.peek().kind {
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Subtract,
            TokenKind::Star => BinaryOp::Multiply,
            TokenKind::Slash => BinaryOp::Divide,
            TokenKind::Percent => BinaryOp::Modulo,

            TokenKind::Amper => BinaryOp::BitwiseAnd,
            TokenKind::Pipe => BinaryOp::BitwiseOr,
            TokenKind::Caret => BinaryOp::BitwiseXor,

            TokenKind::And => BinaryOp::LogicalAnd,
            TokenKind::Or => BinaryOp::LogicalOr,

            TokenKind::Assign => BinaryOp::Assign,
            TokenKind::Dot => BinaryOp::Member,

            TokenKind::Equals => BinaryOp::Equal,
            TokenKind::NotEquals => BinaryOp::Unequal,
            TokenKind::Less => BinaryOp::LessThan,
            TokenKind::LessEquals => BinaryOp::LessEqual,
            TokenKind::Greater => BinaryOp::GreaterThan,
            TokenKind::GreaterEquals => BinaryOp::GreaterEqual,

            // If the peeked token is not an infix operator, return `None`.
            _ => return None,
        })
    }

    /// Tries to parse a postfix operator. This method does not consume the token.
    /// Returns `Some(op)` if the peeked token is a postfix operator, `None` otherwise.
    fn try_parse_postfix_op(&mut self) -> Option<UnaryOp> {
        Some(match self.peek().kind {
            TokenKind::Question => UnaryOp::CheckValue,

            // If the token is not a postfix operator, return `None`.
            _ => return None,
        })
    }

    /// Parses a function call expression.
    fn parse_expr_call(&mut self, func: Expr) -> Option<Expr> {
        // Skip the left parenthesis.
        self.consume();

        // Parse the arguments of the function call.
        let arguments = self.parse_expr_call_args()?;

        // Skip the right parenthesis.
        let right_paren = self.expect(TokenKind::RParen)?;

        // Create the function call expression.
        let span = Span::new(func.span().start, right_paren.span.end, self.source.id());
        Some(Expr::Call(CallExpr {
            function: Box::new(func),
            arguments,
            span,
        }))
    }

    /// Parses the comma-separated arguments of a function call.
    fn parse_expr_call_args(&mut self) -> Option<Box<[Expr]>> {
        let mut arguments = Vec::new();

        if self.peek().kind != TokenKind::RParen {
            arguments.push(self.parse_expr()?);
        }

        while self.peek().kind == TokenKind::Comma {
            // Consume the comma.
            self.consume();

            // Parse the next argument.
            arguments.push(self.parse_expr()?);
        }

        Some(arguments.into_boxed_slice())
    }

    /// Parses an atom, symbol or a literal expression.
    fn parse_expr_operand(&mut self) -> Option<Expr> {
        match self.peek().kind {
            TokenKind::Dot | TokenKind::LBracket => self.parse_expr_literal(),
            TokenKind::Symbol => self.parse_expr_symbol(),
            TokenKind::LBrace => self.parse_expr_block(),

            // If the token is not a symbol or a dot, parse an atom expression.
            _ => self.parse_expr_atom(),
        }
    }

    /// Parses a literal expression.
    fn parse_expr_literal(&mut self) -> Option<Expr> {
        match self.peek().kind {
            TokenKind::LBracket => self.parse_expr_literal_slice(),
            TokenKind::Dot => self.parse_expr_literal_struct_or_enum(),

            _ => unreachable!("this point should not be reached, since this method should only be called if we can parse a literal expression")
        }
    }

    /// Parses a slice literal expression.
    fn parse_expr_literal_slice(&mut self) -> Option<Expr> {
        // Consume the left bracket.
        let left_bracket_token = self.consume();

        // Parse the slice elements.
        let elements = self.parse_expr_literal_slice_elements()?;

        // Consume the right bracket.
        let right_bracket_token = self.expect(TokenKind::RBracket)?;

        // Create the slice literal expression.
        let span = Span::new(
            left_bracket_token.span.start,
            right_bracket_token.span.end,
            self.source.id(),
        );
        Some(Expr::Literal(LiteralExpr {
            kind: LiteralKind::Slice(SliceLiteral { elements }),
            span,
        }))
    }

    /// Parses the elements of a slice literal expression.
    fn parse_expr_literal_slice_elements(&mut self) -> Option<Box<[Expr]>> {
        let mut elements = Vec::new();

        // Parse the first element.
        if self.peek().kind != TokenKind::RBracket {
            elements.push(self.parse_expr()?);
        }

        // Parse the remaining elements.
        while self.peek().kind == TokenKind::Comma {
            // Consume the comma.
            self.consume();

            // Parse the next element.
            elements.push(self.parse_expr()?);
        }

        Some(elements.into())
    }

    /// Parses a struct or enum literal expression.
    fn parse_expr_literal_struct_or_enum(&mut self) -> Option<Expr> {
        // Consume the dot.
        let dot_token = self.consume();

        // Parse the symbol.
        // For now, we only allow symbols to be used as struct or enum names.
        // In the future, we might want to allow expressions to be used as struct or enum names.
        let symbol = self.expect(TokenKind::Symbol)?;

        let symbol_name = symbol.text.into();

        // Parse the struct or enum fields.
        match self.peek().kind {
            TokenKind::LBrace => self.parse_expr_literal_struct(symbol_name, dot_token.span),
            TokenKind::DoubleColon => self.parse_expr_literal_enum(symbol_name, dot_token.span),
            _ => {
                let token = self.peek().clone();
                self.error(token, "expected '{' or '::' after struct or enum name");
                None
            }
        }
    }

    /// Parses a struct literal expression.
    fn parse_expr_literal_struct(&mut self, name: Box<str>, span: Span) -> Option<Expr> {
        // Consume the left brace.
        self.consume();

        // Parse the struct fields.
        let fields = self.parse_expr_literal_struct_fields()?;

        // Consume the right brace.
        let right_brace_token = self.expect(TokenKind::RBrace)?;

        // Create the struct literal expression.
        let span = Span::new(span.start, right_brace_token.span.end, self.source.id());
        Some(Expr::Literal(LiteralExpr {
            kind: LiteralKind::Struct(StructLiteral { name, fields }),
            span,
        }))
    }

    /// Parses a comma-separated list of struct fields.
    fn parse_expr_literal_struct_fields(&mut self) -> Option<Box<[StructLiteralField]>> {
        let mut fields = Vec::new();

        // Try parsing the first field.
        if self.peek().kind == TokenKind::Symbol {
            fields.push(self.parse_expr_literal_struct_field()?);
        }

        // Parse the remaining fields.
        while self.peek().kind == TokenKind::Comma {
            // Consume the comma.
            self.consume();

            // Parse the next field.
            fields.push(self.parse_expr_literal_struct_field()?);
        }

        Some(fields.into())
    }

    /// Parses a struct field.
    fn parse_expr_literal_struct_field(&mut self) -> Option<StructLiteralField> {
        // Parse the field name.
        let name = self.expect(TokenKind::Symbol)?;

        // Parse the colon.
        self.expect(TokenKind::Colon)?;

        // Parse the field value.
        let value = self.parse_expr()?;
        let span = Span::new(name.span.start, value.span().end, self.source.id());

        Some(StructLiteralField {
            name: name.text.into(),
            value,
            span,
        })
    }

    /// Parses an enum literal expression.
    fn parse_expr_literal_enum(&mut self, name: Box<str>, mut span: Span) -> Option<Expr> {
        // Consume the double colon.
        self.expect(TokenKind::DoubleColon)?;

        // Parse the variant name.
        let variant = self.expect(TokenKind::Symbol)?;
        span.end = variant.span.end;

        // Parse the enum body.
        let mut body = None;
        if self.peek().kind == TokenKind::LBrace {
            // Parse the left brace.
            self.consume();

            // Parse the enum fields.
            body = Some(self.parse_expr_literal_struct_fields()?);

            // Consume the right brace.
            let right_brace_token = self.expect(TokenKind::RBrace)?;
            span.end = right_brace_token.span.end;
        }

        // Create the enum literal expression.
        Some(Expr::Literal(LiteralExpr {
            kind: LiteralKind::Enum(EnumLiteral {
                name,
                variant: variant.text.into(),
                body,
            }),
            span,
        }))
    }

    /// Parses an atom expression.
    fn parse_expr_atom(&mut self) -> Option<Expr> {
        match self.peek().kind {
            TokenKind::Integer => self.parse_expr_atom_integer(),
            TokenKind::Boolean => self.parse_expr_atom_boolean(),
            TokenKind::String => self.parse_expr_atom_string(),
            TokenKind::Float => self.parse_expr_atom_float(),
            TokenKind::Char => self.parse_expr_atom_char(),

            _ => {
                let token = self.peek().clone();
                let message = format!(
                    "expected 'INT', 'BOOLEAN', 'STRING', 'FLOAT' OR 'CHAR', found '{}'",
                    token.kind
                );
                self.error(token, message.as_str());
                None
            }
        }
    }

    /// Parses an integer atom expression.
    fn parse_expr_atom_integer(&mut self) -> Option<Expr> {
        let token = self.consume();

        let value = token.text.parse::<u64>().unwrap();
        let span = token.span;

        Some(Expr::Atom(AtomExpr {
            kind: AtomKind::Int(value),
            span,
        }))
    }

    /// Parses a boolean atom expression.
    fn parse_expr_atom_boolean(&mut self) -> Option<Expr> {
        let token = self.consume();

        let value = token.text.parse::<bool>().unwrap();
        let span = token.span;

        Some(Expr::Atom(AtomExpr {
            kind: AtomKind::Bool(value),
            span,
        }))
    }

    /// Parses a string atom expression.
    fn parse_expr_atom_string(&mut self) -> Option<Expr> {
        let token = self.consume();

        let value = token.text[1..token.text.len() - 1].to_string();
        let span = token.span;

        Some(Expr::Atom(AtomExpr {
            kind: AtomKind::String(value),
            span,
        }))
    }

    /// Parses a float atom expression.
    fn parse_expr_atom_float(&mut self) -> Option<Expr> {
        let token = self.consume();

        let value = token.text.parse::<f64>().unwrap();
        let span = token.span;

        Some(Expr::Atom(AtomExpr {
            kind: AtomKind::Float(value),
            span,
        }))
    }

    /// Parses a char atom expression.
    fn parse_expr_atom_char(&mut self) -> Option<Expr> {
        let token = self.consume();

        let value = token.text.chars().nth(1).unwrap();
        let span = token.span;

        Some(Expr::Atom(AtomExpr {
            kind: AtomKind::Char(value),
            span,
        }))
    }

    /// Parses a symbol expression.
    fn parse_expr_symbol(&mut self) -> Option<Expr> {
        let token = self.consume();

        let name = token.text.into();
        let span = token.span;

        Some(Expr::Symbol(SymbolExpr { name, span }))
    }

    /// Parses a block expression.
    pub fn parse_expr_block(&mut self) -> Option<Expr> {
        // Consume the `{`.
        let lbrace_token = self.expect(TokenKind::LBrace)?;
        let mut span = lbrace_token.span;

        let mut has_implicit_return = false;
        let mut exprs = Vec::new();

        if self.peek().kind != TokenKind::RBrace {
            loop {
                // Parse the expression.
                let expr = self.parse_expr()?;
                let require_semicolon = expr.require_semicolon();

                exprs.push(expr);

                if require_semicolon {
                    if self.peek().kind == TokenKind::Semicolon {
                        // Consume the `;`.
                        self.consume();
                    } else {
                        has_implicit_return = true;
                        break;
                    }
                }

                if self.peek().kind == TokenKind::RBrace {
                    break;
                }
            }
        }

        // Consume the `}`.
        let rbrace_token = self.expect(TokenKind::RBrace)?;
        span.end = rbrace_token.span.end;

        // Create the block.
        Some(Expr::Block(BlockExpr {
            exprs: exprs.into(),
            has_implicit_return,
            span,
        }))
    }
}
