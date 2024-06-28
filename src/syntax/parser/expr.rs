//! This module contains the expression parsing logic for the `zen` language.

use crate::{
    source::Span,
    syntax::ast::{
        AtomExpr, AtomKind, BinaryExpr, BinaryOp, Expr, LiteralExpr, LiteralKind, SliceLiteral,
        SymbolExpr, UnaryExpr, UnaryOp,
    },
    token::TokenKind,
};

use super::Parser;

impl Parser<'_> {
    /// Parses an expression using the Pratt parsing algorithm.
    pub fn parse_expr(&mut self) -> Option<Expr> {
        self.parse_expr_bp(0)
    }

    /// Parses an expression with a given binding power.
    /// The `min_binding_power` parameter is the minimum binding power of the operators to parse.
    /// This function uses the Pratt parsing algorithm.
    fn parse_expr_bp(&mut self, min_binding_power: u8) -> Option<Expr> {
        // Parse the left-hand side of the expression.
        // Try to parse a prefix operator, otherwise parse an operand or grouped expression.
        let mut lhs = if let Some(op) = self.try_parse_prefix_op() {
            let right_binding_power = self.get_prefix_binding_power(op);

            // Skip the operator.
            let op_token = self.consume();

            // Parse the right-hand side of the unary expression.
            let operand = self.parse_expr_bp(right_binding_power)?;

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

            let expr = self.parse_expr_bp(0)?;

            // Consume the right parenthesis.
            if let None = self.expect(TokenKind::RParen) {
                // TODO: if we add recovery, we should recover here.
                return None;
            }

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
                let rhs = self.parse_expr_bp(right_binding_power)?;
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
            BinaryOp::LessThan | BinaryOp::LessEqual | BinaryOp::GreaterThan | BinaryOp::GreaterEqual => (17, 18),
            BinaryOp::Add | BinaryOp::Subtract => (19, 20),
            BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => (21, 22),
            BinaryOp::Member => (27, 28),

            // If the operator is not an infix operator, panic.
            _ => unreachable!("this point should not be reached, since this method should only be called if we have a valid infix operator!")
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

    /// Parses an atom, symbol or a literal expression.
    fn parse_expr_operand(&mut self) -> Option<Expr> {
        match self.peek().kind {
            TokenKind::Dot | TokenKind::LBracket => self.parse_expr_literal(),
            TokenKind::Symbol => self.parse_expr_symbol(),

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
        let Some(right_bracket_token) = self.expect(TokenKind::RBracket) else {
            return None;
        };

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
        if let Some(element) = self.parse_expr() {
            elements.push(element);
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
        todo!()
    }

    /// Parses an atom expression.
    fn parse_expr_atom(&mut self) -> Option<Expr> {
        match self.peek().kind {
            TokenKind::Integer => self.parse_expr_atom_integer(),
            TokenKind::Boolean => self.parse_expr_atom_boolean(),
            TokenKind::String => self.parse_expr_atom_string(),
            TokenKind::Float => self.parse_expr_atom_float(),
            TokenKind::Char => self.parse_expr_atom_char(),

            _ => None,
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
}
