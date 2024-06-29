//! This module contains all the parsing logic for types in the `zen` language.

use crate::{
    source::Span,
    syntax::{
        ast::{ArrayType, PointerType, PrimitiveKind, PrimitiveType, ReferenceType, Type},
        parser::Parser,
    },
    token::TokenKind,
};

impl Parser<'_> {
    /// Parses a type.
    pub fn parse_type(&mut self) -> Option<Type> {
        match self.peek().kind {
            TokenKind::Amper => self.parse_type_reference(),
            TokenKind::Star => self.parse_type_pointer(),
            TokenKind::LBracket => self.parse_type_array(),

            _ => self.parse_type_primitive(),
        }
    }

    /// Parses an array type.
    fn parse_type_array(&mut self) -> Option<Type> {
        // Consume the '['.
        let lbracket_token = self.consume();

        // Parse the type.
        let ty = self.parse_type()?;

        // Consume the ';'.
        self.expect(TokenKind::Semicolon)?;

        // Parse the size.
        let length_token = self.expect(TokenKind::Integer)?;
        let size = length_token.text.parse().unwrap();

        // Consume the ']'.
        let rbracket_token = self.expect(TokenKind::RBracket)?;

        // Create the slice type.
        let span = Span::new(
            lbracket_token.span.start,
            rbracket_token.span.end,
            self.source.id(),
        );
        Some(Type::Array(ArrayType {
            ty: Box::new(ty),
            size,
            span,
        }))
    }

    /// Parses a pointer type.
    fn parse_type_pointer(&mut self) -> Option<Type> {
        // Consume the '*'.
        let star_token = self.consume();

        // Check if the pointer is mutable.
        let mutable = if self.peek().kind == TokenKind::KwMut {
            self.consume();
            true
        } else {
            false
        };

        // Parse the type.
        let ty = self.parse_type()?;

        // Create the pointer type.
        let span = Span::new(star_token.span.start, ty.span().end, self.source.id());
        Some(Type::Pointer(PointerType {
            ty: Box::new(ty),
            mutable,
            span,
        }))
    }

    /// Parses a reference type.
    fn parse_type_reference(&mut self) -> Option<Type> {
        // Consume the '&'.
        let amper_token = self.consume();

        // Check if the reference is mutable.
        let mutable = if self.peek().kind == TokenKind::KwMut {
            self.consume();
            true
        } else {
            false
        };

        // Parse the type.
        let ty = self.parse_type()?;

        // Create the reference type.
        let span = Span::new(amper_token.span.start, ty.span().end, self.source.id());
        Some(Type::Reference(ReferenceType {
            ty: Box::new(ty),
            mutable,
            span,
        }))
    }

    /// Parses a primitive type.
    fn parse_type_primitive(&mut self) -> Option<Type> {
        let kind = match self.peek().kind {
            TokenKind::TyI8 => PrimitiveKind::I8,
            TokenKind::TyI16 => PrimitiveKind::I16,
            TokenKind::TyI32 => PrimitiveKind::I32,
            TokenKind::TyI64 => PrimitiveKind::I64,

            TokenKind::TyU8 => PrimitiveKind::U8,
            TokenKind::TyU16 => PrimitiveKind::U16,
            TokenKind::TyU32 => PrimitiveKind::U32,
            TokenKind::TyU64 => PrimitiveKind::U64,

            TokenKind::TyF32 => PrimitiveKind::F32,
            TokenKind::TyF64 => PrimitiveKind::F64,

            TokenKind::TyBool => PrimitiveKind::Bool,
            TokenKind::TyChar => PrimitiveKind::Char,
            TokenKind::TyStr => PrimitiveKind::Str,

            _ => {
                let token = self.peek().clone();
                let message = format!(
                    "expected a primitive type like 'i8', 'bool', or 'str', found '{}'",
                    token.kind
                );
                self.error(token, message.as_str());
                return None;
            }
        };

        // Consume the token.
        let token = self.consume();

        // Create the type.
        Some(Type::Primitive(PrimitiveType {
            kind,
            span: token.span,
        }))
    }
}
