//! This module contains all the parsing logic for types in the `zen` language.

use crate::{
    source::Span,
    syntax::{
        ast::{
            ArrayType, GenericParameter, GenericParameterList, NamedType, PointerType,
            PrimitiveKind, PrimitiveType, ReferenceType, SliceType, StructField, StructTypeDef,
            Type, TypeDef,
        },
        parser::Parser,
    },
    token::TokenKind,
};

impl Parser<'_> {
    /// Parses a struct type definition.
    pub fn parse_typedef_struct(&mut self) -> Option<TypeDef> {
        let struct_token = self.expect(TokenKind::KwStruct)?;
        let mut span = struct_token.span;

        // Parse the name of the struct.
        let name_token = self.expect(TokenKind::Symbol)?;

        // Consume the '{'.
        self.expect(TokenKind::LBrace)?;

        // Parse the fields of the struct.
        let fields = self.parse_typedef_struct_fields()?;

        // Consume the '}'.
        let rbrace_token = self.expect(TokenKind::RBrace)?;
        span.end = rbrace_token.span.end;

        // Create the struct type definition.
        Some(TypeDef::Struct(StructTypeDef {
            name: name_token.text.into(),
            fields,
            span,
        }))
    }

    /// Parses the fields of a struct type definition.
    fn parse_typedef_struct_fields(&mut self) -> Option<Box<[StructField]>> {
        let mut fields = Vec::new();

        if self.peek().kind == TokenKind::Symbol {
            // Parse the first field.
            fields.push(self.parse_typedef_struct_field()?);

            // Parse the rest of the fields.
            while self.peek().kind == TokenKind::Comma {
                // Consume the ','.
                self.consume();

                // Parse the field.
                fields.push(self.parse_typedef_struct_field()?);
            }
        }

        Some(fields.into_boxed_slice())
    }

    /// Parses a field in a struct type definition.
    fn parse_typedef_struct_field(&mut self) -> Option<StructField> {
        // Parse the name of the field.
        let name_token = self.expect(TokenKind::Symbol)?;

        // Consume the ':'.
        self.expect(TokenKind::Colon)?;

        // Parse the type of the field.
        let ty = self.parse_type()?;

        // Create the struct field.
        let span = Span::new(name_token.span.start, ty.span().end, self.source.id());
        Some(StructField {
            name: name_token.text.into(),
            ty: Box::new(ty),
            span,
        })
    }

    /// Parses a type.
    pub fn parse_type(&mut self) -> Option<Type> {
        match self.peek().kind {
            TokenKind::LBracket => self.parse_type_array_or_slice(),
            TokenKind::Amper => self.parse_type_reference(),
            TokenKind::Star => self.parse_type_pointer(),
            TokenKind::Symbol => self.parse_type_named(),

            _ => self.parse_type_primitive(),
        }
    }

    /// Parses a generic parameter list.
    fn parse_type_generics(&mut self) -> Option<GenericParameterList> {
        // Consume the `[`.
        let left_bracket_token = self.expect(TokenKind::LBracket)?;
        let mut span = left_bracket_token.span;

        let mut generics = Vec::new();

        // Parse the first generic parameter.
        generics.push(self.parse_type_generic_parameter()?);

        // Parse the rest of the generic parameters.
        while self.peek().kind == TokenKind::Comma {
            // Consume the `,`.
            self.consume();

            // Parse the generic parameter.
            generics.push(self.parse_type_generic_parameter()?);
        }

        // Consume the `]`.
        let right_bracket_token = self.expect(TokenKind::RBracket)?;
        span.end = right_bracket_token.span.end;

        // Create the generic parameter list.
        Some(GenericParameterList {
            params: generics.into(),
            span,
        })
    }

    /// Parses a generic parameter.
    fn parse_type_generic_parameter(&mut self) -> Option<GenericParameter> {
        // Consume the symbol.
        let ty = self.parse_type()?;

        // Create the generic parameter.
        let span = ty.span();
        Some(GenericParameter {
            ty: Box::new(ty),
            span,
        })
    }

    /// Parses the path of a type.
    fn parse_type_path(&mut self) -> Option<(Box<[Box<str>]>, Span)> {
        let mut path = Vec::new();

        // Consume the first symbol.
        let symbol_token = self.expect(TokenKind::Symbol)?;
        let mut span = symbol_token.span;
        path.push(symbol_token.text.into());

        while self.peek().kind == TokenKind::Dot {
            // Consume the '.'.
            self.consume();

            // Parse the symbol.
            let symbol_token = self.expect(TokenKind::Symbol)?;
            span.end = symbol_token.span.end;
            path.push(symbol_token.text.into());
        }

        Some((path.into(), span))
    }

    /// Parses a named type.
    fn parse_type_named(&mut self) -> Option<Type> {
        let (path, span) = self.parse_type_path()?;

        let generics = if self.peek().kind == TokenKind::LBracket {
            Some(self.parse_type_generics()?)
        } else {
            None
        };

        Some(Type::Named(NamedType {
            path,
            generics,
            span,
        }))
    }

    /// Parses an array or slice type.
    fn parse_type_array_or_slice(&mut self) -> Option<Type> {
        // Consume the '['.
        let lbracket_token = self.consume();

        // Parse the type.
        let ty = self.parse_type()?;

        if self.peek().kind == TokenKind::Semicolon {
            self.parse_type_array(ty, lbracket_token.span)
        } else {
            self.parse_type_slice(ty, lbracket_token.span)
        }
    }

    /// Completes the parsing of a slice type.
    fn parse_type_slice(&mut self, ty: Type, mut span: Span) -> Option<Type> {
        // Consume the ']'.
        let rbracket_token = self.expect(TokenKind::RBracket)?;

        // Create the slice type.
        span.end = rbracket_token.span.end;
        Some(Type::Slice(SliceType {
            ty: Box::new(ty),
            span,
        }))
    }

    /// Completes the parsing of an array type.
    fn parse_type_array(&mut self, ty: Type, mut span: Span) -> Option<Type> {
        // Consume the ';'.
        self.expect(TokenKind::Semicolon)?;

        // Parse the size.
        let length_token = self.expect(TokenKind::Integer)?;
        let size = length_token.text.parse().unwrap();

        // Consume the ']'.
        let rbracket_token = self.expect(TokenKind::RBracket)?;

        // Create the slice type.
        span.end = rbracket_token.span.end;
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
