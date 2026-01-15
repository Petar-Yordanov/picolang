use crate::ast::*;
use crate::diagnostics::{Diagnostic, Span};
use crate::lexer::tokenize;
use crate::token::{Token, TokenKind};

pub fn parse_source(source: &str) -> Result<Module, Vec<Diagnostic>> {
    let tokens = tokenize(source);
    let mut parser = Parser::new(tokens);
    let module = parser.parse_module();
    if parser.diagnostics.is_empty() {
        Ok(module)
    } else {
        Err(parser.diagnostics)
    }
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    pub diagnostics: Vec<Diagnostic>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            diagnostics: Vec::new(),
        }
    }

    fn current(&self) -> &Token {
        &self.tokens[self.pos]
    }

    fn current_kind(&self) -> &TokenKind {
        &self.current().kind
    }

    fn is_at_end(&self) -> bool {
        matches!(self.current_kind(), TokenKind::Eof)
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.pos += 1;
        }
        &self.tokens[self.pos - 1]
    }

    fn error(&mut self, span: Span, msg: impl Into<String>) {
        self.diagnostics
            .push(Diagnostic::new_error(msg.into(), span));
    }

    fn expect_token(&mut self, expected: TokenKind, expected_name: &str) {
        let tok = self.current().clone();
        if tok.kind == expected {
            self.advance();
        } else {
            self.error_expected(tok.span, expected_name, &tok.kind);
        }
    }

    fn expect_ident(&mut self, what: &str) -> String {
        let tok = self.current().clone();
        match tok.kind {
            TokenKind::Ident(ref name) => {
                self.advance();
                name.clone()
            }
            _ => {
                let msg = format!(
                    "expected {} identifier but found {}",
                    what,
                    Self::pretty_token(&tok.kind)
                );
                self.diagnostics.push(Diagnostic::new_error(msg, tok.span));
                "_error_".to_string()
            }
        }
    }

    fn parse_module(&mut self) -> Module {
        let mut items = Vec::new();
        while !self.is_at_end() {
            match self.current_kind() {
                TokenKind::KwImport => {
                    items.push(Item::Import(self.parse_import()));
                }
                TokenKind::KwFn => {
                    items.push(Item::Function(self.parse_function()));
                }
                TokenKind::KwClass => {
                    items.push(Item::Class(self.parse_class()));
                }
                TokenKind::KwEnum => {
                    items.push(Item::Enum(self.parse_enum()));
                }
                TokenKind::Eof => break,
                _ => {
                    let span = self.current().span;

                    self.error(span, "unexpected token at top level");
                    self.advance();
                }
            }
        }
        Module { items }
    }

    fn parse_import(&mut self) -> Import {
        let _kw = self.advance();
        let path = self.parse_path();
        self.expect_token(TokenKind::Semicolon, "';'");
        Import { path }
    }

    fn parse_path(&mut self) -> Path {
        let mut segments = Vec::new();

        let first_tok = self.current().clone();
        let first = self.expect_ident("path");
        segments.push(first);

        let start = first_tok.span.start;
        let mut end = first_tok.span.end;

        while matches!(self.current_kind(), TokenKind::Dot) {
            self.advance();

            let seg_tok = self.current().clone();
            let seg = self.expect_ident("path");
            end = seg_tok.span.end;
            segments.push(seg);
        }

        Path {
            segments,
            span: Span { start, end },
        }
    }

    fn parse_type_params(&mut self) -> Vec<String> {
        let mut params = Vec::new();
        if !matches!(self.current_kind(), TokenKind::LBracket) {
            return params;
        }
        self.advance();
        if !matches!(self.current_kind(), TokenKind::RBracket) {
            loop {
                let name = self.expect_ident("type parameter");
                params.push(name);
                if matches!(self.current_kind(), TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect_token(TokenKind::RBracket, "']'");
        params
    }

    fn parse_function(&mut self) -> Function {
        self.advance();

        let name_tok = self.current().clone();
        let name = self.expect_ident("function");
        let name_span = name_tok.span;

        let type_params = self.parse_type_params();

        self.expect_token(TokenKind::LParen, "'('");
        let mut params = Vec::new();
        if !matches!(self.current_kind(), TokenKind::RParen) {
            loop {
                let param_tok = self.current().clone();
                let param_name = self.expect_ident("parameter");
                let param_span = param_tok.span;

                if matches!(self.current_kind(), TokenKind::Colon) {
                    self.advance();
                }
                let param_ty = self.parse_type();

                params.push(Param {
                    name: param_name,
                    name_span: param_span,
                    ty: param_ty,
                });

                if matches!(self.current_kind(), TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect_token(TokenKind::RParen, "')'");

        self.expect_token(TokenKind::Arrow, "'->'");
        let return_type = self.parse_type();

        let body = self.parse_block();
        Function {
            name,
            name_span,
            type_params,
            params,
            return_type,
            body,
        }
    }

    fn parse_class(&mut self) -> Class {
        self.advance();

        let name_tok = self.current().clone();
        let name = self.expect_ident("class");
        let name_span = name_tok.span;

        let type_params = self.parse_type_params();
        self.expect_token(TokenKind::LBrace, "'{'");

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        while !matches!(self.current_kind(), TokenKind::RBrace | TokenKind::Eof) {
            match self.current_kind() {
                TokenKind::KwFn => {
                    methods.push(self.parse_method());
                }
                TokenKind::Ident(_) => {
                    fields.push(self.parse_field());
                }
                _ => {
                    let span = self.current().span;
                    self.error(span, "unexpected token in class body");
                    self.advance();
                }
            }
        }

        self.expect_token(TokenKind::RBrace, "'}'");

        Class {
            name,
            name_span,
            type_params,
            fields,
            methods,
        }
    }

    fn parse_field(&mut self) -> Field {
        let name_tok = self.current().clone();
        let name = self.expect_ident("field");
        let name_span = name_tok.span;

        if matches!(self.current_kind(), TokenKind::Colon) {
            self.advance();
        }
        let ty = self.parse_type();

        self.expect_token(TokenKind::Semicolon, "';'");
        Field {
            name,
            name_span,
            ty,
        }
    }

    fn parse_method(&mut self) -> Method {
        self.advance();

        let name_tok = self.current().clone();
        let name = self.expect_ident("method");
        let name_span = name_tok.span;

        let type_params = self.parse_type_params();
        self.expect_token(TokenKind::LParen, "'('");

        let mut params = Vec::new();
        if !matches!(self.current_kind(), TokenKind::RParen) {
            loop {
                let param_tok = self.current().clone();
                let param_name = self.expect_ident("parameter");
                let param_span = param_tok.span;

                if matches!(self.current_kind(), TokenKind::Colon) {
                    self.advance();
                }
                let param_ty = self.parse_type();

                params.push(Param {
                    name: param_name,
                    name_span: param_span,
                    ty: param_ty,
                });

                if matches!(self.current_kind(), TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect_token(TokenKind::RParen, "')'");

        self.expect_token(TokenKind::Arrow, "'->'");
        let return_type = self.parse_type();

        let body = self.parse_block();
        Method {
            name,
            name_span,
            type_params,
            params,
            return_type,
            body,
        }
    }

    fn parse_enum(&mut self) -> EnumDef {
        self.advance();

        let name_tok = self.current().clone();
        let name = self.expect_ident("enum");
        let name_span = name_tok.span;

        let type_params = self.parse_type_params();
        self.expect_token(TokenKind::LBrace, "'{'");

        let mut variants = Vec::new();
        while !matches!(self.current_kind(), TokenKind::RBrace | TokenKind::Eof) {
            let variant_tok = self.current().clone();
            let variant_name = self.expect_ident("enum variant");
            let variant_span = variant_tok.span;

            let mut fields = Vec::new();
            if matches!(self.current_kind(), TokenKind::LParen) {
                self.advance();
                if !matches!(self.current_kind(), TokenKind::RParen) {
                    loop {
                        let ty = self.parse_type();
                        fields.push(ty);
                        if matches!(self.current_kind(), TokenKind::Comma) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                self.expect_token(TokenKind::RParen, "')'");
            }

            variants.push(EnumVariant {
                name: variant_name,
                name_span: variant_span,
                fields,
            });

            if matches!(self.current_kind(), TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.expect_token(TokenKind::RBrace, "'}'");

        EnumDef {
            name,
            name_span,
            type_params,
            variants,
        }
    }

    fn parse_type(&mut self) -> Type {
        match self.current_kind() {
            TokenKind::LBracket => {
                self.advance();
                self.expect_token(TokenKind::RBracket, "']'");
                let elem = self.parse_type();
                Type::Array(Box::new(elem))
            }
            TokenKind::KwInt => {
                self.advance();
                Type::Int
            }
            TokenKind::KwBool => {
                self.advance();
                Type::Bool
            }
            TokenKind::KwVoid => {
                self.advance();
                Type::Void
            }
            TokenKind::KwChar => {
                self.advance();
                Type::Char
            }
            TokenKind::KwByte => {
                self.advance();
                Type::Byte
            }
            TokenKind::KwString => {
                self.advance();
                Type::String
            }
            TokenKind::Ident(_) => {
                let path = self.parse_path();
                let mut type_args = Vec::new();
                if matches!(self.current_kind(), TokenKind::LBracket) {
                    self.advance();
                    if !matches!(self.current_kind(), TokenKind::RBracket) {
                        loop {
                            let ty = self.parse_type();
                            type_args.push(ty);
                            if matches!(self.current_kind(), TokenKind::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect_token(TokenKind::RBracket, "']'");
                }
                Type::Named {
                    name: path,
                    type_args,
                }
            }
            _ => {
                let tok = self.current().clone();
                self.error_expected(tok.span, "type", &tok.kind);
                self.advance();
                Type::Int
            }
        }
    }

    fn parse_block(&mut self) -> Block {
        self.expect_token(TokenKind::LBrace, "'{'");
        let mut stmts = Vec::new();
        while !matches!(self.current_kind(), TokenKind::RBrace | TokenKind::Eof) {
            let stmt = self.parse_stmt();
            stmts.push(stmt);
        }
        self.expect_token(TokenKind::RBrace, "'}'");
        Block { stmts }
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.current_kind() {
            TokenKind::KwLet => self.parse_let_stmt(),
            TokenKind::KwIf => self.parse_if_stmt(),
            TokenKind::KwWhile => self.parse_while_stmt(),
            TokenKind::KwFor => self.parse_for_stmt(),
            TokenKind::KwSwitch => self.parse_switch_stmt(),
            TokenKind::KwReturn => self.parse_return_stmt(),
            TokenKind::KwBreak => {
                self.advance();
                self.expect_token(TokenKind::Semicolon, "';'");
                Stmt::Break
            }
            TokenKind::KwContinue => {
                self.advance();
                self.expect_token(TokenKind::Semicolon, "';'");
                Stmt::Continue
            }
            TokenKind::LBrace => Stmt::Block(self.parse_block()),
            _ => {
                let expr = self.parse_expr();
                self.expect_token(TokenKind::Semicolon, "';'");
                Stmt::Expr(expr)
            }
        }
    }

    fn parse_assignment(&mut self, allow_struct_literal: bool) -> Expr {
        let expr = self.parse_logical_or(allow_struct_literal);
        if matches!(self.current_kind(), TokenKind::Eq) {
            let _eq_tok = self.advance().clone();
            let value = self.parse_assignment(allow_struct_literal);
            Expr::Assign {
                target: Box::new(expr),
                value: Box::new(value),
            }
        } else {
            expr
        }
    }

    fn parse_let_stmt(&mut self) -> Stmt {
        self.advance();
        let name = self.expect_ident("variable");

        let mut ty = None;

        if matches!(self.current_kind(), TokenKind::Colon) {
            self.advance();
            ty = Some(self.parse_type());
        } else if !matches!(self.current_kind(), TokenKind::Eq | TokenKind::Semicolon) {
            ty = Some(self.parse_type());
        }

        let mut init = None;
        if matches!(self.current_kind(), TokenKind::Eq) {
            self.advance();
            init = Some(self.parse_expr());
        }

        self.expect_token(TokenKind::Semicolon, "';'");
        Stmt::Let { name, ty, init }
    }

    fn parse_if_stmt(&mut self) -> Stmt {
        self.advance();
        let cond = self.parse_expr_no_struct_literal();
        let then_branch = self.parse_block();
        let mut else_branch = None;
        if matches!(self.current_kind(), TokenKind::KwElse) {
            self.advance();
            if matches!(self.current_kind(), TokenKind::KwIf) {
                let nested_if = self.parse_if_stmt();
                else_branch = Some(Block {
                    stmts: vec![nested_if],
                });
            } else {
                let block = self.parse_block();
                else_branch = Some(block);
            }
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        }
    }

    fn parse_while_stmt(&mut self) -> Stmt {
        self.advance();
        let cond = self.parse_expr_no_struct_literal();
        let body = self.parse_block();
        Stmt::While { cond, body }
    }

    fn parse_for_simple_init(&mut self) -> Option<Box<Stmt>> {
        if matches!(self.current_kind(), TokenKind::Semicolon) {
            self.advance();
            return None;
        }

        let stmt = if matches!(self.current_kind(), TokenKind::KwLet) {
            self.parse_let_stmt()
        } else {
            let expr = self.parse_expr();
            self.expect_token(TokenKind::Semicolon, "';'");
            Stmt::Expr(expr)
        };

        Some(Box::new(stmt))
    }

    fn parse_for_stmt(&mut self) -> Stmt {
        self.advance();
        self.expect_token(TokenKind::LParen, "'('");
        let init = self.parse_for_simple_init();

        let cond = if matches!(self.current_kind(), TokenKind::Semicolon) {
            self.advance();
            None
        } else {
            let c = self.parse_expr();
            self.expect_token(TokenKind::Semicolon, "';'");
            Some(c)
        };

        let step = if matches!(self.current_kind(), TokenKind::RParen) {
            self.advance();
            None
        } else {
            let e = self.parse_expr();
            self.expect_token(TokenKind::RParen, "')'");
            Some(e)
        };

        let body = self.parse_block();
        Stmt::For {
            init,
            cond,
            step,
            body,
        }
    }

    fn parse_switch_stmt(&mut self) -> Stmt {
        self.advance();

        let expr = self.parse_expr_no_struct_literal();
        self.expect_token(TokenKind::LBrace, "'{'");

        let mut cases = Vec::new();
        let mut default_block = None;

        while !matches!(self.current_kind(), TokenKind::RBrace | TokenKind::Eof) {
            match self.current_kind() {
                TokenKind::KwCase => {
                    self.advance();

                    let pat = self.parse_expr_no_struct_literal();

                    if matches!(self.current_kind(), TokenKind::Colon) {
                        self.advance();
                    }

                    let body = self.parse_block();
                    cases.push(SwitchCase { pattern: pat, body });
                }
                TokenKind::KwDefault => {
                    self.advance();

                    if matches!(self.current_kind(), TokenKind::Colon) {
                        self.advance();
                    }

                    let body = self.parse_block();
                    default_block = Some(body);
                }
                _ => {
                    let span = self.current().span;
                    self.error(span, "expected 'case' or 'default' in switch");
                    self.advance();
                }
            }
        }

        self.expect_token(TokenKind::RBrace, "'}'");
        Stmt::Switch {
            expr,
            cases,
            default: default_block,
        }
    }

    fn parse_return_stmt(&mut self) -> Stmt {
        self.advance();
        if matches!(self.current_kind(), TokenKind::Semicolon) {
            self.advance();
            Stmt::Return(None)
        } else {
            let expr = self.parse_expr();
            self.expect_token(TokenKind::Semicolon, "';'");
            Stmt::Return(Some(expr))
        }
    }

    fn pretty_token(kind: &TokenKind) -> String {
        match kind {
            TokenKind::Ident(name) => format!("identifier `{}`", name),
            TokenKind::IntLiteral(v) => format!("integer literal `{}`", v),
            TokenKind::StringLiteral(s) => format!("string literal {:?}", s),
            TokenKind::CharLiteral(c) => format!("character literal {:?}", c),

            TokenKind::True => "`true` keyword".to_string(),
            TokenKind::False => "`false` keyword".to_string(),

            TokenKind::LParen => "`(`".to_string(),
            TokenKind::RParen => "`)`".to_string(),
            TokenKind::LBrace => "`{`".to_string(),
            TokenKind::RBrace => "`}`".to_string(),
            TokenKind::LBracket => "`[`".to_string(),
            TokenKind::RBracket => "`]`".to_string(),
            TokenKind::Comma => "`,`".to_string(),
            TokenKind::Dot => "`.`".to_string(),
            TokenKind::Colon => "`:`".to_string(),
            TokenKind::Semicolon => "`;`".to_string(),
            TokenKind::Arrow => "`->`".to_string(),
            TokenKind::Eq => "`=`".to_string(),
            TokenKind::EqEq => "`==`".to_string(),
            TokenKind::Bang => "`!`".to_string(),
            TokenKind::BangEq => "`!=`".to_string(),
            TokenKind::Plus => "`+`".to_string(),
            TokenKind::Minus => "`-`".to_string(),
            TokenKind::Star => "`*`".to_string(),
            TokenKind::Slash => "`/`".to_string(),
            TokenKind::Percent => "`%`".to_string(),
            TokenKind::Lt => "`<`".to_string(),
            TokenKind::Gt => "`>`".to_string(),
            TokenKind::LtEq => "`<=`".to_string(),
            TokenKind::GtEq => "`>=`".to_string(),
            TokenKind::AndAnd => "`&&`".to_string(),
            TokenKind::OrOr => "`||`".to_string(),
            TokenKind::Question => "`?`".to_string(),

            TokenKind::KwImport => "`import` keyword".to_string(),
            TokenKind::KwFn => "`fn` keyword".to_string(),
            TokenKind::KwClass => "`class` keyword".to_string(),
            TokenKind::KwEnum => "`enum` keyword".to_string(),
            TokenKind::KwLet => "`let` keyword".to_string(),
            TokenKind::KwIf => "`if` keyword".to_string(),
            TokenKind::KwElse => "`else` keyword".to_string(),
            TokenKind::KwWhile => "`while` keyword".to_string(),
            TokenKind::KwFor => "`for` keyword".to_string(),
            TokenKind::KwSwitch => "`switch` keyword".to_string(),
            TokenKind::KwCase => "`case` keyword".to_string(),
            TokenKind::KwDefault => "`default` keyword".to_string(),
            TokenKind::KwReturn => "`return` keyword".to_string(),
            TokenKind::KwBreak => "`break` keyword".to_string(),
            TokenKind::KwContinue => "`continue` keyword".to_string(),
            TokenKind::KwInt => "`int` type".to_string(),
            TokenKind::KwBool => "`bool` type".to_string(),
            TokenKind::KwVoid => "`void` type".to_string(),
            TokenKind::KwChar => "`char` type".to_string(),
            TokenKind::KwByte => "`byte` type".to_string(),
            TokenKind::KwString => "`string` type".to_string(),

            TokenKind::Eof => "end of file".to_string(),
        }
    }

    fn error_expected(&mut self, span: Span, expected: &str, found: &TokenKind) {
        let msg = format!(
            "expected {} but found {}",
            expected,
            Self::pretty_token(found)
        );
        self.diagnostics.push(Diagnostic::new_error(msg, span));
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_assignment(true)
    }

    fn parse_expr_no_struct_literal(&mut self) -> Expr {
        self.parse_assignment(false)
    }

    fn parse_logical_or(&mut self, allow_struct_literal: bool) -> Expr {
        let mut expr = self.parse_logical_and(allow_struct_literal);
        while matches!(self.current_kind(), TokenKind::OrOr) {
            self.advance();
            let right = self.parse_logical_and(allow_struct_literal);
            expr = Expr::Binary {
                op: BinaryOp::Or,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        expr
    }

    fn parse_logical_and(&mut self, allow_struct_literal: bool) -> Expr {
        let mut expr = self.parse_equality(allow_struct_literal);
        while matches!(self.current_kind(), TokenKind::AndAnd) {
            self.advance();
            let right = self.parse_equality(allow_struct_literal);
            expr = Expr::Binary {
                op: BinaryOp::And,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        expr
    }

    fn parse_equality(&mut self, allow_struct_literal: bool) -> Expr {
        let mut expr = self.parse_comparison(allow_struct_literal);
        loop {
            match self.current_kind() {
                TokenKind::EqEq => {
                    self.advance();
                    let right = self.parse_comparison(allow_struct_literal);
                    expr = Expr::Binary {
                        op: BinaryOp::Eq,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                TokenKind::BangEq => {
                    self.advance();
                    let right = self.parse_comparison(allow_struct_literal);
                    expr = Expr::Binary {
                        op: BinaryOp::NotEq,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_comparison(&mut self, allow_struct_literal: bool) -> Expr {
        let mut expr = self.parse_term(allow_struct_literal);
        loop {
            match self.current_kind() {
                TokenKind::Lt => {
                    self.advance();
                    let right = self.parse_term(allow_struct_literal);
                    expr = Expr::Binary {
                        op: BinaryOp::Lt,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                TokenKind::Gt => {
                    self.advance();
                    let right = self.parse_term(allow_struct_literal);
                    expr = Expr::Binary {
                        op: BinaryOp::Gt,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                TokenKind::LtEq => {
                    self.advance();
                    let right = self.parse_term(allow_struct_literal);
                    expr = Expr::Binary {
                        op: BinaryOp::LtEq,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                TokenKind::GtEq => {
                    self.advance();
                    let right = self.parse_term(allow_struct_literal);
                    expr = Expr::Binary {
                        op: BinaryOp::GtEq,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_term(&mut self, allow_struct_literal: bool) -> Expr {
        let mut expr = self.parse_factor(allow_struct_literal);
        loop {
            match self.current_kind() {
                TokenKind::Plus => {
                    self.advance();
                    let right = self.parse_factor(allow_struct_literal);
                    expr = Expr::Binary {
                        op: BinaryOp::Add,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                TokenKind::Minus => {
                    self.advance();
                    let right = self.parse_factor(allow_struct_literal);
                    expr = Expr::Binary {
                        op: BinaryOp::Sub,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_factor(&mut self, allow_struct_literal: bool) -> Expr {
        let mut expr = self.parse_unary(allow_struct_literal);
        loop {
            match self.current_kind() {
                TokenKind::Star => {
                    self.advance();
                    let right = self.parse_unary(allow_struct_literal);
                    expr = Expr::Binary {
                        op: BinaryOp::Mul,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                TokenKind::Slash => {
                    self.advance();
                    let right = self.parse_unary(allow_struct_literal);
                    expr = Expr::Binary {
                        op: BinaryOp::Div,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_unary(&mut self, allow_struct_literal: bool) -> Expr {
        match self.current_kind() {
            TokenKind::Bang => {
                self.advance();
                let expr = self.parse_unary(allow_struct_literal);
                Expr::Unary {
                    op: UnaryOp::Not,
                    expr: Box::new(expr),
                }
            }
            TokenKind::Minus => {
                self.advance();
                let expr = self.parse_unary(allow_struct_literal);
                Expr::Unary {
                    op: UnaryOp::Neg,
                    expr: Box::new(expr),
                }
            }
            _ => self.parse_postfix(allow_struct_literal),
        }
    }

    fn parse_postfix(&mut self, allow_struct_literal: bool) -> Expr {
        let mut expr = self.parse_primary();
        loop {
            match self.current_kind() {
                TokenKind::LParen => {
                    self.advance();
                    let mut args = Vec::new();
                    if !matches!(self.current_kind(), TokenKind::RParen) {
                        loop {
                            let arg = self.parse_assignment(allow_struct_literal);
                            args.push(arg);
                            if matches!(self.current_kind(), TokenKind::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect_token(TokenKind::RParen, "')'");
                    expr = Expr::Call {
                        callee: Box::new(expr),
                        args,
                    };
                }
                TokenKind::Dot => {
                    self.advance();
                    let field = self.expect_ident("field");
                    expr = Expr::Field {
                        target: Box::new(expr),
                        field,
                    };
                }
                TokenKind::LBracket => {
                    self.advance();
                    let index = self.parse_assignment(allow_struct_literal);
                    self.expect_token(TokenKind::RBracket, "']'");
                    expr = Expr::Index {
                        target: Box::new(expr),
                        index: Box::new(index),
                    };
                }
                TokenKind::Question => {
                    self.advance();
                    expr = Expr::Question(Box::new(expr));
                }
                TokenKind::LBrace => {
                    if allow_struct_literal {
                        if matches!(expr, Expr::Path(_)) {
                            let head = expr;
                            expr = self.parse_struct_literal_after_head(head);
                            continue;
                        }
                    }
                    break;
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_struct_literal_after_head(&mut self, head: Expr) -> Expr {
        let typ = match head {
            Expr::Path(path) => Type::Named {
                name: path,
                type_args: Vec::new(),
            },
            other => {
                let span = self.current().span;
                self.error(span, "struct literal must start with a type name");
                return other;
            }
        };

        self.expect_token(TokenKind::LBrace, "'{' for struct literal");

        let mut fields = Vec::new();
        if !matches!(self.current_kind(), TokenKind::RBrace) {
            loop {
                let field_name = self.expect_ident("field name in struct literal");
                self.expect_token(TokenKind::Colon, "':'");
                let value = self.parse_expr();
                fields.push((field_name, value));

                if matches!(self.current_kind(), TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        self.expect_token(TokenKind::RBrace, "'}'");

        Expr::StructLiteral { typ, fields }
    }

    fn parse_primary(&mut self) -> Expr {
        let tok = self.current().clone();
        match tok.kind {
            TokenKind::IntLiteral(v) => {
                self.advance();
                Expr::Literal(Literal::Int(v))
            }
            TokenKind::StringLiteral(ref s) => {
                self.advance();
                Expr::Literal(Literal::String(s.clone()))
            }
            TokenKind::CharLiteral(c) => {
                self.advance();
                Expr::Literal(Literal::Char(c))
            }
            TokenKind::True => {
                self.advance();
                Expr::Literal(Literal::Bool(true))
            }
            TokenKind::False => {
                self.advance();
                Expr::Literal(Literal::Bool(false))
            }
            TokenKind::Ident(_) => {
                let path = self.parse_path();
                Expr::Path(path)
            }
            TokenKind::LParen => {
                self.advance();
                let expr = self.parse_expr();
                self.expect_token(TokenKind::RParen, "')'");
                expr
            }
            TokenKind::LBracket => {
                let ty = self.parse_type();
                self.expect_token(TokenKind::LBrace, "'{' for array literal");

                let mut elements = Vec::new();
                if !matches!(self.current_kind(), TokenKind::RBrace) {
                    loop {
                        let el = self.parse_expr();
                        elements.push(el);
                        if matches!(self.current_kind(), TokenKind::Comma) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                self.expect_token(TokenKind::RBrace, "'}'");

                Expr::ArrayLiteral { ty, elements }
            }
            _ => {
                let tok = self.current().clone();
                self.error_expected(tok.span, "expression", &tok.kind);
                self.advance();
                Expr::Literal(Literal::Int(0))
            }
        }
    }
}
