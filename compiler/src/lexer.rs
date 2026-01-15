use crate::diagnostics::Span;
use crate::token::{Token, TokenKind};

pub struct Lexer<'a> {
    input: &'a str,
    len: usize,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            len: input.len(),
            pos: 0,
        }
    }

    fn peek_char(&self) -> Option<char> {
        if self.pos < self.len {
            Some(self.input.as_bytes()[self.pos] as char)
        } else {
            None
        }
    }

    fn peek_next_char(&self) -> Option<char> {
        if self.pos + 1 < self.len {
            Some(self.input.as_bytes()[self.pos + 1] as char)
        } else {
            None
        }
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.peek_char()?;
        self.pos += 1;
        Some(c)
    }

    fn current_span(&self, start: usize) -> Span {
        Span {
            start,
            end: self.pos,
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek_char() {
                Some(c) if c.is_ascii_whitespace() => {
                    self.advance();
                }
                Some('/') if self.peek_next_char() == Some('/') => {
                    self.advance();
                    self.advance();
                    while let Some(c) = self.peek_char() {
                        if c == '\n' {
                            break;
                        }
                        self.advance();
                    }
                }
                _ => break,
            }
        }
    }

    fn lex_number(&mut self) -> Token {
        let start = self.pos;

        if self.peek_char() == Some('0') {
            if let Some('x') | Some('X') = self.peek_next_char() {
                self.advance();
                self.advance();
                let hex_start = self.pos;

                while let Some(c) = self.peek_char() {
                    if c.is_ascii_hexdigit() {
                        self.advance();
                    } else {
                        break;
                    }
                }

                let span = self.current_span(start);
                let text = &self.input[hex_start..self.pos];
                let value = i64::from_str_radix(text, 16).unwrap_or(0);
                return Token {
                    kind: TokenKind::IntLiteral(value),
                    span,
                };
            }
        }

        while let Some(c) = self.peek_char() {
            if c.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        let span = self.current_span(start);
        let text = &self.input[start..self.pos];
        let value = text.parse::<i64>().unwrap_or(0);

        Token {
            kind: TokenKind::IntLiteral(value),
            span,
        }
    }

    fn lex_ident_or_keyword(&mut self) -> Token {
        let start = self.pos;
        while let Some(c) = self.peek_char() {
            if c.is_ascii_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let span = self.current_span(start);
        let text = &self.input[start..self.pos];

        let kind = match text {
            "fn" => TokenKind::KwFn,
            "class" => TokenKind::KwClass,
            "enum" => TokenKind::KwEnum,
            "import" => TokenKind::KwImport,
            "let" => TokenKind::KwLet,
            "if" => TokenKind::KwIf,
            "else" => TokenKind::KwElse,
            "while" => TokenKind::KwWhile,
            "for" => TokenKind::KwFor,
            "switch" => TokenKind::KwSwitch,
            "case" => TokenKind::KwCase,
            "default" => TokenKind::KwDefault,
            "return" => TokenKind::KwReturn,
            "break" => TokenKind::KwBreak,
            "continue" => TokenKind::KwContinue,

            "int" => TokenKind::KwInt,
            "bool" => TokenKind::KwBool,
            "void" => TokenKind::KwVoid,
            "char" => TokenKind::KwChar,
            "byte" => TokenKind::KwByte,
            "string" => TokenKind::KwString,

            "true" => TokenKind::True,
            "false" => TokenKind::False,

            _ => TokenKind::Ident(text.to_string()),
        };

        Token { kind, span }
    }

    fn lex_string(&mut self) -> Token {
        let start = self.pos;
        self.advance();
        let content_start = self.pos;

        while let Some(c) = self.peek_char() {
            if c == '"' {
                break;
            }
            self.advance();
        }

        let content_end = self.pos;
        let _ = self.advance();

        let span = Span {
            start,
            end: self.pos,
        };

        let text = &self.input[content_start..content_end];
        Token {
            kind: TokenKind::StringLiteral(text.to_string()),
            span,
        }
    }

    fn lex_char(&mut self) -> Token {
        let start = self.pos;
        self.advance(); // opening '

        let ch = match self.peek_char() {
            Some(c) => {
                self.advance();
                c
            }
            None => '\0',
        };

        // consume closing '
        if self.peek_char() == Some('\'') {
            self.advance();
        }

        let span = Span {
            start,
            end: self.pos,
        };

        Token {
            kind: TokenKind::CharLiteral(ch),
            span,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();

        let start = self.pos;
        let c = match self.peek_char() {
            Some(c) => c,
            None => {
                return Token {
                    kind: TokenKind::Eof,
                    span: Span {
                        start: self.pos,
                        end: self.pos,
                    },
                }
            }
        };

        match c {
            '0'..='9' => self.lex_number(),
            'a'..='z' | 'A'..='Z' | '_' => self.lex_ident_or_keyword(),
            '"' => self.lex_string(),
            '\'' => self.lex_char(),

            '(' => {
                self.advance();
                Token {
                    kind: TokenKind::LParen,
                    span: self.current_span(start),
                }
            }
            ')' => {
                self.advance();
                Token {
                    kind: TokenKind::RParen,
                    span: self.current_span(start),
                }
            }
            '{' => {
                self.advance();
                Token {
                    kind: TokenKind::LBrace,
                    span: self.current_span(start),
                }
            }
            '}' => {
                self.advance();
                Token {
                    kind: TokenKind::RBrace,
                    span: self.current_span(start),
                }
            }
            '[' => {
                self.advance();
                Token {
                    kind: TokenKind::LBracket,
                    span: self.current_span(start),
                }
            }
            ']' => {
                self.advance();
                Token {
                    kind: TokenKind::RBracket,
                    span: self.current_span(start),
                }
            }
            ';' => {
                self.advance();
                Token {
                    kind: TokenKind::Semicolon,
                    span: self.current_span(start),
                }
            }
            ':' => {
                self.advance();
                Token {
                    kind: TokenKind::Colon,
                    span: self.current_span(start),
                }
            }
            ',' => {
                self.advance();
                Token {
                    kind: TokenKind::Comma,
                    span: self.current_span(start),
                }
            }
            '.' => {
                self.advance();
                Token {
                    kind: TokenKind::Dot,
                    span: self.current_span(start),
                }
            }
            '+' => {
                self.advance();
                Token {
                    kind: TokenKind::Plus,
                    span: self.current_span(start),
                }
            }
            '-' => {
                if self.peek_next_char() == Some('>') {
                    self.advance();
                    self.advance();
                    Token {
                        kind: TokenKind::Arrow,
                        span: self.current_span(start),
                    }
                } else {
                    self.advance();
                    Token {
                        kind: TokenKind::Minus,
                        span: self.current_span(start),
                    }
                }
            }
            '*' => {
                self.advance();
                Token {
                    kind: TokenKind::Star,
                    span: self.current_span(start),
                }
            }
            '/' => {
                self.advance();
                Token {
                    kind: TokenKind::Slash,
                    span: self.current_span(start),
                }
            }
            '%' => {
                self.advance();
                Token {
                    kind: TokenKind::Percent,
                    span: self.current_span(start),
                }
            }
            '=' => {
                if self.peek_next_char() == Some('=') {
                    self.advance();
                    self.advance();
                    Token {
                        kind: TokenKind::EqEq,
                        span: self.current_span(start),
                    }
                } else {
                    self.advance();
                    Token {
                        kind: TokenKind::Eq,
                        span: self.current_span(start),
                    }
                }
            }
            '!' => {
                if self.peek_next_char() == Some('=') {
                    self.advance();
                    self.advance();
                    Token {
                        kind: TokenKind::BangEq,
                        span: self.current_span(start),
                    }
                } else {
                    self.advance();
                    Token {
                        kind: TokenKind::Bang,
                        span: self.current_span(start),
                    }
                }
            }
            '<' => {
                if self.peek_next_char() == Some('=') {
                    self.advance();
                    self.advance();
                    Token {
                        kind: TokenKind::LtEq,
                        span: self.current_span(start),
                    }
                } else {
                    self.advance();
                    Token {
                        kind: TokenKind::Lt,
                        span: self.current_span(start),
                    }
                }
            }
            '>' => {
                if self.peek_next_char() == Some('=') {
                    self.advance();
                    self.advance();
                    Token {
                        kind: TokenKind::GtEq,
                        span: self.current_span(start),
                    }
                } else {
                    self.advance();
                    Token {
                        kind: TokenKind::Gt,
                        span: self.current_span(start),
                    }
                }
            }
            '&' => {
                if self.peek_next_char() == Some('&') {
                    self.advance();
                    self.advance();
                    Token {
                        kind: TokenKind::AndAnd,
                        span: self.current_span(start),
                    }
                } else {
                    self.advance();
                    Token {
                        kind: TokenKind::AndAnd,
                        span: self.current_span(start),
                    }
                }
            }
            '|' => {
                if self.peek_next_char() == Some('|') {
                    self.advance();
                    self.advance();
                    Token {
                        kind: TokenKind::OrOr,
                        span: self.current_span(start),
                    }
                } else {
                    self.advance();
                    Token {
                        kind: TokenKind::OrOr,
                        span: self.current_span(start),
                    }
                }
            }
            '?' => {
                self.advance();
                Token {
                    kind: TokenKind::Question,
                    span: self.current_span(start),
                }
            }
            _ => {
                // unknown character, advance
                self.advance();
                Token {
                    kind: TokenKind::Eof,
                    span: self.current_span(start),
                }
            }
        }
    }
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    loop {
        let tok = lexer.next_token();
        let is_eof = matches!(tok.kind, TokenKind::Eof);
        tokens.push(tok);
        if is_eof {
            break;
        }
    }
    tokens
}
