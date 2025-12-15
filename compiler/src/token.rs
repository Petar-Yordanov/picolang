use crate::diagnostics::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    KwFn,
    KwClass,
    KwEnum,
    KwImport,
    KwLet,
    KwIf,
    KwElse,
    KwWhile,
    KwFor,
    KwSwitch,
    KwCase,
    KwDefault,
    KwReturn,
    KwBreak,
    KwContinue,

    // Builtin types
    KwInt,
    KwBool,
    KwVoid,
    KwChar,
    KwByte,
    KwString,

    // Literals
    True,
    False,

    // Identifiers and literals
    Ident(String),
    IntLiteral(i64),
    StringLiteral(String),
    CharLiteral(char),

    // Punctuation / operators
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Semicolon,
    Colon,
    Comma,
    Dot,
    Arrow,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Eq,
    EqEq,
    Bang,
    BangEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    AndAnd,
    OrOr,
    Question, // ?

    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}
