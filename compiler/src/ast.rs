use crate::diagnostics::Span;

#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Item {
    Import(Import),
    Function(Function),
    Class(Class),
    Enum(EnumDef),
}

#[derive(Debug, Clone)]
pub struct Import {
    pub path: Path,
}

#[derive(Debug, Clone)]
pub struct Path {
    pub segments: Vec<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub type_params: Vec<String>,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub name_span: Span,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub name_span: Span,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub type_params: Vec<String>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
    pub name_span: Span,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub name_span: Span,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Method {
    pub name: String,
    pub type_params: Vec<String>,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Block,
    pub name_span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub name: String,
    pub type_params: Vec<String>,
    pub variants: Vec<EnumVariant>,
    pub name_span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub name_span: Span,
    pub fields: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        name: String,
        ty: Option<Type>,
        init: Option<Expr>,
    },
    Expr(Expr),
    Return(Option<Expr>),
    If {
        cond: Expr,
        then_branch: Block,
        else_branch: Option<Block>,
    },
    While {
        cond: Expr,
        body: Block,
    },
    For {
        init: Option<Box<Stmt>>,
        cond: Option<Expr>,
        step: Option<Expr>,
        body: Block,
    },
    Switch {
        expr: Expr,
        cases: Vec<SwitchCase>,
        default: Option<Block>,
    },
    Block(Block),
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub pattern: Expr,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Bool,
    Void,
    Char,
    Byte,
    String,
    Array(Box<Type>),
    Named { name: Path, type_args: Vec<Type> },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Path(Path),
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Index {
        target: Box<Expr>,
        index: Box<Expr>,
    },
    Field {
        target: Box<Expr>,
        field: String,
    },
    Question(Box<Expr>),
    StructLiteral {
        typ: Type,
        fields: Vec<(String, Expr)>,
    },
    ArrayLiteral {
        ty: Type,
        elements: Vec<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    String(String),
    Char(char),
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,
    Not,
}
