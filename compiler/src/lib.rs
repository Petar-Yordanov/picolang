pub mod ast;
pub mod diagnostics;
pub mod ir;
pub mod lexer;
pub mod llvm_codegen;
pub mod module_loader;
pub mod parser;
pub mod symbols;
pub mod token;

pub use crate::ast::*;
pub use crate::diagnostics::{Diagnostic, Severity, Span};
pub use crate::parser::parse_source;
