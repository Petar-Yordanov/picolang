#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum Severity {
    Error,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    pub span: Span,
    pub severity: Severity,
}

impl Diagnostic {
    pub fn new_error(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
            severity: Severity::Error,
        }
    }
}
