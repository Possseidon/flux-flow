use std::ops::Range;

pub struct ResultWithDiagnostics<'code, T> {
    pub code: &'code str,
    pub value: Option<T>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub error: anyhow::Error,
    pub range: Range<usize>,
}

#[derive(Debug)]
pub enum Severity {
    Error,
    Warning,
    Info,
}

impl<'code, T> ResultWithDiagnostics<'code, T> {
    pub fn new(code: &'code str, value: Option<T>, diagnostics: Vec<Diagnostic>) -> Self {
        Self {
            code,
            value,
            diagnostics,
        }
    }

    pub fn ok(code: &'code str, value: T) -> Self {
        Self {
            code,
            value: Some(value),
            diagnostics: Default::default(),
        }
    }

    pub fn ok_with_diagnostics(code: &'code str, value: T, diagnostics: Vec<Diagnostic>) -> Self {
        Self {
            code,
            value: Some(value),
            diagnostics,
        }
    }

    pub fn error(code: &'code str, diagnostics: Vec<Diagnostic>) -> Self {
        Self {
            code,
            value: Default::default(),
            diagnostics,
        }
    }

    pub fn unknown_error(code: &'code str) -> Self {
        Self {
            code,
            value: Default::default(),
            diagnostics: Default::default(),
        }
    }
}

pub trait IntoDiagnosticRange {
    fn into_range(self) -> Range<usize>;
}

impl IntoDiagnosticRange for usize {
    fn into_range(self) -> Range<usize> {
        self..self
    }
}

impl IntoDiagnosticRange for Range<usize> {
    fn into_range(self) -> Range<usize> {
        self
    }
}

impl Diagnostic {
    pub fn error(error: impl Into<anyhow::Error>, range: impl IntoDiagnosticRange) -> Self {
        Self {
            severity: Severity::Error,
            error: error.into(),
            range: range.into_range(),
        }
    }

    pub fn warning(error: impl Into<anyhow::Error>, range: impl IntoDiagnosticRange) -> Self {
        Self {
            severity: Severity::Warning,
            error: error.into(),
            range: range.into_range(),
        }
    }

    pub fn info(error: impl Into<anyhow::Error>, range: impl IntoDiagnosticRange) -> Self {
        Self {
            severity: Severity::Info,
            error: error.into(),
            range: range.into_range(),
        }
    }
}
