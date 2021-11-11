use crate::ast::Span;
use crate::file_cache::FileCache;
use std::fmt;
use std::path::PathBuf;

pub enum DiagnosticKind {
    DuplicateName,
}

impl fmt::Display for DiagnosticKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DuplicateName => write!(f, "duplicate name"),
        }
    }
}

pub struct Diagnostic {
    file: PathBuf,
    span: Span,
    kind: DiagnosticKind,
}

pub struct DiagnosticEngine {
    err: Diagnostic,
    help: Vec<Diagnostic>,
}

impl DiagnosticEngine {
    pub fn new(err: Diagnostic) -> Self {
        DiagnosticEngine {
            err,
            help: Vec::new(),
        }
    }

    pub fn help(&mut self, help: Diagnostic) -> &mut Self {
        self.help.push(help);
        self
    }

    pub fn emit(&self, files: &mut FileCache) {
        let file = files.read(&self.err.file).unwrap();
        todo!();
    }
}

fn digits(mut n: usize) -> usize {
    let mut digits = 1;
    while n > 0 {
        digits += 1;
        n /= 10;
    }
    digits
}
