use crate::ast::Span;

#[derive(Clone, Debug, thiserror::Error)]
#[error("{kind}")]
pub struct Locate<K> {
    pub kind: K,
    pub span: Option<Span>,
}

impl<K> From<K> for Locate<K> {
    fn from(kind: K) -> Self {
        Self { kind, span: None }
    }
}
