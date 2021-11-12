use crate::util::id_type::id_type;

pub mod lam_lifted_ast;
pub mod raw_ast;
pub mod resolved_ast;
pub mod typed_ast;

/// The order of passes is:
/// raw_ast ->
/// resolved_ast ->
/// typed_ast ->
/// lam_lifted_ast

#[derive(Clone, Debug)]
pub struct Span(pub usize, pub usize);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ident(pub String);

#[derive(Clone, Debug)]
pub struct IdentPath(pub Vec<Ident>);

// `LocalId`s are allocated per function, not per block.
id_type! { pub LocalId(u32); }

id_type! { pub ArgId(u32); }

id_type! { pub FuncId(u32); }

id_type! { pub LamId(u32); }

id_type! { pub CaptureId(u32); }

id_type! { pub TypeParamId(u32); }

id_type! { pub CustomId(u32); }
