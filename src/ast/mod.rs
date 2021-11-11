use crate::util::id_type::id_type;

pub mod raw_ast;
pub mod resolved_ast;
pub mod typed_ast;

/// The order of passes is:
/// raw_ast -> resolved_ast -> typed_ast;

#[derive(Clone, Debug)]
pub struct Span(pub usize, pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Origin {
    External,
    Internal,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ident(pub String);

#[derive(Clone, Debug)]
pub struct IdentPath(pub Vec<Ident>);

// `LocalId`s are allocated per function, not per block.
id_type! { pub LocalId(u32); }

id_type! { pub FuncId(u32); }

id_type! { pub EnumId(u32); }

id_type! { pub VariantId(u32); }

id_type! { pub StructId(u32); }

id_type! { pub TypeParamId(u32); }

id_type! { pub ModId(u32); }

#[derive(Clone, Debug)]
pub enum CustomId {
    Enum(EnumId),
    Struct(StructId),
}

#[derive(Clone, Debug)]
pub enum TypeId {
    Custom(CustomId),
    TypeParam(TypeParamId),
}

const ARG_ID: LocalId = LocalId(0);
