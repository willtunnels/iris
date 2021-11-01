use crate::ast::raw_ast as raw;
use crate::ast::*;
use crate::util::id_vec::IdVec;
use std::path::PathBuf;

#[derive(Clone, Debug)]
pub enum ModDeclLoc {
    Root,
    ChildOf {
        parent: ModId,
        name: Ident,
        span: Span,
    },
}

#[derive(Clone, Debug)]
pub struct ModSymbols {
    pub file: PathBuf,
    pub decl_loc: ModDeclLoc,
}

#[derive(Clone, Debug)]
pub struct Generics {
    num_params: u32,
}

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub mod_: ModId,
    pub origin: Origin,
    pub name: Ident,
    pub generics: Generics,
    pub arg: Type,
    pub ret: Type,
    pub body: Block,
}

#[derive(Clone, Debug)]
pub enum FieldData {
    Tuple(Vec<(Visibility, Type)>),
    Struct(Vec<(Visibility, Ident, Type)>),
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    pub mod_: ModId,
    pub origin: Origin,
    pub name: Ident,
    pub generics: Generics,
    pub variants: Vec<(Ident, FieldData)>,
}

#[derive(Clone, Debug)]
pub struct StructDef {
    pub mod_: ModId,
    pub origin: Origin,
    pub name: Ident,
    pub generics: Generics,
    pub members: FieldData,
}

#[derive(Clone, Debug)]
pub struct ExprField {
    pub name: Ident,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub ret: Box<Expr>,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    // These variants are both derived from `raw_ast::ExprKind::Var`.
    Local(LocalId),
    Func(FuncId),

    Lit(raw::Lit),
    BinOp(raw::BinOpKind, Box<Expr>, Box<Expr>),

    Tuple(Vec<Expr>),
    Closure(Pat, Box<Expr>),

    EnumTuple(EnumId, VariantId, Vec<Expr>),
    EnumStruct(EnumId, VariantId, Vec<ExprField>),
    Struct(EnumId, VariantId, Vec<ExprField>),

    Field(Box<Expr>, Ident),
    UnnamedField(Box<Expr>, u32),
    App(Box<Expr>, Vec<Expr>),

    Match(Box<Expr>, Vec<(Pat, Block)>),
    If(Box<Expr>, Block, Block),
    Block(Block),
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Assign(LocalId, Option<Type>, Expr),
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct PatField {
    pub name: Ident,
    pub pat: Pat,
}

#[derive(Clone, Debug)]
pub enum PatKind {
    // The "_" symbol, which matches and ignores anything.
    Any,

    Var(LocalId),
    Lit(raw::Lit),
    Tuple(Vec<Pat>),

    EnumTuple(EnumId, VariantId, Vec<Pat>),
    EnumStruct(EnumId, VariantId, Vec<PatField>),
    Struct(StructId, Vec<PatField>),
}

#[derive(Clone, Debug)]
pub struct Pat {
    pub kind: PatKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Type {
    Var(TypeId),
    Func(Vec<Type>),
    Tuple(Vec<Type>),

    // A "custom" type is just a user defined type, like a struct or enum.
    Custom(CustomId, Vec<Type>),
}

#[derive(Clone, Debug)]
pub struct Program {
    pub mod_symbols: IdVec<ModId, ModSymbols>,
    pub funcs: IdVec<FuncId, FuncDef>,
    pub enums: IdVec<EnumId, EnumDef>,
    pub structs: IdVec<StructId, StructDef>,
}
