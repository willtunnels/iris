use crate::ast::*;
use crate::ast::{raw_ast as raw, resolved_ast as res};
use crate::util::id_vec::IdVec;

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub mod_: ModId,
    pub origin: Origin,
    pub name: Ident,
    pub generics: res::Generics,
    pub arg: res::Type,
    pub ret: res::Type,
    pub body: Block,
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
    pub type_: res::Type,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Assign(LocalId, Expr),
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
    Any(res::Type),

    Var(res::Type, LocalId),
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
pub struct Program {
    pub mod_symbols: IdVec<ModId, res::ModSymbols>,
    pub funcs: IdVec<FuncId, FuncDef>,
    pub enums: IdVec<EnumId, res::EnumDef>,
    pub structs: IdVec<StructId, res::StructDef>,
}
