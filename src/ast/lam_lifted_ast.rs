use crate::ast::*;
use crate::ast::{raw_ast as raw, resolved_ast as res};
use crate::util::id_vec::IdVec;

#[derive(Clone, Debug)]
pub enum FuncBody {
    External(IdentPath),
    Internal(Block),
}

#[derive(Clone, Copy, Debug)]
pub enum CallableId {
    Func(FuncId),
    Lam(LamId),
}

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub generics: res::Generics,
    pub args: IdVec<ArgId, res::Type>,
    pub ret: res::Type,
    pub body: FuncBody,
}

#[derive(Clone, Debug)]
pub struct LamDef {
    pub args: IdVec<ArgId, res::Type>,
    pub captures: IdVec<CaptureId, res::Type>,
    pub ret: res::Type,
    pub body: Expr,
}

#[derive(Clone, Debug)]
pub struct LamSymbols {
    pub args: IdVec<ArgId, Ident>,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub ret: Box<Expr>,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Lit(raw::Lit),
    Local(LocalId),
    Arg(ArgId),
    Capture(CaptureId),
    Func(FuncId),

    // In previous passes, the `Lam` expression declared a lambda. Now it instantiates a lambda.
    Lam(LamId, IdVec<CaptureId, Expr>),
    Tuple(Vec<Expr>),

    BinOp(raw::BinOpKind, Box<Expr>, Box<Expr>),
    App(Box<Expr>, Vec<Expr>),
    TupleField(Box<Expr>, u32),

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
    pub type_: res::Type,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Program {
    pub funcs: IdVec<FuncId, FuncDef>,
    pub func_symbols: IdVec<FuncId, res::FuncSymbols>,
    pub lams: IdVec<LamId, LamDef>,
    pub lam_symbols: IdVec<LamId, LamSymbols>,
    pub types: IdVec<CustomId, res::TypeDef>,
    pub type_symbols: IdVec<CustomId, res::TypeSymbols>,
}
