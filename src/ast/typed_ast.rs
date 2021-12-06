use crate::ast::*;
use crate::ast::{raw_ast as raw, resolved_ast as res};
use crate::util::id_vec::IdVec;

#[derive(Clone, Debug)]
pub enum FuncBody {
    External(IdentPath),
    Internal(Expr),
}

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub generics: res::Generics,
    pub args: IdVec<ArgId, res::Type>,
    pub ret: res::Type,
    pub body: FuncBody,
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
    Func(FuncId),

    // It's a little weird to leave identifiers mixed into the resolved ast, but it is easier to
    // leave them here until we do lambda lifting.
    Lam(Vec<LocalId>, Box<Expr>),
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
    pub types: IdVec<CustomId, res::TypeDef>,
    pub type_symbols: IdVec<CustomId, res::TypeSymbols>,
}
