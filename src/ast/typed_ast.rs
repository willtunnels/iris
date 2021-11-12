use crate::ast::resolved_ast as res;
use crate::ast::*;
use crate::util::id_vec::IdVec;

#[derive(Clone, Debug)]
pub enum FuncBody {
    External,
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
pub struct Expr {
    pub kind: res::ExprKind,
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
