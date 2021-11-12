use crate::ast::resolved_ast as res;
use crate::ast::*;
use crate::util::id_vec::IdVec;

#[derive(Clone, Debug)]
pub enum FuncVal {
    External,
    Internal(Lam),
}

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub name: Ident,
    pub generics: res::Generics,
    pub arg: res::Type,
    pub ret: res::Type,
    pub val: FuncVal,
}

#[derive(Clone, Debug)]
pub enum ItemKind {
    FuncDef(FuncDef),
    TypeDef(res::TypeDef),
}

#[derive(Clone, Debug)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub ret: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct Lam(Vec<Ident>, Box<Expr>);

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: res::ExprKind,
    pub type_: res::Type,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Assign(Ident, Expr),
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
    pub types: IdVec<CustomId, res::TypeDef>,
}
