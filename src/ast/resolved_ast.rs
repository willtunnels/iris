use crate::ast::raw_ast as raw;
use crate::ast::*;
use crate::util::id_vec::IdVec;

#[derive(Clone, Debug)]
pub struct Generics {
    pub num_params: u32,
}

#[derive(Clone, Debug)]
pub enum FuncVal {
    External,
    Internal(Lam),
}

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub name: Ident,
    pub generics: Generics,
    pub arg: Type,
    pub ret: Type,
    pub val: FuncVal,
}

#[derive(Clone, Debug)]
pub struct TypeDef {
    pub name: Ident,
    pub generics: Generics,
    pub path: IdentPath,
}

#[derive(Clone, Debug)]
pub enum ItemKind {
    FuncDef(FuncDef),
    TypeDef(TypeDef),
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
pub enum ExprKind {
    Lit(raw::Lit),
    Local(LocalId),
    Func(FuncId),

    Lam(Lam),
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
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Assign(Ident, Option<Type>, Expr),
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Type {
    Var(TypeParamId),
    Custom(CustomId, Vec<Type>),
    Func(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
}

#[derive(Clone, Debug)]
pub struct Program {
    pub funcs: IdVec<FuncId, FuncDef>,
    pub types: IdVec<CustomId, TypeDef>,
}
