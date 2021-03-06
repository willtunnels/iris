use crate::ast::raw_ast as raw;
use crate::ast::*;
use crate::builtins::Builtins;
use crate::util::id_vec::IdVec;

#[derive(Clone, Debug)]
pub struct Generics {
    pub num_params: u32,
}

#[derive(Clone, Debug)]
pub struct GenericSymbols {
    pub params: IdVec<TypeParamId, Ident>,
}

#[derive(Clone, Debug)]
pub enum FuncBody {
    External(IdentPath),
    Internal(Block),
}

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub generics: Generics,
    pub args: IdVec<ArgId, Type>,
    pub ret: Type,
    pub body: FuncBody,
}

#[derive(Clone, Debug)]
pub struct FuncSymbols {
    pub name: Ident,
    pub generics: GenericSymbols,
    pub args: IdVec<ArgId, Ident>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TypeDef {
    pub generics: Generics,
    pub path: IdentPath,
}

#[derive(Clone, Debug)]
pub struct TypeSymbols {
    pub name: Ident,
    pub generics: GenericSymbols,
    pub span: Span,
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

    Lam(Vec<LocalId>, Box<Expr>),
    Tuple(Vec<Expr>),

    UnOp(raw::UnOpKind, Box<Expr>),
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
    Assign(LocalId, Option<Type>, Expr),
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
    Func(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
}

#[derive(Clone, Debug)]
pub struct Program {
    pub funcs: IdVec<FuncId, FuncDef>,
    pub func_symbols: IdVec<FuncId, FuncSymbols>,
    pub types: IdVec<CustomId, TypeDef>,
    pub type_symbols: IdVec<CustomId, TypeSymbols>,
    pub builtins: Builtins,
}
