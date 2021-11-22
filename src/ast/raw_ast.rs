use crate::ast::*;

#[derive(Clone, Debug)]
pub struct Generics {
    pub params: Vec<Ident>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum FuncBody {
    External,
    Internal(Expr),
}

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub name: Ident,
    pub generics: Generics,
    pub args: Vec<(Type, Ident)>,
    pub ret: Type,
    pub body: FuncBody,
}

// This binds a name in the source language to a Rust struct implementing `IType` with fully
// qualified name `path`. E.g.
// ```
// extern type Foo = crate::foo::Foo;
// ```
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

#[derive(Clone, Copy, Debug)]
pub enum BinOpKind {
    And,
    Or,
    Not,
    Plus,
    Minus,
    Times,
    Divide,
    Mod,
}

#[derive(Clone, Debug)]
pub enum Lit {
    Char(char),
    Bool(bool),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    ISize(isize),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    USize(usize),
    F32(f32),
    F64(f64),
    Str(String),
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub ret: Box<Expr>,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Lit(Lit),
    Var(Ident),

    Lam(Vec<Ident>, Box<Expr>),
    Tuple(Vec<Expr>),

    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
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
pub enum TypeKind {
    // A type, potentially parameterized by a list of type arguments. E.g. `Foo<i32, i32>` or the
    // name of one of the type parameters to the current function (in the latter case, later passes
    // will ensure no type arguments were supplied).
    Named(Ident, Vec<Type>),
    Func(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
}

#[derive(Clone, Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Program {
    pub items: Vec<Item>,
}
