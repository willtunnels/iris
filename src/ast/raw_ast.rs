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
pub struct FuncSig {
    pub name: Ident,
    pub generics: Generics,
    pub args: Vec<(Ident, Type)>,
    pub ret: Type,
}

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub sig: FuncSig,
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

#[derive(Clone, Copy, Debug)]
pub enum UOpKind {
    Not,
}

#[derive(Clone, Copy, Debug)]
pub enum UTerm {
    Lit(Lit),
    Quantity(Box<Expr>),
    Var(Ident),
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp6Kind {
    Dot,
}

#[derive(Clone, Copy, Debug)]
pub enum BinTerm6 {
    UOp(UOpKind, UTerm),
    UTerm(UTerm),
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp5Kind {
    Times,
    Divide,
    Mod,
}

#[derive(Clone, Copy, Debug)]
pub enum BinTerm5 {
    BinOp6(BinOp6Kind, Box<BinTerm5>, BinTerm6),
    BinTerm6(BinTerm6),
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp4Kind {
    Plus,
    Minus,
}

#[derive(Clone, Copy, Debug)]
pub enum BinTerm4 {
    BinOp5(BinOp5Kind, Box<BinTerm4>, BinTerm5),
    BinTerm5(BinTerm5),
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp3Kind {
    Equality,
    Inequality,
    LessThan,
    GreaterThan,
    Leq,
    Geq,
}

#[derive(Clone, Copy, Debug)]
pub enum BinTerm3 {
    BinOp4(BinOp4Kind, Box<BinTerm3>, BinTerm4),
    BinTerm4(BinTerm4),
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp2Kind {
    And,
}

#[derive(Clone, Copy, Debug)]
pub enum BinTerm2 {
    BinOp3(BinOp3Kind, Box<BinTerm2>, BinTerm3),
    BinTerm3(BinTerm3),
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp1Kind {
    Or,
}

#[derive(Clone, Copy, Debug)]
pub enum BinTerm1 {
    BinOp2(BinOp2Kind, Box<BinTerm1>, BinTerm2),
    BinTerm2(BinTerm2),
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Lam(Vec<Ident>, Box<Expr>),
    Tuple(Vec<Expr>),
    App(Box<Expr>, Vec<Expr>),
    TupleField(Box<Expr>, u32),
    If(Box<Expr>, Block, Block),
    Block(Block),

    UnaryOp(UOpKind, UTerm),
    UTerm(UTerm),
    BinOp1(BinOp1Kind, Box<Expr>, BinTerm1),
    BinTerm1(BinTerm1),
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
    Func(Vec<Type>, Box<Type>), // args_list, return type
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
