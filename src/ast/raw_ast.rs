use crate::ast::*;

#[derive(Clone, Debug)]
pub struct Generics {
    pub params: Vec<Ident>,
}

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub origin: Origin,
    pub name: Ident,
    pub generics: Generics,
    pub arg: Type,
    pub ret: Type,
    pub body: Closure,
}

#[derive(Clone, Debug)]
pub struct TypeAliasDef {
    pub origin: Origin,
    pub name: Ident,
    pub generics: Generics,
    pub type_: Type,
}

#[derive(Clone, Debug)]
pub enum FieldData {
    Tuple(Vec<(Visibility, Type)>),
    Struct(Vec<(Visibility, Ident, Type)>),
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    pub origin: Origin,
    pub name: Ident,
    pub generics: Generics,
    pub variants: Vec<(Ident, FieldData)>,
}

#[derive(Clone, Debug)]
pub struct StructDef {
    pub origin: Origin,
    pub name: Ident,
    pub generics: Generics,
    pub members: FieldData,
}

#[derive(Clone, Debug)]
pub enum ModDef {
    File(Ident),
    Inline(Ident, Program),
}

#[derive(Clone, Debug)]
pub enum ImportItem {
    // A value, type, or module.
    Ident(Ident),
    Spec(Ident, ImportSpec),
}

#[derive(Clone, Debug)]
pub enum ImportSpec {
    Glob,
    Specific(Vec<ImportItem>),
}

#[derive(Clone, Debug)]
pub enum ItemKind {
    FuncDef(FuncDef),
    TypeAliasDef(TypeAliasDef),
    EnumDef(EnumDef),
    StructDef(StructDef),
    ModDef(ModDef),
    Import(IdentPath, ImportItem),
}

#[derive(Clone, Debug)]
pub struct Item {
    pub vis: Visibility,
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
pub struct ExprField {
    pub name: Ident,
    pub expr: Expr,
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
pub struct Closure(Pat, Box<Expr>);

#[derive(Clone, Debug)]
pub enum ExprKind {
    Var(IdentPath, Ident, Vec<Ident>),
    Lit(Lit),
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),

    Tuple(Vec<Expr>),
    Closure(Closure),

    // These could represent instantiation of a struct or an enum variant. Which is `foo::bar::X(0)`
    // or `foo::bar::X{ y: 0 }`? We cannot know for sure that `bar` is a module and not an enum
    // before semantic analysis.
    TupleStruct(IdentPath, Ident, Vec<Expr>),
    Struct(IdentPath, Ident, Vec<ExprField>),

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
    Assign(Ident, Option<Type>, Expr),
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

    Var(Ident),
    Lit(Lit),
    Tuple(Vec<Pat>),

    // These match tuple-like and struct-like enum variants respectively. They are named somewhat
    // confusingly in order to mirror the variants of `Expr`.
    TupleStruct(IdentPath, Ident, Vec<Pat>),
    Struct(IdentPath, Ident, Vec<PatField>),
}

#[derive(Clone, Debug)]
pub struct Pat {
    pub kind: PatKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Type {
    Named(IdentPath, Ident, Vec<Type>),
    Func(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
}

#[derive(Clone, Debug)]
pub struct Program {
    pub items: Vec<Item>,
}
