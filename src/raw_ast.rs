use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Origin {
    External,
    Internal,
}

#[derive(Debug, Clone)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CtorName(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValName(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeParam(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeName(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModName(pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModPath(pub Vec<ModName>);

#[derive(Debug, Clone)]
pub enum ExportName {
    TypeName(TypeName),
    ValName(ValName),
}

#[derive(Debug, Clone)]
pub enum Item {
    FuncDef(
        Origin,
        Visibility,
        ValName,
        Vec<TypeParam>,
        Type,
        Type,
        Block,
    ),
    EnumDef(Origin, Visibility, TypeName, HashMap<CtorName, Type>),
    TypeDef(Origin, Visibility, TypeName, Type),
    StructDef(Origin, Visibility, TypeName, HashMap<ValName, Type>),
    ModDecl(Visibility, ModName),
    UseDecl(Visibility, ModPath, ExportName),
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOpKind {
    And,
    Or,
    Not,
    Plus,
    Minus,
    Times,
    Divide,
    Mod,
}

#[derive(Debug, Clone)]
pub enum Expr {
    // TODO: handle types plugged into functions explicitally, e.g. f::<i32>()
    Var(ValName),
    QualVar(ModPath, ValName),

    CharLit(char),
    BoolLit(bool),
    I8Lit(i8),
    I16Lit(i16),
    I32Lit(i32),
    I64Lit(i64),
    ISizeLit(isize),
    U8Lit(u8),
    U16Lit(u16),
    U32Lit(u32),
    U64Lit(u64),
    USizeLit(usize),
    F32Lit(f32),
    F64Lit(f64),
    StrLit(String),

    BinaryOp(BinaryOpKind, Box<Expr>, Box<Expr>),
    App(Box<Expr>, Vec<Expr>),
    Closure(Pattern, Box<Expr>),

    Match(Box<Expr>, Vec<(Pattern, Block)>),
    If(Box<Expr>, Block, Block),
    Block(Block),
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Any,
    Var(ValName),
    Tuple(Vec<Pattern>),
    Ctor(ModPath, CtorName, Option<Box<Pattern>>),

    CharConst(char),
    BoolConst(bool),
    I8Const(i8),
    I16Const(i16),
    I32Const(i32),
    I64Const(i64),
    ISizeConst(isize),
    U8Const(u8),
    U16Const(u16),
    U32Const(u32),
    U64Const(u64),
    USizeConst(usize),
    F32Const(f32),
    F64Const(f64),
    StrConst(String),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Assign(ValName, Option<Type>, Expr),
}

#[derive(Debug, Clone)]
pub struct Block {
    stmts: Vec<Stmt>,
    ret: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Nominal(ModPath, TypeName, Vec<Type>),
    Func(Vec<Type>),
    Tuple(Vec<Type>),
}

#[derive(Debug, Clone)]
pub struct Program {
    items: Vec<Item>,
}
