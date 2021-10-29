use crate::util::map::Map;

#[derive(Clone, Copy, Debug)]
pub enum Origin {
    External,
    Internal,
}

#[derive(Clone, Copy, Debug)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CtorName(pub String);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ValName(pub String);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeName(pub String);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ModName(pub String);

#[derive(Clone, Debug)]
pub struct ModPath(pub Vec<ModName>);

#[derive(Clone, Debug)]
pub enum ExportName {
    TypeName(TypeName),
    ValName(ValName),
}

#[derive(Clone, Debug)]
pub enum Item {
    FuncDef {
        orig: Origin,
        vis: Visibility,
        name: ValName,
        type_params: Vec<TypeName>,
        arg: Type,
        ret: Type,
        body: Block,
    },

    EnumDef {
        orig: Origin,
        vis: Visibility,
        name: TypeName,
        ctors: Map<CtorName, Type>,
    },

    TypeDef(Origin, Visibility, TypeName, Type),
    StructDef(Origin, Visibility, TypeName, Map<ValName, Type>),
    ModDecl(Visibility, ModName),
    UseDecl(Visibility, ModPath, ExportName),

    Span(usize, usize, Box<Item>),
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
pub enum Expr {
    // Function values can have their generic types explicitally plugged in, e.g. `f::<i32>(x)`.
    // Hence, a `Var` and a `QualVar` need a `Vec<TypeName>`.
    Var(ValName, Vec<TypeName>),
    QualVar(ModPath, ValName, Vec<TypeName>),

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

    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    App(Box<Expr>, Vec<Expr>),
    Closure(Pattern, Box<Expr>),
    Access(Box<Expr>, ValName),

    Match(Box<Expr>, Vec<(Pattern, Block)>),
    If(Box<Expr>, Block, Block),
    Block(Block),

    Span(usize, usize, Box<Pattern>),
}

#[derive(Clone, Debug)]
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

    Span(usize, usize, Box<Pattern>),
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Assign(ValName, Option<Type>, Expr),
}

#[derive(Clone, Debug)]
pub struct Block {
    stmts: Vec<Stmt>,
    ret: Box<Expr>,
}

#[derive(Clone, Debug)]
pub enum Type {
    Nominal(ModPath, TypeName, Vec<Type>),
    Func(Vec<Type>),
    Tuple(Vec<Type>),
}

#[derive(Clone, Debug)]
pub struct Program {
    items: Vec<Item>,
}
