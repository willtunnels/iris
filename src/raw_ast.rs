#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct TypeName(pub String);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct CtorName(pub String);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct TypeParam(pub String);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct ValName(pub String);

#[derive(Debug, Clone)]
enum Item {
    TypeDef(TypeName, Vec<TypeParam>),
    ValDef(ValName, Type, Expr),
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Any,
    Var(ValName),
    Tuple(Vec<Pattern>),
    Ctor(CtorName, Option<Box<Pattern>>),
    ByteConst(u8),
    IntConst(i64),
    FloatConst(f64),
}

#[derive(Debug, Clone)]
pub enum Type {
    Var(TypeParam),
    App(TypeName, Vec<Type>),
    Tuple(Vec<Type>),
    // Multiple arguments are packaged as a tuple type.
    Func(Box<Type>, Box<Type>),
}

struct Program {
    items: Vec<Item>,
}
