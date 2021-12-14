use crate::ast::CustomId;

#[derive(Clone, Debug)]
pub struct Builtins {
    pub id_bool: CustomId,
    pub id_i8: CustomId,
    pub id_i16: CustomId,
    pub id_i32: CustomId,
    pub id_i64: CustomId,
    pub id_isize: CustomId,
    pub id_u8: CustomId,
    pub id_u16: CustomId,
    pub id_u32: CustomId,
    pub id_u64: CustomId,
    pub id_usize: CustomId,
    pub id_f32: CustomId,
    pub id_f64: CustomId,
    pub id_char: CustomId,
    pub id_str: CustomId,
}

// Added to the list of types during type resolution
pub const BUILTIN_TYPES: &[(&str, &str)] = &[
    ("bool", "iris::runtime::bool_"),
    ("i8", "iris::runtime::i8_"),
    ("i16", "iris::runtime::i16_"),
    ("i32", "iris::runtime::i32_"),
    ("i64", "iris::runtime::i64_"),
    ("isize", "iris::runtime::isize_"),
    ("u8", "iris::runtime::u8_"),
    ("u16", "iris::runtime::u16_"),
    ("u32", "iris::runtime::i32_"),
    ("u64", "iris::runtime::u64_"),
    ("usize", "iris::runtime::usize_"),
    ("f32", "iris::runtime::f32_"),
    ("f64", "burn_toast::runtime::f64_"),
    ("char", "iris::runtime::char_"),
    ("str", "iris::runtime::str_"),
];
