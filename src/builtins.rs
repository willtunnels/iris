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
    ("bool", "burnt_toast::runtime::bool_"),
    ("i8", "burnt_toast::runtime::i8_"),
    ("i16", "burnt_toast::runtime::i16_"),
    ("i32", "burnt_toast::runtime::i32_"),
    ("i64", "burnt_toast::runtime::i64_"),
    ("isize", "burnt_toast::runtime::isize_"),
    ("u8", "burnt_toast::runtime::u8_"),
    ("u16", "burnt_toast::runtime::u16_"),
    ("u32", "burnt_toast::runtime::i32_"),
    ("u64", "burnt_toast::runtime::u64_"),
    ("usize", "burnt_toast::runtime::usize_"),
    ("f32", "burnt_toast::runtime::f32_"),
    ("f64", "burn_toast::runtime::f64_"),
    ("char", "burnt_toast::runtime::char_"),
    ("str", "burnt_toast::runtime::str_"),
];
