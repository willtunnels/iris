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
pub const BUILTIN_TYPES: &[(&str, &[&str])] = &[
    ("bool", &["crate", "runtime", "burnt_toast", "bool_"]),
    ("i8", &["crate", "runtime", "burnt_toast", "i8_"]),
    ("i16", &["crate", "runtime", "burnt_toast", "i16_"]),
    ("i32", &["crate", "runtime", "burnt_toast", "i32_"]),
    ("i64", &["crate", "runtime", "burnt_toast", "i64_"]),
    ("isize", &["crate", "runtime", "burnt_toast", "isize_"]),
    ("u8", &["crate", "runtime", "burnt_toast", "u8_"]),
    ("u16", &["crate", "runtime", "burnt_toast", "u16_"]),
    ("u32", &["crate", "runtime", "burnt_toast", "u32_"]),
    ("u64", &["crate", "runtime", "burnt_toast", "u64_"]),
    ("usize", &["crate", "runtime", "burnt_toast", "usize_"]),
    ("f32", &["crate", "runtime", "burnt_toast", "f32_"]),
    ("f64", &["crate", "runtime", "burnt_toast", "f64_"]),
    ("char", &["crate", "runtime", "burnt_toast", "char_"]),
    ("str", &["crate", "runtime", "burnt_toast", "str_"]),
];
