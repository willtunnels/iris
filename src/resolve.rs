use crate::ast::resolved_ast::ModDeclLoc;
use crate::ast::{self, raw_ast as raw, resolved_ast as res, Visibility};
use crate::file_cache::{File, FileCache};
use crate::util::id_vec::IdVec;
use std::collections::HashMap;
use std::hash::Hash;
use std::path::Path;

pub fn resolve(files: &mut FileCache, main_file: impl AsRef<Path>) -> res::Program {
    resolve_mod_from_file(
        files,
        &mut GlobalContext::new(),
        main_file.as_ref(),
        &ModDeclLoc::Root,
    );
    todo!()
}

// Stand in so the parser can be developed independently from this part of the compiler
fn parse(_file: impl AsRef<Path>) -> raw::Program {
    todo!()
}

// We can get rid of this if the corresponding std api is stabilized:
// https://github.com/rust-lang/rust/issues/82766
fn try_insert<K, V>(map: &mut HashMap<K, V>, k: K, v: V) -> Result<(), ()>
where
    K: Eq + Hash,
{
    use std::collections::hash_map::Entry::*;
    match map.entry(k) {
        Occupied(_) => Err(()),
        Vacant(entry) => {
            entry.insert(v);
            Ok(())
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ItemId {
    Mod(ast::ModId),
    Func(ast::FuncId),
    Struct(ast::StructId),
    Enum(ast::EnumId),
}

#[derive(Clone, Debug)]
struct ModMap {
    items: HashMap<ast::Ident, (Visibility, ItemId)>,
    ctors: HashMap<(ast::EnumId, ast::Ident), ast::VariantId>,
}

impl ModMap {
    fn new() -> Self {
        Self {
            items: HashMap::new(),
            ctors: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
struct GlobalContext {
    mods: IdVec<ast::ModId, ModMap>,
    mod_symbols: IdVec<ast::ModId, res::ModSymbols>,
    // We need to reserve ids before we can actually fill their corresponding definitions. The
    // `Option`s are set to `None` until the module maps are fully populated with symbol names, then
    // we can finish resolution and fill in the definitions.
    funcs: IdVec<ast::FuncId, Option<res::FuncDef>>,
    enums: IdVec<ast::EnumId, Option<res::EnumDef>>,
    structs: IdVec<ast::StructId, Option<res::StructDef>>,
}

impl GlobalContext {
    fn new() -> Self {
        Self {
            mods: IdVec::new(),
            mod_symbols: IdVec::new(),
            funcs: IdVec::new(),
            enums: IdVec::new(),
            structs: IdVec::new(),
        }
    }
}

#[derive(Clone, Debug)]
struct ParamMap(HashMap<ast::Ident, ast::TypeParamId>);

fn span_file(file: &File) -> ast::Span {
    ast::Span(0, file.text().len())
}

fn resolve_mod_def(
    files: &mut FileCache,
    ctx: &mut GlobalContext,
    parent: ast::ModId,
    parent_file: &Path,
    mod_def: &raw::ModDef,
    mod_def_span: ast::Span,
) {
    match mod_def {
        raw::ModDef::File(ident) => {
            let mut file = parent_file.to_owned();
            file.push(&ident.0);
            let decl_loc = res::ModDeclLoc::ChildOf {
                parent,
                name: ident.clone(),
                // TODO: do not unwrap
                span: span_file(files.read(&file).unwrap()),
            };
            resolve_mod_from_file(files, ctx, &file, &decl_loc)
        }
        raw::ModDef::Inline(ident, prog) => {
            let decl_loc = res::ModDeclLoc::ChildOf {
                parent,
                name: ident.clone(),
                span: mod_def_span,
            };
            resolve_mod(files, ctx, parent_file, &decl_loc, prog)
        }
    }
}

fn resolve_mod_from_file(
    files: &mut FileCache,
    ctx: &mut GlobalContext,
    file: &Path,
    decl_loc: &res::ModDeclLoc,
) {
    // TODO: do not unwrap
    let prog = parse(files.read(file).unwrap().text());
    resolve_mod(files, ctx, file, decl_loc, &prog);
}

fn resolve_mod(
    files: &mut FileCache,
    ctx: &mut GlobalContext,
    file: &Path,
    decl_loc: &res::ModDeclLoc,
    prog: &raw::Program,
) {
    // The `ModMap` we push here is just a dummy value.
    let mod_id = ctx.mods.push(ModMap::new());

    {
        let mod_symbols_id = ctx.mod_symbols.push(res::ModSymbols {
            file: file.to_owned(),
            decl_loc: decl_loc.clone(),
        });
        debug_assert_eq!(mod_symbols_id, mod_id);
    }

    let mut mod_map = ModMap::new();
    for item in &prog.items {
        match &item.kind {
            raw::ItemKind::FuncDef(func) => try_insert(
                &mut mod_map.items,
                func.name.clone(),
                (item.vis, ItemId::Func(ctx.funcs.push(None))),
            ),
            raw::ItemKind::TypeAliasDef(alias) => todo!(),
            raw::ItemKind::EnumDef(enum_) => try_insert(
                &mut mod_map.items,
                enum_.name.clone(),
                (item.vis, ItemId::Enum(ctx.enums.push(None))),
            ),
            raw::ItemKind::StructDef(struct_) => try_insert(
                &mut mod_map.items,
                struct_.name.clone(),
                (item.vis, ItemId::Struct(ctx.structs.push(None))),
            ),
            raw::ItemKind::ModDef(mod_) => {
                let name = match mod_ {
                    raw::ModDef::File(name) => name,
                    raw::ModDef::Inline(name, _) => name,
                };
                try_insert(
                    &mut mod_map.items,
                    name.clone(),
                    (item.vis, ItemId::Mod(ctx.mod_symbols.push(todo!()))),
                )
            }
            raw::ItemKind::Import(path, item) => todo!(),
        }
        // TODO: report error
        .unwrap();
    }

    ctx.mods[mod_id] = mod_map;
}

fn build_param_map(generics: &raw::Generics) -> (res::Generics, ParamMap) {
    let mut param_map = HashMap::new();
    for (idx, ident) in generics.params.iter().enumerate() {
        // TODO: handle error
        try_insert(&mut param_map, ident.clone(), ast::TypeParamId(idx as u32)).unwrap()
    }

    let num_params = generics.params.len() as u32;
    (res::Generics { num_params }, ParamMap(param_map))
}

fn resolve_enum_def(
    global_mods: &IdVec<ast::ModId, ModMap>,
    mod_: ast::ModId,
    enum_def: &raw::EnumDef,
) -> res::EnumDef {
    let (generics, param_map) = build_param_map(&enum_def.generics);
    let variants = enum_def
        .variants
        .iter()
        .map(|(ident, field_data)| {
            (
                ident.clone(),
                resolve_field_data(global_mods, mod_, &param_map, field_data),
            )
        })
        .collect();

    res::EnumDef {
        mod_,
        origin: enum_def.origin,
        name: enum_def.name.clone(),
        generics,
        variants,
    }
}

fn resolve_struct_def(
    global_mods: &IdVec<ast::ModId, ModMap>,
    mod_: ast::ModId,
    struct_def: &raw::StructDef,
) -> res::StructDef {
    let (generics, param_map) = build_param_map(&struct_def.generics);
    let members = resolve_field_data(global_mods, mod_, &param_map, &struct_def.members);

    res::StructDef {
        mod_,
        origin: struct_def.origin,
        name: struct_def.name.clone(),
        generics,
        members,
    }
}

fn resolve_field_data(
    global_mods: &IdVec<ast::ModId, ModMap>,
    mod_: ast::ModId,
    type_params: &ParamMap,
    field_data: &raw::FieldData,
) -> res::FieldData {
    match field_data {
        raw::FieldData::Tuple(fields) => res::FieldData::Tuple(
            fields
                .iter()
                .map(|(vis, type_)| (*vis, resolve_type(global_mods, mod_, type_params, type_)))
                .collect(),
        ),
        raw::FieldData::Struct(fields) => res::FieldData::Struct(
            fields
                .iter()
                .map(|(vis, ident, type_)| {
                    (
                        *vis,
                        ident.clone(),
                        resolve_type(global_mods, mod_, type_params, type_),
                    )
                })
                .collect(),
        ),
    }
}

// Anything that can result from resolving an `IdentPath`.
enum QualId {
    Mod(ast::ModId),
    Func(ast::FuncId),
    Struct(ast::StructId),
    Enum(ast::EnumId),
    Variant(ast::VariantId),
}

impl From<ItemId> for QualId {
    fn from(id: ItemId) -> Self {
        match id {
            ItemId::Mod(id) => QualId::Mod(id),
            ItemId::Func(id) => QualId::Func(id),
            ItemId::Struct(id) => QualId::Struct(id),
            ItemId::Enum(id) => QualId::Enum(id),
        }
    }
}

// TODO: handle `crate::` properly
fn resolve_with_path(
    global_mods: &IdVec<ast::ModId, ModMap>,
    mod_: ast::ModId,
    path: &ast::IdentPath,
    ident: &ast::Ident,
) -> QualId {
    let item_from_mod = |search_mod, item_ident| {
        // TODO: handle error: No such item
        let (vis, item) = global_mods[search_mod].items.get(item_ident).unwrap();
        // TODO: handle error: Item is private
        assert!(*item == ItemId::Mod(mod_) || *vis == ast::Visibility::Public);
        *item
    };

    let mut prev = mod_;
    let mut curr = ItemId::Mod(mod_);
    for elem in &path.0 {
        let inner_item = match curr {
            ItemId::Mod(inner) => inner,
            // TODO: handle error: Item is not a module
            _ => panic!(),
        };
        prev = inner_item;
        curr = item_from_mod(inner_item, elem);
    }

    match curr {
        ItemId::Mod(inner) => item_from_mod(inner, ident).into(),
        // TODO: handle error: No such variant
        // TODO: get rid of `clone` on `ident`
        ItemId::Enum(inner) => QualId::Variant(
            *global_mods[prev]
                .ctors
                .get(&(inner, ident.clone()))
                .unwrap(),
        ),
        _ => panic!(),
    }
}

fn resolve_type(
    global_mods: &IdVec<ast::ModId, ModMap>,
    mod_: ast::ModId,
    type_params: &ParamMap,
    type_: &raw::Type,
) -> res::Type {
    match type_ {
        raw::Type::Named(path, ident, types) => {
            if path.0.len() == 0 {
                if let Some(param_id) = type_params.0.get(ident) {
                    // We don't support higher-kinded types, so type params can never be
                    // instantiated with other types. E.g. `fn foo<T>() { let x: T<u32> = T(0); }`
                    // could never be valid.
                    // TODO: handle error
                    assert!(types.len() == 0);
                    res::Type::Var(*param_id)
                } else {
                    todo!()
                }
            } else {
                todo!()
            }
        }
        raw::Type::Func(arg, ret) => res::Type::Func(
            Box::new(resolve_type(global_mods, mod_, type_params, arg)),
            Box::new(resolve_type(global_mods, mod_, type_params, ret)),
        ),
        raw::Type::Tuple(types) => res::Type::Tuple(
            types
                .iter()
                .map(|item_type| resolve_type(global_mods, mod_, type_params, item_type))
                .collect(),
        ),
    }
}

#[derive(Clone, Debug)]
struct LocalScope {
    names: Vec<ast::Ident>,
}

impl LocalScope {
    fn new() -> Self {
        Self { names: Vec::new() }
    }
}

#[derive(Clone, Debug)]
struct LocalContext {
    scopes: Vec<LocalScope>,
    locals: HashMap<ast::Ident, ast::LocalId>,
    next_id: ast::LocalId,
}

impl LocalContext {
    fn new() -> Self {
        LocalContext {
            scopes: vec![LocalScope::new()],
            locals: HashMap::new(),
            // `LocalId(0)` is reserved for the argument
            next_id: ast::LocalId(1),
        }
    }

    fn get(&self, name: &ast::Ident) -> Option<ast::LocalId> {
        self.locals.get(name).cloned()
    }

    fn insert(&mut self, name: ast::Ident) -> Result<ast::LocalId, ()> {
        let id = ast::LocalId(self.next_id.0);
        self.next_id.0 += 1;

        try_insert(&mut self.locals, name.clone(), id)?;
        self.scopes.last_mut().unwrap().names.push(name);

        Ok(id)
    }

    fn new_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.scopes.push(LocalScope::new());
        let ret = f(self);

        let scope = self.scopes.pop().unwrap();
        for name in &scope.names {
            self.locals.remove(name);
        }

        ret
    }
}

fn resolve_expr(locals: &mut LocalContext, expr: &raw::Expr) -> res::Expr {
    match &expr.kind {
        raw::ExprKind::Var(_, _, _) => todo!(),
        raw::ExprKind::Lit(lit) => res::Expr {
            kind: res::ExprKind::Lit(lit.clone()),
            span: expr.span.clone(),
        },
        raw::ExprKind::BinOp(kind, left, right) => res::Expr {
            kind: res::ExprKind::BinOp(
                *kind,
                Box::new(resolve_expr(locals, left)),
                Box::new(resolve_expr(locals, right)),
            ),
            span: expr.span.clone(),
        },
        raw::ExprKind::Tuple(elems) => res::Expr {
            kind: res::ExprKind::Tuple(
                elems
                    .iter()
                    .map(|elem| resolve_expr(locals, elem))
                    .collect(),
            ),
            span: expr.span.clone(),
        },
        raw::ExprKind::Closure(_) => todo!(),
        raw::ExprKind::TupleStruct(_, _, _) => todo!(),
        raw::ExprKind::Struct(_, _, _) => todo!(),
        raw::ExprKind::Field(obj_expr, field) => res::Expr {
            kind: res::ExprKind::Field(Box::new(resolve_expr(locals, obj_expr)), field.clone()),
            span: expr.span.clone(),
        },
        raw::ExprKind::UnnamedField(obj_expr, field) => res::Expr {
            kind: res::ExprKind::UnnamedField(Box::new(resolve_expr(locals, obj_expr)), *field),
            span: expr.span.clone(),
        },
        raw::ExprKind::App(_, _) => todo!(),
        raw::ExprKind::Match(matched_expr, arms) => res::Expr {
            kind: res::ExprKind::Match(
                Box::new(resolve_expr(locals, matched_expr)),
                arms.iter()
                    .map(|(pat, block)| (resolve_pat(locals, pat), resolve_block(locals, block)))
                    .collect(),
            ),
            span: expr.span.clone(),
        },
        raw::ExprKind::If(cond, true_br, false_br) => res::Expr {
            kind: res::ExprKind::If(
                Box::new(resolve_expr(locals, cond)),
                resolve_block(locals, true_br),
                resolve_block(locals, false_br),
            ),
            span: expr.span.clone(),
        },
        raw::ExprKind::Block(block) => res::Expr {
            kind: res::ExprKind::Block(resolve_block(locals, block)),
            span: expr.span.clone(),
        },
    }
}

fn resolve_block(locals: &mut LocalContext, block: &raw::Block) -> res::Block {
    locals.new_scope(|locals| res::Block {
        stmts: block
            .stmts
            .iter()
            .map(|stmt| resolve_stmt(locals, stmt))
            .collect(),
        ret: Box::new(resolve_expr(locals, &block.ret)),
    })
}

fn resolve_stmt(locals: &mut LocalContext, stmt: &raw::Stmt) -> res::Stmt {
    match &stmt.kind {
        raw::StmtKind::Assign(ident, type_, expr) => {
            // TODO: don't unwrap
            let id = locals.insert(ident.clone()).unwrap();
            let type_ = todo!();
            let expr = resolve_expr(locals, expr);
            res::Stmt {
                kind: res::StmtKind::Assign(id, type_, expr),
                span: stmt.span.clone(),
            }
        }
    }
}

fn resolve_pat(locals: &mut LocalContext, pat: &raw::Pat) -> res::Pat {
    todo!()
}
