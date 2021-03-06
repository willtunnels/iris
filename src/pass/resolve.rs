use crate::ast::{self, raw_ast as raw, resolved_ast as res};
use crate::builtins::{Builtins, BUILTIN_TYPES};
use crate::file_cache::FileCache;
use crate::parse;
use crate::report_error::Locate;
use crate::util::id_vec::IdVec;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::path::{Path, PathBuf};
use std::result::Result as StdResult;

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("{0}")]
    // XXX: LALRPOP errors have a lifetime. We convert to a string to avoid handling that.
    CannotParse(String),
    #[error("cannot read file \"{0}\"; \"{1}\"")]
    CannotReadFile(PathBuf, #[source] std::io::Error),
    #[error("no type named \"{0}\" in scope")]
    TypeNotFound(ast::Ident),
    #[error("no variable named \"{0}\" in scope")]
    VarNotFound(ast::Ident),
    #[error("expected {0} type params, found {0}")]
    WrongNumTypeParams(u32, u32),
    #[error("duplicate func name \"{0}\"")]
    DuplacateFuncName(ast::Ident),
    #[error("duplicate type name \"{0}\"")]
    DuplacateTypeName(ast::Ident),
    #[error("duplicate type param name \"{0}\"")]
    DuplacateTypeParamName(ast::Ident),
    #[error("duplicate argument name \"{0}\"")]
    DuplacateArgName(ast::Ident),
}

pub type Error = Locate<ErrorKind>;

pub type Result<T> = StdResult<T, Error>;

pub fn resolve(files: &mut FileCache, file_path: impl AsRef<Path>) -> Result<res::Program> {
    let file = files
        .read(file_path.as_ref())
        .map_err(|err| ErrorKind::CannotReadFile(file_path.as_ref().to_owned(), err))?;

    resolve_prog(parse::parse_prog(file.text()).map_err(|err| Error {
        kind: ErrorKind::CannotParse(format!("{}", err)),
        span: extract_span(&err),
    })?)
}

fn extract_span(err: &parse::ParseError) -> Option<ast::Span> {
    use lalrpop_util::ParseError::*;
    match err {
        InvalidToken { location } => Some(ast::Span(*location, *location)),
        UnrecognizedEOF { location, .. } => Some(ast::Span(*location, *location)),
        UnrecognizedToken { token, .. } => Some(ast::Span(token.0, token.2)),
        ExtraToken { token } => Some(ast::Span(token.0, token.2)),
        User { .. } => None,
    }
}

// We can get rid of this if the corresponding std api is stabilized:
// https://github.com/rust-lang/rust/issues/82766
fn try_insert<K, V>(map: &mut HashMap<K, V>, k: K, v: V) -> StdResult<(), ()>
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

#[derive(Clone, Debug)]
struct ModMap {
    funcs: HashMap<ast::Ident, ast::FuncId>,
    types: HashMap<ast::Ident, ast::CustomId>,
}

impl ModMap {
    fn new() -> Self {
        Self {
            funcs: HashMap::new(),
            types: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
struct GlobalContext {
    mod_: ModMap,
    funcs: IdVec<ast::FuncId, Option<res::FuncDef>>,
    func_symbols: IdVec<ast::FuncId, res::FuncSymbols>,
    types: IdVec<ast::CustomId, Option<res::TypeDef>>,
    type_symbols: IdVec<ast::CustomId, res::TypeSymbols>,
}

impl GlobalContext {
    fn new() -> Self {
        Self {
            mod_: ModMap::new(),
            funcs: IdVec::new(),
            func_symbols: IdVec::new(),
            types: IdVec::new(),
            type_symbols: IdVec::new(),
        }
    }
}

#[derive(Clone, Debug)]
struct ParamMap(HashMap<ast::Ident, ast::TypeParamId>);

fn gen_builtin_def(name: &str, path: &str) -> raw::Item {
    let def = raw::TypeDef {
        name: ast::Ident(name.into()),
        generics: raw::Generics {
            params: Vec::new(),
            // TODO: use a better span
            span: ast::Span(0, 0),
        },
        path: ast::IdentPath(
            path.split("::")
                .map(|ident| ast::Ident(ident.to_owned()))
                .collect(),
        ),
    };
    raw::Item {
        kind: raw::ItemKind::TypeDef(def),
        // TODO: use a better span
        span: ast::Span(0, 0),
    }
}

fn resolve_prog(mut prog: raw::Program) -> Result<res::Program> {
    let mut ctx = GlobalContext::new();

    // All builtins should be placed at the beginning of `items` so that duplicate type errors point
    // to user defined types, rather than builtin types
    let mut items = BUILTIN_TYPES
        .iter()
        .map(|(name, path)| gen_builtin_def(name, path))
        .collect::<Vec<_>>();
    // TODO: for a large program this would be quite expensive
    items.append(&mut prog.items);
    prog.items = items;

    // Fill in `ctx.mod_`, `ctx.func_symbols`, and `ctx.type_symbols`. Identifiers are reserved in
    // `ctx.funcs` and `ctx.types`, but all values are set to `None`. To resolve `FuncDef`s or
    // `TypeDef`s we need to register all function and type names up front, since defintions are not
    // ordered top-to-bottom.
    for item in &prog.items {
        match &item.kind {
            raw::ItemKind::FuncDef(def) => {
                let id = ctx.funcs.push(None);
                try_insert(&mut ctx.mod_.funcs, def.name.clone(), id).map_err(|()| Locate {
                    kind: ErrorKind::DuplacateFuncName(def.name.clone()),
                    span: Some(item.span),
                })?;

                {
                    let symbols_id = ctx.func_symbols.push(build_func_symbols(def, item.span));
                    debug_assert_eq!(symbols_id, id);
                }
            }
            raw::ItemKind::TypeDef(def) => {
                let id = ctx.types.push(None);
                try_insert(&mut ctx.mod_.types, def.name.clone(), id).map_err(|()| Locate {
                    kind: ErrorKind::DuplacateTypeName(def.name.clone()),
                    span: Some(item.span),
                })?;

                {
                    let symbols_id = ctx.type_symbols.push(build_type_symbols(def, item.span));
                    debug_assert_eq!(symbols_id, id);
                }
            }
        }
    }

    // Now we can actually resolve `FuncDef`s and `TypeDef`s.
    for item in &prog.items {
        match &item.kind {
            raw::ItemKind::FuncDef(def) => {
                let id = ctx.mod_.funcs[&def.name];
                debug_assert!(ctx.funcs[id].is_none());
                ctx.funcs[id] = Some(resolve_func_def(&ctx, def, item.span)?);
            }
            raw::ItemKind::TypeDef(def) => {
                let id = ctx.mod_.types[&def.name];
                debug_assert!(ctx.types[id].is_none());
                ctx.types[id] = Some(resolve_type_def(def));
            }
        }
    }

    let check_id = |id, name| {
        assert!(ctx.type_symbols[ast::CustomId(id)].name.0 == name);
        ast::CustomId(id)
    };
    // Builtin types are inserted in this order at the beginning of `items`
    let builtins = Builtins {
        id_bool: check_id(0, "bool"),
        id_i8: check_id(1, "i8"),
        id_i16: check_id(2, "i16"),
        id_i32: check_id(3, "i32"),
        id_i64: check_id(4, "i64"),
        id_isize: check_id(5, "isize"),
        id_u8: check_id(6, "u8"),
        id_u16: check_id(7, "u16"),
        id_u32: check_id(8, "u32"),
        id_u64: check_id(9, "u64"),
        id_usize: check_id(10, "usize"),
        id_f32: check_id(11, "f32"),
        id_f64: check_id(12, "f64"),
        id_char: check_id(13, "char"),
        id_str: check_id(14, "str"),
    };

    Ok(res::Program {
        funcs: ctx.funcs.into_mapped(|_id, def| def.unwrap()),
        func_symbols: ctx.func_symbols,
        types: ctx.types.into_mapped(|_id, def| def.unwrap()),
        type_symbols: ctx.type_symbols,
        builtins,
    })
}

fn build_func_symbols(def: &raw::FuncDef, span: ast::Span) -> res::FuncSymbols {
    let generics = res::GenericSymbols {
        params: IdVec::from_items(def.generics.params.clone()),
    };
    let args = IdVec::from_items(def.args.iter().map(|(ident, _)| ident.clone()).collect());
    res::FuncSymbols {
        name: def.name.clone(),
        generics,
        args,
        span,
    }
}

fn build_type_symbols(def: &raw::TypeDef, span: ast::Span) -> res::TypeSymbols {
    let generics = res::GenericSymbols {
        params: IdVec::from_items(def.generics.params.clone()),
    };
    res::TypeSymbols {
        name: def.name.clone(),
        generics,
        span,
    }
}

fn resolve_func_def(
    ctx: &GlobalContext,
    def: &raw::FuncDef,
    span: ast::Span,
) -> Result<res::FuncDef> {
    let generics = res::Generics {
        num_params: def.generics.params.len() as u32,
    };
    let type_params = build_param_map(&def.generics)?;
    let args = IdVec::from_items(
        def.args
            .iter()
            .map(|(_, type_)| resolve_type(ctx, &type_params, type_))
            .collect::<Result<_>>()?,
    );
    let ret = resolve_type(ctx, &type_params, &def.ret)?;
    let body = match &def.body {
        raw::FuncBody::External(path) => res::FuncBody::External(path.clone()),
        raw::FuncBody::Internal(expr) => {
            let mut locals =
                LocalContext::with_args(def.args.iter().map(|(ident, _)| ident), span)?;
            res::FuncBody::Internal(resolve_block(ctx, &type_params, &mut locals, expr)?)
        }
    };
    Ok(res::FuncDef {
        generics,
        args,
        ret,
        body,
    })
}

fn resolve_type_def(def: &raw::TypeDef) -> res::TypeDef {
    let generics = res::Generics {
        num_params: def.generics.params.len() as u32,
    };
    res::TypeDef {
        generics,
        path: def.path.clone(),
    }
}

fn build_param_map(generics: &raw::Generics) -> Result<ParamMap> {
    let mut param_map = HashMap::new();
    for (i, ident) in generics.params.iter().enumerate() {
        let make_err = |()| Locate {
            kind: ErrorKind::DuplacateTypeParamName(ident.clone()),
            span: Some(generics.span),
        };
        try_insert(&mut param_map, ident.clone(), ast::TypeParamId(i as u32)).map_err(make_err)?;
    }
    Ok(ParamMap(param_map))
}

fn resolve_type(
    ctx: &GlobalContext,
    type_params: &ParamMap,
    type_: &raw::Type,
) -> Result<res::Type> {
    Ok(match &type_.kind {
        raw::TypeKind::Named(ident, type_args) => {
            if let Some(id) = type_params.0.get(ident) {
                if type_args.len() != 0 {
                    return Err(Locate {
                        kind: ErrorKind::WrongNumTypeParams(0, type_args.len() as u32),
                        span: Some(type_.span),
                    });
                }
                res::Type::Var(*id)
            } else {
                let id = ctx.mod_.types.get(ident).cloned().ok_or_else(|| Locate {
                    kind: ErrorKind::TypeNotFound(ident.clone()),
                    span: Some(type_.span),
                })?;
                let type_args = type_args
                    .iter()
                    .map(|type_arg| resolve_type(ctx, type_params, type_arg))
                    .collect::<Result<_>>()?;
                res::Type::Custom(id, type_args)
            }
        }
        raw::TypeKind::Func(args, ret) => res::Type::Func(
            args.iter()
                .map(|arg| resolve_type(ctx, type_params, arg))
                .collect::<Result<_>>()?,
            Box::new(resolve_type(ctx, type_params, ret)?),
        ),
        raw::TypeKind::Tuple(elems) => res::Type::Tuple(
            elems
                .iter()
                .map(|elem| resolve_type(ctx, type_params, elem))
                .collect::<Result<_>>()?,
        ),
    })
}

#[derive(Clone, Debug)]
struct LocalScope {
    // Variables introduced in the inner scope. These must be removed from the bindings map at the
    // end of the scope.
    new: HashSet<ast::Ident>,
    // Variables in the outer scope which have been shadowed in the inner scope. These must be
    // restored in the bindings map at the end of the scope.
    shadowed: Vec<(ast::Ident, VarId)>,
}

impl LocalScope {
    fn new() -> Self {
        Self {
            new: HashSet::new(),
            shadowed: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
enum VarId {
    Arg(ast::ArgId),
    Local(ast::LocalId),
}

#[derive(Clone, Debug)]
struct LocalContext {
    scopes: Vec<LocalScope>,
    vars: HashMap<ast::Ident, VarId>,
    next_local_id: ast::LocalId,
}

impl LocalContext {
    fn with_args<'a>(args: impl Iterator<Item = &'a ast::Ident>, span: ast::Span) -> Result<Self> {
        let mut locals = LocalContext {
            scopes: vec![LocalScope::new()],
            vars: HashMap::new(),
            next_local_id: ast::LocalId(0),
        };

        for (i, ident) in args.enumerate() {
            let make_err = |()| Locate {
                kind: ErrorKind::DuplacateArgName(ident.clone()),
                span: Some(span),
            };
            let id = VarId::Arg(ast::ArgId(i as u32));
            try_insert(&mut locals.vars, ident.clone(), id).map_err(make_err)?;
        }

        Ok(locals)
    }

    fn get(&self, name: &ast::Ident) -> Option<VarId> {
        self.vars.get(name).cloned()
    }

    fn insert(&mut self, name: ast::Ident) -> ast::LocalId {
        let id = ast::LocalId(self.next_local_id.0);
        self.next_local_id.0 += 1;

        let scope = self.scopes.last_mut().unwrap();

        if let Some(old_id) = self.vars.insert(name.clone(), VarId::Local(id)) {
            // If `vars` already has an entry for this name but we have not seen it yet in this
            // scope, then we must be about to write over an entry from the parent scope for this
            // name.
            if !scope.new.contains(&name) {
                scope.shadowed.push((name.clone(), old_id));
            }
        }

        scope.new.insert(name);
        id
    }

    fn new_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.scopes.push(LocalScope::new());

        let ret = f(self);

        let scope = self.scopes.pop().unwrap();
        for name in &scope.new {
            self.vars.remove(name);
        }
        for (name, id) in scope.shadowed {
            self.vars.insert(name, id);
        }

        ret
    }
}

fn resolve_expr(
    ctx: &GlobalContext,
    type_params: &ParamMap,
    locals: &mut LocalContext,
    expr: &raw::Expr,
) -> Result<res::Expr> {
    let kind = match &expr.kind {
        raw::ExprKind::Lit(lit) => res::ExprKind::Lit(lit.clone()),
        raw::ExprKind::Var(ident) => locals
            .get(ident)
            .map(|id| match id {
                VarId::Arg(id) => res::ExprKind::Arg(id),
                VarId::Local(id) => res::ExprKind::Local(id),
            })
            .or_else(|| ctx.mod_.funcs.get(ident).cloned().map(res::ExprKind::Func))
            .ok_or_else(|| Locate {
                kind: ErrorKind::VarNotFound(ident.clone()),
                span: Some(expr.span),
            })?,
        raw::ExprKind::Lam(args, body) => {
            let (args, body) = locals.new_scope::<_, Result<_>>(|locals| {
                let args = args.iter().map(|arg| locals.insert(arg.clone())).collect();
                let body = resolve_expr(ctx, type_params, locals, &**body)?;
                Ok((args, body))
            })?;
            res::ExprKind::Lam(args, Box::new(body))
        }
        raw::ExprKind::Tuple(elems) => res::ExprKind::Tuple(
            elems
                .iter()
                .map(|elem| resolve_expr(ctx, type_params, locals, elem))
                .collect::<Result<_>>()?,
        ),
        raw::ExprKind::UnOp(op, expr) => {
            res::ExprKind::UnOp(*op, Box::new(resolve_expr(ctx, type_params, locals, expr)?))
        }
        raw::ExprKind::BinOp(op, left, right) => res::ExprKind::BinOp(
            *op,
            Box::new(resolve_expr(ctx, type_params, locals, left)?),
            Box::new(resolve_expr(ctx, type_params, locals, right)?),
        ),
        raw::ExprKind::App(abs, args) => res::ExprKind::App(
            Box::new(resolve_expr(ctx, type_params, locals, abs)?),
            args.iter()
                .map(|arg| resolve_expr(ctx, type_params, locals, arg))
                .collect::<Result<_>>()?,
        ),
        raw::ExprKind::TupleField(tuple, field) => res::ExprKind::TupleField(
            Box::new(resolve_expr(ctx, type_params, locals, tuple)?),
            *field,
        ),
        raw::ExprKind::If(cond, true_br, false_br) => res::ExprKind::If(
            Box::new(resolve_expr(ctx, type_params, locals, cond)?),
            resolve_block(ctx, type_params, locals, true_br)?,
            resolve_block(ctx, type_params, locals, false_br)?,
        ),
        raw::ExprKind::Block(block) => {
            res::ExprKind::Block(resolve_block(ctx, type_params, locals, block)?)
        }
    };
    Ok(res::Expr {
        kind,
        span: expr.span,
    })
}

fn resolve_block(
    ctx: &GlobalContext,
    type_params: &ParamMap,
    locals: &mut LocalContext,
    block: &raw::Block,
) -> Result<res::Block> {
    locals.new_scope(|locals| {
        Ok(res::Block {
            stmts: block
                .stmts
                .iter()
                .map(|stmt| resolve_stmt(ctx, type_params, locals, stmt))
                .collect::<Result<_>>()?,
            ret: Box::new(resolve_expr(ctx, type_params, locals, &block.ret)?),
        })
    })
}

fn resolve_stmt(
    ctx: &GlobalContext,
    type_params: &ParamMap,
    locals: &mut LocalContext,
    stmt: &raw::Stmt,
) -> Result<res::Stmt> {
    match &stmt.kind {
        raw::StmtKind::Assign(ident, type_, expr) => {
            let type_ = type_
                .as_ref()
                .map(|type_| resolve_type(ctx, type_params, &type_))
                .transpose()?;
            let expr = resolve_expr(ctx, type_params, locals, expr)?;
            let id = locals.insert(ident.clone());
            Ok(res::Stmt {
                kind: res::StmtKind::Assign(id, type_, expr),
                span: stmt.span,
            })
        }
    }
}
