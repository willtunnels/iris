use crate::ast::{self, raw_ast as raw, resolved_ast as res};
use crate::file_cache::FileCache;
use crate::report_error::Locate;
use crate::util::id_vec::IdVec;
use std::collections::HashMap;
use std::hash::Hash;
use std::path::{Path, PathBuf};
use std::result::Result as StdResult;

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
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

// A stand-in so the parser can be developed independently from this part of the compiler
fn parse(_file: impl AsRef<Path>) -> raw::Program {
    todo!()
}

pub fn resolve(files: &mut FileCache, file_path: impl AsRef<Path>) -> Result<res::Program> {
    let file = files
        .read(file_path.as_ref())
        .map_err(|err| ErrorKind::CannotReadFile(file_path.as_ref().to_owned(), err))?;
    resolve_prog(&parse(file.text()))
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

fn resolve_prog(prog: &raw::Program) -> Result<res::Program> {
    let mut ctx = GlobalContext::new();

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

    Ok(res::Program {
        funcs: ctx.funcs.into_mapped(|_id, def| def.unwrap()),
        func_symbols: ctx.func_symbols,
        types: ctx.types.into_mapped(|_id, def| def.unwrap()),
        type_symbols: ctx.type_symbols,
    })
}

fn build_func_symbols(def: &raw::FuncDef, span: ast::Span) -> res::FuncSymbols {
    let generics = res::GenericSymbols {
        params: IdVec::from_items(def.generics.params.clone()),
    };
    let args = IdVec::from_items(def.args.iter().map(|(_, ident)| ident.clone()).collect());
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
            .map(|(type_, _)| resolve_type(ctx, &type_params, type_))
            .collect::<Result<_>>()?,
    );
    let ret = resolve_type(ctx, &type_params, &def.ret)?;
    let body = match &def.body {
        raw::FuncBody::External => res::FuncBody::External,
        raw::FuncBody::Internal(expr) => {
            let mut locals =
                LocalContext::with_args(def.args.iter().map(|(_, ident)| ident), span)?;
            res::FuncBody::Internal(resolve_expr(ctx, &type_params, &mut locals, expr)?)
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
        raw::TypeKind::Func(arg, ret) => res::Type::Func(
            Box::new(resolve_type(ctx, type_params, arg)?),
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
    names: Vec<ast::Ident>,
}

impl LocalScope {
    fn new() -> Self {
        Self { names: Vec::new() }
    }
}

#[derive(Clone, Debug)]
enum LookupResult {
    Arg(ast::ArgId),
    Local(ast::LocalId),
}

#[derive(Clone, Debug)]
struct LocalContext {
    args: HashMap<ast::Ident, ast::ArgId>,
    scopes: Vec<LocalScope>,
    locals: HashMap<ast::Ident, ast::LocalId>,
    next_local_id: ast::LocalId,
}

impl LocalContext {
    fn with_args<'a>(args: impl Iterator<Item = &'a ast::Ident>, span: ast::Span) -> Result<Self> {
        let mut locals = LocalContext {
            args: HashMap::new(),
            scopes: vec![LocalScope::new()],
            locals: HashMap::new(),
            next_local_id: ast::LocalId(0),
        };

        for (i, ident) in args.enumerate() {
            let make_err = |()| Locate {
                kind: ErrorKind::DuplacateArgName(ident.clone()),
                span: Some(span),
            };
            try_insert(&mut locals.args, ident.clone(), ast::ArgId(i as u32)).map_err(make_err)?;
        }

        Ok(locals)
    }

    fn get(&self, name: &ast::Ident) -> Option<LookupResult> {
        self.locals
            .get(name)
            .map(|id| LookupResult::Local(*id))
            .or_else(|| self.args.get(name).map(|id| LookupResult::Arg(*id)))
    }

    // If the variable already exists, it is overwritten. This implements shadowing within a scope.
    fn insert(&mut self, name: ast::Ident) -> ast::LocalId {
        let id = ast::LocalId(self.next_local_id.0);
        self.next_local_id.0 += 1;

        self.locals.insert(name.clone(), id);
        self.scopes.last_mut().unwrap().names.push(name);
        id
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

fn resolve_expr(
    ctx: &GlobalContext,
    type_params: &ParamMap,
    locals: &mut LocalContext,
    expr: &raw::Expr,
) -> Result<res::Expr> {
    Ok(match &expr.kind {
        raw::ExprKind::Lit(lit) => res::Expr {
            kind: res::ExprKind::Lit(lit.clone()),
            span: expr.span,
        },
        raw::ExprKind::Var(ident) => {
            let result = locals.get(ident).ok_or_else(|| Locate {
                kind: ErrorKind::VarNotFound(ident.clone()),
                span: Some(expr.span),
            })?;
            let kind = match result {
                LookupResult::Arg(id) => res::ExprKind::Arg(id),
                LookupResult::Local(id) => res::ExprKind::Local(id),
            };
            res::Expr {
                kind,
                span: expr.span,
            }
        }
        raw::ExprKind::Lam(_, _) => todo!(),
        raw::ExprKind::Tuple(elems) => res::Expr {
            kind: res::ExprKind::Tuple(
                elems
                    .iter()
                    .map(|elem| resolve_expr(ctx, type_params, locals, elem))
                    .collect::<Result<_>>()?,
            ),
            span: expr.span,
        },
        raw::ExprKind::BinOp(kind, left, right) => res::Expr {
            kind: res::ExprKind::BinOp(
                *kind,
                Box::new(resolve_expr(ctx, type_params, locals, left)?),
                Box::new(resolve_expr(ctx, type_params, locals, right)?),
            ),
            span: expr.span,
        },
        raw::ExprKind::App(abs, args) => res::Expr {
            kind: res::ExprKind::App(
                Box::new(resolve_expr(ctx, type_params, locals, abs)?),
                args.iter()
                    .map(|arg| resolve_expr(ctx, type_params, locals, arg))
                    .collect::<Result<_>>()?,
            ),
            span: expr.span,
        },
        raw::ExprKind::TupleField(tuple, field) => res::Expr {
            kind: res::ExprKind::TupleField(
                Box::new(resolve_expr(ctx, type_params, locals, tuple)?),
                *field,
            ),
            span: expr.span,
        },
        raw::ExprKind::If(cond, true_br, false_br) => res::Expr {
            kind: res::ExprKind::If(
                Box::new(resolve_expr(ctx, type_params, locals, cond)?),
                resolve_block(ctx, type_params, locals, true_br)?,
                resolve_block(ctx, type_params, locals, false_br)?,
            ),
            span: expr.span,
        },
        raw::ExprKind::Block(block) => res::Expr {
            kind: res::ExprKind::Block(resolve_block(ctx, type_params, locals, block)?),
            span: expr.span,
        },
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
            let id = locals.insert(ident.clone());
            let type_ = type_
                .as_ref()
                .map(|type_| resolve_type(ctx, type_params, &type_))
                .transpose()?;
            let expr = resolve_expr(ctx, type_params, locals, expr)?;
            Ok(res::Stmt {
                kind: res::StmtKind::Assign(id, type_, expr),
                span: stmt.span,
            })
        }
    }
}
