use crate::ast::{lam_lifted_ast as lifted, typed_ast as typed};
use crate::util::id_vec::IdVec;

pub fn lift(prog: typed::Program) -> lifted::Program {
    lifted::Program {
        funcs: IdVec::from_items(
            prog.funcs
                .into_iter()
                .map(|(_, func)| lift_func(func))
                .collect(),
        ),
        func_symbols: prog.func_symbols,
        lams: IdVec::new(),
        lam_symbols: IdVec::new(),
        types: prog.types,
        type_symbols: prog.type_symbols,
    }
}

fn lift_func(func: typed::FuncDef) -> lifted::FuncDef {
    let body = match func.body {
        typed::FuncBody::External(path) => lifted::FuncBody::External(path),
        typed::FuncBody::Internal(block) => lifted::FuncBody::Internal(lift_block(block)),
    };
    lifted::FuncDef {
        generics: func.generics,
        args: func.args,
        ret: func.ret,
        body,
    }
}

fn lift_block(block: typed::Block) -> lifted::Block {
    let stmts = block.stmts.into_iter().map(lift_stmt).collect();
    let ret = Box::new(lift_expr(*block.ret));
    lifted::Block { stmts, ret }
}

fn lift_stmt(stmt: typed::Stmt) -> lifted::Stmt {
    let kind = match stmt.kind {
        typed::StmtKind::Assign(id, expr) => lifted::StmtKind::Assign(id, lift_expr(expr)),
    };
    lifted::Stmt {
        kind,
        type_: stmt.type_,
        span: stmt.span,
    }
}

fn lift_expr(expr: typed::Expr) -> lifted::Expr {
    use lifted::ExprKind as L;
    use typed::ExprKind as T;

    let kind = match expr.kind {
        T::Lit(lit) => L::Lit(lit),
        T::Local(id) => L::Local(id),
        T::Arg(id) => L::Arg(id),
        T::Func(id) => L::Func(id),
        T::Lam(_, _) => unimplemented!("closures are not supported yet :("),
        T::Tuple(items) => L::Tuple(items.into_iter().map(lift_expr).collect()),
        T::BinOp(op, left, right) => {
            L::BinOp(op, Box::new(lift_expr(*left)), Box::new(lift_expr(*right)))
        }
        T::App(func, args) => L::App(
            Box::new(lift_expr(*func)),
            args.into_iter().map(lift_expr).collect(),
        ),
        T::TupleField(tuple, field) => L::TupleField(Box::new(lift_expr(*tuple)), field),
        T::If(cond, tbr, fbr) => {
            L::If(Box::new(lift_expr(*cond)), lift_block(tbr), lift_block(fbr))
        }
        T::Block(block) => L::Block(lift_block(block)),
    };

    lifted::Expr {
        kind,
        type_: expr.type_,
        span: expr.span,
    }
}

/*

struct GlobalContext {
    lams: IdVec<ast::LamId, lifted::LamDef>,
}

#[derive(Clone, Debug)]
struct CaptureMap {
    captures: HashMap<ast::LocalId, ast::CaptureId>,
}

impl CaptureMap {
    fn translate(&mut self, local: ast::LocalId) -> lifted::ExprKind {
        let fresh_id = ast::CaptureId(self.captures.len() as u32);
        let id = self.captures.entry(local).or_insert(fresh_id);
        lifted::ExprKind::Capture(*id)
    }
}

fn lift(prog: typed::Program) -> lifted::Program {
    lifted::Program {
        funcs: todo!(),
        func_symbols: prog.func_symbols,
        lams: todo!(),
        lam_symbols: todo!(),
        types: prog.types,
        type_symbols: prog.type_symbols,
    }
}

fn lift_expr(ctx: &mut GlobalContext, expr: &typed::Expr) -> lifted::Expr {
    use lifted::ExprKind as LKind;
    use typed::ExprKind as TKind;

    let kind = match &expr.kind {
        TKind::Lit(lit) => LKind::Lit(lit.clone()),
        TKind::Local(_) => todo!(),
        TKind::Arg(_) => todo!(),
        TKind::Func(id) => LKind::Func(*id),
        TKind::Lam(_, _) => todo!(),
        TKind::Tuple(elems) => {
            LKind::Tuple(elems.iter().map(|elem| lift_expr(ctx, elem)).collect())
        }
        TKind::BinOp(op, left, right) => LKind::BinOp(
            *op,
            Box::new(lift_expr(ctx, left)),
            Box::new(lift_expr(ctx, right)),
        ),
        TKind::App(_, _) => todo!(),
        TKind::TupleField(tuple, idx) => LKind::TupleField(Box::new(lift_expr(ctx, tuple)), *idx),
        TKind::If(cond, true_br, false_br) => LKind::If(
            Box::new(lift_expr(ctx, cond)),
            lift_block(ctx, true_br),
            lift_block(ctx, false_br),
        ),
        TKind::Block(block) => LKind::Block(lift_block(ctx, block)),
    };
    lifted::Expr {
        kind,
        type_: expr.type_.clone(),
        span: expr.span,
    }
}

fn lift_block(ctx: &mut GlobalContext, block: &typed::Block) -> lifted::Block {
    lifted::Block {
        stmts: block
            .stmts
            .iter()
            .map(|stmt| lift_stmt(ctx, stmt))
            .collect(),
        ret: Box::new(lift_expr(ctx, &block.ret)),
    }
}

fn lift_stmt(ctx: &mut GlobalContext, stmt: &typed::Stmt) -> lifted::Stmt {
    use lifted::StmtKind as LKind;
    use typed::StmtKind as TKind;

    let kind = match &stmt.kind {
        TKind::Assign(id, expr) => LKind::Assign(*id, lift_expr(ctx, expr)),
    };
    lifted::Stmt {
        kind,
        type_: stmt.type_.clone(),
        span: stmt.span,
    }
}

*/
