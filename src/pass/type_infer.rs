use crate::ast::{resolved_ast as res, typed_ast as typed};
use crate::util::id_vec::IdVec;

// TODO: we can get away with type inference if we don't implement closures

pub fn infer(prog: res::Program) -> typed::Program {
    typed::Program {
        funcs: IdVec::from_items(
            prog.funcs
                .into_iter()
                .map(|(_, func)| infer_func(func))
                .collect(),
        ),
        func_symbols: prog.func_symbols,
        types: prog.types,
        type_symbols: prog.type_symbols,
    }
}

fn dummy_type() -> res::Type {
    res::Type::Tuple(Vec::new())
}

fn infer_func(func: res::FuncDef) -> typed::FuncDef {
    let body = match func.body {
        res::FuncBody::External(path) => typed::FuncBody::External(path),
        res::FuncBody::Internal(block) => typed::FuncBody::Internal(infer_block(block)),
    };
    typed::FuncDef {
        generics: func.generics,
        args: func.args,
        ret: func.ret,
        body,
    }
}

fn infer_block(block: res::Block) -> typed::Block {
    let stmts = block.stmts.into_iter().map(infer_stmt).collect();
    let ret = Box::new(infer_expr(*block.ret));
    typed::Block { stmts, ret }
}

fn infer_stmt(stmt: res::Stmt) -> typed::Stmt {
    let kind = match stmt.kind {
        res::StmtKind::Assign(id, _, expr) => typed::StmtKind::Assign(id, infer_expr(expr)),
    };
    typed::Stmt {
        kind,
        type_: dummy_type(),
        span: stmt.span,
    }
}

fn infer_expr(expr: res::Expr) -> typed::Expr {
    use res::ExprKind as T;
    use typed::ExprKind as L;

    let kind = match expr.kind {
        T::Lit(lit) => L::Lit(lit),
        T::Local(id) => L::Local(id),
        T::Arg(id) => L::Arg(id),
        T::Func(id) => L::Func(id),
        T::Lam(_, _) => unimplemented!("closures are not supported yet :("),
        T::Tuple(items) => L::Tuple(items.into_iter().map(infer_expr).collect()),
        T::UnOp(_, _) => unimplemented!("unary operators are not supported yet :("),
        T::BinOp(op, left, right) => L::BinOp(
            op,
            Box::new(infer_expr(*left)),
            Box::new(infer_expr(*right)),
        ),
        T::App(func, args) => L::App(
            Box::new(infer_expr(*func)),
            args.into_iter().map(infer_expr).collect(),
        ),
        T::TupleField(tuple, field) => L::TupleField(Box::new(infer_expr(*tuple)), field),
        T::If(cond, tbr, fbr) => L::If(
            Box::new(infer_expr(*cond)),
            infer_block(tbr),
            infer_block(fbr),
        ),
        T::Block(block) => L::Block(infer_block(block)),
    };

    typed::Expr {
        kind,
        type_: dummy_type(),
        span: expr.span,
    }
}

/*

// TODO: enforce proper constraints on types used in arithmetic

id_type! { TypeVar(u32); }

#[derive(Debug, Clone, Copy)]
pub enum Error {
    Mismatch(String, String),
}

#[derive(Debug, Clone)]
enum UnifyError {
    Mismatch,
    Recursive,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Assign {
    Unknown,
    Equal(TypeVar),
    Param(ast::TypeParamId),
    App(ast::TypeId, Vec<TypeVar>),
    Tuple(Vec<TypeVar>),
    Func(Vec<TypeVar>, TypeVar),
}

#[derive(Clone, Debug)]
struct Context {
    // `RefCell` is used the implement the "occurs" check. `Context` does not actually have interior
    // mutability.
    vars: IdVec<TypeVar, RefCell<Assign>>,
}

impl Context {
    fn new() -> Self {
        Context { vars: IdVec::new() }
    }

    fn new_var(&mut self, assign: Assign) -> TypeVar {
        self.vars.push(RefCell::new(assign))
    }

    fn obtain(&self, var: TypeVar) -> Result<RefMut<Assign>, UnifyError> {
        self.vars[var]
            .try_borrow_mut()
            .map_err(|_| UnifyError::Recursive)
    }

    fn follow(&self, var: TypeVar) -> Result<TypeVar, UnifyError> {
        let mut assign = self.obtain(var)?;
        if let Assign::Equal(curr_dest) = assign.deref_mut() {
            *curr_dest = self.follow(*curr_dest)?;
            Ok(*curr_dest)
        } else {
            Ok(var)
        }
    }

    fn unify_rec(&self, root_var1: TypeVar, root_var2: TypeVar) -> Result<(), UnifyError> {
        let var1 = self.follow(root_var1)?;
        let var2 = self.follow(root_var2)?;

        if var1 == var2 {
            return Ok(());
        }

        let mut assign1 = self.obtain(var1)?;
        let mut assign2 = self.obtain(var2)?;

        match (assign1.deref(), assign2.deref()) {
            (Assign::Equal(_), _) => unreachable!(),
            (_, Assign::Equal(_)) => unreachable!(),

            (Assign::Unknown, _) => {
                *assign1 = Assign::Equal(var2);
                return Ok(());
            }

            (_, Assign::Unknown) => {
                *assign2 = Assign::Equal(var1);
                return Ok(());
            }

            (Assign::Param(param1), Assign::Param(param2)) => {
                if param1 != param2 {
                    return Err(UnifyError::Mismatch);
                }
            }

            (Assign::App(id1, args1), Assign::App(id2, args2)) => {
                if id1 != id2 {
                    return Err(UnifyError::Mismatch);
                }

                if args1.len() != args2.len() {
                    return Err(UnifyError::Mismatch);
                }

                for (&arg1, &arg2) in args1.iter().zip(args2.iter()) {
                    self.unify_rec(arg1, arg2)?;
                }
            }

            (Assign::Tuple(items1), Assign::Tuple(items2)) => {
                if items1.len() != items2.len() {
                    return Err(UnifyError::Mismatch);
                }

                for (&item1, &item2) in items1.iter().zip(items2.iter()) {
                    self.unify_rec(item1, item2)?;
                }
            }

            (Assign::Func(args1, ret1), Assign::Func(args2, ret2)) => {
                if args1.len() != args2.len() {
                    return Err(UnifyError::Mismatch);
                }

                for (&arg1, &arg2) in args1.iter().zip(args2.iter()) {
                    self.unify_rec(arg1, arg2);
                }

                self.unify_rec(*ret1, *ret2)?;
            }

            _ => {
                return Err(UnifyError::Mismatch);
            }
        }

        // This is purely an optimization
        *assign1 = Assign::Equal(var2);

        Ok(())
    }

    fn unify(&mut self, expected: TypeVar, actual: TypeVar) -> Result<(), RawError> {
        self.unify_rec(expected, actual).map_err(|err| match err {
            UnifyError::Recursive => RawErrorKind::Recursive.into(),
            UnifyError::Mismatch => RawErrorKind::Mismatch { expected, actual }.into(),
        })
    }
}

*/
