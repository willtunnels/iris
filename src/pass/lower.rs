use crate::ast::{
    lam_lifted_ast::*,
    raw_ast::{BinOpKind, Lit},
    resolved_ast::Type,
    *,
};
use std::io::Write;

const STATELESS: &str = "crate::burnt_toast::runtime::Stateless";

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("cannot write to output")]
    CannotWrite(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

struct Writer<W> {
    inner: W,
    depth: u32,
}

impl<W: Write> Writer<W> {
    fn new(write: W) -> Self {
        Self {
            inner: write,
            depth: 0, // columns
        }
    }

    fn scope<T, F>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(&mut Writer<W>) -> Result<T>,
    {
        self.depth += 1;
        let res = f(self)?;
        self.depth -= 1;
        Ok(res)
    }

    fn write(&mut self, buf: &[u8]) -> Result<()> {
        for _ in 0..self.depth {
            self.inner.write(b"    ")?;
        }
        self.inner.write(buf)?;
        Ok(())
    }

    fn writeln(&mut self, buf: &[u8]) -> Result<()> {
        self.write(buf)?;
        self.inner.write(b"\n")?;
        Ok(())
    }
}

fn mangle_type_param(id: TypeParamId) -> String {
    format!("T{}", id.0)
}

fn mangle_local(id: LocalId) -> String {
    format!("v{}", id.0)
}

fn mangle_arg(id: ArgId) -> String {
    format!("a{}", id.0)
}

fn mangle_capture(id: CaptureId) -> String {
    format!("c{}", id.0)
}

fn func_path(ident: &Ident) -> String {
    format!("super::func::{}", ident)
}

fn state_path(ident: &Ident) -> String {
    format!("super::state::{}", ident)
}

fn sep<I, T, F>(mut iter: I, mut to_string: F, sep: &str) -> String
where
    I: Iterator<Item = T>,
    F: FnMut(T) -> String,
{
    let mut result = String::new();
    while let Some(item) = iter.next() {
        result.push_str(&to_string(item));
        result.push_str(sep);
    }

    // Remove the trailing separator, if there is one
    if result.len() > sep.len() {
        result.truncate(result.len() - sep.len());
    }

    result
}

// TODO: deduplicate
fn try_sep<I, T, F>(mut iter: I, mut to_string: F, sep: &str) -> Result<String>
where
    I: Iterator<Item = T>,
    F: FnMut(T) -> Result<String>,
{
    let mut result = String::new();
    while let Some(item) = iter.next() {
        result.push_str(&to_string(item)?);
        result.push_str(sep);
    }

    // Remove the trailing separator, if there is one
    if result.len() > sep.len() {
        result.truncate(result.len() - sep.len());
    }

    Ok(result)
}

pub fn lower(prog: &Program, write: impl Write) -> Result<()> {
    let mut writer = Writer::new(write);
    for func in &prog.funcs {
        lower_func(prog, &func.0, &mut writer)?;
    }
    Ok(())
}

fn lower_type(prog: &Program, type_: &Type) -> String {
    let lower_types = |iter| sep(iter, |type_| lower_type(prog, type_), ", ");

    match type_ {
        Type::Var(id) => mangle_type_param(*id),

        Type::Custom(id, generics) => format!(
            "{}<{}>",
            prog.type_symbols[id].name,
            lower_types(generics.iter()),
        ),

        Type::Func(args, ret) => format!(
            "Box<dyn Fn({}) -> {}>",
            lower_types(args.iter()),
            lower_type(prog, ret)
        ),

        Type::Tuple(items) => format!("({})", lower_types(items.iter())),
    }
}

fn lower_func<W: Write>(prog: &Program, id: &FuncId, writer: &mut Writer<W>) -> Result<()> {
    let def = &prog.funcs[id];
    let symbols = &prog.func_symbols[id];

    let body = match &def.body {
        FuncBody::External(_) => return Ok(()),
        FuncBody::Internal(block) => lower_block(prog, block)?,
    };

    let structs = match &def.calls.1 {
        CallableId::Func()
    };

    let generics = sep(
        0..def.generics.num_params,
        |i| format!("{}: IType", mangle_type_param(TypeParamId(i))),
        ", ",
    );

    let args = sep(
        def.args.iter(),
        |(id, type_)| format!("{}: {}", mangle_arg(id), lower_type(prog, type_)),
        ", ",
    );

    let ret = lower_type(prog, &def.ret);

    let sig = format!("fn {}<{}>({}) -> {} {{", &symbols.name, generics, args, ret);
    writer.writeln(sig.as_bytes());

    writer.scope(|writer| writer.write(body.as_bytes()));
    writer.writeln("}".as_bytes());

    Ok(())
}

fn lower_block(prog: &Program, block: &Block) -> Result<String> {
    let stmts = try_sep(block.stmts.iter(), |stmt| lower_stmt(prog, stmt), "\n")?;

    let ret = lower_expr(prog, &block.ret)?;
    Ok(format!("{}\n{}", stmts, ret))
}

fn lower_stmt(prog: &Program, stmt: &Stmt) -> Result<String> {
    Ok(match &stmt.kind {
        StmtKind::Assign(id, expr) => {
            format!("let {} = {};", mangle_local(*id), lower_expr(prog, expr)?)
        }
    })
}

fn lower_expr(prog: &Program, expr: &Expr) -> Result<String> {
    Ok(match &expr.kind {
        ExprKind::Lit(lit) => match lit {
            Lit::Bool(val) => format!("{}({})", STATELESS, val),
            Lit::I8(val) => format!("{}({}i8)", STATELESS, val),
            Lit::I16(val) => format!("{}({}i16)", STATELESS, val),
            Lit::I32(val) => format!("{}({}i32)", STATELESS, val),
            Lit::I64(val) => format!("{}({}i64)", STATELESS, val),
            Lit::ISize(val) => format!("{}({}isize)", STATELESS, val),
            Lit::U8(val) => format!("{}({}u8)", STATELESS, val),
            Lit::U16(val) => format!("{}({}u16)", STATELESS, val),
            Lit::U32(val) => format!("{}({}u32)", STATELESS, val),
            Lit::U64(val) => format!("{}({}u64)", STATELESS, val),
            Lit::USize(val) => format!("{}({}usize)", STATELESS, val),
            Lit::F32(val) => format!("{}({}f32)", STATELESS, val),
            Lit::F64(val) => format!("{}({}f64)", STATELESS, val),
            Lit::Char(val) => format!("{}('{}')", STATELESS, val),
            Lit::Str(val) => format!("{}(\"{}\")", STATELESS, val),
        },
        ExprKind::Local(id) => mangle_local(*id),
        ExprKind::Arg(id) => mangle_arg(*id),
        ExprKind::Capture(id) => mangle_capture(*id),
        ExprKind::Func(id) => func_path(&prog.func_symbols[id].name),
        ExprKind::Lam(id, id_vec) => todo!(), // iclosure
        ExprKind::Tuple(exprs) => format!(
            "({})", try_sep(exprs.iter(), |expr| lower_expr(prog, expr), ", ")?;
        ),

        /*
            struct Stateless<T>(T);
        */
        ExprKind::BinOp(op, a, b) => match *op {
            // `&&` and `||` are not overloadable, so they have to be handled here rather than in
            // the runtime
            BinOpKind::And => format!(
                "{}(({}).0 && ({}).0)",
                STATELESS,
                lower_expr(prog, a)?,
                lower_expr(prog, b)?
            ),
            BinOpKind::Or => format!(
                "{}(({}).0 || ({}).0)",
                STATELESS,
                lower_expr(prog, a)?,
                lower_expr(prog, b)?
            ),
            BinOpKind::Eq => format!("{} == {}", lower_expr(prog, a)?, lower_expr(prog, b)?),
            BinOpKind::NotEq => format!("{} != {}", lower_expr(prog, a)?, lower_expr(prog, b)?),
            BinOpKind::Lt => format!("{} < {}", lower_expr(prog, a)?, lower_expr(prog, b)?),
            BinOpKind::Lte => format!("{} <= {}", lower_expr(prog, a)?, lower_expr(prog, b)?),
            BinOpKind::Gt => format!("{} > {}", lower_expr(prog, a)?, lower_expr(prog, b)?),
            BinOpKind::Gte => format!("{} >= {}", lower_expr(prog, a)?, lower_expr(prog, b)?),
            BinOpKind::Add => format!("{} + {}", lower_expr(prog, a)?, lower_expr(prog, b)?),
            BinOpKind::Sub => format!("{} - {}", lower_expr(prog, a)?, lower_expr(prog, b)?),
            BinOpKind::Mul => format!("{} * {}", lower_expr(prog, a)?, lower_expr(prog, b)?),
            BinOpKind::Div => format!("{} / {}", lower_expr(prog, a)?, lower_expr(prog, b)?),
            BinOpKind::Mod => format!("{} % {}", lower_expr(prog, a)?, lower_expr(prog, b)?),
        },

        /*
        fn foo () {
            let x = f(y, z);
                    ^^^^^^^^^^^^^^^^  App(Func(id), [Local(id), Local(id)])
        }
        */
        ExprKind::App(id, func, args) => {
            let func = match &func.kind {
                ExprKind::Func(id) => id,
                _ => unimplemented!(),
            };
            //f(g(y,z))
            // App(Func(f), [App(Func(g), [Local(y), Local(z)])])
            let arguments = try_sep(args.iter(), |arg| lower_expr(prog, arg), ", ")?;
            format!("self.state{}.next({})", id.0, arguments)
        }

        ExprKind::TupleField(expr, index) => format!("{}.{}", lower_expr(prog, expr)?, index),
        ExprKind::If(_, _, _) => todo!(),

        ExprKind::Block(block) => lower_block(prog, block)?,
    })
}

// mod func {
//     // all funcs
//     struct foo { ... }
// }

// mod state {
//     // all states
//     struct foo { ... }
// }

/*

FFn::default().init()

*/

/*
fn bar() {
    let t = g(x);
    let y = t.0(x);
}

*/

// //////////////////////////////////////////////////////////////////////////////////

// fn foo(x: X) -> ... {
//     let y = f(x);
//     let a = y + 2;
//     let z = 2 + h(g(a), x);
//     z
// }

// struct Block0Fn {
//   y_fn: FFn,
//   temp0_fn: GFn,
//   z_fn: HFn,
// }

// struct Block0State {
//   y_state: FState,
//   temp0_state: GState,
//   z_state: HState,
// }

// impl<X: IType, Z: IType, S: IState<X, Y>> IFn<X, Z, S> for Block0Fn {
//   fn init(self, x: X) -> (S, Z) {
//     let (f_state, y) = self.y_fn.init(x.clone());
//     let a = y + 2;
//     let (g_state)
//     let (z_state, z_out) = self.z_fn.init((temp0_out, input));
//     (Block0State { y_state, temp0_state, z_state }, z_out)
//   }
// }

// impl IState for Block0State {
//   fn next(&mut self, action: X::Action) -> Z::Action {
//     let y_action = self.y_state.next(action.clone());
//     let y_action = y_action + 2;
//     let temp0_fn_action = self.temp0_state.next(y_action);
//     let z_action = self.z_state.next((temp0_fn_aciton, action));
//     z_action
//   }
// }

/*

extern fn zip(...) -> ... = ::something_in_rust::zip;
extern fn filter_eq(...) -> ... = ::something_in_rust::filter_eq;
extern fn len(...) -> ... = ::something_in_rust::len;

fn count(v: IVec<i32>, w: IVec<i32>) -> i32 {
    len(filter_eq(zip(v, w))) % 2
}

v = [0, 1, 2, 3] |-> [8, 5, 2, 3]
w = [4, 5, 2, 7] |-> [1, 5, 8, 7]

v_action: Map<Stateless<i32>, Stateless<i32>::Action> = {0: 8, 1: 5}
w_action: Map<Stateless<i32>, Stateless<i32>::Action> = {0: 1, 2: 8}

zip_action: Map<Stateless<i32>, (Stateless<i32>, Stateless<i32>)::Action> = {0: (8, 1), 1: (5, id), 2: (id, 8)}
filter_eq_action: Map<Stateless<i32>, (Statless<i32>, Stateless<i32>)::Action> = {added: {2: (id, 8)}, removed: {1: (5, id)} }
len_action: i32 = old_length + 1 - 1

struct count_fn {
    f0: zip_fn,
    f1: filter_eq_fn,
    f2: len_fn,
}

struct count_state {
    s0: zip_state,
    s1: filter_eq_state,
    s2: len_state,
}

impl IFn<(IVec<Stateless<i32>, IVec<Stateless<i32>), Stateless<i32>, count_state> for count_fn {
  fn init(self, arg: (IVec<Stateless<i32>, IVec<Stateless<i32>)) -> (count_state, Stateless<i32>) {
    let (s0, v0) = self.f0.init((arg.0.clone(), arg.1.clone()));
    let (s1, v1) = self.f1.init(v0.clone());
    let (s2, v2) = self.f2.init(v1.clone());
    let v3 = v2 % Stateless(2);
    (count_state { s0, s1, s2 }, v3)
  }
}

impl IState<(IVec<Stateless<i32>, IVec<Stateless<i32>), Stateless<i32>> for count_state {
  fn next(&mut self, arg: (IVec<Stateless<i32>, IVec<Stateless<i32>)::Action) -> Stateless<i32>::Action {
    let a0 = self.s0.next(arg.clone());
    let a1 = self.s1.next(a0.clone());
    let a2 = self.s2.next(a1.clone());
    let a3 = a2 % 2;
    a3
  }
}

burnt_toast_mod!(inc);
fn main() {
    let v = ivec![0, 1, 2, 3];
    let w = ivec![4, 5, 2, 7];

    let count = inc::count_fn::new();
    let (state, res) = count.init((v, w));

    let v_action = make_ivec_action([(0, 8), (1, 5)]);
    let w_action = make_ivec_action([(0, 1), (2, 8)]);
    let res = res.apply(state.next((v_action, w_action)));
}

*/

////////////////////////////////////////////

// fn foo(x: X) -> ... {
//     let y = f(x);
//     let z = h(g(y), x);
//     z
// }

// struct Block0Fn {
//   y_fn: FFn,
//   temp0_fn: GFn,
//   z_fn: HFn,
// }

// struct Block0State {
//   y_state: FState,
//   temp0_state: GState,
//   z_state: HState,
// }

// impl<X: IType, Z: IType, S: IState<X, Y>> IFn<X, Z, S> for Block0Fn {
//   fn init(self, input: X) -> (S, Z) {
//     let (y_state, y_out) = self.y_fn.init(input.clone());
//     let (temp0_state, temp0_out) = self.temp0_fn.init(y_out);
//     let (z_state, z_out) = self.z_fn.init((temp0_out, input));
//     (Block0State { y_state, temp0_state, z_state }, z_out)
//   }
// }

// impl IState for Block0State {
//   fn next(&mut self, action: X::Action) -> Z::Action {
//     let y_action = self.y_state.next(action.clone());
//     let temp0_fn_action = self.temp0_state.next(y_action);
//     let z_action = self.z_state.next((temp0_fn_aciton, action));
//     z_action
//   }
// }

/*
fn foo(x: int) -> int {
    if x < 2 {
        return f(x);
    } else {
        return g(x);
    }
}

struct Block0Fn {
    fun1: FFn,
    fun2: GFn,
    fun3: HFn,
}

struct Block0State {
   f_state: FState,
   g_state: GState,
   h_state: HState
}

impl<X: IType, Z: IType, S: IState<X, Y>> IFn<X, Z, S> for Block0Fn {
    fn init(self, input: X) -> (S, Z) {
        let (z_state, z_out) = if input < 2 {
            self.fun1.init(input.clone());
            Block0State {}
        } else {
            self.fun2.init(input.clone());
        }



        self.z_fn.init((temp0_out, input));
        (Block0State { y_state, temp0_state, z_state }, z_out)
   }
 }






*/

// //////////////////////////////////////////////////////////////////////////////////

// fn twice<T, F: T -> T>(f: F, x: T) -> T {
//   f(f(x))
// }

// fn foo() {
//     ...
//     twice(|x| x + y, 5);
//     ...
// }

// struct TwiceFn<T, F>(PhantomData(T), PhantomData(F));

// struct TwiceState<T, F: IClosure<T, T>> {
//   temp0_state: F::Eval::State,
//   temp1_state: F::Eval::State,
// }

// impl<T: IType, F: IClosure<T, T>> IFn<(F, T), T> for TwiceFn<T, F> {
//   type State = TwiceState<T, F>;
//   fn init(self, input: (F, T)) -> (Self::State, T) {
//     let (temp0_state, temp0) = F::eval().init((input.0.clone(), input.1));
//     let (temp1_state, temp1) = F::eval().init((input.0, input, temp0));
//     (TwiceState { temp0_state, temp1_state }, temp1)
//   }
// }

// impl<T: IType, F: IClosure<T, T>> IState<(F, T), T> for TwiceState<T, F> {
//   fn next(&mut self, action: (F::Action, T::Action)) -> T::Action {
//     let temp0 = self.temp0_state.next(action.0.clone(), action.1);
//     let temp1 = self.temp1_state.next(action.0, temp0);
//     temp1
//   }
// }

// writer.write(b"hello world");
// writer.scope(|writer| {
//     writer.write(b"hello world");
//     writer.scope(|writer| Ok(()));
//     Ok(())
// });
// writer.write(b"hello world");
// Ok(())
