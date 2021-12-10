use crate::ast::{
    lam_lifted_ast::*,
    raw_ast::{BinOpKind, Lit},
    resolved_ast::Type,
    *,
};
use std::io::Write;

const STATELESS: &str = "burnt_toast::runtime::Stateless";
const ITYPE: &str = "burnt_toast::runtime::IType";
const IFN: &str = "burnt_toast::runtime::IFn";
const ISTATE: &str = "burnt_toast::runtime::IState";

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

    // TODO: every time we encounter a new line, perform the indent again
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

// Currently not used
// fn mangle_arg(id: ArgId) -> String {
//     format!("arg.{}", id.0)
// }

fn mangle_capture(id: CaptureId) -> String {
    format!("c{}", id.0)
}

fn func_path(ident: &Ident) -> String {
    format!("super::fns::{}", ident.0)
}

fn state_path(ident: &Ident) -> String {
    format!("super::states::{}", ident.0)
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

fn make_arg<I, T, F>(mut iter: I, mut to_string: F) -> String
where
    I: ExactSizeIterator<Item = T>,
    F: FnMut(T) -> String,
{
    if iter.len() == 1 {
        to_string(iter.next().unwrap())
    } else {
        // If len is 0 we should return the empty tuple, so this works in every case
        format!("({})", sep(iter, to_string, ","))
    }
}

fn gen_structs<W: Write>(
    prog: &Program,
    all_func_calls: &Vec<(Ident, Vec<FuncId>)>,
    target: &Target,
    writer: &mut Writer<W>,
) -> Result<()> {
    let (prefix, attrs) = match target {
        Target::IFn => ("f", ""),
        Target::IState => ("s", "#[derive(serde::Serialize)]\n"),
    };
    let path_func = match target {
        Target::IFn => func_path,
        Target::IState => state_path,
    };

    for (func_name, func_calls) in all_func_calls {
        let mut body = String::new();
        let mut index = 0;
        for call_id in func_calls {
            let call_name = match &prog.funcs[call_id].body {
                FuncBody::External(path) => sep(path.0.iter(), |iden| iden.0.clone(), "::"),
                FuncBody::Internal(_) => path_func(&prog.func_symbols[call_id].name),
            };
            body.push_str(&format!("{}{}: {},\n", prefix, index, call_name));
            index += 1;
        }
        writer.writeln(
            format!(
                "{}#[allow(non_camel_case_types)]\npub struct {} {{",
                attrs, func_name.0
            )
            .as_bytes(),
        )?;
        writer.scope(|writer| writer.write(body.as_bytes()))?;
        writer.writeln("}".as_bytes())?;
    }
    Ok(())
}

pub fn lower(prog: &Program, write: impl Write) -> Result<()> {
    let mut writer = Writer::new(write);

    // Maps functions to a list of the function calls found therin.
    let mut all_func_calls: Vec<(Ident, Vec<FuncId>)> = Vec::new();

    writer.writeln("pub mod fns {".as_bytes())?;
    for func in &prog.funcs {
        let mut func_calls: Vec<FuncId> = Vec::new();
        lower_to_ifn(prog, func.0, &mut writer, &mut func_calls)?;
        let func_name = prog.func_symbols[func.0].name.clone();
        all_func_calls.push((func_name, func_calls));
    }

    writer.scope(|writer| gen_structs(prog, &all_func_calls, &Target::IFn, writer))?;
    writer.writeln("}".as_bytes())?;

    writer.writeln("pub mod states {".as_bytes())?;
    for func in &prog.funcs {
        // XXX: we don't need to collect the function calls twice
        let mut unused: Vec<FuncId> = Vec::new();
        lower_to_istate(prog, func.0, &mut writer, &mut unused)?;
    }

    writer.scope(|writer| gen_structs(prog, &all_func_calls, &Target::IState, writer))?;
    writer.writeln("}".as_bytes())?;

    Ok(())
}

fn lower_type(prog: &Program, type_: &Type) -> String {
    let lower_types = |iter| sep(iter, |type_| lower_type(prog, type_), ", ");

    match type_ {
        Type::Var(id) => mangle_type_param(*id),

        Type::Custom(id, generics) => format!(
            "{}<{}>",
            sep(prog.types[id].path.0.iter(), |elem| elem.0.clone(), "::"),
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

fn lower_to_ifn<W: Write>(
    prog: &Program,
    id: FuncId,
    writer: &mut Writer<W>,
    func_calls: &mut Vec<FuncId>,
) -> Result<()> {
    let def = &prog.funcs[id];
    let symbols = &prog.func_symbols[id];

    let ifn_body = match &def.body {
        FuncBody::External(_) => return Ok(()),
        FuncBody::Internal(block) => lower_func_body(prog, block, func_calls, id, &Target::IFn)?,
    };

    let generics = sep(
        0..def.generics.num_params,
        |i| format!("{}: IType", mangle_type_param(TypeParamId(i))),
        ", ",
    );

    let arg_type = make_arg(def.args.iter(), |arg| lower_type(prog, arg.1));
    let ret_type = lower_type(prog, &def.ret);
    let istate_s = format!("{}", state_path(&symbols.name));
    let ifun_s = format!("{}", func_path(&symbols.name));

    let ifn_sig = format!(
        "impl<{generics}> {trait_} for {struct_} {{\ntype X={arg};\ntype Y={ret};\ntype S={state};",
        generics = generics,
        trait_ = IFN,
        struct_ = ifun_s,
        arg = arg_type,
        ret = ret_type,
        state = istate_s,
    );

    let init_sig = format!(
        "fn init<{}>(self, arg: {}) -> ({}, {}) {{",
        generics, arg_type, istate_s, ret_type
    );

    writer.writeln(ifn_sig.as_bytes())?;
    writer.writeln(init_sig.as_bytes())?;
    writer.scope(|writer| writer.write(ifn_body.as_bytes()))?;
    writer.write("}\n}".as_bytes())?;

    Ok(())
}

fn lower_to_istate<W: Write>(
    prog: &Program,
    id: FuncId,
    writer: &mut Writer<W>,
    func_calls: &mut Vec<FuncId>,
) -> Result<()> {
    let def = &prog.funcs[id];
    let symbols = &prog.func_symbols[id];

    let istate_body = match &def.body {
        FuncBody::External(_) => return Ok(()),
        FuncBody::Internal(block) => lower_func_body(prog, block, func_calls, id, &Target::IState)?,
    };

    let generics = sep(
        0..def.generics.num_params,
        |i| format!("{}: IType", mangle_type_param(TypeParamId(i))),
        ", ",
    );

    let arg_type = make_arg(def.args.iter(), |arg| lower_type(prog, arg.1));
    let ret_type = lower_type(prog, &def.ret);
    let istate_s = format!("{}", state_path(&symbols.name));

    let istate_sig = format!(
        "impl<{generics}> {trait_} for {struct_} {{\ntype X={arg};\ntype Y={ret};",
        generics = generics,
        trait_ = ISTATE,
        struct_ = istate_s,
        arg = arg_type,
        ret = ret_type,
    );

    let next_sig = format!(
        "fn next<{}>(&mut self, arg: <{} as {}>::Action) -> <{} as {}>::Action {{",
        generics, arg_type, ITYPE, ret_type, ITYPE,
    );

    writer.writeln(istate_sig.as_bytes())?;
    writer.writeln(next_sig.as_bytes())?;
    writer.scope(|writer| writer.write(istate_body.as_bytes()))?;
    writer.write("}\n}".as_bytes())?;

    Ok(())
}

#[derive(Clone, Debug)]
enum Target {
    IFn,
    IState,
}

fn lower_block(
    prog: &Program,
    block: &Block,
    func_calls: &mut Vec<FuncId>,
    func_id: FuncId,
    app_counter: &mut i32,
    value_counter: &mut i32,
    target: &Target,
) -> Result<String> {
    let stmts = try_sep(
        block.stmts.iter(),
        |stmt| {
            lower_stmt(
                prog,
                stmt,
                func_calls,
                func_id,
                app_counter,
                value_counter,
                target,
            )
        },
        "\n",
    )?;

    let block_return = lower_expr(
        prog,
        &block.ret,
        func_calls,
        func_id,
        app_counter,
        value_counter,
        target,
    )?;

    Ok(format!("{}\n{}", stmts, block_return))
}

fn lower_func_body(
    prog: &Program,
    block: &Block,
    func_calls: &mut Vec<FuncId>,
    func_id: FuncId,
    target: &Target,
) -> Result<String> {
    let mut app_counter = 0;
    let mut value_counter = 0;

    let block_body = lower_block(
        prog,
        block,
        func_calls,
        func_id,
        &mut app_counter,
        &mut value_counter,
        target,
    )?;

    let func_name = &prog.func_symbols[func_id].name;
    let states = sep(0..app_counter, |i| format!("s{}", i), ", ");
    let last_value_count = value_counter - 1;

    let return_value = match target {
        Target::IFn => format!(
            "({} {{ {} }}, v{})",
            state_path(func_name),
            states,
            last_value_count
        ),
        Target::IState => format!("v{}", last_value_count),
    };

    Ok(format!("{}\n{}", block_body, return_value))
}

fn lower_stmt(
    prog: &Program,
    stmt: &Stmt,
    func_calls: &mut Vec<FuncId>,
    func_id: FuncId,
    app_counter: &mut i32,
    value_counter: &mut i32,
    target: &Target,
) -> Result<String> {
    Ok(match &stmt.kind {
        StmtKind::Assign(id, expr) => {
            let prelude = lower_expr(
                prog,
                expr,
                func_calls,
                func_id,
                app_counter,
                value_counter,
                target,
            )?;
            format!(
                "{}\nlet {} = v{};",
                prelude,
                mangle_local(*id),
                *value_counter - 1
            )
        }
    })
}

fn lower_expr(
    prog: &Program,
    expr: &Expr,
    func_calls: &mut Vec<FuncId>,
    func_id: FuncId,
    app_counter: &mut i32,
    value_counter: &mut i32,
    target: &Target,
) -> Result<String> {
    let ret = match &expr.kind {
        ExprKind::Lit(lit) => {
            let literal = match lit {
                Lit::Bool(val) => format!("{}", val),
                Lit::I8(val) => format!("{}", val),
                Lit::I16(val) => format!("{}", val),
                Lit::I32(val) => format!("{}", val),
                Lit::I64(val) => format!("{}", val),
                Lit::ISize(val) => format!("{}", val),
                Lit::U8(val) => format!("{}", val),
                Lit::U16(val) => format!("{}", val),
                Lit::U32(val) => format!("{}", val),
                Lit::U64(val) => format!("{}", val),
                Lit::USize(val) => format!("{}", val),
                Lit::F32(val) => format!("{}", val),
                Lit::F64(val) => format!("{}", val),
                Lit::Char(val) => format!("{}", val),
                Lit::Str(val) => format!("{}", val),
            };
            let literal = match target {
                Target::IFn => format!("{}({})", STATELESS, literal),
                Target::IState => literal,
            };
            format!("let v{} = {};", *value_counter, literal)
        }
        ExprKind::Local(id) => format!("let {} = {};", *value_counter, mangle_local(*id)),

        ExprKind::Arg(arg_id) => {
            let index = prog.func_symbols[func_id]
                .args
                .iter()
                .position(|(id, _)| id.0 == arg_id.0);

            match index {
                Some(i) => format!("let v{} = arg.{};", *value_counter, i),
                None => todo!(),
            }
        }
        ExprKind::Capture(_id) => todo!(), //mangle_capture(*id),
        ExprKind::Func(id) => {
            let name = func_path(&prog.func_symbols[id].name);
            format!("let v{} = {};", *value_counter, name)
        }
        ExprKind::Lam(_id, _id_vec) => todo!(), // iclosure

        ExprKind::Tuple(exprs) => {
            let mut prelude = String::new();
            let mut values = Vec::new();
            for expr in exprs.iter() {
                prelude.push_str(&format!(
                    "{}\n",
                    lower_expr(
                        prog,
                        expr,
                        func_calls,
                        func_id,
                        app_counter,
                        value_counter,
                        target,
                    )?
                ));
                values.push(*value_counter - 1);
            }
            let fields = sep(values.iter(), |v| format!("v{}", v), ", ");
            format!("{}let v{} = ({})", prelude, *value_counter, fields)
        }

        /*
            struct Stateless<T>(T);
        */
        ExprKind::BinOp(op, a, b) => {
            let prelude_left = lower_expr(
                prog,
                a,
                func_calls,
                func_id,
                app_counter,
                value_counter,
                target,
            )?;
            let left_value = *value_counter - 1;
            let prelude_right = lower_expr(
                prog,
                b,
                func_calls,
                func_id,
                app_counter,
                value_counter,
                target,
            )?;
            let right_value = *value_counter - 1;

            let op = match *op {
                BinOpKind::And => "&&",
                BinOpKind::Or => "||",
                BinOpKind::Eq => "==",
                BinOpKind::NotEq => "!=",
                BinOpKind::Lt => "<",
                BinOpKind::Lte => "<=",
                BinOpKind::Gt => ">",
                BinOpKind::Gte => ">=",
                BinOpKind::Add => "+",
                BinOpKind::Sub => "-",
                BinOpKind::Mul => "*",
                BinOpKind::Div => "/",
                BinOpKind::Mod => "%",
            };

            let expr = match target {
                Target::IFn => {
                    format!("{}(v{}.0 {} v{}.0)", STATELESS, left_value, op, right_value)
                }
                Target::IState => format!("v{} {} v{}", left_value, op, right_value),
            };

            format!(
                "{}\n{}\nlet v{} = {};",
                prelude_left, prelude_right, *value_counter, expr
            )
        }

        /*
        fn foo () {
            let x = f(y, z);
                    ^^^^^^^^^^^^^^^^  App(Func(id), [Local(id), Local(id)])
        }
        */
        ExprKind::App(func, args) => {
            let app_id = match &func.kind {
                ExprKind::Func(id) => id,
                _ => unimplemented!(),
            };

            // Add this application to func_calls
            func_calls.push(*app_id);

            let mut prelude = String::new();
            let mut args_list = Vec::new();
            for arg in args.iter() {
                prelude.push_str(&format!(
                    "{}\n",
                    lower_expr(
                        prog,
                        arg,
                        func_calls,
                        func_id,
                        app_counter,
                        value_counter,
                        target,
                    )?
                ));
                args_list.push(*value_counter - 1);
            }
            let args_string = make_arg(args_list.iter(), |v| format!("v{}.clone()", v));
            let let_string = match target {
                Target::IFn => format!(
                    "let (s{}, v{}) = self.f{}.init({});",
                    *app_counter, *value_counter, *app_counter, args_string
                ),
                Target::IState => format!(
                    "let v{} = self.s{}.next({});",
                    *value_counter, *app_counter, args_string
                ),
            };
            *app_counter += 1;
            format!("{}{}", prelude, let_string)
        }

        ExprKind::TupleField(expr, index) => {
            let prelude = lower_expr(
                prog,
                expr,
                func_calls,
                func_id,
                app_counter,
                value_counter,
                target,
            )?;
            format!(
                "{}\nlet v{} = v{}.{};",
                prelude,
                *value_counter,
                *value_counter - 1,
                index
            )
        }
        ExprKind::If(_, _, _) => todo!(),

        ExprKind::Block(block) => lower_block(
            prog,
            block,
            func_calls,
            func_id,
            app_counter,
            value_counter,
            target,
        )?,
    };
    *value_counter += 1;
    Ok(ret)
}

// fn lower_func<W: Write>(prog: &Program, id: &FuncId, writer: &mut Writer<W>) -> Result<()> {
//     let def = &prog.funcs[id];
//     let symbols = &prog.func_symbols[id];

//     let body = match &def.body {
//         FuncBody::External(_) => return Ok(()),
//         FuncBody::Internal(block) => lower_block(prog, block)?,
//     };

//     // generate IFn and IState structs here

//     // let structs = match &def.calls.1 {
//     //     CallableId::Func()
//     // };

//     let generics = sep(
//         0..def.generics.num_params,
//         |i| format!("{}: IType", mangle_type_param(TypeParamId(i))),
//         ", ",
//     );

//     let args = sep(
//         def.args.iter(),
//         |(id, type_)| format!("{}: {}", mangle_arg(id), lower_type(prog, type_)),
//         ", ",
//     );

//     let ret = lower_type(prog, &def.ret);

//     let sig = format!("fn {}<{}>({}) -> {} {{", &symbols.name, generics, args, ret);
//     writer.writeln(sig.as_bytes())?;

//     writer.scope(|writer| writer.write(body.as_bytes()))?;
//     writer.writeln("}".as_bytes())?;

//     Ok(())
// }

/////////////////////////////////////////

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
filter_eq_action: Map<Stateless<i32>, (Stateless<i32>, Stateless<i32>)::Action> = {added: {2: (id, 8)}, removed: {1: (5, id)} }
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
