use crate::ast::{self, resolved_ast as res};
use crate::util::id_type::id_type;
use crate::util::id_vec::IdVec;

id_type! { TypeVarId(u32); }

enum Constraint {
    Eq(TypeVarId, TypeVarId),
}

struct ConstraintGraph {
    next_var_id: u32,
    constraints: Vec<Constraint>,
}

impl ConstraintGraph {
    fn new() -> Self {
        Self {
            next_var_id: 0,
            constraints: Vec::new(),
        }
    }

    fn fresh(&mut self) -> TypeVarId {
        let id = TypeVarId(self.next_var_id);
        self.next_var_id += 1;
        id
    }

    fn constrain(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    fn solve() -> Solution {
        todo!()
    }
}

struct Solution {
    types: IdVec<TypeVarId, res::Type>,
}

impl Solution {
    fn get_type(&self, var: TypeVarId) -> &res::Type {
        &self.types[var]
    }
}

fn gen_constraints_prog(graph: &mut ConstraintGraph, prog: &res::Program) {
    for (_, func) in &prog.funcs {
        gen_constraints_block(graph, func, &func.body);
    }
}

fn gen_constraints_block(graph: &mut ConstraintGraph, func: &res::FuncDef, block: &res::Block) {
    todo!()
}

fn gen_constraints_expr(graph: &mut ConstraintGraph, expr: &res::Expr) {}
