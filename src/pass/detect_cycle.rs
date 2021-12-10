use crate::ast::lam_lifted_ast as lam;

enum Color {
    White,
    Gray,
    Black,
}

impl Default for Color {
    fn default() -> Self {
        Self::White
    }
}

pub fn detect_cycle(_prog: &lam::Program) {
    // let call_graph = build_call_graph(prog);
}
