use iris::iris_mod;
use iris::runtime::{IFn, IState, IType, Stateless};

iris_mod!(inc);

fn test_call_add() {
    let call_add = inc::fns::call_add::default();
    let (mut state, res) = call_add.init((Stateless(1), Stateless(2)));
    assert_eq!(res.0, 6);
    let res = res.apply(state.next((3, 4))).unwrap();
    assert_eq!(res.0, 14);
}

fn test_use_lets() {
    let use_lets = inc::fns::use_lets::default();
    let (mut state, res) = use_lets.init((Stateless(1), Stateless(2)));
    assert_eq!(res.0, 330);
    let res = res.apply(state.next((3, 4))).unwrap();
    assert_eq!(res.0, 370);
}

// fn test_call_extern() {
//     let call_extern = inc::fns::call_add::default();
//     let (mut state, res) = call_extern.init((Stateless(1), Stateless(2)));
//     assert_eq!(res.0, 6);
//     let res = res.apply(state.next((3, 4))).unwrap();
//     assert_eq!(res.0, 14);
// }

fn main() {
    test_call_add();
    test_use_lets();
}
