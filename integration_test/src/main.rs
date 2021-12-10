use burnt_toast::burnt_toast_mod;
use burnt_toast::runtime::{IFn, IState, IType, Stateless};

burnt_toast_mod!(inc);

fn main() {
    let add = inc::fns::add {};
    let (mut state, res) = add.init((Stateless(1), Stateless(2)));
    println!("{}", res.0);

    let res = res.apply(state.next((3, 4))).unwrap();
    println!("{}", res.0);
}
