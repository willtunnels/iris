use paste::paste;
use serde::Serialize;
use std::{collections::HashMap, marker::PhantomData};

/*

Definition:

  Let f: X -> Y, x: X, s: IState<X, Y>.  Let y = f(x).

  We say s "models" f at x if for all a: X::Action, x': X such that

    x.apply(a) == Some(x')

  we have

    y.apply(s.next(a).0) == Some(f(x'))

  and s.next(a).1 models f at x'.

Definition:

  Let f: X -> Y, g: IFn<X, Y>.  We say g "models" f if for all x: X we have

    g.init(x).0 == f(x)

  and g.init(x).1 models f at x.

//////////////////////////////////////////////////////////////////////////////////

fn foo(x: ...) -> ... {
    ...
    let y = f(x);
    let z = h(g(y), x);
    z
    ...
}

struct Block0Fn {
  y_fn: FFn,
  temp0_fn: GFn,
  z_fn: HFn,
}

struct Block0State {
  y_state: FState,
  temp0_state: GState,
  z_state: HState,
}

impl<X: IType, Z: IType, S: IState<X, Y>> IFn<X, Z, S> for Block0Fn {
  fn init(self, input: X) -> (S, Z) {
    let (y_state, y_out) = self.y_fn.init(input.clone());
    let (temp0_state, temp0_out) = self.temp0_fn.init(y_out);
    let (z_state, z_out) = self.z_fn.init((temp0_out, input));
    (Block0State { y_state, temp0_state, z_state }, z_out)
  }
}

impl IState for Block0State {
  fn next(&mut self, action: X::Action) -> Z::Action {
    let y_action = self.y_state.next(action.clone());
    let temp0_fn_action = self.temp0_state.next(y_action);
    let z_action = self.z_state.next((temp0_fn_aciton, action));
    z_action
  }
}

//////////////////////////////////////////////////////////////////////////////////

fn twice<T, F: T -> T>(f: F, x: T) -> T {
  f(f(x))
}

fn foo() {
    ...
    twice(|x| x + y, 5);
    ...
}

struct TwiceFn<T, F>(PhantomData(T), PhantomData(F));

struct TwiceState<T, F: IClosure<T, T>> {
  temp0_state: F::Eval::State,
  temp1_state: F::Eval::State,
}

impl<T: IType, F: IClosure<T, T>> IFn<(F, T), T> for TwiceFn<T, F> {
  type State = TwiceState<T, F>;
  fn init(self, input: (F, T)) -> (Self::State, T) {
    let (temp0_state, temp0) = F::eval().init((input.0.clone(), input.1));
    let (temp1_state, temp1) = F::eval().init((input.0, input, temp0));
    (TwiceState { temp0_state, temp1_state }, temp1)
  }
}

impl<T: IType, F: IClosure<T, T>> IState<(F, T), T> for TwiceState<T, F> {
  fn next(&mut self, action: (F::Action, T::Action)) -> T::Action {
    let temp0 = self.temp0_state.next(action.0.clone(), action.1);
    let temp1 = self.temp1_state.next(action.0, temp0);
    temp1
  }
}

*/

trait IType: Sized + Clone {
    type Action: Clone;

    fn id(x: Self) -> Self::Action;

    fn is_id_hint(_: Self::Action) -> bool {
        false
    }

    fn compose(a1: Self::Action, a2: Self::Action) -> Option<Self::Action>;

    fn apply(self, a: Self::Action) -> Option<Self>;
}

trait IState<X, Y>: Serialize
where
    X: IType,
    Y: IType,
{
    fn next(&mut self, action: X::Action) -> Y::Action;
}

trait IFn<X, Y, S>
where
    X: IType,
    Y: IType,
    S: IState<X, Y>,
{
    fn init(self, input: X) -> (S, Y);
}

trait IClosure<X, Y, S, F>: IType
where
    X: IType,
    Y: IType,
    S: IState<X, Y>,
    F: IFn<X, Y, S>,
{
    fn eval() -> F;
}

#[derive(Clone, Debug, Serialize)]
struct IVec<T> {
    items: Vec<T>,
}

#[derive(Clone, Debug)]
struct IVecAction<T: IType> {
    insertions: Vec<T>,
    updates: HashMap<usize, T::Action>,
    // removes: Vec<usize>,
}

impl<T: IType> IType for IVec<T> {
    type Action = IVecAction<T>;

    fn id(_: Self) -> Self::Action {
        IVecAction {
            insertions: Vec::new(),
            updates: HashMap::new(),
            // removes: Vec::new(),
        }
    }

    fn is_id_hint(a: Self::Action) -> bool {
        a.insertions.is_empty() && a.updates.is_empty() /* && a.removes.is_empty() */
    }

    /*
     vec = [1, 1, 1]
     a1 = { updates: {1: 2}, removes: [0] }
     a2 = { removes: [1], updates: {0: 3} }

     vec.apply(a1) = [2, 1]
     vec.apply(a2) = [3]

     compose(a1, a2) = { removes: [0, 1], updates: {0: 3}  }

     Maybe: remove a1 updates to indices removed by a1&a2 in `compose` and in `apply` do removals and then updates?
    */

    fn compose(a1: Self::Action, a2: Self::Action) -> Option<Self::Action> {
        let insertions = [a1.insertions, a2.insertions].concat();
        let mut updates = a1.updates.clone();
        for (k, v2) in a2.updates {
            updates
                .entry(k)
                .and_modify(|v1| *v1 = T::compose(v1.clone(), v2.clone()).unwrap())
                .or_insert(v2);
        }
        // let removes = [a1.removes, a2.removes].concat();
        Some(IVecAction {
            insertions,
            updates,
            // removes,
        })
    }

    fn apply(mut self, a: Self::Action) -> Option<Self> {
        for item in a.insertions {
            self.items.push(item);
        }
        for (k, v) in a.updates {
            self.items[k] = self.items[k].clone().apply(v).unwrap();
        }
        Some(self)
    }
}

#[derive(Clone, Debug)]
struct Replace<T> {
    val: T,
}

#[derive(Clone, Debug)]
enum ReplaceAction<T: IType> {
    Replace(T),
    Modify(T::Action),
}

impl<T: IType> IType for Replace<T> {
    type Action = ReplaceAction<T>;

    fn id(x: Self) -> Self::Action {
        ReplaceAction::Modify(T::id(x.val))
    }

    fn is_id_hint(a: Self::Action) -> bool {
        use ReplaceAction as A;
        match a {
            // TODO: require `IType: Eq`?
            A::Replace(_) => false,
            A::Modify(a) => T::is_id_hint(a),
        }
    }

    fn compose(a1: Self::Action, a2: Self::Action) -> Option<Self::Action> {
        use ReplaceAction as A;
        Some(match (a1, a2) {
            (_, A::Replace(a2)) => A::Replace(a2),
            (A::Replace(a1), A::Modify(a2)) => A::Replace(a1.apply(a2).unwrap()), // TODO: review
            (A::Modify(a1), A::Modify(a2)) => A::Modify(T::compose(a1, a2).unwrap()),
        })
    }

    fn apply(self, a: Self::Action) -> Option<Self> {
        use ReplaceAction as A;
        Some(match a {
            A::Replace(a) => Replace { val: a },
            A::Modify(a) => Replace {
                val: self.val.apply(a).unwrap(),
            },
        })
    }
}

#[derive(Clone, Copy, Debug)]
struct VecGet;

#[derive(Clone, Debug, Serialize)]
struct VecGetState<T> {
    vec: IVec<T>,
    idx: usize,
}

type Idx = Stateless<usize>;

impl<T: IType + Serialize> IState<(IVec<T>, Idx), Replace<T>> for VecGetState<T> {
    fn next(&mut self, a: <(IVec<T>, Idx) as IType>::Action) -> ReplaceAction<T> {
        if a.1 == self.idx {
            match a.0.updates.get(&a.1) {
                None => ReplaceAction::Modify(T::id(self.vec.items[a.1])),
                Some(item_a) => ReplaceAction::Modify(item_a.clone()),
            }
        } else {
            let ret_a = ReplaceAction::Replace(self.vec.items[a.1].clone());
            if let Some(item_a) = a.0.updates.get(&a.1) {
                ret_a = Replace::compose(ret_a, ReplaceAction::Modify(item_a.clone())).unwrap();
            }
            ret_a
        }
    }
}

/*

////////////////////////////////////////
// Written in our langague
////////////////////////////////////////

extern IVec<T> = ::crate::runtime::IVec;
fn sum(vec: IVec<i32>, i: Stateless<i32>, j: Stateless<i32>) -> i32 {
   add(vec_get(vec, i), vec_get(vec, j))
}

////////////////////////////////////////
// Compiler output (which is Rust code):
////////////////////////////////////////

#[derive(Clone, Debug, Serialize)]
IStateSum {
    s0: VecGetState<Stateless<i32>>,
    s1: VecGetState<Stateless<i32>>,
    s2: AddState<Stateless<i32>, Stateless<i32>>,
}

impl IState<(IVec<Stateless<i32>>, Stateless<i32>, Stateless<i32>), Stateless<i32>> for IStateSum {
    fn next(&mut self, a: (IVec<Stateless<i32>>, Stateless<i32>, Stateless<i32>)::Action) -> Stateless<i32>::Action {
        let fst = self.s0.next((a.0.clone(), a.1));
        let snd = self.s1.next((a.0, a.2));
        self.next((fst, snd))
    }
}

#[derive(Clone, Debug, Serialize, Default)]
IFnSum {
    f0: VecGetFn<Stateless<i32>>,
    f1: VecGetFn<Stateless<i32>>,
    f2: AddFn<Stateless<i32>, Stateless<i32>>,
}

impl IFnSum {
    fn new() -> Self {
        Self {
            f0: VecGetFn::new(),
            f1: VecGetFn::new(),
            f2: AddFn::new(),
        }
    }
}

impl IFn<(IVec<Stateless<i32>>, Stateless<i32>, Stateless<i32>), Stateless<i32>> for IFnSum {
    fn init(self, args: (IVec<Stateless<i32>>, Stateless<i32>, Stateless<i32>)) -> (IStateSum, Stateless<i32>) {
        let (s0, fst) = self.f0.init((args.0.clone(), args.1.clone()));
        let (s1, snd) = self.f1.init((args.0.clone(), args.2.clone()));
        let (s2, res) = self.f2.init((args.1, args.2));
        (IStateSum { s0, s1, s2 }, res)
    }
}

////////////////////////////////////////
// Written in Rust
////////////////////////////////////////

include_source!(inc);
fn make_change() {
    // Create the initial state
    let v: IVec<Stateless<i32>> = IVec { items: vec![0, 1, 2] };
    let i = Stateless { val: 0 }; // A Stateless<i32> action is an i32
    let j = Stateless { val: 1 };

    // Run the pipeline
    let sum = inc::IFnSum::new();
    let (res, state) = sum.init((v, i, j));

    // Create a diff on the initial state
    let mut modifications = HashMap::new();
    modifications.insert(0, 7);
    let a = IVecAction { modifications, pushes: vec![...] }

    // Rerun the pipeline incrementally
    let new_res = res.apply(state.next((a, 0, 1)));
}

////////////////////////////////////////
// A more interesting example
////////////////////////////////////////

fn sum(vec: IVec<i32>, i: i32, j: i32) -> i32 {
   add(expensive(vec_get(vec, i)), expensive(vec_get(vec, j)))
}

*/

// ...
// let x = get(vec, i)
// ...

// get(vec: IVec<T>, index_to_remove: usize) -> T

// impl<T: IType + Serialize> IFn<(IVec<T>, EqGuard<Idx>), Replace<T>, VecGetState<T>> for VecGet {
//     fn init(self, a: (IVec<T>, EqGuard<Idx>)) -> (VecGetState<T>, Replace<T>) {
//         let val = a.0.items[a.1.val.val].clone();
//         (
//             VecGetState {
//                 vec: a.0,
//                 idx: a.1.val.val,
//             },
//             Replace { val },
//         )
//     }
// }

impl<T: IType> IType for Box<T> {
    type Action = T::Action;

    fn id(x: Self) -> Self::Action {
        T::id(*x)
    }

    fn is_id_hint(a: Self::Action) -> bool {
        T::is_id_hint(a)
    }

    fn compose(a1: Self::Action, a2: Self::Action) -> Option<Self::Action> {
        T::compose(a1, a2)
    }

    fn apply(self, a: Self::Action) -> Option<Self> {
        (*self).apply(a).map(Box::new)
    }
}

macro_rules! impl_itype_tuple {
    ($($name:ident)+) => {
        #[allow(non_snake_case)]
        impl<$($name: IType),+> IType for ($($name,)+) {
            type Action = ($($name::Action),+);

            fn id(tuple: Self) -> Self::Action {
                let ($($name,)+) = tuple;
                ($($name::id($name)),+)
            }

            fn is_id_hint(a: Self::Action) -> bool {
                let ($($name,)+) = a;
                let mut b = true;
                $(b = b && $name::is_id_hint($name);)+
                b
            }

            fn compose(a1: Self::Action, a2: Self::Action) -> Option<Self::Action> {
                paste! {
                    let ($([<a1_ $name>],)+) = a1;
                    let ($([<a2_ $name>],)+) = a2;
                    Some(($($name::compose([<a1_ $name>], [<a2_ $name>])?),+))
                }
            }

            fn apply(self, a: Self::Action) -> Option<Self> {
                paste! {
                    let ($($name,)+) = self;
                    let ($([<a_ $name>],)+) = a;
                    Some(($($name.apply([<a_ $name>])?),+))
                }
            }
        }
    };
}

impl_itype_tuple! { E0 E1 }
impl_itype_tuple! { E0 E1 E2 }
impl_itype_tuple! { E0 E1 E2 E3 }
impl_itype_tuple! { E0 E1 E2 E3 E4 }
impl_itype_tuple! { E0 E1 E2 E3 E4 E5 }
impl_itype_tuple! { E0 E1 E2 E3 E4 E5 E6 }
impl_itype_tuple! { E0 E1 E2 E3 E4 E5 E6 E7 }
impl_itype_tuple! { E0 E1 E2 E3 E4 E5 E6 E7 E8 }
impl_itype_tuple! { E0 E1 E2 E3 E4 E5 E6 E7 E8 E9 }
impl_itype_tuple! { E0 E1 E2 E3 E4 E5 E6 E7 E8 E9 E10}
impl_itype_tuple! { E0 E1 E2 E3 E4 E5 E6 E7 E8 E9 E10 E11 }
impl_itype_tuple! { E0 E1 E2 E3 E4 E5 E6 E7 E8 E9 E10 E11 E12 }
impl_itype_tuple! { E0 E1 E2 E3 E4 E5 E6 E7 E8 E9 E10 E11 E12 E13 }
impl_itype_tuple! { E0 E1 E2 E3 E4 E5 E6 E7 E8 E9 E10 E11 E12 E13 E14 }
impl_itype_tuple! { E0 E1 E2 E3 E4 E5 E6 E7 E8 E9 E10 E11 E12 E13 E14 E15 }
impl_itype_tuple! { E0 E1 E2 E3 E4 E5 E6 E7 E8 E9 E10 E11 E12 E13 E14 E15 E16 }

#[derive(Clone)]
struct Stateless<X> {
    val: X,
}

impl<X> Stateless<X> {
    fn new(val: X) -> Self {
        Stateless { val }
    }
}

impl<X: Clone> IType for Stateless<X> {
    type Action = X;

    fn id(x: Self) -> X {
        x.val
    }

    fn compose(_: X, a2: X) -> Option<X> {
        Some(a2)
    }

    fn apply(self, a: X) -> Option<Self> {
        Some(Stateless::new(a))
    }
}

#[derive(Clone)]
struct EqGuard<X> {
    val: X,
}

#[derive(Clone)]
enum EqGuardAction<X: IType> {
    Same,
    Modify(X::Action),
}

impl<X> EqGuard<X> {
    fn new(val: X) -> Self {
        EqGuard { val }
    }
}

impl<X: IType> IType for EqGuard<X> {
    type Action = EqGuardAction<X>;

    fn id(_: Self) -> Self::Action {
        EqGuardAction::Same
    }

    fn is_id_hint(a: Self::Action) -> bool {
        match a {
            EqGuardAction::Same => true,
            EqGuardAction::Modify(a) => X::is_id_hint(a),
        }
    }

    fn compose(a1: Self::Action, a2: Self::Action) -> Option<Self::Action> {
        Some(match (a1, a2) {
            (EqGuardAction::Same, a2) => a2,
            (a1, EqGuardAction::Same) => a1,
            (EqGuardAction::Modify(a1), EqGuardAction::Modify(a2)) => {
                EqGuardAction::Modify(X::compose(a1, a2)?)
            }
        })
    }

    fn apply(self, a: Self::Action) -> Option<Self> {
        Some(match a {
            EqGuardAction::Same => self,
            EqGuardAction::Modify(a) => EqGuard::new(self.val.apply(a)?),
        })
    }
}
