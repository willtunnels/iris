use paste::paste;
use serde::Serialize;

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

impl<X: IType> IType for Stateless<X> {
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
