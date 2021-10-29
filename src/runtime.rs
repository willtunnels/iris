use paste::paste;
use serde::Serialize;

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
    fn init(self, input: X::Action) -> (S, Y);
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
