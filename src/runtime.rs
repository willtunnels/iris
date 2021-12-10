use paste::paste;
use serde::{ser, Serialize};
use std::collections::HashMap;

/// Definition:
///
///   Let f: X -> Y, x: X, s: IState<X, Y>. Let y = f(x).
///
///   We say s "models" f at x if for all a: X::Action, x': X such that
///
///     x.apply(a) == Some(x')
///
///   we have
///
///     y.apply(s.next(a).0) == Some(f(x'))
///
///   and s.next(a).1 models f at x'.
///
/// Definition:
///
///   Let f: X -> Y, g: IFn<X, Y>. We say g "models" f if for all x: X we have
///
///     g.init(x).0 == f(x)
///
///   and g.init(x).1 models f at x.

pub trait IType: Sized + Clone + ser::Serialize {
    type Action: Clone;

    fn id(x: Self) -> Self::Action;

    fn is_id_hint(_: Self::Action) -> bool {
        false
    }

    fn compose(a1: Self::Action, a2: Self::Action) -> Option<Self::Action>;

    fn apply(self, a: Self::Action) -> Option<Self>;
}

pub trait IState: ser::Serialize {
    type X: IType;
    type Y: IType;
    fn next(
        &mut self,
        action: <<Self as IState>::X as IType>::Action,
    ) -> <<Self as IState>::Y as IType>::Action;
}

pub trait IFn {
    type X: IType;
    type Y: IType;
    type S: IState<X = Self::X, Y = Self::Y>;
    fn init(self, input: Self::X) -> (Self::S, Self::Y);
}

pub trait IClosure<X, Y, S, F>: IType {
    type X: IType;
    type Y: IType;
    type S: IState<X = Self::X, Y = Self::Y>;
    type F: IFn<X = Self::X, Y = Self::Y, S = Self::S>;
    fn eval(self) -> F;
}

/*
struct If;

struct IfState<X> {
    cond: bool,
    tval: X,
    fval: X,
}

impl<X> IFn<(Stateless<bool>, X, X), X, IfState> for If
where
    X: IType,
{
    fn init(self, input: (Stateless<bool>, X, X)) -> (IfState<X>, X) {
        if input.0.val {
            (IfState { cond: input.0, tval: input.1.clone(), fval: input.2 }, input.1)
        } else {
            (IfState { cond: input.0, tval: input.1, fval: input.2.clone() }, input.2)
        }
    }
}

impl<X> IState<(Stateless<bool>, X, X), X> for IfState
where
    X: IType,
{
    fn next(&mut self, action: (Stateless<bool>, X, X)::Action) -> X::Action {
        if cond == action.0 {

        } else {

        }
    }
}
*/

/*
let x = ...;
let y = ...;
let z = ...;
if cond {
    f(x, z)
} else {
    f(y)
}

if_(cond, f(x, z), f(y))

fn if_(b: bool, left: X, right X) -> X {
    if b {
        left
    } else {
        right
    }
}

*/

#[derive(Clone, Debug, Serialize)]
struct IArray<T: IType> {
    items: im_rc::Vector<T>,
}

#[derive(Clone, Debug)]
struct IArrayAction<T: IType> {
    pushes: Vec<T>,
    // The values are not actually optional. Here `Option` is used so that we can take out the value
    // and operate on it, avoiding a copy.
    updates: HashMap<usize, Option<T::Action>>,
}

impl<T: IType> IType for IArray<T> {
    type Action = IArrayAction<T>;

    fn id(_: Self) -> Self::Action {
        IArrayAction {
            pushes: Vec::new(),
            updates: HashMap::new(),
        }
    }

    fn is_id_hint(a: Self::Action) -> bool {
        a.pushes.is_empty() && a.updates.is_empty()
    }

    fn compose(a1: Self::Action, mut a2: Self::Action) -> Option<Self::Action> {
        let mut pushes = a1.pushes;
        pushes.append(&mut a2.pushes);

        let mut updates = a1.updates;
        for (k, v2) in a2.updates {
            if let Some(v1) = updates.get_mut(&k) {
                *v1 = Some(T::compose(v1.take().unwrap(), v2.unwrap())?);
            } else {
                updates.insert(k, v2);
            }
        }
        Some(IArrayAction { pushes, updates })
    }

    fn apply(mut self, a: Self::Action) -> Option<Self> {
        for item in a.pushes {
            self.items.push_back(item);
        }
        for (k, v) in a.updates {
            let item = &mut self.items[k];
            *item = item.clone().apply(v.unwrap())?;
        }
        Some(self)
    }
}

#[derive(Clone, Copy, Debug)]
struct ArrayGet;

#[derive(Clone, Debug, Serialize)]
struct ArrayGetState<T: IType> {
    arr: IArray<T>,
    idx: usize,
}

impl<T: IType> IState for ArrayGetState<T> {
    type X = (IArray<T>, usize_);
    type Y = Replace<T>;
    fn next(&mut self, mut a: <(IArray<T>, usize_) as IType>::Action) -> ReplaceAction<T> {
        if a.1 == self.idx {
            match a.0.updates.get_mut(&a.1) {
                None => ReplaceAction::Update(T::id(self.arr.items[a.1].clone())),
                Some(item) => ReplaceAction::Update(item.take().unwrap()),
            }
        } else {
            let mut result = ReplaceAction::Replace(self.arr.items[a.1].clone());
            if let Some(item) = a.0.updates.get_mut(&a.1) {
                let update = ReplaceAction::Update(item.take().unwrap());
                // TODO: should we really unwrap the `compose`?
                result = Replace::compose(result, update).unwrap();
            }
            result
        }
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

#[derive(Debug, Clone, Serialize)]
pub struct Stateless<T: Clone + ser::Serialize>(pub T);

impl<T: Clone + ser::Serialize> IType for Stateless<T> {
    type Action = T;

    fn id(x: Self) -> T {
        x.0
    }

    fn compose(_: T, a2: T) -> Option<T> {
        Some(a2)
    }

    fn apply(self, a: T) -> Option<Self> {
        Some(Stateless(a))
    }
}

#[allow(non_camel_case_types)]
pub type bool_ = Stateless<bool>;
#[allow(non_camel_case_types)]
pub type i8_ = Stateless<i8>;
#[allow(non_camel_case_types)]
pub type i16_ = Stateless<i16>;
#[allow(non_camel_case_types)]
pub type i32_ = Stateless<i32>;
#[allow(non_camel_case_types)]
pub type i64_ = Stateless<i64>;
#[allow(non_camel_case_types)]
pub type isize_ = Stateless<isize>;
#[allow(non_camel_case_types)]
pub type u8_ = Stateless<u8>;
#[allow(non_camel_case_types)]
pub type u16_ = Stateless<u16>;
#[allow(non_camel_case_types)]
pub type u32_ = Stateless<u32>;
#[allow(non_camel_case_types)]
pub type u64_ = Stateless<u64>;
#[allow(non_camel_case_types)]
pub type usize_ = Stateless<usize>;
#[allow(non_camel_case_types)]
pub type f32_ = Stateless<f32>;
#[allow(non_camel_case_types)]
pub type f64_ = Stateless<f64>;
#[allow(non_camel_case_types)]
pub type char_ = Stateless<char>;
#[allow(non_camel_case_types)]
pub type str_ = Stateless<String>;

#[derive(Debug, Clone, Serialize)]
pub struct EqGuard<T>(pub T);

#[derive(Clone)]
pub enum EqGuardAction<T: IType> {
    Id,
    Update(T::Action),
}

impl<T: IType> IType for EqGuard<T> {
    type Action = EqGuardAction<T>;

    fn id(_: Self) -> Self::Action {
        EqGuardAction::Id
    }

    fn is_id_hint(a: Self::Action) -> bool {
        match a {
            EqGuardAction::Id => true,
            EqGuardAction::Update(a) => T::is_id_hint(a),
        }
    }

    fn compose(a1: Self::Action, a2: Self::Action) -> Option<Self::Action> {
        Some(match (a1, a2) {
            (EqGuardAction::Id, a2) => a2,
            (a1, EqGuardAction::Id) => a1,
            (EqGuardAction::Update(a1), EqGuardAction::Update(a2)) => {
                EqGuardAction::Update(T::compose(a1, a2)?)
            }
        })
    }

    fn apply(self, a: Self::Action) -> Option<Self> {
        Some(match a {
            EqGuardAction::Id => self,
            EqGuardAction::Update(a) => EqGuard(self.0.apply(a)?),
        })
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct Replace<T: IType>(pub T);

#[derive(Clone, Debug)]
pub enum ReplaceAction<T: IType> {
    Replace(T),
    Update(T::Action),
}

impl<T: IType> IType for Replace<T> {
    type Action = ReplaceAction<T>;

    fn id(x: Self) -> Self::Action {
        ReplaceAction::Update(T::id(x.0))
    }

    fn is_id_hint(a: Self::Action) -> bool {
        use ReplaceAction as A;
        match a {
            // TODO: require `IType: Eq`?
            A::Replace(_) => false,
            A::Update(a) => T::is_id_hint(a),
        }
    }

    fn compose(a1: Self::Action, a2: Self::Action) -> Option<Self::Action> {
        use ReplaceAction as A;
        Some(match (a1, a2) {
            (_, A::Replace(a2)) => A::Replace(a2),
            (A::Replace(a1), A::Update(a2)) => A::Replace(a1.apply(a2).unwrap()), // TODO: review
            (A::Update(a1), A::Update(a2)) => A::Update(T::compose(a1, a2).unwrap()),
        })
    }

    fn apply(self, a: Self::Action) -> Option<Self> {
        use ReplaceAction as A;
        Some(match a {
            A::Replace(a) => Replace(a),
            A::Update(a) => Replace(self.0.apply(a)?),
        })
    }
}
