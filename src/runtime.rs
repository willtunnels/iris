use paste::paste;
use serde::{ser, Serialize};
use std::{collections::HashMap, marker::PhantomData};

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

pub trait IFn: Default {
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

#[derive(Clone, Debug, Serialize)]
pub struct IArray<T: IType> {
    items: im_rc::Vector<T>,
}

impl<T: IType> IArray<T> {
    pub fn new() -> Self {
        Self {
            items: im_rc::Vector::new(),
        }
    }

    pub fn push(&mut self, item: T) {
        self.items.push_back(item);
    }
}

#[macro_export]
macro_rules! iarray {
    () => { $crate::runtime::IArray::new() };

    ($($x:expr),* $(,)?) => {{
        let mut arr = $crate::runtime::IArray::new();
        $(
            arr.push($x);
        )*
            arr
    }};
}

#[derive(Clone, Debug)]
pub struct IArrayAction<T: IType> {
    pushes: Vec<T>,
    // The values are not actually optional. Here `Option` is used so that we can take out the value
    // and operate on it, avoiding a copy.
    updates: HashMap<usize, Option<T::Action>>,
}

impl<T: IType> IArrayAction<T> {
    pub fn new() -> Self {
        Self {
            pushes: Vec::new(),
            updates: HashMap::new(),
        }
    }

    pub fn push(&mut self, item: T) -> &mut Self {
        self.pushes.push(item);
        self
    }

    pub fn update(&mut self, idx: usize, action: T::Action) -> &mut Self {
        self.updates.insert(idx, Some(action));
        self
    }
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

#[derive(Clone, Debug)]
#[allow(non_camel_case_types)]
struct zip<T: IType>(PhantomData<T>);

impl<T: IType> Default for zip<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: IType> IFn for zip<T> {
    type X = (IArray<T>, IArray<T>);
    type Y = IArray<(T, T)>;
    type S = zip_state<T>;

    fn init(self, input: Self::X) -> (Self::S, Self::Y) {
        let state = zip_state {
            fst: input.0.clone(),
            snd: input.1.clone(),
        };
        let mut items = im_rc::Vector::new();
        for tup in input.0.items.into_iter().zip(input.1.items.into_iter()) {
            items.push_back(tup);
        }
        (state, IArray { items })
    }
}

#[derive(Clone, Debug, Serialize)]
#[allow(non_camel_case_types)]
struct zip_state<T: IType> {
    fst: IArray<T>,
    snd: IArray<T>,
}

impl<T: IType> IState for zip_state<T> {
    type X = (IArray<T>, IArray<T>);
    type Y = IArray<(T, T)>;

    fn next(&mut self, action: <Self::X as IType>::Action) -> <Self::Y as IType>::Action {
        let new_fst_len = self.fst.items.len() + action.0.pushes.len();
        let new_snd_len = self.snd.items.len() + action.1.pushes.len();
        todo!()
    }
}

#[derive(Clone, Debug)]
#[allow(non_camel_case_types)]
struct len<T: IType>(PhantomData<T>);

impl<T: IType> Default for len<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: IType> IFn for len<T> {
    type X = IArray<T>;
    type Y = usize_;
    type S = len_state<T>;

    fn init(self, input: Self::X) -> (Self::S, Self::Y) {
        let res = input.items.len();
        let state = len_state {
            old_len: res,
            phantom: PhantomData,
        };
        (state, Stateless(res))
    }
}

#[derive(Clone, Debug, Serialize)]
#[allow(non_camel_case_types)]
struct len_state<T: IType> {
    old_len: usize,
    phantom: PhantomData<T>,
}

impl<T: IType> IState for len_state<T> {
    type X = IArray<T>;
    type Y = usize_;

    fn next(&mut self, action: <Self::X as IType>::Action) -> <Self::Y as IType>::Action {
        self.old_len + action.pushes.len()
    }
}

#[derive(Clone, Debug, Default)]
#[allow(non_camel_case_types)]
struct filter_eq;

impl IFn for filter_eq {
    type X = IArray<(i32_, i32_)>;
    type Y = IArray<(i32_, i32_)>;
    type S = filter_eq_state;

    fn init(self, input: Self::X) -> (Self::S, Self::Y) {
        let mut origins = Vec::new();
        let mut arr = IArray::new();

        for (i, (x, y)) in input.items.iter().enumerate() {
            if x.0 == y.0 {
                origins.push(i);
                arr.items.push_back((x.clone(), y.clone()));
            }
        }
        let state = filter_eq_state {
            origins,
            arr: arr.clone(),
        };

        (state, arr)
    }
}

#[derive(Clone, Debug, Serialize)]
#[allow(non_camel_case_types)]
struct filter_eq_state {
    origins: Vec<usize>,
    arr: IArray<(i32_, i32_)>,
}

impl IState for filter_eq_state {
    type X = IArray<(i32_, i32_)>;
    type Y = IArray<(i32_, i32_)>;

    fn next(&mut self, action: <Self::X as IType>::Action) -> <Self::Y as IType>::Action {
        todo!()
    }
}

#[derive(Clone, Debug)]
#[allow(non_camel_case_types)]
struct get;

#[derive(Clone, Debug, Serialize)]
#[allow(non_camel_case_types)]
struct get_state<T: IType> {
    arr: IArray<T>,
    idx: usize,
}

impl<T: IType> IState for get_state<T> {
    type X = (IArray<T>, usize_);
    type Y = Replace<T>;
    fn next(&mut self, mut a: <Self::X as IType>::Action) -> <Self::Y as IType>::Action {
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
