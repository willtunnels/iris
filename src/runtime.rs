trait IType: Sized + Clone + 'static {
    type Action;

    fn id(x: Self) -> Self::Action;

    fn is_id_hint(_: Self::Action) -> bool {
        false
    }

    fn compose(a1: Self::Action, a2: Self::Action) -> Option<Self::Action>;

    fn apply(self, a: Self::Action) -> Option<Self>;
}

struct IState<X: IType, Y: IType> {
    next: Box<dyn Fn(X::Action) -> (Y::Action, IState<X, Y>)>,
}

fn istate<X, Y, F>(next: F) -> IState<X, Y>
where
    X: IType,
    Y: IType,
    F: Fn(X::Action) -> (Y::Action, IState<X, Y>) + 'static,
{
    IState {
        next: Box::new(next),
    }
}

struct IFn<X: IType, Y: IType> {
    from_scratch: Box<dyn Fn(X) -> (Y, IState<X, Y>)>,
}

fn ifn<X, Y, F>(from_scratch: F) -> IFn<X, Y>
where
    X: IType,
    Y: IType,
    F: Fn(X) -> (Y, IState<X, Y>) + 'static,
{
    IFn {
        from_scratch: Box::new(from_scratch),
    }
}

fn id<X: IType>() -> IFn<X, X> {
    fn state<X: IType>(a: X::Action) -> (X::Action, IState<X, X>) {
        (a, istate(state))
    }
    ifn(|x| (x, istate(state)))
}

fn compose<X, Y, Z>(f: IFn<X, Y>, g: IFn<Y, Z>) -> IFn<X, Z>
where
    X: IType,
    Y: IType,
    Z: IType,
{
    fn mk_state<X, Y, Z>(state_f: IState<X, Y>, state_g: IState<Y, Z>) -> IState<X, Z>
    where
        X: IType,
        Y: IType,
        Z: IType,
    {
        istate(move |a_x| {
            let (a_y, state_f) = (state_f.next)(a_x);
            let (a_z, state_g) = (state_g.next)(a_y);
            (a_z, mk_state(state_f, state_g))
        })
    }
    ifn(move |x| {
        let (y, state_f) = (f.from_scratch)(x);
        let (z, state_g) = (g.from_scratch)(y);
        (z, mk_state(state_f, state_g))
    })
}

impl<X: IType, Y: IType> IType for (X, Y) {
    type Action = (X::Action, Y::Action);

    fn id(t: Self) -> Self::Action {
        (X::id(t.0), Y::id(t.1))
    }

    fn is_id_hint(a: Self::Action) -> bool {
        X::is_id_hint(a.0) && Y::is_id_hint(a.1)
    }

    fn compose(a1: Self::Action, a2: Self::Action) -> Option<Self::Action> {
        Some((X::compose(a1.0, a2.0)?, Y::compose(a1.1, a2.1)?))
    }

    fn apply(self, a: Self::Action) -> Option<Self> {
        Some((self.0.apply(a.0)?, self.1.apply(a.1)?))
    }
}

fn map_pair<X1, Y1, X2, Y2>(f: IFn<X1, Y1>, g: IFn<X2, Y2>) -> IFn<(X1, X2), (Y1, Y2)>
where
    X1: IType,
    X2: IType,
    Y1: IType,
    Y2: IType,
{
    fn mk_state<X1, Y1, X2, Y2>(
        state_f: IState<X1, Y1>,
        state_g: IState<X2, Y2>,
    ) -> IState<(X1, X2), (Y1, Y2)>
    where
        X1: IType,
        X2: IType,
        Y1: IType,
        Y2: IType,
    {
        istate(move |(a_x, a_y)| {
            let (a_x, state_f) = (state_f.next)(a_x);
            let (a_y, state_g) = (state_g.next)(a_y);
            ((a_x, a_y), mk_state(state_f, state_g))
        })
    }
    ifn(move |(x, y)| {
        let (x, state_f) = (f.from_scratch)(x);
        let (y, state_g) = (g.from_scratch)(y);
        ((x, y), mk_state(state_f, state_g))
    })
}

fn proj0<X: IType, Y: IType>() -> IFn<(X, Y), X> {
    fn state<X: IType, Y: IType>(a: <(X, Y) as IType>::Action) -> (X::Action, IState<(X, Y), X>) {
        (a.0, istate(state))
    }
    ifn(|(x, _)| (x, istate(state)))
}

fn proj1<X: IType, Y: IType>() -> IFn<(X, Y), Y> {
    fn state<X: IType, Y: IType>(a: <(X, Y) as IType>::Action) -> (Y::Action, IState<(X, Y), Y>) {
        (a.1, istate(state))
    }
    ifn(|(_, y)| (y, istate(state)))
}

#[derive(Clone)]
struct NoCache<X> {
    val: X,
}

impl<X> NoCache<X> {
    fn new(val: X) -> Self {
        NoCache { val }
    }
}

impl<X: IType> IType for NoCache<X> {
    type Action = X;

    fn id(x: Self) -> X {
        x.val
    }

    fn compose(_: X, a2: X) -> Option<X> {
        Some(a2)
    }

    fn apply(self, a: X) -> Option<Self> {
        Some(NoCache::new(a))
    }
}

#[derive(Clone)]
struct EqGuard<X> {
    val: X,
}

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

fn check_eq<X: IType + PartialEq>() -> IFn<NoCache<X>, EqGuard<X>> {
    fn mk_state<X: IType + PartialEq>(old: X) -> IState<NoCache<X>, EqGuard<X>> {
        istate(move |new| {
            if old == new {
                (EqGuardAction::Same, mk_state(old.clone()))
            } else {
                (EqGuardAction::Modify(X::id(new.clone())), mk_state(new))
            }
        })
    }
    ifn(|x: NoCache<X>| (EqGuard::new(x.val.clone()), mk_state(x.val)))
}
