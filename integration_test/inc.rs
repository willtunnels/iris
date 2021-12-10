mod fns {
    impl IFn<(i32, i32), i32, super::states::add_state> for super::fns::add_fn {
        fn init(self, arg: (i32, i32)) -> (super::states::add_state, i32) {
            let v0 = arg.0;
            let v1 = arg.1;
            let v2 = v0 + v1;
            (add_state {}, v2)
        }
    }
    struct add_fn {}
}
mod states {
    impl IState<(i32, i32), i32> for super::states::add_state {
        fn next(
            &mut self,
            arg: <(i32, i32) as crate::burnt_toast::IType>::Action,
        ) -> <i32 as crate::burnt_toast::IType>::Action {
            let v0 = arg.0;
            let v1 = arg.1;
            let v2 = v0 + v1;
            v2
        }
    }
    struct add_state {}
}
