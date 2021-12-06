use crate::ast::{self, resolved_ast as res};
use crate::util::id_type::id_type;
use crate::util::id_vec::IdVec;

// TODO: enforce proper constraints on types used in arithmetic

id_type! { TypeVar(u32); }

#[derive(Debug, Clone, Copy)]
pub enum Error {
    Mismatch(String, String),
}

#[derive(Debug, Clone)]
enum UnifyError {
    Mismatch,
    Recursive,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Assign {
    Unknown,
    Equal(TypeVar),
    Param(ast::TypeParamId),
    App(ast::TypeId, Vec<TypeVar>),
    Tuple(Vec<TypeVar>),
    Func(Vec<TypeVar>, TypeVar),
}

#[derive(Clone, Debug)]
struct Context {
    // `RefCell` is used the implement the "occurs" check. `Context` does not actually have interior
    // mutability.
    vars: IdVec<TypeVar, RefCell<Assign>>,
}

impl Context {
    fn new() -> Self {
        Context { vars: IdVec::new() }
    }

    fn new_var(&mut self, assign: Assign) -> TypeVar {
        self.vars.push(RefCell::new(assign))
    }

    fn obtain(&self, var: TypeVar) -> Result<RefMut<Assign>, UnifyError> {
        self.vars[var]
            .try_borrow_mut()
            .map_err(|_| UnifyError::Recursive)
    }

    fn follow(&self, var: TypeVar) -> Result<TypeVar, UnifyError> {
        let mut assign = self.obtain(var)?;
        if let Assign::Equal(curr_dest) = assign.deref_mut() {
            *curr_dest = self.follow(*curr_dest)?;
            Ok(*curr_dest)
        } else {
            Ok(var)
        }
    }

    fn unify_rec(&self, root_var1: TypeVar, root_var2: TypeVar) -> Result<(), UnifyError> {
        let var1 = self.follow(root_var1)?;
        let var2 = self.follow(root_var2)?;

        if var1 == var2 {
            return Ok(());
        }

        let mut assign1 = self.obtain(var1)?;
        let mut assign2 = self.obtain(var2)?;

        match (assign1.deref(), assign2.deref()) {
            (Assign::Equal(_), _) => unreachable!(),
            (_, Assign::Equal(_)) => unreachable!(),

            (Assign::Unknown, _) => {
                *assign1 = Assign::Equal(var2);
                return Ok(());
            }

            (_, Assign::Unknown) => {
                *assign2 = Assign::Equal(var1);
                return Ok(());
            }

            (Assign::Param(param1), Assign::Param(param2)) => {
                if param1 != param2 {
                    return Err(UnifyError::Mismatch);
                }
            }

            (Assign::App(id1, args1), Assign::App(id2, args2)) => {
                if id1 != id2 {
                    return Err(UnifyError::Mismatch);
                }

                if args1.len() != args2.len() {
                    return Err(UnifyError::Mismatch);
                }

                for (&arg1, &arg2) in args1.iter().zip(args2.iter()) {
                    self.unify_rec(arg1, arg2)?;
                }
            }

            (Assign::Tuple(items1), Assign::Tuple(items2)) => {
                if items1.len() != items2.len() {
                    return Err(UnifyError::Mismatch);
                }

                for (&item1, &item2) in items1.iter().zip(items2.iter()) {
                    self.unify_rec(item1, item2)?;
                }
            }

            (Assign::Func(args1, ret1), Assign::Func(args2, ret2)) => {
                if args1.len() != args2.len() {
                    return Err(UnifyError::Mismatch);
                }

                for (&arg1, &arg2) in args1.iter().zip(args2.iter()) {
                    self.unify_rec(arg1, arg2);
                }

                self.unify_rec(*ret1, *ret2)?;
            }

            _ => {
                return Err(UnifyError::Mismatch);
            }
        }

        // This is purely an optimization
        *assign1 = Assign::Equal(var2);

        Ok(())
    }

    fn unify(&mut self, expected: TypeVar, actual: TypeVar) -> Result<(), RawError> {
        self.unify_rec(expected, actual).map_err(|err| match err {
            UnifyError::Recursive => RawErrorKind::Recursive.into(),
            UnifyError::Mismatch => RawErrorKind::Mismatch { expected, actual }.into(),
        })
    }
}
