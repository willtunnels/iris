use crate::util::id_type::Id;
use crate::util::id_vec::IdVec;

pub struct Graph<N: Id> {
    adjacent: IdVec<N, Vec<N>>,
}
