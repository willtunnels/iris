use crate::util::id_type::Id;
use crate::util::id_vec::IdVec;
use std::cell::Cell;

pub trait Union {
    // Properties:
    // (1) Should be commutative since `DisjointSets` does not guarantee which order sets will be
    // merged in.
    // (2) `DisjoinSets` guarantees that a failed union operation leaves its stored data unchanged
    // iff this function does not change `self` upon failure.
    fn union(&mut self, other: Self) -> Result<(), ()>;
}

pub struct DisjoinSets<I, D> {
    nodes: IdVec<I, Node<I, D>>,
}

struct Node<I, D> {
    // Allows path compression without taking `self` mutably.
    parent: Cell<I>,
    rank: I,
    // Will be `None` except in the representative node of each equivalence class.
    data: Option<Box<D>>,
}

impl<I: Id + Copy, D: Union> DisjoinSets<I, D> {
    pub fn new() -> Self {
        Self {
            nodes: IdVec::new(),
        }
    }

    pub fn make_set(&mut self, data: D) -> I {
        let parent = self.nodes.len();
        let id = self.nodes.push(Node {
            parent: Cell::new(I::from_index_unchecked(parent)),
            rank: I::from_index_unchecked(0),
            data: Some(Box::new(data)),
        });

        debug_assert_eq!(id.to_index(), parent);
        id
    }

    pub fn union(&mut self, x: I, y: I) -> Result<(), ()> {
        let mut data_x = self.nodes[x].data.take().unwrap();
        let data_y = self.nodes[y].data.take().unwrap();
        if let Err(()) = data_x.union(*data_y) {}

        let rank_x = self.nodes[x].rank.to_index();
        let rank_y = self.nodes[y].rank.to_index();

        // Union by rank to keep trees short.
        if rank_x > rank_y {
            self.nodes[y].parent.set(x);
        } else if rank_x < rank_y {
            self.nodes[x].parent.set(y);
        } else {
            self.nodes[y].parent.set(x);
            let new_rank = I::from_index_unchecked(rank_y + 1);
            self.nodes[x].rank = new_rank;
        }

        Ok(())
    }

    fn find(&self, x: I) -> I {
        let node = &self.nodes[x];
        let mut parent = node.parent.get();

        if parent.to_index() != x.to_index() {
            // Apply path compression.
            parent = self.find(parent);
            node.parent.set(parent);
        }
        parent
    }

    pub fn data(&self, x: I) -> &D {
        let root = self.find(x);
        self.nodes[root].data.as_ref().unwrap().as_ref()
    }

    pub fn data_mut(&mut self, x: I) -> &mut D {
        let root = self.find(x);
        self.nodes[root].data.as_mut().unwrap().as_mut()
    }
}
