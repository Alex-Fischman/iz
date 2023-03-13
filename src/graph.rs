extern crate std;
use crate::map::{Key, Map};
use std::option::Option;

pub struct Graph<N: Key, E>(Map<N, Map<N, E>>);

impl<N: Key, E> Graph<N, E> {
    pub fn new() -> Graph<N, E> {
        Graph(Map::new())
    }

    pub fn node(&mut self, node: N) -> Option<Map<N, E>> {
        self.0.insert(node, Map::new())
    }

    pub fn edge(&mut self, parent: N, child: N, edge: E) {
        assert!(self.0.contains_key(&parent), "missing parent");
        assert!(self.0.contains_key(&child), "missing child");
        self.0.get_mut(&parent).unwrap().insert(child, edge);
    }

    pub fn children(&self, parent: &N) -> &Map<N, E> {
        self.0.get(parent).unwrap()
    }

    pub fn children_mut(&mut self, parent: &N) -> &mut Map<N, E> {
        self.0.get_mut(parent).unwrap()
    }
}
