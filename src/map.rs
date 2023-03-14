extern crate std;
use crate::panic;
use std::{
    clone::Clone,
    cmp::Eq,
    collections::HashMap,
    hash::Hash,
    iter::{ExactSizeIterator, IntoIterator, Iterator},
    ops::RangeBounds,
    option::{Option, Option::None, Option::Some},
    vec::Vec,
};

pub trait Key: Clone + Eq + Hash {}
impl<T: Clone + Eq + Hash> Key for T {}

// has fast lookup from HashMap and ordering from Vec
pub struct Map<K: Key, V> {
    idxs: HashMap<K, usize>,
    keys: Vec<K>,
    vals: Vec<V>,
}

// Map-ish methods
impl<K: Key, V> Map<K, V> {
    pub fn new() -> Map<K, V> {
        Map {
            idxs: HashMap::new(),
            keys: Vec::new(),
            vals: Vec::new(),
        }
    }

    pub fn keys(&self) -> &[K] {
        &self.keys
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.idxs.get(key).map(|i| &self.vals[*i])
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.idxs.get(key).map(|i| &mut self.vals[*i])
    }

    pub fn contains_key(&self, key: &K) -> bool {
        self.idxs.contains_key(key)
    }

    pub fn insert(&mut self, key: K, val: V) -> Option<V> {
        match self.idxs.get(&key) {
            Some(i) => Some(std::mem::replace(&mut self.vals[*i], val)),
            None => {
                self.idxs.insert(key.clone(), self.keys.len());
                self.keys.push(key);
                self.vals.push(val);
                None
            }
        }
    }

    pub fn splice<'a, R, A, X, B, Y>(&'a mut self, range: R, keys: A, vals: B) -> (Vec<K>, Vec<V>)
    where
        R: RangeBounds<usize> + Clone,
        A: IntoIterator<IntoIter = X>,
        B: IntoIterator<IntoIter = Y>,
        X: ExactSizeIterator<Item = K> + 'a,
        Y: ExactSizeIterator<Item = V> + 'a,
    {
        let keys = keys.into_iter();
        let vals = vals.into_iter();
        if keys.len() != vals.len() {
            panic!("keys and vals had different lengths")
        }
        let keys = self.keys.splice(range.clone(), keys).collect();
        let vals = self.vals.splice(range, vals).collect();
        // completely rebuild idxs; has to be linear anyway
        self.idxs = self
            .keys
            .iter()
            .enumerate()
            .map(|(i, key)| (key.clone(), i))
            .collect();
        (keys, vals)
    }
}
