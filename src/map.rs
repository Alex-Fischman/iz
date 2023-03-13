extern crate std;
use std::{
    clone::Clone,
    cmp::Eq,
    collections::HashMap,
    hash::Hash,
    iter::{IntoIterator, Iterator},
    ops::{Index, RangeBounds},
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

    pub fn len(&self) -> usize {
        self.keys.len()
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.idxs.get(key).map(|i| &self.vals[*i])
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.idxs.get(key).map(|i| &mut self.vals[*i])
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

    pub fn contains_key(&self, key: &K) -> bool {
        self.idxs.contains_key(key)
    }

    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.keys.iter()
    }
}

// Vec-ish methods
impl<K: Key, V> Index<usize> for Map<K, V> {
    type Output = K;
    fn index(&self, i: usize) -> &K {
        &self.keys[i]
    }
}

impl<K: Key, V> Map<K, V> {
    fn rebuild_idxs(&mut self) {
        self.idxs = self
            .keys
            .iter()
            .cloned()
            .enumerate()
            .map(|(i, key)| (key, i))
            .collect()
    }

    pub fn remove(&mut self, key: &K) -> Option<V> {
        match self.idxs.get(key).cloned() {
            Some(i) => {
                self.keys.remove(i);
                self.rebuild_idxs();
                Some(self.vals.remove(i))
            }
            None => None,
        }
    }

    pub fn splice<R, Ks, Vs>(&mut self, range: R, keys: Ks, vals: Vs) -> (Vec<K>, Vec<V>)
    where
        R: RangeBounds<usize> + Clone,
        Ks: IntoIterator<Item = K>,
        Vs: IntoIterator<Item = V>,
    {
        let keys = self.keys.splice(range.clone(), keys).collect();
        let vals = self.vals.splice(range, vals).collect();
        self.rebuild_idxs();
        (keys, vals)
    }
}
