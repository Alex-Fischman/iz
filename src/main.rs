#![no_implicit_prelude]
#![allow(clippy::print_with_newline)]
extern crate std;

use std::{
    collections::HashMap,
    env::args,
    clone::Clone,
    cmp::Eq,
    fs::read_to_string,
    iter::Iterator,
    ops::{FnOnce},
    option::{Option, Option::Some, Option::None},
    hash::Hash,
    result::{Result, Result::Ok},
    string::String,
    vec::Vec,
    {format, print},
};

trait Key: Clone + Eq + Hash {}
impl<T: Clone + Eq + Hash> Key for T {}

struct IndexMap<K: Key, V> {
    indices: HashMap<K, usize>,
    entries: Vec<(K, V)>,
}

impl<K: Key, V> IndexMap<K, V> {
    fn new() -> IndexMap<K, V> {
        IndexMap {
            indices: HashMap::new(),
            entries: Vec::new(),
        }
    }

    fn get(&self, key: &K) -> Option<&V> {
        match self.indices.get(key) {
            Some(i) => Some(&self.entries[*i].1),
            None => None,
        }
    }

    fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        match self.indices.get(key) {
            Some(i) => Some(&mut self.entries[*i].1),
            None => None,
        }
    }

    fn insert(&mut self, key: K, value: V) -> Option<V> {
        match self.indices.get(&key) {
            Some(i) => Some(std::mem::replace(&mut self.entries[*i].1, value)),
            None => {
                self.indices.insert(key.clone(), self.entries.len());
                self.entries.push((key, value));
                None
            }
        }
    }

    fn remove(&mut self, key: &K) -> Option<V> {
        match self.indices.remove(key) {
            Some(i) => Some(self.entries.remove(i).1),
            None => None,
        }
    }

    fn entry(&mut self, key: K) -> Entry<K, V> {
        Entry(self, key)
    }
}

struct Entry<'a, K: 'a + Key, V: 'a>(&'a mut IndexMap<K, V>, K);

impl<'a, K: 'a + Key, V: 'a> Entry<'a, K, V> {
    fn and_modify<F: FnOnce(&mut V)>(self, f: F) -> Entry<'a, K, V> {
        match self.0.get_mut(&self.1) {
            Some(value) => f(value),
            None => {},
        }
        self
    }
}

struct Graph<N: Hash + Eq + Clone, E>(HashMap<N, IndexMap<N, E>>);

fn main() -> Result<(), String> {
    let args: Vec<String> = args().collect();
    let file = args
        .get(1)
        .ok_or("pass a .iz file as a command line argument")?;
    let text = read_to_string(file).map_err(|_| format!("could not read {}", file))?;



    print!("{}\n", text);

    Ok(())
}
