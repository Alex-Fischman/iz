#![no_implicit_prelude]
#![allow(clippy::print_with_newline)]
extern crate std;

use std::{
    clone::Clone,
    cmp::Eq,
    collections::HashMap,
    env::args,
    fs::read_to_string,
    hash::Hash,
    iter::Iterator,
    option::{Option, Option::None, Option::Some},
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
        self.indices.get(key).map(|i| &self.entries[*i].1)
    }

    fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.indices.get(key).map(|i| &mut self.entries[*i].1)
    }

    fn remove(&mut self, key: &K) -> Option<V> {
        self.indices.remove(key).map(|i| self.entries.remove(i).1)
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

    fn or_insert(&mut self, key: K, value: V) -> &mut V {
        match self.indices.get(&key) {
            Some(i) => &mut self.entries[*i].1,
            None => {
                self.insert(key.clone(), value);
                self.get_mut(&key).unwrap()
            }
        }
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
