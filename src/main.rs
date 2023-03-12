#![no_implicit_prelude]
#![allow(clippy::print_with_newline)]
extern crate std;

use std::{
    clone::Clone,
    cmp::Eq,
    collections::HashMap,
    env::args,
    fmt::{Display, Formatter, Result as FmtResult},
    fs::read_to_string,
    hash::Hash,
    iter::Iterator,
    option::{Option, Option::None, Option::Some},
    result::{Result, Result::Ok},
    string::String,
    vec::Vec,
    {format, print, write},
};

trait Key: Clone + Eq + Hash {}
impl<T: Clone + Eq + Hash> Key for T {}

struct IndexMap<K: Key, V> {
    idxs: HashMap<K, usize>,
    keys: Vec<K>,
    vals: Vec<V>,
}

impl<K: Key, V> IndexMap<K, V> {
    fn new() -> IndexMap<K, V> {
        IndexMap {
            idxs: HashMap::new(),
            keys: Vec::new(),
            vals: Vec::new(),
        }
    }

    fn get(&self, key: &K) -> Option<&V> {
        self.idxs.get(key).map(|i| &self.vals[*i])
    }

    fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.idxs.get(key).map(|i| &mut self.vals[*i])
    }

    fn remove(&mut self, key: &K) -> Option<V> {
        self.idxs.remove(key).map(|i| {
            self.keys.remove(i);
            self.vals.remove(i)
        })
    }

    fn insert(&mut self, key: K, val: V) -> Option<V> {
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

    fn or_insert(&mut self, key: K, val: V) -> &mut V {
        match self.idxs.get(&key) {
            Some(i) => &mut self.vals[*i],
            None => {
                self.insert(key.clone(), val);
                self.get_mut(&key).unwrap()
            }
        }
    }
}

struct Graph<T: Key>(IndexMap<T, IndexMap<T, ()>>);

impl<T: Key> Graph<T> {
    fn add_node(&mut self, node: T) -> Option<IndexMap<T, ()>> {
        self.0.insert(node, IndexMap::new())
    }

    fn add_edge(&mut self, parent: T, child: T) {
        self.0.or_insert(parent, IndexMap::new()).insert(child, ());
    }

    fn children(&self, parent: T) -> &[T] {
        &self.0.get(&parent).unwrap().keys
    }

    fn children_mut(&self, parent: T) -> &mut Vec<T> {
        &mut self.0.get(&parent).unwrap().keys
    }
}

#[derive(Clone)]
struct Location<'a> {
    src: &'a str,
    row: usize,
    col: usize,
}

impl Display for Location<'_> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}:{}:{}", self.src, self.row, self.col)
    }
}

struct Context<'a> {
    id: usize,
    graph: Graph<usize>,
    chars: HashMap<usize, char>,
    locations: HashMap<usize, Location<'a>>,
}

impl<'a> Context<'a> {
    fn new() -> Context<'a> {
        Context {
            id: 0,
            graph: Graph(IndexMap::new()),
            chars: HashMap::new(),
            locations: HashMap::new(),
        }
    }

    fn add_node(&mut self) -> usize {
        let node = self.id;
        self.id += 1;
        self.graph.add_node(node);
        node
    }

    // if self.graph is a tree, will work as expected
    // if self.graph is a DAG, will print shared nodes multiple times
    // if self.graph has cycles, will loop forever
    fn print_tree(&self, root: usize, indent: usize) {
        print!("{} {}", "----".repeat(indent), root);
        if let Some(Location { src, row, col }) = self.locations.get(&root) {
            print!("@{}:{}:{}", src, row, col);
        }
        print!(":");
        if let Some(c) = self.chars.get(&root) {
            print!("\t{}", c);
        }

        for child in self.graph.children(root) {
            self.print_tree(child, indent + 1);
        }
    }
}

fn main() -> Result<(), String> {
    let args: Vec<String> = args().collect();
    let file = args
        .get(1)
        .ok_or("pass a .iz file as a command line argument")?;
    let text = read_to_string(file).map_err(|_| format!("could not read {}", file))?;

    let mut context = Context::new();
    assert!(context.add_node() == 0); // 0 is used as a root node

    let mut location = Location {
        src: file,
        row: 1,
        col: 1,
    };
    for c in text.chars() {
        let node = context.add_node();
        context.graph.add_edge(0, node);
        context.chars.insert(node, c);
        context.locations.insert(node, location.clone());
        if c == '\n' {
            location.row += 1;
            location.col = 1;
        } else {
            location.col += 1;
        }
    }

    let passes: &[fn(&mut Context) -> Result<(), String>] = &[
        // tokenize
        // remove_comments,
        // chars_to_strings,
        // remove_whitespace,
        // parse
        // group_brackets,
        // group_operators,
        // unroll_operators,
        // unroll_brackets,
        // transform
        // collect_macros,
        // sort_macros,
        // substitute_macros,
        // strings_to_ops,
        // backend
        // check_nodes_are_ops,
        // collect_labels,
        // interpret,
    ];
    for pass in passes {
        pass(&mut context)?
    }

    context.print_tree(0, 0);

    Ok(())
}
