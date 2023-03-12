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

    fn value(&self, key: &K) -> Option<&V> {
        self.idxs.get(key).map(|i| &self.vals[*i])
    }

    fn value_mut(&mut self, key: &K) -> Option<&mut V> {
        self.idxs.get(key).map(|i| &mut self.vals[*i])
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
                self.value_mut(&key).unwrap()
            }
        }
    }
}

struct Graph<T: Key>(IndexMap<T, IndexMap<T, ()>>);

impl<T: Key> Graph<T> {
    fn node(&mut self, node: T) -> Option<IndexMap<T, ()>> {
        self.0.insert(node, IndexMap::new())
    }

    fn edge(&mut self, parent: T, child: T) {
        self.0.or_insert(parent, IndexMap::new()).insert(child, ());
    }

    fn children(&self, parent: T) -> &[T] {
        &self.0.value(&parent).unwrap().keys
    }

    fn children_mut(&mut self, parent: T) -> &mut Vec<T> {
        &mut self.0.value_mut(&parent).unwrap().keys
    }
}

struct Context<'a> {
    #[allow(unused)]
    file: &'a str,
    text: &'a str,

    id: usize,
    graph: Graph<usize>,

    locations: HashMap<usize, usize>,
    chars: HashMap<usize, char>,
}

impl<'a> Context<'a> {
    fn new(file: &'a str, text: &'a str) -> Context<'a> {
        Context {
            file,
            text,

            id: 0,
            graph: Graph(IndexMap::new()),

            locations: HashMap::new(),
            chars: HashMap::new(),
        }
    }

    fn node(&mut self) -> usize {
        let node = self.id;
        self.id += 1;
        self.graph.node(node);
        node
    }

    #[allow(unused)]
    fn location(&self, node: usize) -> String {
        let mut row = 1;
        let mut col = 1;
        for c in self.text.chars().take(*self.locations.get(&node).unwrap()) {
            if c == '\n' {
                row += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        format!("{}:{}:{}", self.file, row, col)
    }

    // if self.graph is a tree, will work as expected
    // if self.graph is a DAG, will print shared nodes multiple times
    // if self.graph has cycles, will loop forever
    fn print_tree(&self, root: usize, indent: usize) {
        print!("{}{:05}", "------- ".repeat(indent), root);
        if let Some(location) = self.locations.get(&root) {
            print!("\t{}", location);
        }
        if let Some(c) = self.chars.get(&root) {
            print!("\t{:?}", c);
        }
        print!("\n");
        for child in self.graph.children(root) {
            self.print_tree(*child, indent + 1);
        }
    }
}

fn main() -> Result<(), String> {
    let args: Vec<String> = args().collect();
    let file = args
        .get(1)
        .ok_or("pass a .iz file as a command line argument")?;
    let text = read_to_string(file).map_err(|_| format!("could not read {}", file))?;

    let mut context = Context::new(file, &text);
    assert!(context.node() == 0); // 0 is used as a root node

    for (i, c) in text.chars().enumerate() {
        let node = context.node();
        context.graph.edge(0, node);
        context.locations.insert(node, i);
        context.chars.insert(node, c);
    }

    let passes = [
        // tokenize
        remove_comments,
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

fn remove_comments(context: &mut Context) -> Result<(), String> {
    let mut i = 0;
    let children = context.graph.children_mut(0);
    while i < children.len() {
        if context.chars.get(&children[i]) == Some(&'#') {
            let mut j = i;
            while j < children.len() && context.chars.remove(&children[j]) != Some('\n') {
                j += 1;
            }
            context.chars.insert(children[j], '\n');
            children.drain(i..j);
        } else {
            i += 1;
        }
    }
    Ok(())
}
