#![no_implicit_prelude]
#![allow(clippy::print_with_newline)]
extern crate std;

use std::{
    clone::Clone,
    cmp::Eq,
    collections::HashMap,
    default::Default,
    env::args,
    fs::read_to_string,
    hash::Hash,
    iter::Iterator,
    option::{Option, Option::None, Option::Some},
    string::String,
    vec::Vec,
    {eprintln, format, matches, print},
};

macro_rules! panic {
    () => {{ std::process::exit(-1); }};
    ($fmt:literal) => {{ eprintln!($fmt); std::process::exit(-1); }};
    ($fmt:literal, $($arg:tt)*) => {{ eprintln!($fmt, $($arg)*); std::process::exit(-1); }};
}

trait Key: Clone + Eq + Hash {}
impl<T: Clone + Eq + Hash> Key for T {}

struct IndexMap<K: Key, V> {
    idxs: HashMap<K, usize>,
    keys: Vec<K>,
    vals: Vec<V>,
}

impl<K: Key, V> IndexMap<K, V> {
    fn get(&self, key: &K) -> Option<&V> {
        self.idxs.get(key).map(|i| &self.vals[*i])
    }

    fn get_mut(&mut self, key: &K) -> Option<&mut V> {
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
}

impl<K: Key, V: Default> IndexMap<K, V> {
    fn or_default(&mut self, key: K) -> &mut V {
        match self.idxs.get(&key) {
            Some(i) => &mut self.vals[*i],
            None => {
                self.insert(key.clone(), V::default());
                self.get_mut(&key).unwrap()
            }
        }
    }
}

impl<K: Key, V> Default for IndexMap<K, V> {
    fn default() -> IndexMap<K, V> {
        IndexMap {
            idxs: HashMap::default(),
            keys: Vec::default(),
            vals: Vec::default(),
        }
    }
}

struct Graph<T: Key>(IndexMap<T, IndexMap<T, ()>>);

impl<T: Key> Graph<T> {
    fn node(&mut self, node: T) -> Option<IndexMap<T, ()>> {
        self.0.insert(node, IndexMap::default())
    }

    fn edge(&mut self, parent: T, child: T) {
        self.0.or_default(parent).insert(child, ());
    }

    fn children(&self, parent: T) -> &[T] {
        &self.0.get(&parent).unwrap().keys
    }

    fn children_mut(&mut self, parent: T) -> &mut Vec<T> {
        &mut self.0.get_mut(&parent).unwrap().keys
    }
}

struct Context<'a> {
    file: &'a str,
    text: &'a str,
    id: usize,
    graph: Graph<usize>,
    tokens: HashMap<usize, &'a str>,
}

impl<'a> Context<'a> {
    fn new(file: &'a str, text: &'a str) -> Context<'a> {
        Context {
            file,
            text,
            id: 0,
            graph: Graph(IndexMap::default()),
            tokens: HashMap::new(),
        }
    }

    fn node(&mut self) -> usize {
        let node = self.id;
        self.id += 1;
        self.graph.node(node);
        node
    }

    fn location(&self, node: usize) -> Option<String> {
        let mut row = 1;
        let mut col = 1;
        let end = self.tokens.get(&node)?.as_ptr();
        for (i, c) in self.text.char_indices() {
            if self.text[i..].as_ptr() == end {
                break;
            } else if c == '\n' {
                row += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        Some(format!("{}:{}:{}", self.file, row, col))
    }

    // if self.graph is a tree, will work as expected
    // if self.graph is a DAG, will print shared nodes multiple times
    // if self.graph has cycles, will loop forever
    fn print_tree(&self, root: usize, indent: usize) {
        print!("{}{:05}", "------- ".repeat(indent), root,);
        if let Some(location) = self.location(root) {
            print!(" at {}", location);
        }
        if let Some(token) = self.tokens.get(&root) {
            print!("\t{:?}", token);
        }
        print!("\n");
        for child in self.graph.children(root) {
            self.print_tree(*child, indent + 1);
        }
    }
}

fn main() {
    let args: Vec<String> = args().collect();
    let file = args.get(1);
    let file = file.unwrap_or_else(|| panic!("expected command line argument"));
    let text = read_to_string(file).unwrap_or_else(|_| panic!("could not read {}", file));

    let mut context = Context::new(file, &text);
    assert!(context.node() == 0); // 0 is used as a root node

    let is = text.char_indices().map(|(i, _)| i);
    let js = text
        .char_indices()
        .map(|(j, _)| j)
        ;
    for (i, j) in is.zip(js.skip(1)
        .chain([text.len()])) {
        let node = context.node();
        context.graph.edge(0, node);
        context.tokens.insert(node, &text[i..j]);
    }

    let passes = [
        // tokenize
        remove_comments,
        group_tokens,
        remove_whitespace,
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
        pass(&mut context)
    }

    context.print_tree(0, 0);
}

fn remove_comments(context: &mut Context) {
    let mut i = 0;
    while i < context.graph.children(0).len() {
        let child = context.graph.children(0)[i];
        let c = context
            .tokens
            .get(&child)
            .unwrap_or_else(|| panic!("missing token at {}", context.location(child).unwrap()));
        if c == &"#" {
            let mut j = i;
            while j < context.graph.children(0).len()
                && context.tokens[&context.graph.children(0)[j]] != "\n"
            {
                context.tokens.remove(&context.graph.children(0)[j]);
                j += 1;
            }
            context.graph.children_mut(0).drain(i..j);
        } else {
            i += 1;
        }
    }
}

fn is_bracket(s: &str) -> bool {
    s.chars()
        .all(|c| matches!(c, '(' | ')' | '{' | '}' | '[' | ']'))
}

fn is_identifier(s: &str) -> bool {
    s.chars()
        .all(|c| matches!(c, '-' | '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'))
}

fn is_whitespace(s: &str) -> bool {
    s.chars().all(char::is_whitespace)
}

fn token_type(s: &str) -> usize {
    match s {
        _ if is_identifier(s) => 0,
        _ if is_whitespace(s) => 1,
        _ if is_bracket(s) => 2,
        _ => 3, // operators
    }
}

fn group_tokens(context: &mut Context) {
    let mut i = 1;
    let children = context.graph.children_mut(0);
    while i < children.len() {
        let curr = context.tokens[&children[i]];
        let prev = context.tokens[&children[i - 1]];
        // don't want ((s       can consume #s
        if !is_bracket(curr) && !is_whitespace(curr) && token_type(curr) == token_type(prev) {
            context.tokens.insert(children[i - 1], unsafe {
                std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                    prev.as_ptr(),
                    prev.len() + curr.len(),
                ))
            });
            context.tokens.remove(&children[i]).unwrap();
            children.remove(i);
        } else {
            i += 1;
        }
    }
}

fn remove_whitespace(context: &mut Context) {
    let mut i = 0;
    let children = context.graph.children_mut(0);
    while i < children.len() {
        if is_whitespace(context.tokens[&children[i]]) {
            context.tokens.remove(&children[i]).unwrap();
            children.remove(i);
        } else {
            i += 1;
        }
    }
}
