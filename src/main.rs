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
    iter::{ExactSizeIterator, IntoIterator},
    ops::RangeBounds,
    option::{Option, Option::None, Option::Some},
    string::String,
    vec::Vec,
    {format, matches, print},
};

macro_rules! panic {
    () => {{ std::process::exit(-1); }};
    ($fmt:literal) => {{ std::eprint!($fmt); std::process::exit(-1); }};
    ($fmt:literal, $($arg:tt)*) => {{ std::eprint!($fmt, $($arg)*); std::process::exit(-1); }};
}

pub trait Key: Clone + Eq + Hash {}
impl<T: Clone + Eq + Hash> Key for T {}

// has fast lookup from HashMap and ordering from Vec
pub struct Map<K: Key, V> {
    idxs: HashMap<K, usize>,
    keys: Vec<K>,
    vals: Vec<V>,
}

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

pub struct Graph<N: Key, E>(Map<N, Map<N, E>>);

impl<N: Key, E> Graph<N, E> {
    pub fn new() -> Graph<N, E> {
        Graph(Map::new())
    }

    pub fn node(&mut self, node: N) {
        self.0.insert(node, Map::new());
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

// package these to make Token as small as possible
#[derive(PartialEq)]
struct Source {
    name: String,
    text: String,
}

#[derive(Clone)]
struct Token<'a> {
    source: &'a Source,
    lo: usize,
    hi: usize,
}

impl Token<'_> {
    fn as_str(&self) -> &str {
        &self.source.text[self.lo..self.hi]
    }

    fn location(&self) -> String {
        let mut row = 1;
        let mut col = 1;
        for (i, c) in self.source.text.char_indices() {
            if i == self.lo {
                break;
            } else if c == '\n' {
                row += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        format!("{}:{}:{}", self.source.name, row, col)
    }
}

struct Context<'a> {
    graph: Graph<usize, ()>,
    tokens: Vec<Token<'a>>,
}

impl<'a> Context<'a> {
    fn node(&mut self, token: Token<'a>) -> usize {
        let node = self.tokens.len();
        self.graph.node(node);
        self.tokens.push(token);
        node
    }

    // if self.graph is a tree, will work as expected
    // if self.graph is a DAG, will print shared nodes multiple times
    // if self.graph has cycles, will loop forever
    fn print_tree(&self, root: usize, indent: usize) {
        print!("{}{:5}", "\t".repeat(indent), root);
        if let Some(token) = self.tokens.get(root) {
            print!(" at {}\t{:?}", token.location(), token.as_str());
        }
        print!("\n");
        for child in self.graph.children(&root).keys() {
            self.print_tree(*child, indent + 1);
        }
    }
}

fn main() {
    let args: Vec<String> = args().collect();
    let file = args.get(1);
    let file = file.unwrap_or_else(|| panic!("expected command line argument\n"));
    let text = read_to_string(file).unwrap_or_else(|_| panic!("could not read {}\n", file));
    let source = &Source {
        name: file.clone(),
        text,
    };

    let mut c = Context {
        graph: Graph::new(),
        tokens: Vec::new(),
    };
    let root = c.node(Token {
        source,
        lo: 0,
        hi: 0,
    });

    let is = source.text.char_indices().map(|(i, _)| i);
    let js = source.text.char_indices().map(|(j, _)| j);
    for (i, j) in is.zip(js.skip(1).chain([source.text.len()])) {
        let node = c.node(Token {
            source,
            lo: i,
            hi: j,
        });
        c.graph.edge(root, node, ());
    }

    let passes = [
        // tokenize
        remove_comments,
        group_tokens,
        remove_whitespace,
        // // parse
        group_brackets,
        // group_operators,
        // unroll_operators,
        // unroll_brackets,
        // // transform
        // collect_macros,
        // sort_macros,
        // substitute_macros,
        // strings_to_ops,
        // // backend
        // check_nodes_are_ops,
        // collect_labels,
        // interpret,
    ];
    for pass in passes {
        pass(&mut c, &root)
    }

    c.print_tree(root, 0);
}

fn remove_comments(c: &mut Context, root: &usize) {
    let mut i = 0;
    let children = c.graph.children_mut(root);
    while i < children.keys().len() {
        if c.tokens[children.keys()[i]].as_str() == "#" {
            let mut j = i;
            while j < children.keys().len() && c.tokens[children.keys()[j]].as_str() != "\n" {
                j += 1;
            }
            children.splice(i..=j, [], []);
        } else {
            i += 1;
        }
    }
}

fn group_tokens(c: &mut Context, root: &usize) {
    let is_bracket = |s: &str| matches!(s, "(" | ")" | "{" | "}" | "[" | "]");
    let token_type = |s: &str| {
        if s.chars()
            .all(|c| matches!(c, '-' | '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'))
        {
            0
        } else if s.chars().all(char::is_whitespace) {
            1
        } else if is_bracket(s) {
            2
        } else {
            3
        }
    };

    let mut i = 1;
    let children = c.graph.children_mut(root);
    while i < children.keys().len() {
        let curr = &c.tokens[children.keys()[i]];
        let prev = &c.tokens[children.keys()[i - 1]];
        if !is_bracket(curr.as_str())
            && token_type(curr.as_str()) == token_type(prev.as_str())
            && curr.source == prev.source
            && prev.hi == curr.lo
        {
            c.tokens.get_mut(children.keys()[i - 1]).unwrap().hi = curr.hi;
            children.splice(i..=i, [], []);
        } else {
            i += 1;
        }
    }
}

fn remove_whitespace(c: &mut Context, root: &usize) {
    let mut i = 0;
    let children = c.graph.children_mut(root);
    while i < children.keys().len() {
        if c.tokens[children.keys()[i]]
            .as_str()
            .chars()
            .all(char::is_whitespace)
        {
            children.splice(i..=i, [], []);
        } else {
            i += 1;
        }
    }
}

fn group_brackets(c: &mut Context, root: &usize) {
    let match_opener = |token: &Token| match token.as_str() {
        "(" => ")",
        "{" => "}",
        "[" => "]",
        s => panic!("{} is not a token", s),
    };
    let mut stack = Vec::new();
    let mut i = 0;
    while i < c.graph.children(root).keys().len() {
        let child = c.graph.children(root).keys()[i];
        let token = &c.tokens[child];
        match token.as_str() {
            "(" | "{" | "[" => {
                stack.push(child);
                i += 1
            }
            ")" | "}" | "]" => match stack.pop() {
                None => panic!("extra {} at {}\n", token.as_str(), token.location()),
                Some(popped) => {
                    let opener = &c.tokens[popped];
                    if match_opener(opener) == token.as_str() {
                        c.graph.children_mut(root).splice(i..=i, [], []);
                    } else {
                        panic!(
                            "{} matched with {} at {} and {}\n",
                            opener.as_str(),
                            token.as_str(),
                            opener.location(),
                            token.location(),
                        )
                    }
                }
            },
            _ if stack.is_empty() => i += 1,
            _ => {
                let vs = c.graph.children_mut(root).splice(i..=i, [], []);
                let top = c.graph.children_mut(stack.last().unwrap());
                top.splice(top.keys().len().., vs.0, vs.1);
            }
        }
    }
}
