#![no_implicit_prelude]
#![allow(clippy::print_with_newline)]
extern crate std;

mod graph;
mod map;

use crate::graph::Graph;
use std::{
    clone::Clone,
    cmp::Eq,
    collections::HashMap,
    env::args,
    fs::read_to_string,
    hash::Hash,
    iter::Iterator,
    marker::Copy,
    option::Option::Some,
    string::String,
    vec::Vec,
    {eprint, format, matches, print},
};

macro_rules! panic {
    () => {{ std::process::exit(-1); }};
    ($fmt:literal) => {{ eprint!($fmt); std::process::exit(-1); }};
    ($fmt:literal, $($arg:tt)*) => {{ eprint!($fmt, $($arg)*); std::process::exit(-1); }};
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct Node(usize);

// package these to make Token as small as possible
#[derive(PartialEq)]
struct Source {
    name: String,
    text: String,
}

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
    id: Node,
    graph: Graph<Node, ()>,
    tokens: HashMap<Node, Token<'a>>,
}

impl<'a> Context<'a> {
    fn node(&mut self) -> Node {
        let node = self.id;
        self.id.0 += 1;
        self.graph.node(node);
        node
    }

    // if self.graph is a tree, will work as expected
    // if self.graph is a DAG, will print shared nodes multiple times
    // if self.graph has cycles, will loop forever
    fn print_tree(&self, root: Node, indent: usize) {
        print!("{}{:05}", "------- ".repeat(indent), root.0);
        if let Some(token) = self.tokens.get(&root) {
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

    let mut context = Context {
        id: Node(0),
        graph: Graph::new(),
        tokens: HashMap::new(),
    };
    let root = context.node();

    let is = source.text.char_indices().map(|(i, _)| i);
    let js = source.text.char_indices().map(|(j, _)| j);
    for (i, j) in is.zip(js.skip(1).chain([source.text.len()])) {
        let node = context.node();
        context.graph.edge(root, node, ());
        context.tokens.insert(
            node,
            Token {
                source,
                lo: i,
                hi: j,
            },
        );
    }

    let passes = [
        // tokenize
        remove_comments,
        group_tokens,
        remove_whitespace,
        // // parse
        // group_brackets,
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
        pass(&mut context, root)
    }

    context.print_tree(root, 0);
}

fn remove_comments(context: &mut Context, root: Node) {
    let mut i = 0;
    while i < context.graph.children(&root).len() {
        if context.tokens[&context.graph.children(&root)[i]].as_str() == "#" {
            let mut j = i;
            while j < context.graph.children(&root).len()
                && context
                    .tokens
                    .remove(&context.graph.children(&root)[j])
                    .unwrap()
                    .as_str()
                    != "\n"
            {
                j += 1;
            }
            context.graph.children_mut(&root).splice(i..=j, [], []);
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

fn group_tokens(context: &mut Context, root: Node) {
    let mut i = 1;
    let children = context.graph.children_mut(&root);
    while i < children.len() {
        let curr = &context.tokens[&children[i]];
        let prev = &context.tokens[&children[i - 1]];
        if !is_bracket(curr.as_str())
            && token_type(curr.as_str()) == token_type(prev.as_str())
            && curr.source == prev.source
            && prev.hi == curr.lo
        {
            let curr = context.tokens.remove(&children[i]).unwrap();
            context.tokens.get_mut(&children[i - 1]).unwrap().hi = curr.hi;
            children.splice(i..=i, [], []);
        } else {
            i += 1;
        }
    }
}

fn remove_whitespace(context: &mut Context, root: Node) {
    let mut i = 0;
    let children = context.graph.children_mut(&root);
    while i < children.len() {
        if is_whitespace(context.tokens[&children[i]].as_str()) {
            context.tokens.remove(&children[i]).unwrap();
            children.splice(i..=i, [], []);
        } else {
            i += 1;
        }
    }
}
