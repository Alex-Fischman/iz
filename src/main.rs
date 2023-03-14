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
    {format, matches, print},
};

#[macro_export]
macro_rules! panic {
    () => {{ std::process::exit(-1); }};
    ($fmt:literal) => {{ std::eprint!($fmt); std::process::exit(-1); }};
    ($fmt:literal, $($arg:tt)*) => {{ std::eprint!($fmt, $($arg)*); std::process::exit(-1); }};
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Node(usize);

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
        print!("{}{:5}", "\t".repeat(indent), root.0);
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
        pass(&mut context, &root)
    }

    context.print_tree(root, 0);
}

fn remove_comments(context: &mut Context, root: &Node) {
    let mut i = 0;
    let children = context.graph.children_mut(root);
    while i < children.keys().len() {
        if context.tokens[&children.keys()[i]].as_str() == "#" {
            let mut j = i;
            while j < children.keys().len()
                && context
                    .tokens
                    .remove(&children.keys()[j])
                    .unwrap()
                    .as_str()
                    != "\n"
            {
                j += 1;
            }
            children.splice(i..=j, [], []);
        } else {
            i += 1;
        }
    }
}

fn group_tokens(context: &mut Context, root: &Node) {
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
    let children = context.graph.children_mut(root);
    while i < children.keys().len() {
        let curr = &context.tokens[&children.keys()[i]];
        let prev = &context.tokens[&children.keys()[i - 1]];
        if !is_bracket(curr.as_str())
            && token_type(curr.as_str()) == token_type(prev.as_str())
            && curr.source == prev.source
            && prev.hi == curr.lo
        {
            let curr = context.tokens.remove(&children.keys()[i]).unwrap();
            context.tokens.get_mut(&children.keys()[i - 1]).unwrap().hi = curr.hi;
            children.splice(i..=i, [], []);
        } else {
            i += 1;
        }
    }
}

fn remove_whitespace(context: &mut Context, root: &Node) {
    let mut i = 0;
    let children = context.graph.children_mut(root);
    while i < children.keys().len() {
        if context.tokens[&children.keys()[i]]
            .as_str()
            .chars()
            .all(char::is_whitespace)
        {
            context.tokens.remove(&children.keys()[i]).unwrap();
            children.splice(i..=i, [], []);
        } else {
            i += 1;
        }
    }
}

fn group_brackets(context: &mut Context, root: &Node) {
    enum Target<'a> {
        Nothing,
        Round(&'a Token<'a>),
        Curly(&'a Token<'a>),
        Square(&'a Token<'a>),
    }

    group_brackets(context, root, &mut 0, Target::Nothing);
    fn group_brackets(context: &mut Context, root: &Node, i: &mut usize, target: Target) {
        while *i < context.graph.children(root).keys().len() {
            let child = context.graph.children(root).keys()[*i];
            *i += 1;
            let token = context.tokens.get(&child).cloned().unwrap();
            let mut handle_open_bracket = |target| {
                let start = *i;
                group_brackets(context, root, i, target);
                let cs = context.graph.children_mut(root);
                let mut vecs = cs.splice(start..*i, [], []);
                // remove the closing bracket
                vecs.0.pop();
                vecs.1.pop();
                let cs = context.graph.children_mut(&child);
                cs.splice(0..0, vecs.0, vecs.1);
                *i = start;
            };
            match token.as_str() {
                "(" => handle_open_bracket(Target::Round(&token)),
                "{" => handle_open_bracket(Target::Curly(&token)),
                "[" => handle_open_bracket(Target::Square(&token)),
                ")" if matches!(target, Target::Round(_)) => return,
                ")" => panic!("extra ) at {}\n", token.location()),
                "}" if matches!(target, Target::Curly(_)) => return,
                "}" => panic!("extra }} at {}\n", token.location()),
                "]" if matches!(target, Target::Square(_)) => return,
                "]" => panic!("extra ] at {}\n", token.location()),
                _ => {}
            }
        }
        match target {
            Target::Nothing => {}
            Target::Round(token) => panic!("missing ) for ( at {}\n", token.location()),
            Target::Curly(token) => panic!("missing }} for {{ at {}\n", token.location()),
            Target::Square(token) => panic!("missing ] for [ at {}\n", token.location()),
        }
    }
}
