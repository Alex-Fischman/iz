#![no_implicit_prelude]
#![allow(clippy::print_with_newline)]
extern crate std;

mod graph;
mod map;

use crate::graph::Graph;
use std::{
    clone::Clone,
    env::args,
    fs::read_to_string,
    iter::Iterator,
    option::{Option::None, Option::Some},
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

    let mut context = Context {
        graph: Graph::new(),
        tokens: Vec::new(),
    };
    let root = context.node(Token {
        source,
        lo: 0,
        hi: 0,
    });

    let is = source.text.char_indices().map(|(i, _)| i);
    let js = source.text.char_indices().map(|(j, _)| j);
    for (i, j) in is.zip(js.skip(1).chain([source.text.len()])) {
        let node = context.node(Token {
            source,
            lo: i,
            hi: j,
        });
        context.graph.edge(root, node, ());
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

fn remove_comments(context: &mut Context, root: &usize) {
    let mut i = 0;
    let children = context.graph.children_mut(root);
    while i < children.keys().len() {
        if context.tokens[children.keys()[i]].as_str() == "#" {
            let mut j = i;
            while j < children.keys().len() && context.tokens[children.keys()[j]].as_str() != "\n" {
                j += 1;
            }
            children.splice(i..=j, [], []);
        } else {
            i += 1;
        }
    }
}

fn group_tokens(context: &mut Context, root: &usize) {
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
        let curr = &context.tokens[children.keys()[i]];
        let prev = &context.tokens[children.keys()[i - 1]];
        if !is_bracket(curr.as_str())
            && token_type(curr.as_str()) == token_type(prev.as_str())
            && curr.source == prev.source
            && prev.hi == curr.lo
        {
            context.tokens.get_mut(children.keys()[i - 1]).unwrap().hi = curr.hi;
            children.splice(i..=i, [], []);
        } else {
            i += 1;
        }
    }
}

fn remove_whitespace(context: &mut Context, root: &usize) {
    let mut i = 0;
    let children = context.graph.children_mut(root);
    while i < children.keys().len() {
        if context.tokens[children.keys()[i]]
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

fn group_brackets(context: &mut Context, root: &usize) {
    let match_opener = |token: &Token| match token.as_str() {
        "(" => ")",
        "{" => "}",
        "[" => "]",
        s => panic!("{} is not a token", s),
    };
    let mut stack = Vec::new();
    let mut i = 0;
    while i < context.graph.children(root).keys().len() {
        let child = context.graph.children(root).keys()[i];
        let token = &context.tokens[child];
        match token.as_str() {
            "(" | "{" | "[" => {
                stack.push(child);
                i += 1
            }
            ")" | "}" | "]" => match stack.pop() {
                None => panic!("extra {} at {}\n", token.as_str(), token.location()),
                Some(popped) => {
                    let opener = &context.tokens[popped];
                    if match_opener(opener) == token.as_str() {
                        context.graph.children_mut(root).splice(i..=i, [], []);
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
                let vs = context.graph.children_mut(root).splice(i..=i, [], []);
                let top = context.graph.children_mut(stack.last().unwrap());
                top.splice(top.keys().len().., vs.0, vs.1);
            }
        }
    }
}
