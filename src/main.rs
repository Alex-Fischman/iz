#![no_implicit_prelude]
extern crate std;

use std::{
    any::Any,
    borrow::ToOwned,
    boxed::Box,
    clone::Clone,
    collections::HashMap,
    env::args,
    fs::read_to_string,
    iter::{Extend, Iterator},
    ops::Deref,
    option::{Option, Option::None, Option::Some},
    string::String,
    vec::Vec,
    {format, matches, println, vec},
};

macro_rules! panic {
    () => {{ std::process::exit(-1); }};
    ($fmt:literal) => {{ std::eprint!($fmt); std::process::exit(-1); }};
    ($fmt:literal, $($arg:tt)*) => {{ std::eprint!($fmt, $($arg)*); std::process::exit(-1); }};
}

// these can't go inside Token for a few reasons:
// - Token should be as small as possible
// - Token needs a reference to the beginning of the file for location
// - Token needs a reference to the file name for location as well
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

impl Deref for Token<'_> {
    type Target = str;
    fn deref(&self) -> &str {
        &self.source.text[self.lo..self.hi]
    }
}

impl Token<'_> {
    // not stored directly because it's only useful in error messages
    // so we only ever care about finding a few locations per compile
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

struct Tree<'a> {
    token: Token<'a>,
    children: Vec<Tree<'a>>,
}

impl Tree<'_> {
    fn new(source: &Source, lo: usize, hi: usize) -> Tree {
        Tree {
            token: Token { source, lo, hi },
            children: Vec::new(),
        }
    }
}

struct Data(HashMap<String, Box<dyn Any>>);

impl Data {
    fn insert<T: Any>(&mut self, key: &str, value: T) -> Option<Box<dyn Any>> {
        self.0.insert(key.to_owned(), Box::new(value))
    }

    fn get<T: Any>(&self, key: &str) -> &T {
        match self.0.get(key) {
            Some(value) => match value.downcast_ref::<T>() {
                Some(value) => value,
                None => panic!("{} had the wrong type in data\n", key),
            },
            None => panic!("{} was not found in data\n", key),
        }
    }
}

fn main() {
    let args: Vec<String> = args().collect();
    let file = args.get(1);
    let file = file.unwrap_or_else(|| panic!("expected command line argument\n"));
    let text = read_to_string(file).unwrap_or_else(|_| panic!("could not read {}\n", file));
    let name = file.clone();
    let source = Source { name, text };

    let mut tree = Tree::new(&source, 0, 0);
    let is = source.text.char_indices().map(|(i, _)| i);
    let js = source.text.char_indices().map(|(j, _)| j);
    for (i, j) in is.zip(js.skip(1).chain([source.text.len()])) {
        tree.children.push(Tree::new(&source, i, j));
    }

    let mut data = Data(HashMap::new());

    let passes = [
        // tokenize
        remove_comments,
        group_tokens,
        remove_whitespace,
        // parse
        group_brackets,
        insert_default_operators,
        group_operators,
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
        pass(&mut tree, &mut data)
    }

    print_tree(&tree, 0);
    fn print_tree(tree: &Tree, indent: usize) {
        println!(
            "{}{}:\t{}",
            "\t".repeat(indent),
            tree.token.location(),
            tree.token.deref(),
        );
        for child in &tree.children {
            print_tree(child, indent + 1);
        }
    }
}

fn remove_comments(tree: &mut Tree, _data: &mut Data) {
    let mut i = 0;
    while i < tree.children.len() {
        if tree.children[i].token.deref() == "#" {
            let mut j = i;
            while j < tree.children.len() && tree.children[j].token.deref() != "\n" {
                j += 1;
            }
            tree.children.drain(i..=j);
        } else {
            i += 1;
        }
    }
}

fn group_tokens(tree: &mut Tree, _data: &mut Data) {
    let is_bracket = |s: &str| matches!(s, "(" | ")" | "{" | "}" | "[" | "]");
    let is_ident_char = |c: char| matches!(c, '-' | '_' | 'a'..='z' | 'A'..='Z' | '0'..='9');
    let token_type = |s: &str| match s {
        _ if s.chars().all(is_ident_char) => 0,
        _ if s.chars().all(char::is_whitespace) => 1,
        _ if is_bracket(s) => 2,
        _ => 3,
    };

    let mut i = 1;
    while i < tree.children.len() {
        let curr = &tree.children[i].token;
        let prev = &tree.children[i - 1].token;
        if !is_bracket(curr)
            && token_type(curr) == token_type(prev)
            && curr.source == prev.source
            && prev.hi == curr.lo
        {
            tree.children[i - 1].token.hi = curr.hi;
            tree.children.remove(i);
        } else {
            i += 1;
        }
    }
}

fn remove_whitespace(tree: &mut Tree, _data: &mut Data) {
    tree.children
        .retain(|child| !child.token.chars().all(char::is_whitespace));
}

fn group_brackets(tree: &mut Tree, _data: &mut Data) {
    let match_opener = |token: &str| match token {
        "(" => ")",
        "{" => "}",
        "[" => "]",
        s => panic!("{} is not a bracket\n", s),
    };

    let mut openers: Vec<(usize, Token)> = Vec::new();
    let mut i = 0;
    while i < tree.children.len() {
        match tree.children[i].token.deref() {
            "(" | "{" | "[" => openers.push((i, tree.children[i].token.clone())),
            ")" | "}" | "]" => match openers.pop() {
                Some((l, opener)) if match_opener(&opener) == tree.children[i].token.deref() => {
                    let mut cs: Vec<Tree> = tree.children.drain(l + 1..=i).collect();
                    cs.pop(); // remove closing bracket
                    tree.children[l].children = cs;
                    i = l;
                }
                Some((_, opener)) => panic!(
                    "{} matched with {} at {} and {}\n",
                    opener.deref(),
                    tree.children[i].token.deref(),
                    opener.location(),
                    tree.children[i].token.location(),
                ),
                None => panic!(
                    "extra {} at {}\n",
                    tree.children[i].token.deref(),
                    tree.children[i].token.location()
                ),
            },
            _ => {}
        }
        i += 1;
    }
    if let Some((_, opener)) = openers.last() {
        panic!(
            "no {} for the {} at {}\n",
            match_opener(opener),
            opener.deref(),
            opener.location()
        )
    }
}

struct Operator {
    _func: Option<String>,
    left: usize,
    right: usize,
}

struct Operators {
    ops: HashMap<String, Operator>,
    right_assoc: bool,
}

impl Operators {
    fn new(ops: &[(&str, Option<&str>, usize, usize)], right_assoc: bool) -> Operators {
        let ops = ops
            .iter()
            .map(|&(name, func, left, right)| {
                (
                    name.to_owned(),
                    Operator {
                        _func: func.map(|s| s.to_owned()),
                        left,
                        right,
                    },
                )
            })
            .collect();
        Operators { ops, right_assoc }
    }
}

fn insert_default_operators(_tree: &mut Tree, data: &mut Data) {
    let precedences = vec![
        Operators::new(&[(":", None, 1, 0)], false),
        Operators::new(&[("?", None, 1, 0)], false),
        Operators::new(&[("~", None, 0, 1)], true),
        Operators::new(&[("$", None, 0, 1)], true),
        Operators::new(&[("-", Some("neg"), 0, 1), ("!", Some("not"), 0, 1)], true),
        Operators::new(&[("+", Some("add"), 1, 1)], false),
    ];
    data.insert("precedences", precedences);
}

fn group_operators(tree: &mut Tree, data: &mut Data) {
    for child in &mut tree.children {
        group_operators(child, data)
    }

    let precedences = data.get::<Vec<_>>("precedences");
    for Operators { ops, right_assoc } in precedences {
        let mut i = if *right_assoc {
            tree.children.len().wrapping_sub(1)
        } else {
            0
        };
        while i < tree.children.len() {
            if let Some(op) = ops.get(tree.children[i].token.deref()) {
                if i < op.left || i + op.right >= tree.children.len() {
                    panic!(
                        "{} is missing arguments at {}\n",
                        tree.children[i].token.deref(),
                        tree.children[i].token.location()
                    )
                }
                let mut cs: Vec<Tree> = tree.children.drain(i - op.left..i).collect();
                i -= op.left;
                cs.extend(tree.children.drain(i + 1..=i + op.right));
                tree.children[i].children = cs;
            }
            i = if *right_assoc {
                i.wrapping_sub(1)
            } else {
                i + 1
            };
        }
    }
}
