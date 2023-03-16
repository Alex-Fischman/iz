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
    {format, matches, println},
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

struct Tree<T> {
    x: T,
    children: Vec<Tree<T>>,
}

struct Data(HashMap<String, Box<dyn Any>>);

impl Data {
    fn insert<V: Any>(&mut self, key: &str, value: V) -> Option<Box<dyn Any>> {
        self.0.insert(key.to_owned(), Box::new(value))
    }

    fn get<T: 'static>(&self, key: &str) -> &T {
        match self.0.get(key) {
            Some(value) => match value.downcast_ref::<T>() {
                Some(value) => value,
                None => panic!("{} had the wrong type in data", key),
            },
            None => panic!("{} not found in data", key),
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

    let ops = |ops: &[(&str, Option<&str>, usize, usize)]| {
        ops.iter()
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
            .collect()
    };
    let precedences: Precedences = [
        (ops(&[(":", None, 1, 0)]), false),
        (ops(&[("?", None, 1, 0)]), false),
        (ops(&[("~", None, 0, 1)]), true),
        (ops(&[("$", None, 0, 1)]), true),
        (
            ops(&[("-", Some("neg"), 0, 1), ("!", Some("not"), 0, 1)]),
            true,
        ),
        (ops(&[("+", Some("add"), 1, 1)]), false),
    ]
    .to_vec();

    let mut tree = Tree {
        x: Token {
            source,
            lo: 0,
            hi: 0,
        },
        children: Vec::new(),
    };
    let mut data = Data(HashMap::new());
    data.insert("precedences", precedences);

    let is = source.text.char_indices().map(|(i, _)| i);
    let js = source.text.char_indices().map(|(j, _)| j);
    for (i, j) in is.zip(js.skip(1).chain([source.text.len()])) {
        tree.children.push(Tree {
            x: Token {
                source,
                lo: i,
                hi: j,
            },
            children: Vec::new(),
        });
    }

    let passes = [
        // tokenize
        remove_comments,
        group_tokens,
        remove_whitespace,
        // parse
        group_brackets,
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
    fn print_tree(tree: &Tree<Token>, indent: usize) {
        println!(
            "{}at {}:\t{}",
            "\t".repeat(indent),
            tree.x.location(),
            tree.x.deref(),
        );
        for child in &tree.children {
            print_tree(child, indent + 1);
        }
    }
}

fn remove_comments(tree: &mut Tree<Token>, _data: &mut Data) {
    let mut i = 0;
    while i < tree.children.len() {
        if tree.children[i].x.deref() == "#" {
            let mut j = i;
            while j < tree.children.len() && tree.children[j].x.deref() != "\n" {
                j += 1;
            }
            tree.children.drain(i..=j);
        } else {
            i += 1;
        }
    }
}

fn group_tokens(tree: &mut Tree<Token>, _data: &mut Data) {
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
    while i < tree.children.len() {
        let curr = &tree.children[i].x;
        let prev = &tree.children[i - 1].x;
        if !is_bracket(curr)
            && token_type(curr) == token_type(prev)
            && curr.source == prev.source
            && prev.hi == curr.lo
        {
            tree.children[i - 1].x.hi = curr.hi;
            tree.children.remove(i);
        } else {
            i += 1;
        }
    }
}

fn remove_whitespace(tree: &mut Tree<Token>, _data: &mut Data) {
    tree.children
        .retain(|child| !child.x.chars().all(char::is_whitespace));
}

fn group_brackets(tree: &mut Tree<Token>, _data: &mut Data) {
    let match_opener = |token: &str| match token {
        "(" => ")",
        "{" => "}",
        "[" => "]",
        s => panic!("{} is not a bracket\n", s),
    };
    let mut openers: Vec<(usize, Token)> = Vec::new();
    let mut i = 0;
    while i < tree.children.len() {
        #[allow(clippy::needless_borrow)]
        match tree.children[i].x.deref() {
            "(" | "{" | "[" => openers.push((i, tree.children[i].x.clone())),
            ")" | "}" | "]" => match openers.pop() {
                Some((l, opener)) if match_opener(&opener) == tree.children[i].x.deref() => {
                    let mut cs: Vec<Tree<Token>> = tree.children.drain(l + 1..=i).collect();
                    cs.pop(); // remove closing bracket
                    tree.children[l].children = cs;
                    i = l;
                }
                Some((_, opener)) => panic!(
                    "{} matched with {} at {} and {}\n",
                    opener.deref(),
                    tree.children[i].x.deref(),
                    opener.location(),
                    tree.children[i].x.location(),
                ),
                None => panic!(
                    "extra {} at {}\n",
                    tree.children[i].x.deref(),
                    tree.children[i].x.location()
                ),
            },
            _ => {}
        }
        i += 1;
    }
    if let Some((_, opener)) = openers.last() {
        panic!(
            "no {} for {} at {}\n",
            match_opener(opener),
            opener.deref(),
            opener.location()
        )
    }
}

#[derive(Clone)]
struct Operator {
    _func: Option<String>,
    left: usize,
    right: usize,
}
// a list of precedence levels, each of which contains
// a map from strings to Operators
// a bool for right associativity
type Precedences = Vec<(HashMap<String, Operator>, bool)>;

fn group_operators(tree: &mut Tree<Token>, data: &mut Data) {
    for child in &mut tree.children {
        group_operators(child, data)
    }
    let precedences: &Precedences = data.get("precedences");
    for (ops, right_assoc) in precedences {
        let mut i = if *right_assoc {
            tree.children.len().wrapping_sub(1)
        } else {
            0
        };
        while i < tree.children.len() {
            if let Some(op) = ops.get(tree.children[i].x.deref()) {
                if i < op.left || i + op.right >= tree.children.len() {
                    panic!(
                        "{} is missing arguments at {}",
                        tree.children[i].x.deref(),
                        tree.children[i].x.location()
                    )
                }
                let mut cs: Vec<Tree<Token>> = tree.children.drain(i - op.left..i).collect();
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
