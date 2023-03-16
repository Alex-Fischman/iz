#![no_implicit_prelude]
extern crate std;

use std::{
    borrow::ToOwned,
    clone::Clone,
    collections::HashMap,
    env::args,
    fmt::{Display, Formatter, Result as FmtResult},
    fs::read_to_string,
    iter::{Extend, Iterator},
    ops::{Deref, DerefMut},
    option::{Option, Option::None, Option::Some},
    result::Result::Ok,
    string::String,
    vec::Vec,
    {format, matches, println, writeln},
};

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

struct Context<'a> {
    tree: Tree<Token<'a>>,
    precedences: Precedences<'a>,
}

impl<'a> Deref for Context<'a> {
    type Target = Tree<Token<'a>>;
    fn deref(&self) -> &Tree<Token<'a>> {
        &self.tree
    }
}

impl<'a> DerefMut for Context<'a> {
    fn deref_mut(&mut self) -> &mut Tree<Token<'a>> {
        &mut self.tree
    }
}

impl Display for Context<'_> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        fn print_tree(tree: &Tree<Token>, f: &mut Formatter, indent: usize) -> FmtResult {
            writeln!(
                f,
                "{}at {}:\t{}",
                "\t".repeat(indent),
                tree.x.location(),
                tree.x.as_str(),
            )?;
            for child in &tree.children {
                print_tree(child, f, indent + 1)?;
            }
            Ok(())
        }
        print_tree(&self.tree, f, 0)
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
        let mut map = HashMap::new();
        for &(name, func, left, right) in ops {
            map.insert(
                name.to_owned(),
                Operator {
                    func: func.map(|s| s.to_owned()),
                    left,
                    right,
                },
            );
        }
        map
    };
    let precedences: Precedences = &[
        (ops(&[(":", None, 1, 0)]), false),
        (ops(&[("?", None, 1, 0)]), false),
        (ops(&[("~", None, 0, 1)]), true),
        (ops(&[("$", None, 0, 1)]), true),
        (
            ops(&[("-", Some("neg"), 0, 1), ("!", Some("not"), 0, 1)]),
            true,
        ),
        (ops(&[("+", Some("add"), 1, 1)]), false),
    ];

    let mut c: Context = Context {
        tree: Tree {
            x: Token {
                source,
                lo: 0,
                hi: 0,
            },
            children: Vec::new(),
        },
        precedences,
    };

    let is = source.text.char_indices().map(|(i, _)| i);
    let js = source.text.char_indices().map(|(j, _)| j);
    for (i, j) in is.zip(js.skip(1).chain([source.text.len()])) {
        c.children.push(Tree {
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
        pass(&mut c)
    }

    println!("{}", c);
}

fn remove_comments(c: &mut Context) {
    let mut i = 0;
    while i < c.children.len() {
        if c.children[i].x.as_str() == "#" {
            let mut j = i;
            while j < c.children.len() && c.children[j].x.as_str() != "\n" {
                j += 1;
            }
            c.children.drain(i..=j);
        } else {
            i += 1;
        }
    }
}

fn group_tokens(c: &mut Context) {
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
    while i < c.children.len() {
        let curr = &c.children[i].x;
        let prev = &c.children[i - 1].x;
        if !is_bracket(curr.as_str())
            && token_type(curr.as_str()) == token_type(prev.as_str())
            && curr.source == prev.source
            && prev.hi == curr.lo
        {
            c.children[i - 1].x.hi = curr.hi;
            c.children.remove(i);
        } else {
            i += 1;
        }
    }
}

fn remove_whitespace(c: &mut Context) {
    c.tree
        .children
        .retain(|child| !child.x.as_str().chars().all(char::is_whitespace));
}

fn group_brackets(c: &mut Context) {
    let match_opener = |token: &Token| match token.as_str() {
        "(" => ")",
        "{" => "}",
        "[" => "]",
        s => panic!("{} is not a bracket\n", s),
    };
    let mut openers = Vec::new();
    let mut i = 0;
    while i < c.children.len() {
        match c.children[i].x.as_str() {
            "(" | "{" | "[" => openers.push((i, c.children[i].x.clone())),
            ")" | "}" | "]" => match openers.pop() {
                Some((l, opener)) if match_opener(&opener) == c.children[i].x.as_str() => {
                    let mut cs: Vec<Tree<Token>> = c.children.drain(l + 1..=i).collect();
                    cs.pop(); // remove closing bracket
                    c.children[l].children = cs;
                    i = l;
                }
                Some((_, opener)) => panic!(
                    "{} matched with {} at {} and {}\n",
                    opener.as_str(),
                    c.children[i].x.as_str(),
                    opener.location(),
                    c.children[i].x.location(),
                ),
                None => panic!(
                    "extra {} at {}\n",
                    c.children[i].x.as_str(),
                    c.children[i].x.location()
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
            opener.as_str(),
            opener.location()
        )
    }
}

#[derive(Clone)]
struct Operator {
    func: Option<String>,
    left: usize,
    right: usize,
}
// a list of precedence levels, each of which contains
// a map from strings to Operators
// a bool for right associativity
type Precedences<'a> = &'a [(HashMap<String, Operator>, bool)];

fn group_operators(c: &mut Context) {
    group_operators(&mut c.tree, c.precedences);
    fn group_operators(tree: &mut Tree<Token>, precedences: Precedences) {
        for child in &mut tree.children {
            group_operators(child, precedences)
        }

        for (ops, right_assoc) in precedences {
            let mut i = if *right_assoc {
                tree.children.len().wrapping_sub(1)
            } else {
                0
            };
            while i < tree.children.len() {
                if let Some(op) = ops.get(tree.children[i].x.as_str()) {
                    if i < op.left || i + op.right >= tree.children.len() {
                        panic!(
                            "{} is missing arguments at {}",
                            tree.children[i].x.as_str(),
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
}
