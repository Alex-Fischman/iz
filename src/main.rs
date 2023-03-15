#![no_implicit_prelude]
extern crate std;

use std::{
    clone::Clone,
    env::args,
    fs::read_to_string,
    iter::Iterator,
    option::{Option::None, Option::Some},
    string::String,
    vec::Vec,
    {format, matches, println},
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
    token: Token<'a>,
    children: Vec<Context<'a>>,
}

// if self.graph is a tree, will work as expected
// if self.graph is a DAG, will print shared nodes multiple times
// if self.graph has cycles, will loop forever
fn print_tree(c: &Context, indent: usize) {
    println!(
        "{}at {}:\t{}",
        "\t".repeat(indent),
        c.token.location(),
        c.token.as_str(),
    );
    for child in &c.children {
        print_tree(child, indent + 1);
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

    let mut c: Context = Context {
        token: Token {
            source,
            lo: 0,
            hi: 0,
        },
        children: Vec::new(),
    };

    let is = source.text.char_indices().map(|(i, _)| i);
    let js = source.text.char_indices().map(|(j, _)| j);
    for (i, j) in is.zip(js.skip(1).chain([source.text.len()])) {
        c.children.push(Context {
            token: Token {
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
        pass(&mut c)
    }

    print_tree(&c, 0);
}

fn remove_comments(c: &mut Context) {
    let mut i = 0;
    while i < c.children.len() {
        if c.children[i].token.as_str() == "#" {
            let mut j = i;
            while j < c.children.len() && c.children[j].token.as_str() != "\n" {
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
        let curr = &c.children[i].token;
        let prev = &c.children[i - 1].token;
        if !is_bracket(curr.as_str())
            && token_type(curr.as_str()) == token_type(prev.as_str())
            && curr.source == prev.source
            && prev.hi == curr.lo
        {
            c.children[i - 1].token.hi = curr.hi;
            c.children.remove(i);
        } else {
            i += 1;
        }
    }
}

fn remove_whitespace(c: &mut Context) {
    c.children
        .retain(|child| !child.token.as_str().chars().all(char::is_whitespace));
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
        match c.children[i].token.as_str() {
            "(" | "{" | "[" => openers.push((i, c.children[i].token.clone())),
            ")" | "}" | "]" => match openers.pop() {
                Some((l, opener)) if match_opener(&opener) == c.children[i].token.as_str() => {
                    let mut cs: Vec<Context> = c.children.drain(l + 1..=i).collect();
                    cs.pop(); // remove closing bracket
                    c.children[l].children = cs;
                    i = l;
                }
                Some((_, opener)) => panic!(
                    "{} matched with {} at {} and {}\n",
                    opener.as_str(),
                    c.children[i].token.as_str(),
                    opener.location(),
                    c.children[i].token.location(),
                ),
                None => panic!(
                    "extra {} at {}\n",
                    c.children[i].token.as_str(),
                    c.children[i].token.location()
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
