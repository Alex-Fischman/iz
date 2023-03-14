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

use graph::{Graph, Node};
mod graph {
    extern crate std;
    use std::{collections::HashMap, vec::Vec};

    #[derive(Clone, Copy, Eq, Hash, PartialEq)]
    pub struct Node(usize);
    pub struct Graph<T> {
        nodes: Vec<T>,
        edges: HashMap<Node, Vec<Node>>,
    }

    impl<T> Graph<T> {
        pub fn new() -> Graph<T> {
            Graph {
                nodes: Vec::new(),
                edges: HashMap::new(),
            }
        }

        pub fn node(&mut self, x: T) -> Node {
            let node = self.nodes.len();
            self.nodes.push(x);
            self.edges.insert(Node(node), Vec::new());
            Node(node)
        }

        pub fn data(&self, node: Node) -> &T {
            &self.nodes[node.0]
        }

        pub fn data_mut(&mut self, node: Node) -> &mut T {
            &mut self.nodes[node.0]
        }

        pub fn children(&self, node: Node) -> &[Node] {
            &self.edges[&node]
        }

        pub fn children_mut(&mut self, node: Node) -> &mut Vec<Node> {
            self.edges.get_mut(&node).unwrap()
        }
    }
}

type Context<'a> = Graph<Token<'a>>;

// if self.graph is a tree, will work as expected
// if self.graph is a DAG, will print shared nodes multiple times
// if self.graph has cycles, will loop forever
fn print_tree(c: &Context, root: &Node, indent: usize) {
    println!(
        "{}at {}:\t{}",
        "\t".repeat(indent),
        c.data(*root).location(),
        c.data(*root).as_str(),
    );
    for child in c.children(*root) {
        print_tree(c, child, indent + 1);
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

    let mut c = Graph::new();
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
        c.children_mut(root).push(node);
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
        pass(&mut c, root)
    }

    print_tree(&c, &root, 0);
}

fn remove_comments(c: &mut Context, root: Node) {
    let mut i = 0;
    while i < c.children(root).len() {
        if c.data(c.children(root)[i]).as_str() == "#" {
            let mut j = i;
            while j < c.children(root).len() && c.data(c.children(root)[j]).as_str() != "\n" {
                j += 1;
            }
            c.children_mut(root).drain(i..=j);
        } else {
            i += 1;
        }
    }
}

fn group_tokens(c: &mut Context, root: Node) {
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
    while i < c.children(root).len() {
        let curr = &c.data(c.children(root)[i]);
        let prev = &c.data(c.children(root)[i - 1]);
        if !is_bracket(curr.as_str())
            && token_type(curr.as_str()) == token_type(prev.as_str())
            && curr.source == prev.source
            && prev.hi == curr.lo
        {
            c.data_mut(c.children(root)[i - 1]).hi = curr.hi;
            c.children_mut(root).remove(i);
        } else {
            i += 1;
        }
    }
}

fn remove_whitespace(c: &mut Context, root: Node) {
    let mut i = 0;
    while i < c.children(root).len() {
        if c.data(c.children(root)[i])
            .as_str()
            .chars()
            .all(char::is_whitespace)
        {
            c.children_mut(root).remove(i);
        } else {
            i += 1;
        }
    }
}

fn group_brackets(c: &mut Context, root: Node) {
    let match_opener = |token: &Token| match token.as_str() {
        "(" => ")",
        "{" => "}",
        "[" => "]",
        s => panic!("{} is not a token", s),
    };
    let mut stack = Vec::new();
    let mut i = 0;
    while i < c.children(root).len() {
        let child = c.children(root)[i];
        let token = c.data(child);
        match token.as_str() {
            "(" | "{" | "[" => {
                stack.push(child);
                i += 1
            }
            ")" | "}" | "]" => match stack.pop() {
                None => panic!("extra {} at {}\n", token.as_str(), token.location()),
                Some(popped) => {
                    let opener = &c.data(popped);
                    if match_opener(opener) == token.as_str() {
                        c.children_mut(root).remove(i);
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
                let child = c.children_mut(root).remove(i);
                c.children_mut(*stack.last().unwrap()).push(child);
            }
        }
    }
}
