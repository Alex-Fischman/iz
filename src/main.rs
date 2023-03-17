#![no_implicit_prelude]
extern crate std;

use std::{
    any::Any,
    borrow::ToOwned,
    boxed::Box,
    clone::Clone,
    collections::HashMap,
    env::args,
    fmt::{Debug, Display, Formatter, Result as FmtResult},
    fs::read_to_string,
    iter::{Extend, IntoIterator, Iterator},
    ops::{Deref, Index, IndexMut},
    option::{Option, Option::None, Option::Some},
    string::String,
    vec::Vec,
    {format, matches, println, write},
};

macro_rules! panic {
    () => {{ std::process::exit(-1); }};
    ($fmt:literal) => {{ std::eprintln!($fmt); std::process::exit(-1); }};
    ($fmt:literal, $($arg:tt)*) => {{ std::eprintln!($fmt, $($arg)*); std::process::exit(-1); }};
}

// these can't go inside Token for a few reasons:
// - Token should be as small as possible
// - Token needs a reference to the beginning of the file for location
// - Token needs a reference to the file name for location as well
#[derive(Hash, PartialEq, Eq)]
struct Source {
    name: String,
    text: String,
}

#[derive(Clone, Hash, PartialEq, Eq)]
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
    // since Data can only hold 'static, we can't use Token<'_>
    // so instead we use a usize and intentionally forget the lifetime information
    // apparantly this is safe? maybe because we never try to go backwards?
    // this doesn't need to consider self.hi because tokens never overlap
    fn key(&self) -> usize {
        self.deref() as *const str as *const u8 as usize
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        if self.lo == self.hi {
            return FmtResult::Ok(());
        }
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
        write!(
            f,
            "{} at {}:{}:{}",
            self.deref(),
            self.source.name,
            row,
            col
        )
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
        self.0
            .get(key)
            .unwrap_or_else(|| panic!("{} was not found in data", key))
            .downcast_ref::<T>()
            .unwrap_or_else(|| panic!("{} had the wrong type in data", key))
    }
}

fn main() {
    let args: Vec<String> = args().collect();
    let file = args.get(1);
    let file = file.unwrap_or_else(|| panic!("expected command line argument"));
    let text = read_to_string(file).unwrap_or_else(|_| panic!("could not read {}", file));
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
        parse_int_literals,
        group_brackets,
        insert_default_operators,
        group_operators,
        // flatten
        unroll_operators,
        unroll_brackets,
        // transformation
        generate_ops,
        // backend
        interpret,
    ];
    for pass in passes {
        pass(&mut tree, &mut data)
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

fn parse_int_literals(tree: &mut Tree, data: &mut Data) {
    let ints: HashMap<usize, i64> = tree
        .children
        .iter()
        .filter_map(|child| {
            child
                .token
                .parse::<i64>()
                .ok()
                .map(|int| (child.token.key(), int))
        })
        .collect();
    data.insert("ints", ints);
}

fn group_brackets(tree: &mut Tree, _data: &mut Data) {
    let match_opener = |token: &str| match token {
        "(" => ")",
        "{" => "}",
        "[" => "]",
        s => panic!("{} is not a bracket", s),
    };

    let mut openers = Vec::new();
    let mut i = 0;
    while i < tree.children.len() {
        match tree.children[i].token.deref() {
            "(" | "{" | "[" => openers.push(i),
            ")" | "}" | "]" => match openers.pop() {
                None => panic!("extra {}", tree.children[i].token),
                Some(l) => {
                    let opener = &tree.children[l].token;
                    if match_opener(opener) == tree.children[i].token.deref() {
                        let mut cs: Vec<Tree> = tree.children.drain(l + 1..=i).collect();
                        cs.pop(); // remove closing bracket
                        tree.children[l].children = cs;
                        i = l;
                    } else {
                        panic!("{} matched with {}", opener, tree.children[i].token)
                    }
                }
            },
            _ => {}
        }
        i += 1;
    }
    if let Some(l) = openers.pop() {
        let opener = &tree.children[l].token;
        panic!("no {} for the {}", match_opener(opener), opener)
    }
}

fn unroll_brackets(tree: &mut Tree, _data: &mut Data) {
    for child in &mut tree.children {
        unroll_brackets(child, _data)
    }

    let mut i = 0;
    while i < tree.children.len() {
        if tree.children[i].token.deref() == "(" {
            let cs: Vec<Tree> = tree.children[i].children.drain(..).collect();
            let len = cs.len();
            tree.children.splice(i..=i, cs);
            i += len;
        } else {
            i += 1;
        }
    }
}

struct Operator {
    // for now, Some is a function unroll and None is a macro invoke
    // this will change once macro infrastructure is set up
    func: Option<String>,
    left: usize,
    right: usize,
}

enum Associativity {
    Left,
    Right,
}

struct Operators {
    ops: HashMap<String, Operator>,
    associativity: Associativity,
}

fn insert_default_operators(_tree: &mut Tree, data: &mut Data) {
    fn ops(ops: &[(&str, Option<&str>, usize, usize)], associativity: Associativity) -> Operators {
        let ops = ops
            .iter()
            .map(|&(name, func, left, right)| {
                let func = func.map(|s| s.to_owned());
                (name.to_owned(), Operator { func, left, right })
            })
            .collect();
        Operators { ops, associativity }
    }

    use Associativity::{Left, Right};
    let precedences = [
        ops(&[(":", None, 1, 0)], Left),
        ops(&[("?", None, 1, 0)], Left),
        ops(&[("~", None, 0, 1)], Right),
        ops(&[("$", None, 0, 1)], Right),
        ops(&[("-", Some("neg"), 0, 1), ("!", Some("not"), 0, 1)], Right),
        ops(&[("+", Some("add"), 1, 1)], Left),
    ];
    data.insert("precedences", precedences.into_iter().collect::<Vec<_>>());
}

fn group_operators(tree: &mut Tree, data: &mut Data) {
    for child in &mut tree.children {
        group_operators(child, data)
    }

    let precedences: &Vec<_> = data.get("precedences");
    for Operators { ops, associativity } in precedences {
        let mut i = match associativity {
            Associativity::Left => 0,
            Associativity::Right => tree.children.len().wrapping_sub(1),
        };
        while i < tree.children.len() {
            if let Some(op) = ops.get(tree.children[i].token.deref()) {
                if i < op.left || i + op.right >= tree.children.len() {
                    panic!("missing arguments for {}", tree.children[i].token,)
                }
                let mut cs: Vec<Tree> = tree.children.drain(i - op.left..i).collect();
                i -= op.left;
                cs.extend(tree.children.drain(i + 1..=i + op.right));
                tree.children[i].children = cs;
            }
            i = match associativity {
                Associativity::Left => i + 1,
                Associativity::Right => i.wrapping_sub(1),
            };
        }
    }
}

fn unroll_operators(tree: &mut Tree, data: &mut Data) {
    for child in &mut tree.children {
        unroll_operators(child, data)
    }

    let precedences: &Vec<Operators> = data.get("precedences");
    let mut i = 0;
    while i < tree.children.len() {
        let s = tree.children[i].token.deref();
        if let Some(op) = precedences.iter().find_map(|ops| ops.ops.get(s)) {
            if let Some(_func) = &op.func {
                let cs: Vec<Tree> = tree.children[i].children.drain(..).collect();
                let len = cs.len();
                tree.children.splice(i..i, cs);
                i += len;
            }
        }
        i += 1;
    }
}

struct Stack(Vec<i64>);

impl Index<i64> for Stack {
    type Output = i64;
    fn index(&self, i: i64) -> &i64 {
        &self.0[i as usize]
    }
}

impl IndexMut<i64> for Stack {
    fn index_mut(&mut self, i: i64) -> &mut i64 {
        let i = i as usize;
        if self.0.len() <= i {
            self.0.resize(i + 1, 0);
        }
        &mut self.0[i]
    }
}

trait Operation: Debug {
    fn run(&self, pc: &mut i64, sp: &mut i64, stack: &mut Stack, data: &Data);
}

mod operations {
    use crate::*;
    #[derive(Debug)]
    pub struct Push(pub i64);
    impl Operation for Push {
        fn run(&self, _pc: &mut i64, sp: &mut i64, stack: &mut Stack, _data: &Data) {
            stack[*sp + 1] = self.0;
            *sp += 1;
        }
    }
    #[derive(Debug)]
    pub struct Move(pub i64);
    impl Operation for Move {
        fn run(&self, _pc: &mut i64, sp: &mut i64, _stack: &mut Stack, _data: &Data) {
            *sp -= self.0;
        }
    }
    #[derive(Debug)]
    pub struct Copy(pub i64);
    impl Operation for Copy {
        fn run(&self, _pc: &mut i64, sp: &mut i64, stack: &mut Stack, _data: &Data) {
            stack[*sp + 1] = stack[*sp - self.0];
            *sp += 1;
        }
    }
    #[derive(Debug)]
    pub struct Add;
    impl Operation for Add {
        fn run(&self, _pc: &mut i64, sp: &mut i64, stack: &mut Stack, _data: &Data) {
            stack[*sp - 1] += stack[*sp];
            *sp -= 1;
        }
    }
    #[derive(Debug)]
    pub struct Neg;
    impl Operation for Neg {
        fn run(&self, _pc: &mut i64, sp: &mut i64, stack: &mut Stack, _data: &Data) {
            stack[*sp] = -stack[*sp];
        }
    }
    #[derive(Debug)]
    pub struct Ltz;
    impl Operation for Ltz {
        fn run(&self, _pc: &mut i64, sp: &mut i64, stack: &mut Stack, _data: &Data) {
            stack[*sp] = (stack[*sp] < 0) as i64;
        }
    }
    #[derive(Debug)]
    pub struct Jumpz(pub String);
    impl Operation for Jumpz {
        fn run(&self, pc: &mut i64, sp: &mut i64, stack: &mut Stack, data: &Data) {
            let labels: &HashMap<String, i64> = data.get("labels");
            match labels.get(&self.0) {
                Some(i) => {
                    if stack[*sp] == 0 {
                        *pc = *i;
                    }
                    *sp -= 1;
                }
                None => panic!("could not find label {}", self.0),
            }
        }
    }
    #[derive(Debug)]
    pub struct Label(pub String);
    impl Operation for Label {
        fn run(&self, _pc: &mut i64, _sp: &mut i64, _stack: &mut Stack, _data: &Data) {}
    }
}

fn generate_ops(tree: &mut Tree, data: &mut Data) {
    let ints: &HashMap<usize, i64> = data.get("ints");
    let precedences: &Vec<Operators> = data.get("precedences");
    let mut ops: HashMap<usize, Box<dyn Operation>> = HashMap::new();
    let mut labels: HashMap<String, i64> = HashMap::new();
    for (i, child) in tree.children.iter_mut().enumerate() {
        if let Some(int) = ints.get(&child.token.key()) {
            ops.insert(child.token.key(), Box::new(operations::Push(*int)));
        } else {
            let s = child.token.deref();
            let s = precedences
                .iter()
                .find_map(|ops| ops.ops.get(s))
                .and_then(|Operator { func, .. }| func.as_ref().map(|s| s.as_str()))
                .unwrap_or(s);
            match s {
                "~" => {
                    let arg = child.children.pop().unwrap();
                    let arg = ints.get(&arg.token.key()).unwrap();
                    ops.insert(child.token.key(), Box::new(operations::Move(*arg)));
                }
                "$" => {
                    let arg = child.children.pop().unwrap();
                    let arg = ints.get(&arg.token.key()).unwrap();
                    ops.insert(child.token.key(), Box::new(operations::Copy(*arg)));
                }
                "add" => {
                    ops.insert(child.token.key(), Box::new(operations::Add));
                }
                "neg" => {
                    ops.insert(child.token.key(), Box::new(operations::Neg));
                }
                "ltz" => {
                    ops.insert(child.token.key(), Box::new(operations::Ltz));
                }
                "?" => {
                    let label = child.children.pop().unwrap().token.deref().to_owned();
                    ops.insert(child.token.key(), Box::new(operations::Jumpz(label)));
                }
                ":" => {
                    let label = child.children.pop().unwrap().token.deref().to_owned();
                    labels.insert(label.clone(), i as i64);
                    ops.insert(child.token.key(), Box::new(operations::Label(label)));
                }
                _ => {}
            }
        }
    }
    data.insert("ops", ops);
    data.insert("labels", labels);
}

fn interpret(tree: &mut Tree, data: &mut Data) {
    let ops: &HashMap<usize, Box<dyn Operation>> = data.get("ops");
    for child in &tree.children {
        if !child.children.is_empty() {
            panic!("found child of {}", child.token)
        }
        if ops.get(&child.token.key()).is_none() {
            panic!("no operation for {}", child.token)
        }
    }

    let mut pc = 0;
    let mut sp: i64 = -1;
    let mut stack = Stack(Vec::new());
    while let Some(child) = tree.children.get(pc as usize) {
        let op = &ops[&child.token.key()];
        op.run(&mut pc, &mut sp, &mut stack, data);
        pc += 1;
        match sp {
            -1 => println!("{:<32}\t", format!("{:?}", op)),
            sp => println!(
                "{:<32}\t{:?}",
                format!("{:?}", op),
                &stack.0[0..=sp as usize]
            ),
        }
    }
}
