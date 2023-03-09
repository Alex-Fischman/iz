#![no_implicit_prelude]
extern crate std;

use std::borrow::ToOwned;
use std::clone::Clone;
use std::collections::HashMap;
use std::env::args;
use std::fmt::{Debug, Formatter, Result};
use std::fs::read_to_string;
use std::iter::{Extend, Iterator};
use std::ops::{FnMut, Index, IndexMut};
use std::option::{Option, Option::None, Option::Some};
use std::result::Result::Ok;
use std::string::{String, ToString};
use std::vec::Vec;
use std::{matches, panic, print, println, write, writeln};

type Node = usize;

struct Context {
    id: usize,
    edges: HashMap<Node, Vec<Node>>,
    chars: HashMap<Node, char>,
    strings: HashMap<Node, String>,
    ints: HashMap<Node, i64>,
}

impl Debug for Context {
    fn fmt(&self, f: &mut Formatter) -> Result {
        fn print_tree(context: &Context, f: &mut Formatter, node: Node, depth: usize) -> Result {
            write!(f, "{} {}:", "----".repeat(depth), node)?;
            if let Some(c) = context.chars.get(&node) {
                write!(f, " {}", c)?;
            }
            if let Some(s) = context.strings.get(&node) {
                write!(f, " {}", s)?;
            }
            if let Some(i) = context.ints.get(&node) {
                write!(f, " {}", i)?;
            }
            writeln!(f)?;
            for child in &context.edges[&node] {
                print_tree(context, f, *child, depth + 1)?;
            }
            Ok(())
        }
        print_tree(self, f, 0, 0)
    }
}

impl Context {
    fn new() -> Context {
        Context {
            id: 0,
            edges: HashMap::new(),
            chars: HashMap::new(),
            strings: HashMap::new(),
            ints: HashMap::new(),
        }
    }

    fn id(&mut self) -> usize {
        self.id += 1;
        self.id - 1
    }

    fn preorder<F: FnMut(&mut Context, Node)>(&mut self, root: Node, mut f: F) {
        preorder(self, root, &mut f);
        fn preorder<F: FnMut(&mut Context, Node)>(context: &mut Context, node: Node, f: &mut F) {
            f(context, node);
            for child in context.edges[&node].clone() {
                preorder(context, child, f)
            }
        }
    }

    fn postorder<F: FnMut(&mut Context, Node)>(&mut self, root: Node, mut f: F) {
        postorder(self, root, &mut f);
        fn postorder<F: FnMut(&mut Context, Node)>(context: &mut Context, node: Node, f: &mut F) {
            for child in context.edges[&node].clone() {
                postorder(context, child, f)
            }
            f(context, node);
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Op {
    Push(i64),
    Move(i64),
    Copy(i64),
    Add,
    Neg,
    Ltz,
    Jumpz(String),
    Label(String),
}

struct Memory(Vec<i64>);

impl Index<i64> for Memory {
    type Output = i64;
    fn index(&self, i: i64) -> &i64 {
        &self.0[i as usize]
    }
}

impl IndexMut<i64> for Memory {
    fn index_mut(&mut self, i: i64) -> &mut i64 {
        let i = i as usize;
        if self.0.len() <= i {
            self.0.resize(i + 1, 0);
        }
        &mut self.0[i]
    }
}

fn main() {
    // frontend
    let args: Vec<String> = args().collect();
    let file = args.get(1).expect("no file passed");
    let text = read_to_string(file).expect("could not read file");
    let mut context = Context::new();

    let root = context.id();
    context.edges.insert(root, Vec::new());
    assert!(root == 0);

    for c in text.chars() {
        let node = context.id();
        context.edges.insert(node, Vec::new());
        context.edges.get_mut(&0).unwrap().push(node);
        context.chars.insert(node, c);
    }

    // compiler
    let passes = [
        // here context is flat
        remove_comments,
        chars_to_strings,
        remove_whitespace,
        // here context is a tree
        group_brackets,
        group_operators,
        unroll_operators,
        substitute_macros,
        // here context is a DAG
        unroll_brackets,
        integer_literals,
    ];
    for pass in passes {
        pass(&mut context)
    }

    // backend
    let code: Vec<Op> = context.edges[&0]
        .iter()
        .cloned()
        .map(|node| {
            if let Some(int) = context.ints.get(&node) {
                Op::Push(*int)
            } else if let Some(s) = context.strings.get(&node) {
                match s.as_str() {
                    "~" => Op::Move(*context.ints.get(&context.edges[&node][0]).unwrap()),
                    "$" => Op::Copy(*context.ints.get(&context.edges[&node][0]).unwrap()),
                    "add" => Op::Add,
                    "neg" => Op::Neg,
                    "ltz" => Op::Ltz,
                    "?" => Op::Jumpz(
                        context
                            .strings
                            .get(&context.edges[&node][0])
                            .unwrap()
                            .clone(),
                    ),
                    ":" => Op::Label(
                        context
                            .strings
                            .get(&context.edges[&node][0])
                            .unwrap()
                            .clone(),
                    ),
                    s => panic!("expected an op, found {s}"),
                }
            } else {
                panic!("expected an int or a string, found neither")
            }
        })
        .collect();

    let mut labels = HashMap::new();
    for (pc, op) in code.iter().enumerate() {
        if let Op::Label(label) = op {
            let old = labels.insert(label.clone(), pc as i64);
            assert!(old.is_none());
        }
    }

    let mut pc = 0;
    let mut data = Memory(Vec::new());
    let mut sp = -1;

    while (pc as usize) < code.len() {
        print!("{:?}\t", code[pc as usize]);

        match &code[pc as usize] {
            Op::Push(i) => {
                data[sp + 1] = *i;
                sp += 1;
            }
            Op::Move(i) => sp -= i,
            Op::Copy(i) => {
                data[sp + 1] = data[sp - i];
                sp += 1;
            }
            Op::Add => {
                data[sp - 1] += data[sp];
                sp -= 1;
            }
            Op::Neg => data[sp] = -data[sp],
            Op::Ltz => data[sp] = (data[sp] < 0) as i64,
            Op::Jumpz(label) if labels.contains_key(label) => {
                if data[sp] == 0 {
                    pc = labels[label];
                }
                sp -= 1;
            }
            Op::Jumpz(label) => panic!("unknown label {}", label),
            Op::Label(_) => {}
        }

        match sp {
            -1 => println!(),
            sp => println!("{:?}", &data.0[0..=sp as usize]),
        }

        pc += 1;
    }
}

fn remove_comments(context: &mut Context) {
    let mut i = 0;
    while i < context.edges[&0].len() {
        if context.chars.get(&context.edges[&0][i]) == Some(&'#') {
            let mut j = i;
            while j < context.edges[&0].len()
                && context.chars.remove(&context.edges[&0][j]) != Some('\n')
            {
                j += 1;
            }
            context.chars.insert(context.edges[&0][j], '\n');
            context.edges.get_mut(&0).unwrap().drain(i..j);
        } else {
            i += 1;
        }
    }
}

fn chars_to_strings(context: &mut Context) {
    fn is_bracket(c: char) -> bool {
        matches!(c, '(' | ')' | '{' | '}' | '[' | ']')
    }

    fn is_identifier(c: char) -> bool {
        matches!(c, '-' | '_' | 'a'..='z' | 'A'..='Z' | '0'..='9')
    }

    fn char_type(c: char) -> usize {
        match c {
            _ if is_identifier(c) => 0,
            _ if c.is_whitespace() => 1,
            _ if is_bracket(c) => 2,
            _ => 3, // operators
        }
    }

    assert!(context.strings.is_empty());
    let mut i = 0;
    while i < context.edges[&0].len() {
        let c = context.chars.remove(&context.edges[&0][i]).unwrap();
        if i == 0 {
            context.strings.insert(context.edges[&0][i], c.to_string());
            i += 1;
        } else {
            let s = context.strings.get_mut(&context.edges[&0][i - 1]).unwrap();
            if !is_bracket(c) && char_type(c) == char_type(s.chars().next().unwrap()) {
                s.push(c);
                context.edges.get_mut(&0).unwrap().remove(i);
            } else {
                context.strings.insert(context.edges[&0][i], c.to_string());
                i += 1;
            }
        }
    }

    assert!(context.chars.is_empty());
}

fn remove_whitespace(context: &mut Context) {
    let mut i = 0;
    while i < context.edges[&0].len() {
        let s = context.strings.get(&context.edges[&0][i]).unwrap();
        if s.chars().next().unwrap().is_whitespace() {
            context.edges.get_mut(&0).unwrap().remove(i);
        } else {
            i += 1;
        }
    }
}

fn group_brackets(context: &mut Context) {
    bracket_matcher(context, &mut 0, None);
    fn bracket_matcher(context: &mut Context, i: &mut usize, target: Option<&str>) {
        while *i < context.edges[&0].len() {
            let child = context.edges[&0][*i];
            *i += 1;
            let s = context.strings.get(&child).unwrap().clone();
            let mut handle_open_bracket = |t| {
                let start = *i;
                bracket_matcher(context, i, Some(t));
                let mut cs: Vec<Node> = context
                    .edges
                    .get_mut(&0)
                    .unwrap()
                    .drain(start..*i)
                    .collect();
                cs.pop();
                *i = start;
                let n = context.edges[&0][*i - 1];
                assert!(context.edges[&n].is_empty());
                *context.edges.get_mut(&child).unwrap() = cs;
            };
            match s.as_str() {
                "(" => handle_open_bracket(")"),
                "{" => handle_open_bracket("}"),
                "[" => handle_open_bracket("]"),
                ")" | "}" | "]" => match target {
                    Some(t) if s == t => return,
                    _ => panic!("extra {s}"),
                },
                _ => {}
            }
        }
        if let Some(s) = target {
            panic!("missing {s}");
        }
    }
}

fn unroll_brackets(context: &mut Context) {
    context.postorder(0, |context, node| {
        let mut i = 0;
        while i < context.edges[&node].len() {
            let child = context.edges[&node][i];
            if context.strings.get(&child) == Some(&"(".to_owned()) {
                let cs = context.edges[&child].clone().to_vec();
                context.edges.get_mut(&node).unwrap().splice(i..=i, cs);
            } else {
                i += 1;
            }
        }
    });
}

//                   name     func     left   right  unroll
type Operator<'a> = (&'a str, &'a str, usize, usize, bool);
//                               right associativity
const OPERATORS: &[(&[Operator], bool)] = &[
    (&[(":", ":", 1, 0, false)], false),
    (&[("?", "?", 1, 0, false)], false),
    (&[("~", "~", 0, 1, false)], true),
    (&[("$", "$", 0, 1, false)], true),
    (&[("-", "neg", 0, 1, true), ("!", "not", 0, 1, true)], true),
    (&[("+", "add", 1, 1, true)], false),
    (&[("macro", "macro", 0, 2, false)], true),
];

fn group_operators(context: &mut Context) {
    context.postorder(0, |context, node| {
        for (ops, right) in OPERATORS {
            let mut i = if *right {
                context.edges[&node].len().wrapping_sub(1)
            } else {
                0
            };
            while let Some(child) = context.edges[&node].get(i).copied() {
                let s = context.strings.get(&child).unwrap().clone();
                if let Some(op) = ops.iter().find(|op| op.0 == s) {
                    if i < op.2 || i + op.3 >= context.edges[&node].len() {
                        panic!("not enough operator arguments for {s}");
                    }
                    let mut cs: Vec<Node> = context
                        .edges
                        .get_mut(&node)
                        .unwrap()
                        .drain(i + 1..=i + op.3)
                        .collect();
                    cs.extend(context.edges.get_mut(&node).unwrap().drain(i - op.2..i));
                    i -= op.2;
                    *context.edges.get_mut(&child).unwrap() = cs;
                }
                i = if *right { i.wrapping_sub(1) } else { i + 1 }
            }
        }
    });
}

fn unroll_operators(context: &mut Context) {
    context.postorder(0, |context, node| {
        let mut i = 0;
        while i < context.edges[&node].len() {
            let child = context.edges[&node][i];
            let s = context.strings.get(&child).unwrap();
            if let Some(op) = OPERATORS
                .iter()
                .find_map(|(ops, _)| ops.iter().find(|op| op.0 == s))
            {
                if op.4 {
                    let cs: Vec<Node> = context.edges.get_mut(&child).unwrap().drain(..).collect();
                    let l = cs.len();
                    context.edges.get_mut(&node).unwrap().splice(i..i, cs);
                    i += l;
                }
                context.strings.insert(child, op.1.to_owned());
            }
            i += 1;
        }
    });
}

fn integer_literals(context: &mut Context) {
    assert!(context.ints.is_empty());
    context.postorder(0, |context, node| {
        if let Some(s) = context.strings.get(&node) {
            if let Ok(int) = s.parse::<i64>() {
                context.strings.remove(&node);
                context.ints.insert(node, int);
            }
        }
    });
}

fn substitute_macros(context: &mut Context) {
    let mut macros: HashMap<String, Node> = HashMap::new();
    context.postorder(0, |context, node| {
        let mut i = 0;
        while i < context.edges[&node].len() {
            let child = context.edges[&node][i];
            if context.strings.get(&child) == Some(&"macro".to_owned()) {
                let key = context.edges[&child][0];
                let value = context.edges[&child][1];
                let old = macros.insert(context.strings.get(&key).unwrap().clone(), value);
                assert!(old.is_none());
                context.edges.get_mut(&node).unwrap().remove(i);
            } else {
                i += 1;
            }
        }
    });

    // replace bodies of recursive macros
    // doesn't clone nodes; links are dynamic
    let lookup = macros.clone(); // borrow checker
    let replace_node = |string: Option<&String>, node: &mut Node| {
        if let Some(s) = string {
            if let Some(replacement) = lookup.get(s) {
                *node = *replacement;
            }
        }
    };
    for node in macros.values_mut() {
        context.postorder(*node, |context, node| {
            for child in context.edges.get_mut(&node).unwrap() {
                replace_node(context.strings.get(child), child)
            }
        });
        replace_node(context.strings.get(node), node);
    }

    context.postorder(0, |context, node| {
        for child in context.edges.get_mut(&node).unwrap() {
            replace_node(context.strings.get(child), child)
        }
    });

    // cycle checking
    for root in macros.values() {
        for child in context.edges[root].clone() {
            context.preorder(child, |_context, node| {
                if node == *root {
                    panic!("macro loop detected");
                }
            });
        }
    }
}
