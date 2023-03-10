#![no_implicit_prelude]
extern crate std;

use std::borrow::ToOwned;
use std::clone::Clone;
use std::collections::HashMap;
use std::convert::From;
use std::env::args;
use std::fmt::{Debug, Formatter, Result};
use std::fs::read_to_string;
use std::iter::{Extend, Iterator};
use std::ops::{FnMut, Index, IndexMut};
use std::option::{Option, Option::None, Option::Some};
use std::result::Result::Ok;
use std::string::{String, ToString};
use std::vec::Vec;
use std::{matches, panic, print, println, vec, write, writeln};

type Node = usize;

// holds all the information given to different passes
struct Context {
    id: usize,
    edges: HashMap<Node, Vec<Node>>, // adjency list from nodes to children
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

// should be extensible
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

// for the bytecode interpreter
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
    context.edges.insert(root, vec![]);
    assert!(root == 0);

    for c in text.chars() {
        let node = context.id();
        context.edges.insert(node, vec![]);
        context.edges.get_mut(&0).unwrap().push(node);
        context.chars.insert(node, c);
    }

    // compiler
    let passes = [
        // context starts out flat
        remove_comments,
        chars_to_strings,
        remove_whitespace,
        group_brackets, // changes context to a tree
        group_and_unroll_operators,
        unroll_brackets,
        integer_literals,
        substitute_macros, // changes context to a DAG
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
    let mut data = Memory(vec![]);
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
    let children = &mut context.edges.get_mut(&0).unwrap();
    while i < children.len() {
        if context.chars.get(&children[i]) == Some(&'#') {
            let mut j = i;
            while j < children.len() && context.chars.remove(&children[j]) != Some('\n') {
                j += 1;
            }
            context.chars.insert(children[j], '\n');
            children.drain(i..j);
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
    let children = &mut context.edges.get_mut(&0).unwrap();
    while i < children.len() {
        let c = context.chars.remove(&children[i]).unwrap();
        if i == 0 {
            context.strings.insert(children[i], c.to_string());
            i += 1;
        } else {
            let s = context.strings.get_mut(&children[i - 1]).unwrap();
            if !is_bracket(c) && char_type(c) == char_type(s.chars().next().unwrap()) {
                s.push(c);
                children.remove(i);
            } else {
                context.strings.insert(children[i], c.to_string());
                i += 1;
            }
        }
    }
    assert!(context.chars.is_empty());
}

fn remove_whitespace(context: &mut Context) {
    let mut i = 0;
    let children = &mut context.edges.get_mut(&0).unwrap();
    while i < children.len() {
        let s = context.strings.get(&children[i]).unwrap();
        if s.chars().next().unwrap().is_whitespace() {
            children.remove(i);
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
            let mut handle_open_bracket = |close| {
                let start = *i;
                bracket_matcher(context, i, Some(close));
                let mut cs: Vec<Node> = context
                    .edges
                    .get_mut(&0)
                    .unwrap()
                    .drain(start..*i)
                    .collect();
                cs.pop(); // remove the closing bracket
                *context.edges.get_mut(&child).unwrap() = cs;
                *i = start;
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
                let cs: Vec<Node> = context.edges.get_mut(&child).unwrap().drain(..).collect();
                let l = cs.len();
                context.edges.get_mut(&node).unwrap().splice(i..=i, cs);
                i += l;
            } else {
                i += 1;
            }
        }
    });
}

fn group_and_unroll_operators(context: &mut Context) {
    //                   func     left   right  unroll
    type Operator<'a> = (&'a str, usize, usize, bool);
    //                                           right associativity
    let operators: Vec<(HashMap<&str, Operator>, bool)> = vec![
        (HashMap::from([(":", (":", 1, 0, false))]), false),
        (HashMap::from([("?", ("?", 1, 0, false))]), false),
        (HashMap::from([("~", ("~", 0, 1, false))]), true),
        (HashMap::from([("$", ("$", 0, 1, false))]), true),
        (
            HashMap::from([("-", ("neg", 0, 1, true)), ("!", ("not", 0, 1, true))]),
            true,
        ),
        (HashMap::from([("+", ("add", 1, 1, true))]), false),
        (HashMap::from([("macro", ("macro", 0, 2, false))]), true),
    ];

    context.postorder(0, |context, node| {
        for (ops, right_assoc) in &operators {
            let mut i = if *right_assoc {
                context.edges[&node].len().wrapping_sub(1)
            } else {
                0
            };
            while let Some(child) = context.edges[&node].get(i).copied() {
                let s = context.strings.get(&child).unwrap().clone();
                if let Some((_func, left, right, _unroll)) = ops.get(&*s) {
                    if i < *left || i + right >= context.edges[&node].len() {
                        panic!("not enough operator arguments for {s}");
                    }
                    let mut cs: Vec<Node> = context
                        .edges
                        .get_mut(&node)
                        .unwrap()
                        .drain(i + 1..=i + right)
                        .collect();
                    cs.extend(context.edges.get_mut(&node).unwrap().drain(i - left..i));
                    i -= left;
                    *context.edges.get_mut(&child).unwrap() = cs;
                }
                i = if *right_assoc {
                    i.wrapping_sub(1)
                } else {
                    i + 1
                }
            }
        }
    });

    context.postorder(0, |context, node| {
        let mut i = 0;
        while i < context.edges[&node].len() {
            let child = context.edges[&node][i];
            let s = context.strings.get(&child).unwrap();
            if let Some((func, _left, _right, unroll)) =
                operators.iter().find_map(|(ops, _)| ops.get(&**s))
            {
                if *unroll {
                    let cs: Vec<Node> = context.edges.get_mut(&child).unwrap().drain(..).collect();
                    let l = cs.len();
                    context.edges.get_mut(&node).unwrap().splice(i..i, cs);
                    i += l;
                }
                context.strings.insert(child, (*func).to_owned());
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
    let mut macros = vec![];
    context.postorder(0, |context, node| {
        let mut i = 0;
        while i < context.edges[&node].len() {
            let child = context.edges[&node][i];
            if context.strings.get(&child) == Some(&"macro".to_owned()) {
                let key = context.strings.remove(&context.edges[&child][0]).unwrap();
                let value = context.edges[&child][1..].to_vec();
                macros.push((key, value));
                context.edges.get_mut(&node).unwrap().remove(i);
            } else {
                i += 1;
            }
        }
    });

    // adjancency list for macro dependency graph
    type Macro = usize;
    type Edges = HashMap<Macro, HashMap<Macro, ()>>;
    let mut edges: Edges = HashMap::new();
    for (i, (_, nodes)) in macros.iter().enumerate() {
        for (j, (key, _)) in macros.iter().enumerate() {
            let mut dependency = false;
            for node in nodes {
                context.postorder(*node, |context, node| {
                    if context.strings.get(&node) == Some(key) {
                        dependency = true;
                    }
                });
            }
            if dependency {
                edges.entry(i).or_default().insert(j, ());
            }
        }
    }

    fn has_incoming(edges: &Edges, i: Macro) -> bool {
        for ws in edges.values() {
            for (w, ()) in ws {
                if i == *w {
                    return true;
                }
            }
        }
        false
    }

    // topological sort using Kahn's algorithm
    let mut sorted = vec![];
    let mut no_incoming: Vec<Macro> = macros
        .iter()
        .enumerate()
        .map(|(i, _)| i)
        .filter(|i| !has_incoming(&edges, *i))
        .collect();

    while let Some(v) = no_incoming.pop() {
        sorted.push(v);
        if let Some(ws) = edges.remove(&v) {
            for (w, ()) in ws {
                if !has_incoming(&edges, w) {
                    no_incoming.push(w)
                }
            }
        }
    }

    if !edges.is_empty() {
        for (v, ws) in edges {
            for (w, ()) in ws {
                println!("{:?} -> {:?}", macros[v].0, macros[w].0);
            }
        }
        panic!("macro dependency cycle detected")
    }

    for i in sorted {
        let (key, value) = &macros[i];
        context.postorder(0, |context, node| {
            let mut i = 0;
            while i < context.edges[&node].len() {
                let child = context.edges[&node][i];
                if context.strings.get(&child) == Some(key) {
                    context
                        .edges
                        .get_mut(&node)
                        .unwrap()
                        .splice(i..=i, value.clone());
                    i += value.len();
                } else {
                    i += 1;
                }
            }
        });
    }
}
