#![no_implicit_prelude]
extern crate std;

use std::{
    borrow::ToOwned,
    clone::Clone,
    collections::HashMap,
    convert::From,
    env::args,
    fmt::{Debug, Formatter, Result as FmtResult},
    fs::read_to_string,
    iter::{Extend, IntoIterator, Iterator},
    ops::{FnMut, Index, IndexMut},
    option::{Option, Option::None, Option::Some},
    result::{Result, Result::Err, Result::Ok},
    string::{String, ToString},
    vec::Vec,
    {matches, print, println, vec, write, writeln},
};

// usually a key in a map that stores some other data
type Node = usize;
// adjacency list, usually outgoing unless otherwise specified
// methods are all named assuming it's outgoing
#[derive(Clone)]
struct Edges(HashMap<Node, Vec<Node>>);

impl Edges {
    fn add_edge(&mut self, a: Node, b: Node) {
        self.0.entry(a).or_default().push(b)
    }

    const EMPTY: &[Node] = &[];
    fn children(&self, parent: Node) -> &[Node] {
        self.0.get(&parent).map_or(Edges::EMPTY, Vec::as_slice)
    }

    fn children_mut(&mut self, parent: Node) -> &mut Vec<Node> {
        self.0.entry(parent).or_default()
    }

    // guarantees that all nodes are present
    // this doesn't matter for add_edge, children, or children_mut
    // this does matter for topological sort
    fn from_fn<N, F>(nodes: N, mut has_edge: F) -> Edges
    where
        N: IntoIterator<Item = Node> + Clone,
        F: FnMut(Node, Node) -> bool,
    {
        let mut out = Edges(HashMap::new());
        for a in nodes.clone() {
            out.0.entry(a).or_default();
            for b in nodes.clone() {
                if has_edge(a, b) {
                    out.add_edge(a, b)
                }
            }
        }
        out
    }

    // topological sort using Kahn's algorithm
    // either returns a sorted list of indices into nodes
    // or returns an INCOMING adjancency list of remaining edges
    fn topological_sort(&self) -> Result<Vec<Node>, Edges> {
        let mut incoming = Edges(HashMap::new());
        for (v, ws) in &self.0 {
            for w in ws {
                incoming.add_edge(*w, *v);
            }
        }

        let mut sorted = vec![];
        let mut no_incoming: Vec<usize> = self.0.keys().copied().collect();
        no_incoming.retain(|i| !incoming.0.contains_key(i));

        while let Some(v) = no_incoming.pop() {
            sorted.push(v);
            for w in self.children(v) {
                incoming.children_mut(*w).retain(|i| *i != v);
                if incoming.children(*w).is_empty() {
                    no_incoming.push(*w)
                }
            }
        }

        if sorted.len() == self.0.len() {
            Ok(sorted)
        } else {
            Err(incoming)
        }
    }
}

// holds all the information given to different passes
struct Context<'a> {
    id: usize,
    edges: Edges,
    locs: HashMap<Node, Location<&'a str>>,
    chars: HashMap<Node, char>,
    strings: HashMap<Node, String>,
    ops: HashMap<Node, Op>,
    operators: Operators<'a>,
    labels: HashMap<String, i64>,
    macros: Vec<(String, Vec<Node>)>,
}

impl Debug for Context<'_> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        fn print_tree(context: &Context, f: &mut Formatter, node: Node, depth: usize) -> FmtResult {
            write!(f, "{} {}", "----".repeat(depth), node)?;
            if let Some(Location { row, col, src }) = context.locs.get(&node) {
                write!(f, "@{}:{}:{}", src, row, col)?;
            }
            write!(f, ":")?;
            if let Some(c) = context.chars.get(&node) {
                write!(f, " {}", c)?;
            }
            if let Some(s) = context.strings.get(&node) {
                write!(f, " {}", s)?;
            }
            if let Some(o) = context.ops.get(&node) {
                write!(f, " {:?}", o)?;
            }
            writeln!(f)?;
            for child in context.edges.children(node) {
                print_tree(context, f, *child, depth + 1)?;
            }
            Ok(())
        }
        print_tree(self, f, 0, 0)
    }
}

impl Context<'_> {
    fn id(&mut self) -> usize {
        self.id += 1;
        self.id - 1
    }

    fn replace_children_postorder<F>(&mut self, node: Node, f: &mut F)
    where
        F: FnMut(&mut Context, Node) -> Option<Vec<Node>>,
    {
        let mut i = 0;
        while i < self.edges.children(node).len() {
            let child = self.edges.children(node)[i];
            self.replace_children_postorder(child, f);
            if let Some(nodes) = f(self, child) {
                let l = nodes.len();
                self.edges.children_mut(node).splice(i..=i, nodes);
                i += l;
            } else {
                i += 1;
            }
        }
    }
}

//                   func     left   right  unroll
type Operator<'a> = (&'a str, usize, usize, bool);
//                                                        right associativity
type Operators<'a> = Vec<(HashMap<&'a str, Operator<'a>>, bool)>;

#[derive(Clone, Debug)]
struct Location<Src: ToString> {
    row: usize,
    col: usize,
    src: Src,
}

impl<Src: ToString> Location<Src> {
    fn cloned(&self) -> Location<String> {
        Location {
            row: self.row,
            col: self.col,
            src: self.src.to_string(),
        }
    }
}

#[derive(Clone)]
struct E(Error, Option<Location<String>>);

use Error::*;
#[derive(Clone)]
enum Error {
    NoFilePassed,
    CouldNotRead(String),

    ExtraCloseBracket(String),
    MissingCloseBracket(String),
    MissingOperatorArgs(String),

    MacroDependencyCycle { names: Vec<String>, incoming: Edges },

    ExpectedInt,
    UnknownOpCode(String),
    MissingOpCode,
    UnknownLabel(String),
    RedeclaredLabel(String),

    ExpectedEmptyData(String),
}

impl Debug for E {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match &self.0 {
            Error::NoFilePassed => write!(f, "pass a .iz file as a command line argument"),
            Error::CouldNotRead(file) => write!(f, "could not read from file {}", file),

            Error::ExtraCloseBracket(bracket) => write!(f, "extra {}", bracket),
            Error::MissingCloseBracket(bracket) => write!(f, "missing {}", bracket),
            Error::MissingOperatorArgs(op) => write!(f, "missing args for {}", op),

            Error::MacroDependencyCycle { names, incoming } => {
                writeln!(f, "macro dependency cycle detected")?;
                for (w, vs) in &incoming.0 {
                    for v in vs {
                        writeln!(f, "{} -> {}", names[*v], names[*w])?
                    }
                }
                Ok(())
            }

            Error::ExpectedInt => write!(f, "expected int"),
            Error::UnknownOpCode(op) => write!(f, "unknown op code {}", op),
            Error::MissingOpCode => write!(f, "expected string or int for op, found neither"),
            Error::UnknownLabel(label) => write!(f, "unknown label {}", label),
            Error::RedeclaredLabel(label) => write!(f, "label {} is declared twice", label),

            Error::ExpectedEmptyData(data) => {
                write!(
                    f,
                    "[debug] expected context.{} to be empty but it wasn't",
                    data
                )
            }
        }?;
        if let Some(Location { row, col, src }) = &self.1 {
            write!(f, " at {}:{}:{}", src, row, col)?;
        }
        Ok(())
    }
}

fn main() -> Result<(), E> {
    let args: Vec<String> = args().collect();
    let file = args.get(1).ok_or(E(NoFilePassed, None))?;
    let text = read_to_string(file).map_err(|_| E(CouldNotRead(file.clone()), None))?;

    let mut context = Context {
        id: 1, // 0 is the root
        locs: HashMap::new(),
        edges: Edges(HashMap::new()),
        chars: HashMap::new(),
        strings: HashMap::new(),
        ops: HashMap::new(),
        operators: vec![
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
        ],
        labels: HashMap::new(),
        macros: vec![],
    };

    let mut loc = Location {
        src: file.as_str(),
        row: 1,
        col: 1,
    };
    for c in text.chars() {
        let node = context.id();
        context.edges.add_edge(0, node);
        context.chars.insert(node, c);
        context.locs.insert(node, loc.clone());
        if c == '\n' {
            loc.row += 1;
            loc.col = 1;
        } else {
            loc.col += 1;
        }
    }

    let passes = [
        // tokenize
        remove_comments,
        group_tokens,
        remove_whitespace,
        // parse
        group_brackets,
        group_operators,
        unroll_operators,
        unroll_brackets,
        // transform
        collect_macros,
        sort_macros,
        substitute_macros,
        strings_to_ops,
        // backend
        collect_labels,
        interpret,
    ];
    for pass in passes {
        pass(&mut context)?
    }

    Ok(())
}

fn remove_comments(context: &mut Context) -> Result<(), E> {
    let mut i = 0;
    let children = context.edges.children_mut(0);
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
    Ok(())
}

fn group_tokens(context: &mut Context) -> Result<(), E> {
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

    if !context.strings.is_empty() {
        return Err(E(ExpectedEmptyData("strings".to_owned()), None));
    }
    let mut i = 0;
    let children = context.edges.children_mut(0);
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
    if !context.chars.is_empty() {
        return Err(E(ExpectedEmptyData("chars".to_owned()), None));
    }
    Ok(())
}

fn remove_whitespace(context: &mut Context) -> Result<(), E> {
    let mut i = 0;
    let children = context.edges.children_mut(0);
    while i < children.len() {
        let s = context.strings.get(&children[i]).unwrap();
        if s.chars().next().unwrap().is_whitespace() {
            children.remove(i);
        } else {
            i += 1;
        }
    }
    Ok(())
}

fn group_brackets(context: &mut Context) -> Result<(), E> {
    fn group_brackets(
        context: &mut Context,
        i: &mut usize,
        target: Option<(&str, Location<&str>)>,
    ) -> Result<(), E> {
        while *i < context.edges.children(0).len() {
            let child = context.edges.children(0)[*i];
            *i += 1;
            let s = context.strings.get(&child).unwrap().clone();
            let mut handle_open_bracket = |close| -> Result<(), E> {
                let start = *i;
                group_brackets(context, i, Some((close, context.locs[&child].clone())))?;
                let mut cs: Vec<Node> = context.edges.children_mut(0).drain(start..*i).collect();
                cs.pop(); // remove the closing bracket
                *context.edges.children_mut(child) = cs;
                *i = start;
                Ok(())
            };
            match s.as_str() {
                "(" => handle_open_bracket(")"),
                "{" => handle_open_bracket("}"),
                "[" => handle_open_bracket("]"),
                ")" | "}" | "]" => match target {
                    Some((t, _)) if s == t => return Ok(()),
                    _ => Err(E(ExtraCloseBracket(s), Some(context.locs[&child].cloned()))),
                },
                _ => Ok(()),
            }?
        }
        if let Some((s, l)) = target {
            Err(E(MissingCloseBracket(s.to_owned()), Some(l.cloned())))
        } else {
            Ok(())
        }
    }
    group_brackets(context, &mut 0, None)
}

fn group_operators(context: &mut Context) -> Result<(), E> {
    fn group_operators(context: &mut Context, node: Node) -> Result<(), E> {
        #[allow(clippy::unnecessary_to_owned)] // github.com/rust-lang/rust-clippy/issues/8148
        for child in context.edges.children(node).to_owned() {
            group_operators(context, child)?
        }

        for (ops, right_assoc) in &context.operators {
            let mut i = if *right_assoc {
                context.edges.children(node).len().wrapping_sub(1)
            } else {
                0
            };
            while let Some(child) = context.edges.children(node).get(i).copied() {
                let s = context.strings.get(&child).unwrap().clone();
                if let Some((_func, left, right, _unroll)) = ops.get(&*s) {
                    if i < *left || i + right >= context.edges.children(node).len() {
                        return Err(E(
                            MissingOperatorArgs(s),
                            Some(context.locs[&child].cloned()),
                        ));
                    }
                    let mut cs: Vec<Node> = context
                        .edges
                        .children_mut(node)
                        .drain(i + 1..=i + right)
                        .collect();
                    cs.extend(context.edges.children_mut(node).drain(i - left..i));
                    i -= left;
                    *context.edges.children_mut(child) = cs;
                }
                i = if *right_assoc {
                    i.wrapping_sub(1)
                } else {
                    i + 1
                }
            }
        }
        Ok(())
    }
    group_operators(context, 0)
}

fn unroll_operators(context: &mut Context) -> Result<(), E> {
    context.replace_children_postorder(0, &mut |context, node| {
        let s = context.strings.get(&node).unwrap();
        if let Some((func, _left, _right, unroll)) =
            context.operators.iter().find_map(|(ops, _)| ops.get(&**s))
        {
            context.strings.insert(node, (*func).to_owned());
            if *unroll {
                let mut cs: Vec<Node> = context.edges.children_mut(node).drain(..).collect();
                cs.push(node);
                Some(cs)
            } else {
                None
            }
        } else {
            None
        }
    });
    Ok(())
}

fn unroll_brackets(context: &mut Context) -> Result<(), E> {
    context.replace_children_postorder(0, &mut |context, node| {
        if context.strings.get(&node) == Some(&"(".to_owned()) {
            Some(context.edges.children_mut(node).drain(..).collect())
        } else {
            None
        }
    });

    Ok(())
}

fn collect_macros(context: &mut Context) -> Result<(), E> {
    context.replace_children_postorder(0, &mut |context, node| {
        if context.strings.get(&node) == Some(&"macro".to_owned()) {
            let key = context
                .strings
                .remove(&context.edges.children(node)[0])
                .unwrap();
            let value = context.edges.children(node)[1..].to_vec();
            context.macros.push((key, value));
            Some(vec![])
        } else {
            None
        }
    });
    Ok(())
}

// sort macros by dependency so that all nestings get expanded
fn sort_macros(context: &mut Context) -> Result<(), E> {
    let indices = Edges::from_fn(0..context.macros.len(), |i, j| -> bool {
        fn depends(context: &Context, nodes: &[Node], key: &String) -> bool {
            for node in nodes {
                if depends(context, context.edges.children(*node), key)
                    || context.strings.get(node) == Some(key)
                {
                    return true;
                }
            }
            false
        }
        depends(context, &context.macros[i].1, &context.macros[j].0)
    })
    .topological_sort()
    .map_err(|incoming| MacroDependencyCycle {
        names: context
            .macros
            .iter()
            .map(|(name, _)| name.clone())
            .collect(),
        incoming,
    })
    .map_err(|e| E(e, None))?;
    let mut sorted = vec![];
    for i in indices {
        sorted.push(context.macros[i].clone());
    }
    context.macros = sorted;
    Ok(())
}

fn substitute_macros(context: &mut Context) -> Result<(), E> {
    #[allow(clippy::unnecessary_to_owned)] // github.com/rust-lang/rust-clippy/issues/8148
    for (key, value) in context.macros.to_owned() {
        context.replace_children_postorder(0, &mut |context, node| {
            if context.strings.get(&node) == Some(&key) {
                Some(value.clone())
            } else {
                None
            }
        });
    }

    Ok(())
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

fn strings_to_ops(context: &mut Context) -> Result<(), E> {
    for node in context.edges.children(0) {
        let s = context
            .strings
            .get(node)
            .ok_or(E(MissingOpCode, Some(context.locs[node].cloned())))?;
        context.ops.insert(
            *node,
            match s.as_str() {
                "~" => {
                    let child = context.edges.children(*node)[0];
                    Op::Move(
                        context
                            .strings
                            .get(&child)
                            .and_then(|s| s.parse::<i64>().ok())
                            .ok_or(E(ExpectedInt, Some(context.locs[&child].cloned())))?,
                    )
                }
                "$" => {
                    let child = context.edges.children(*node)[0];
                    Op::Copy(
                        context
                            .strings
                            .get(&child)
                            .and_then(|s| s.parse::<i64>().ok())
                            .ok_or(E(ExpectedInt, Some(context.locs[&child].cloned())))?,
                    )
                }
                "add" => Op::Add,
                "neg" => Op::Neg,
                "ltz" => Op::Ltz,
                "?" => Op::Jumpz(
                    context
                        .strings
                        .get(&context.edges.children(*node)[0])
                        .unwrap()
                        .clone(),
                ),
                ":" => Op::Label(
                    context
                        .strings
                        .get(&context.edges.children(*node)[0])
                        .unwrap()
                        .clone(),
                ),
                _ => match s.parse::<i64>() {
                    Ok(i) => Op::Push(i),
                    Err(_) => {
                        return Err(E(
                            UnknownOpCode(s.clone()),
                            Some(context.locs[node].cloned()),
                        ))
                    }
                },
            },
        );
    }
    Ok(())
}

fn collect_labels(context: &mut Context) -> Result<(), E> {
    for (pc, node) in context.edges.children(0).iter().enumerate() {
        let op = context.ops.get(node).unwrap();
        if let Op::Label(label) = op {
            let old = context.labels.insert(label.clone(), pc as i64);
            if old.is_some() {
                return Err(E(RedeclaredLabel(label.clone()), None));
            }
        }
    }
    Ok(())
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

fn interpret(context: &mut Context) -> Result<(), E> {
    let mut pc = 0;
    let mut data = Memory(vec![]);
    let mut sp = -1;

    while (pc as usize) < context.edges.children(0).len() {
        let op = context
            .ops
            .get(&context.edges.children(0)[pc as usize])
            .unwrap();

        print!("{:?}\t", op);
        match op {
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
            Op::Jumpz(label) if !context.labels.contains_key(label) => {
                return Err(E(UnknownLabel(label.clone()), None))
            }
            Op::Jumpz(label) => {
                if data[sp] == 0 {
                    pc = context.labels[label];
                }
                sp -= 1;
            }
            Op::Label(_) => {}
        }

        match sp {
            -1 => println!(),
            sp => println!("{:?}", &data.0[0..=sp as usize]),
        }

        pc += 1;
    }

    Ok(())
}
