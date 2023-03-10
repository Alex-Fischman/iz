#![no_implicit_prelude]
extern crate std;

use std::{
    borrow::ToOwned,
    clone::Clone,
    collections::HashMap,
    convert::From,
    env::args,
    fmt::{Debug, Display, Formatter, Result as FmtResult},
    fs::read_to_string,
    iter::{Extend, Iterator},
    ops::{FnMut, Index, IndexMut},
    option::{Option, Option::None, Option::Some},
    result::{Result, Result::Err, Result::Ok},
    string::{String, ToString},
    vec::Vec,
    {matches, print, println, vec, write, writeln},
};

type Node = usize;
type Edges = HashMap<Node, Vec<Node>>; // outgoing adjacency list

// holds all the information given to different passes
struct Context<'a> {
    id: usize,
    edges: Edges,
    locs: HashMap<Node, Location<&'a str>>,
    chars: HashMap<Node, char>,
    strings: HashMap<Node, String>,
    ints: HashMap<Node, i64>,
}

impl Debug for Context<'_> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        fn print_tree(context: &Context, f: &mut Formatter, node: Node, depth: usize) -> FmtResult {
            write!(f, "{} {}", "----".repeat(depth), node)?;
            if let Some(l) = context.locs.get(&node) {
                write!(f, "@{}", l)?;
            }
            write!(f, ":")?;
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
        while i < self.edges[&node].len() {
            let child = self.edges[&node][i];
            self.replace_children_postorder(child, f);
            if let Some(nodes) = f(self, child) {
                let l = nodes.len();
                self.edges.get_mut(&node).unwrap().splice(i..=i, nodes);
                i += l;
            } else {
                i += 1;
            }
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

impl<Src: ToString> Display for Location<Src> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}:{}:{}", self.src.to_string(), self.row, self.col)
    }
}

#[derive(Clone)]
struct E(Error, Option<Location<String>>);

impl Debug for E {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match &self.1 {
            None => write!(f, "{}", self.0),
            Some(loc) => write!(f, "{} at {}", self.0, loc),
        }
    }
}

use Error::*;
#[derive(Clone, Debug)]
enum Error {
    NoFilePassed,
    CouldNotRead(String),

    ExtraCloseBracket(String),
    MissingCloseBracket(String),
    MissingOperatorArgs(String),

    MacroDependencyCycle { names: Vec<String>, edges: Edges },

    UnknownOpCode(String),
    MissingOpCode,
    UnknownLabel(String),
    RedeclaredLabel(String),

    ExpectedEmptyData(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Error::NoFilePassed => write!(f, "pass a .iz file as a command line argument"),
            Error::CouldNotRead(file) => write!(f, "could not read from file {}", file),

            Error::ExtraCloseBracket(bracket) => write!(f, "extra {}", bracket),
            Error::MissingCloseBracket(bracket) => write!(f, "missing {}", bracket),
            Error::MissingOperatorArgs(op) => write!(f, "missing args for {}", op),

            Error::MacroDependencyCycle { names, edges } => {
                writeln!(f, "macro dependency cycle detected")?;
                for (v, ws) in edges {
                    for w in ws {
                        writeln!(f, "{} -> {}", names[*v], names[*w])?
                    }
                }
                Ok(())
            }

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
        }
    }
}

fn main() -> Result<(), E> {
    // frontend
    let args: Vec<String> = args().collect();
    let file = args.get(1).ok_or(E(NoFilePassed, None))?;
    let text = read_to_string(file).map_err(|_| E(CouldNotRead(file.clone()), None))?;

    let mut context = Context {
        id: 1, // 0 is the root
        locs: HashMap::new(),
        edges: HashMap::new(),
        chars: HashMap::new(),
        strings: HashMap::new(),
        ints: HashMap::new(),
    };
    context.edges.insert(0, vec![]);

    let mut row = 1;
    let mut col = 1;
    for c in text.chars() {
        let node = context.id();
        context.edges.insert(node, vec![]);
        context.edges.get_mut(&0).unwrap().push(node);
        context.chars.insert(node, c);
        context.locs.insert(
            node,
            Location {
                src: file,
                row,
                col,
            },
        );
        if c == '\n' {
            row += 1;
            col = 1;
        } else {
            col += 1;
        }
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
        pass(&mut context)?
    }

    // backend
    let code = context.edges[&0]
        .iter()
        .cloned()
        .map(|node| {
            if let Some(int) = context.ints.get(&node) {
                Ok(Op::Push(*int))
            } else if let Some(s) = context.strings.get(&node) {
                match s.as_str() {
                    "~" => Ok(Op::Move(
                        *context.ints.get(&context.edges[&node][0]).unwrap(),
                    )),
                    "$" => Ok(Op::Copy(
                        *context.ints.get(&context.edges[&node][0]).unwrap(),
                    )),
                    "add" => Ok(Op::Add),
                    "neg" => Ok(Op::Neg),
                    "ltz" => Ok(Op::Ltz),
                    "?" => Ok(Op::Jumpz(
                        context
                            .strings
                            .get(&context.edges[&node][0])
                            .unwrap()
                            .clone(),
                    )),
                    ":" => Ok(Op::Label(
                        context
                            .strings
                            .get(&context.edges[&node][0])
                            .unwrap()
                            .clone(),
                    )),
                    _ => Err(E(
                        UnknownOpCode(s.clone()),
                        Some(context.locs[&node].cloned()),
                    )),
                }
            } else {
                Err(E(MissingOpCode, Some(context.locs[&node].cloned())))
            }
        })
        .collect::<Result<Vec<Op>, _>>()?;

    let mut labels = HashMap::new();
    for (pc, op) in code.iter().enumerate() {
        if let Op::Label(label) = op {
            let old = labels.insert(label.clone(), pc as i64);
            if old.is_some() {
                return Err(E(RedeclaredLabel(label.clone()), None));
            }
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
            Op::Jumpz(label) if !labels.contains_key(label) => {
                return Err(E(UnknownLabel(label.clone()), None))
            }
            Op::Jumpz(label) => {
                if data[sp] == 0 {
                    pc = labels[label];
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

fn remove_comments(context: &mut Context) -> Result<(), E> {
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
    Ok(())
}

fn chars_to_strings(context: &mut Context) -> Result<(), E> {
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
    if !context.chars.is_empty() {
        return Err(E(ExpectedEmptyData("chars".to_owned()), None));
    }

    Ok(())
}

fn remove_whitespace(context: &mut Context) -> Result<(), E> {
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
    Ok(())
}

fn group_brackets(context: &mut Context) -> Result<(), E> {
    fn bracket_matcher(
        context: &mut Context,
        i: &mut usize,
        target: Option<(&str, Location<&str>)>,
    ) -> Result<(), E> {
        while *i < context.edges[&0].len() {
            let child = context.edges[&0][*i];
            *i += 1;
            let s = context.strings.get(&child).unwrap().clone();
            let mut handle_open_bracket = |close| -> Result<(), E> {
                let start = *i;
                bracket_matcher(context, i, Some((close, context.locs[&child].clone())))?;
                let mut cs: Vec<Node> = context
                    .edges
                    .get_mut(&0)
                    .unwrap()
                    .drain(start..*i)
                    .collect();
                cs.pop(); // remove the closing bracket
                *context.edges.get_mut(&child).unwrap() = cs;
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
    bracket_matcher(context, &mut 0, None)
}

fn unroll_brackets(context: &mut Context) -> Result<(), E> {
    context.replace_children_postorder(0, &mut |context, node| {
        if context.strings.get(&node) == Some(&"(".to_owned()) {
            Some(context.edges.get_mut(&node).unwrap().drain(..).collect())
        } else {
            None
        }
    });
    Ok(())
}

fn group_and_unroll_operators(context: &mut Context) -> Result<(), E> {
    //                   func     left   right  unroll
    type Operator<'a> = (&'a str, usize, usize, bool);
    //                                                        right associativity
    type Operators<'a> = Vec<(HashMap<&'a str, Operator<'a>>, bool)>;
    let operators = vec![
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

    group_operators(context, 0, &operators)?;
    fn group_operators(context: &mut Context, node: Node, operators: &Operators) -> Result<(), E> {
        for child in context.edges[&node].clone() {
            group_operators(context, child, operators)?
        }

        for (ops, right_assoc) in operators {
            let mut i = if *right_assoc {
                context.edges[&node].len().wrapping_sub(1)
            } else {
                0
            };
            while let Some(child) = context.edges[&node].get(i).copied() {
                let s = context.strings.get(&child).unwrap().clone();
                if let Some((_func, left, right, _unroll)) = ops.get(&*s) {
                    if i < *left || i + right >= context.edges[&node].len() {
                        return Err(E(
                            MissingOperatorArgs(s),
                            Some(context.locs[&child].cloned()),
                        ));
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
        Ok(())
    }

    context.replace_children_postorder(0, &mut |context, node| {
        let s = context.strings.get(&node).unwrap();
        if let Some((func, _left, _right, unroll)) =
            operators.iter().find_map(|(ops, _)| ops.get(&**s))
        {
            context.strings.insert(node, (*func).to_owned());
            if *unroll {
                let mut cs: Vec<Node> = context.edges.get_mut(&node).unwrap().drain(..).collect();
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

fn integer_literals(context: &mut Context) -> Result<(), E> {
    integer_literals(context, 0);
    fn integer_literals(context: &mut Context, node: Node) {
        for child in context.edges[&node].clone() {
            integer_literals(context, child)
        }
        if let Some(s) = context.strings.get(&node) {
            if let Ok(int) = s.parse::<i64>() {
                context.strings.remove(&node);
                context.ints.insert(node, int);
            }
        }
    }

    Ok(())
}

fn substitute_macros(context: &mut Context) -> Result<(), E> {
    let mut macros = vec![];
    context.replace_children_postorder(0, &mut |context, node| {
        if context.strings.get(&node) == Some(&"macro".to_owned()) {
            let key = context.strings.remove(&context.edges[&node][0]).unwrap();
            let value = context.edges[&node][1..].to_vec();
            macros.push((key, value));
            Some(vec![])
        } else {
            None
        }
    });

    let sorted = topological_sort(&macros, |(_, nodes), (key, _)| -> bool {
        fn depends(context: &mut Context, nodes: &[Node], key: &String) -> bool {
            for node in nodes {
                if depends(context, &context.edges[node].clone(), key)
                    || context.strings.get(node) == Some(key)
                {
                    return true;
                }
            }
            false
        }
        depends(context, nodes, key)
    });

    let sorted = sorted
        .map_err(|edges| MacroDependencyCycle {
            names: macros.iter().map(|(name, _)| name.clone()).collect(),
            edges,
        })
        .map_err(|e| E(e, None))?;

    for i in sorted {
        let (key, value) = &macros[i];
        context.replace_children_postorder(0, &mut |context, node| {
            if context.strings.get(&node) == Some(key) {
                Some(value.clone())
            } else {
                None
            }
        });
    }

    Ok(())
}

// topological sort using Kahn's algorithm on an adjacency list
// returns either a sorted list of indices into nodes
// or an adjacency list of the cycles if they exist
fn topological_sort<N, F>(nodes: &[N], mut has_edge: F) -> Result<Vec<usize>, Edges>
where
    F: FnMut(&N, &N) -> bool,
{
    let mut edges: Edges = HashMap::new();
    for (i, a) in nodes.iter().enumerate() {
        for (j, b) in nodes.iter().enumerate() {
            if has_edge(a, b) {
                edges.entry(i).or_default().push(j);
            }
        }
    }

    fn has_incoming(edges: &Edges, i: usize) -> bool {
        for ws in edges.values() {
            for w in ws {
                if i == *w {
                    return true;
                }
            }
        }
        false
    }

    let mut sorted = vec![];
    let mut no_incoming: Vec<usize> = (0..nodes.len())
        .filter(|i| !has_incoming(&edges, *i))
        .collect();

    while let Some(v) = no_incoming.pop() {
        sorted.push(v);
        if let Some(ws) = edges.remove(&v) {
            for w in ws {
                if !has_incoming(&edges, w) {
                    no_incoming.push(w)
                }
            }
        }
    }

    if edges.is_empty() {
        Ok(sorted)
    } else {
        Err(edges)
    }
}
