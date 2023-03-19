extern crate std;

use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::ops::{Deref, Index, IndexMut};
use std::rc::Rc;

// override for panic macro, comment this out if you need a stack trace of the compiler
// macro_rules! panic {
//     () => {{ std::process::exit(-1); }};
//     ($fmt:literal) => {{ std::eprintln!($fmt); std::process::exit(-1); }};
//     ($fmt:literal, $($arg:tt)*) => {{ std::eprintln!($fmt, $($arg)*); std::process::exit(-1); }};
// }

// these can't go inside Token for a few reasons:
// - Token should be as small as possible
// - Token needs a reference to the beginning of the file for location
// - Token needs a reference to the file name for location as well
#[derive(Hash, PartialEq, Eq)]
struct Source {
    name: String,
    text: String,
}

// Rc is used instead of a reference because Any can only be 'static
#[derive(Clone, Hash, PartialEq, Eq)]
struct Token {
    source: Rc<Source>,
    lo: usize,
    hi: usize,
}

impl Deref for Token {
    type Target = str;
    fn deref(&self) -> &str {
        &self.source.text[self.lo..self.hi]
    }
}

impl Display for Token {
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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Id(usize);
const GLOBAL: Id = Id(usize::MAX);

#[derive(Clone)]
struct Tree {
    id: Id,
    children: Vec<Tree>,
}

// this is separate from Context because we don't want these methods to borrow Context.tree
struct Data(HashMap<(TypeId, Id), Box<dyn Any>>);
// when using these methods, use a type alias if it contains a custom type
// otherwise, use a wrapper struct to avoid future TypeId conflicts
impl Data {
    fn insert<T: Any>(&mut self, id: Id, value: T) -> Option<Box<dyn Any>> {
        self.0.insert((TypeId::of::<T>(), id), Box::new(value))
    }

    fn get<T: Any>(&self, id: Id) -> Option<&T> {
        self.0.get(&(TypeId::of::<T>(), id))?.downcast_ref::<T>()
    }

    fn get_mut<T: Any>(&mut self, id: Id) -> Option<&mut T> {
        self.0
            .get_mut(&(TypeId::of::<T>(), id))?
            .downcast_mut::<T>()
    }
}

struct Context {
    next: Id,
    tree: Tree,
    data: Data,
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let file = args.get(1);
    let file = file.unwrap_or_else(|| panic!("expected command line argument"));
    let text = std::fs::read_to_string(file).unwrap_or_else(|_| panic!("could not read {}", file));
    let name = file.clone();
    let source = Rc::new(Source { name, text });

    let mut c = Context {
        next: Id(1),
        tree: Tree {
            id: Id(0),
            children: Vec::new(),
        },
        data: Data(HashMap::new()),
    };

    let is = source.text.char_indices().map(|(i, _)| i);
    let js = source.text.char_indices().map(|(j, _)| j);
    for (i, j) in is.zip(js.skip(1).chain([source.text.len()])) {
        let id = c.next;
        c.next.0 += 1;
        let token = Token {
            source: source.clone(),
            lo: i,
            hi: j,
        };
        c.data.insert(id, token);
        let children = Vec::new();
        c.tree.children.push(Tree { id, children });
    }

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
        // analysis
        gather_assignments,
        substitute_assignments_bad,
        // flatten
        unroll_operators,
        unroll_brackets,
        // transformation
        generate_ops,
        // backend
        interpret,
    ];
    for pass in passes {
        pass(&mut c)
    }
}

fn remove_comments(c: &mut Context) {
    let mut i = 0;
    while i < c.tree.children.len() {
        if c.data.get::<Token>(c.tree.children[i].id).unwrap().deref() == "#" {
            let mut j = i;
            while j < c.tree.children.len()
                && c.data.get::<Token>(c.tree.children[j].id).unwrap().deref() != "\n"
            {
                j += 1;
            }
            c.tree.children.drain(i..=j);
        } else {
            i += 1;
        }
    }
}

fn group_tokens(c: &mut Context) {
    let is_bracket = |s: &str| matches!(s, "(" | ")" | "{" | "}" | "[" | "]");
    let is_ident_char = |c: char| matches!(c, '-' | '_' | 'a'..='z' | 'A'..='Z' | '0'..='9');
    let token_type = |s: &str| match s {
        _ if s.chars().all(is_ident_char) => 0,
        _ if s.chars().all(char::is_whitespace) => 1,
        _ if is_bracket(s) => 2,
        _ => 3,
    };

    let mut i = 1;
    while i < c.tree.children.len() {
        let curr = c.data.get::<Token>(c.tree.children[i].id).unwrap();
        let prev = c.data.get::<Token>(c.tree.children[i - 1].id).unwrap();
        if !is_bracket(curr)
            && token_type(curr) == token_type(prev)
            && curr.source == prev.source
            && prev.hi == curr.lo
        {
            c.data
                .get_mut::<Token>(c.tree.children[i - 1].id)
                .unwrap()
                .hi = curr.hi;
            c.tree.children.remove(i);
        } else {
            i += 1;
        }
    }
}

fn remove_whitespace(c: &mut Context) {
    c.tree.children.retain(|child| {
        !c.data
            .get::<Token>(child.id)
            .unwrap()
            .chars()
            .all(char::is_whitespace)
    });
}

fn parse_int_literals(c: &mut Context) {
    for child in &c.tree.children {
        if let Ok(int) = c.data.get::<Token>(child.id).unwrap().parse::<i64>() {
            c.data.insert::<i64>(child.id, int);
        }
    }
}

fn group_brackets(c: &mut Context) {
    let match_opener = |token: &str| match token {
        "(" => ")",
        "{" => "}",
        "[" => "]",
        s => panic!("{} is not a bracket", s),
    };

    let mut openers = Vec::new();
    let mut i = 0;
    while i < c.tree.children.len() {
        let token = c.data.get::<Token>(c.tree.children[i].id).unwrap();
        match token.deref() {
            "(" | "{" | "[" => openers.push(i),
            ")" | "}" | "]" => match openers.pop() {
                None => panic!("extra {}", token),
                Some(l) => {
                    let opener = c.data.get::<Token>(c.tree.children[l].id).unwrap();
                    if match_opener(opener) == token.deref() {
                        let mut cs: Vec<Tree> = c.tree.children.drain(l + 1..=i).collect();
                        cs.pop(); // remove closing bracket
                        c.tree.children[l].children = cs;
                        i = l;
                    } else {
                        panic!("{} matched with {}", opener, token)
                    }
                }
            },
            _ => {}
        }
        i += 1;
    }
    if let Some(l) = openers.pop() {
        let opener = c.data.get::<Token>(c.tree.children[l].id).unwrap();
        panic!("no {} for the {}", match_opener(opener), opener)
    }
}

fn unroll_brackets(c: &mut Context) {
    unroll_brackets(&mut c.tree, &mut c.data);
    fn unroll_brackets(tree: &mut Tree, data: &mut Data) {
        for child in &mut tree.children {
            unroll_brackets(child, data)
        }

        let mut i = 0;
        while i < tree.children.len() {
            if data.get::<Token>(tree.children[i].id).unwrap().deref() == "(" {
                let cs: Vec<Tree> = tree.children[i].children.drain(..).collect();
                let len = cs.len();
                tree.children.splice(i..=i, cs);
                i += len;
            } else {
                i += 1;
            }
        }
    }
}

struct Operator {
    func: String,
    left: usize,
    right: usize,
    unroll: bool,
}

enum Associativity {
    Left,
    Right,
}

struct Operators {
    ops: HashMap<String, Operator>,
    associativity: Associativity,
}

type Precendences = Vec<Operators>;

fn insert_default_operators(c: &mut Context) {
    fn ops(ops: &[(&str, &str, usize, usize, bool)], associativity: Associativity) -> Operators {
        let ops = ops
            .iter()
            .map(|&(name, func, left, right, unroll)| {
                (
                    name.to_owned(),
                    Operator {
                        func: func.to_owned(),
                        left,
                        right,
                        unroll,
                    },
                )
            })
            .collect();
        Operators { ops, associativity }
    }

    use Associativity::{Left, Right};
    let precedences = [
        ops(&[(":", "label", 1, 0, false)], Left),
        ops(&[("?", "jumpz", 1, 0, false)], Left),
        ops(&[("~", "move", 0, 1, false)], Right),
        ops(&[("$", "copy", 0, 1, false)], Right),
        ops(&[("-", "neg", 0, 1, true), ("!", "not", 0, 1, true)], Right),
        ops(&[("+", "add", 1, 1, true)], Left),
        ops(&[("=", "assign", 1, 1, false)], Right),
    ];
    c.data
        .insert::<Precendences>(GLOBAL, precedences.into_iter().collect());
}

fn group_operators(c: &mut Context) {
    group_operators(&mut c.tree, &mut c.data);
    fn group_operators(tree: &mut Tree, data: &mut Data) {
        for child in &mut tree.children {
            group_operators(child, data)
        }

        let precedences = data.get::<Precendences>(GLOBAL).unwrap();
        for Operators { ops, associativity } in precedences {
            let mut i = match associativity {
                Associativity::Left => 0,
                Associativity::Right => tree.children.len().wrapping_sub(1),
            };
            while i < tree.children.len() {
                let token = data.get::<Token>(tree.children[i].id).unwrap();
                if let Some(op) = ops.get(token.deref()) {
                    if i < op.left || i + op.right >= tree.children.len() {
                        panic!("missing arguments for {}", token)
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
}

fn unroll_operators(c: &mut Context) {
    unroll_operators(&mut c.tree, &mut c.data);
    fn unroll_operators(tree: &mut Tree, data: &mut Data) {
        for child in &mut tree.children {
            unroll_operators(child, data)
        }

        let precedences = data.get::<Precendences>(GLOBAL).unwrap();
        let mut i = 0;
        while i < tree.children.len() {
            let s = data.get::<Token>(tree.children[i].id).unwrap().deref();
            if let Some(op) = precedences.iter().find_map(|ops| ops.ops.get(s)) {
                if op.unroll {
                    let cs: Vec<Tree> = tree.children[i].children.drain(..).collect();
                    let len = cs.len();
                    tree.children.splice(i..i, cs);
                    i += len;
                }
            }
            i += 1;
        }
    }
}

struct Namespace {
    parent: Id,
    trees: HashMap<String, Tree>,
}

impl Namespace {
    fn get<'a>(&'a self, data: &'a Data, key: &str) -> Option<&Tree> {
        self.trees.get(key)?;
        data.get::<Namespace>(self.parent)
            .and_then(|namespace| namespace.get(data, key))
    }
}

fn gather_assignments(c: &mut Context) {
    gather_assignments(&mut c.tree, &mut c.data, GLOBAL);
    fn gather_assignments(tree: &mut Tree, data: &mut Data, parent: Id) {
        let mut trees = HashMap::new();
        let mut i = 0;
        while i < tree.children.len() {
            gather_assignments(&mut tree.children[i], data, tree.id);

            let s = data.get::<Token>(tree.children[i].id).unwrap().deref();
            let s = data
                .get::<Precendences>(GLOBAL)
                .unwrap()
                .iter()
                .find_map(|ops| ops.ops.get(s))
                .map_or(s, |op| op.func.as_str());

            if s == "assign" {
                let mut assignment = tree.children.remove(i);
                let key = assignment.children.remove(0);
                let key = data.get::<Token>(key.id).unwrap().deref().to_owned();
                let value = assignment.children.pop().unwrap();
                trees.insert(key, value);
            } else {
                i += 1;
            }
        }
        data.insert(tree.id, Namespace { parent, trees });
    }
}

// this is bad because it only works for the simplest possible macros
// in all other cases (stack vars, function calls, macros with arguments) this won't work
fn substitute_assignments_bad(c: &mut Context) {
    substitute_assignments_bad(&mut c.tree, &mut c.data);
    fn substitute_assignments_bad(tree: &mut Tree, data: &mut Data) {
        let mut i = 0;
        while i < tree.children.len() {
            substitute_assignments_bad(&mut tree.children[i], data);

            let s = data.get::<Token>(tree.children[i].id).unwrap().deref();
            if let Some(replacement) = data.get::<Namespace>(tree.id).unwrap().get(data, s) {
                tree.children[i] = replacement.clone();
            }
            i += 1;
        }
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

struct Memory<'a> {
    pc: i64,
    sp: i64,
    stack: Stack,
    data: &'a Data,
}

trait Operation: Debug {
    fn run(&self, memory: &mut Memory);
}

type Op = Box<dyn Operation>;
struct Labels(HashMap<String, i64>);

#[derive(Debug)]
pub struct Push(pub i64);
impl Operation for Push {
    fn run(&self, memory: &mut Memory) {
        memory.stack[memory.sp + 1] = self.0;
        memory.sp += 1;
    }
}
#[derive(Debug)]
pub struct Move(pub i64);
impl Operation for Move {
    fn run(&self, memory: &mut Memory) {
        memory.sp -= self.0;
    }
}
#[derive(Debug)]
pub struct Copy(pub i64);
impl Operation for Copy {
    fn run(&self, memory: &mut Memory) {
        memory.stack[memory.sp + 1] = memory.stack[memory.sp - self.0];
        memory.sp += 1;
    }
}
#[derive(Debug)]
pub struct Add;
impl Operation for Add {
    fn run(&self, memory: &mut Memory) {
        memory.stack[memory.sp - 1] += memory.stack[memory.sp];
        memory.sp -= 1;
    }
}
#[derive(Debug)]
pub struct Neg;
impl Operation for Neg {
    fn run(&self, memory: &mut Memory) {
        memory.stack[memory.sp] = -memory.stack[memory.sp];
    }
}
#[derive(Debug)]
pub struct Ltz;
impl Operation for Ltz {
    fn run(&self, memory: &mut Memory) {
        memory.stack[memory.sp] = (memory.stack[memory.sp] < 0) as i64;
    }
}
#[derive(Debug)]
pub struct Jumpz(pub String);
impl Operation for Jumpz {
    fn run(&self, memory: &mut Memory) {
        let labels = memory.data.get::<Labels>(GLOBAL).unwrap();
        match labels.0.get(&self.0) {
            Some(i) => {
                if memory.stack[memory.sp] == 0 {
                    memory.pc = *i;
                }
                memory.sp -= 1;
            }
            None => panic!("could not find label {}", self.0),
        }
    }
}
#[derive(Debug)]
pub struct Label(pub String);
impl Operation for Label {
    fn run(&self, _memory: &mut Memory) {}
}

fn generate_ops(c: &mut Context) {
    let mut labels: HashMap<String, i64> = HashMap::new();
    for (i, child) in c.tree.children.iter_mut().enumerate() {
        if let Some(int) = c.data.get::<i64>(child.id) {
            c.data.insert::<Op>(child.id, Box::new(Push(*int)));
        } else {
            let s = c.data.get::<Token>(child.id).unwrap().deref();
            let s = c
                .data
                .get::<Precendences>(GLOBAL)
                .unwrap()
                .iter()
                .find_map(|ops| ops.ops.get(s))
                .map_or(s, |op| op.func.as_str());
            match s {
                "move" => {
                    let arg = c.data.get::<i64>(child.children.pop().unwrap().id).unwrap();
                    c.data.insert::<Op>(child.id, Box::new(Move(*arg)));
                }
                "copy" => {
                    let arg = c.data.get::<i64>(child.children.pop().unwrap().id).unwrap();
                    c.data.insert::<Op>(child.id, Box::new(Copy(*arg)));
                }
                "add" => {
                    c.data.insert::<Op>(child.id, Box::new(Add));
                }
                "neg" => {
                    c.data.insert::<Op>(child.id, Box::new(Neg));
                }
                "ltz" => {
                    c.data.insert::<Op>(child.id, Box::new(Ltz));
                }
                "jumpz" => {
                    let arg = child.children.pop().unwrap().id;
                    let label = c.data.get::<Token>(arg).unwrap().deref().to_owned();
                    c.data.insert::<Op>(child.id, Box::new(Jumpz(label)));
                }
                "label" => {
                    let arg = child.children.pop().unwrap().id;
                    let label = c.data.get::<Token>(arg).unwrap().deref().to_owned();
                    labels.insert(label.clone(), i as i64);
                    c.data.insert::<Op>(child.id, Box::new(Label(label)));
                }
                _ => {}
            }
        }
    }
    c.data.insert::<Labels>(GLOBAL, Labels(labels));
}

fn interpret(c: &mut Context) {
    for child in &c.tree.children {
        let token = c.data.get::<Token>(child.id).unwrap();
        if c.data.get::<Op>(child.id).is_none() {
            panic!("no operation for {}", token)
        }
        if !child.children.is_empty() {
            panic!("found child of {}", token)
        }
    }

    let mut memory = Memory {
        pc: 0,
        sp: -1,
        stack: Stack(Vec::new()),
        data: &c.data,
    };
    while let Some(child) = c.tree.children.get(memory.pc as usize) {
        let op = c.data.get::<Op>(child.id).unwrap();
        op.run(&mut memory);
        memory.pc += 1;
        match memory.sp {
            -1 => println!("{:<32}\t", format!("{:?}", op)),
            sp => println!(
                "{:<32}\t{:?}",
                format!("{:?}", op),
                &memory.stack.0[0..=sp as usize]
            ),
        }
    }
}
