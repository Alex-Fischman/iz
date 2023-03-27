// override for panic macro, comment this out if you need a stack trace of the compiler
// macro_rules! panic {
//     () => {{ std::process::exit(-1); }};
//     ($fmt:literal) => {{ std::eprintln!($fmt); std::process::exit(-1); }};
//     ($fmt:literal, $($arg:tt)*) => {{ std::eprintln!($fmt, $($arg)*); std::process::exit(-1); }};
// }

#[derive(Hash, PartialEq, Eq)]
struct Source {
    name: String,
    text: String,
}

use std::rc::Rc; // only needed to get around Any restrictions, not necessary for bootstrapping
#[derive(Clone, Hash, PartialEq, Eq)]
struct Token {
    source: Rc<Source>,
    lo: usize,
    hi: usize,
}

use std::ops::Deref;
impl Deref for Token {
    type Target = str;
    fn deref(&self) -> &str {
        &self.source.text[self.lo..self.hi]
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.lo == self.hi {
            return Ok(());
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
        write!(f, "{} at {}:{}:{}", self.deref(), self.source.name, row, col)
    }
}

use std::collections::HashMap;
struct Data(HashMap<TypeId, Box<dyn Any>>);
impl Data {
    fn insert<T: Any>(&mut self, x: T) -> Option<T> {
        self.0.insert(TypeId::of::<T>(), Box::new(x)).map(|any| *any.downcast().unwrap())
    }

    fn remove<T: Any>(&mut self) -> Option<T> {
        self.0.remove(&TypeId::of::<T>()).map(|any| *any.downcast().unwrap())
    }

    fn get<T: Any>(&self) -> Option<&T> {
        self.0.get(&TypeId::of::<T>()).map(|any| any.downcast_ref().unwrap())
    }

    fn get_mut<T: Any>(&mut self) -> Option<&mut T> {
        self.0.get_mut(&TypeId::of::<T>()).map(|any| any.downcast_mut().unwrap())
    }
}

use std::any::{Any, TypeId};
struct Tree {
    data: Data,
    children: Vec<Tree>,
}

fn main() {
    // frontend
    let args: Vec<String> = std::env::args().collect();
    let name = args.get(1).unwrap_or_else(|| panic!("missing .iz file")).to_owned();
    let text = std::fs::read_to_string(&name)
        .unwrap_or_else(|_| panic!("could not read {}", name));
    let source = Rc::new(Source { name, text });

    let mut tree = Tree { data: Data(HashMap::new()), children: Vec::new() };
    let is = source.text.char_indices().map(|(i, _)| i);
    let js = source.text.char_indices().map(|(j, _)| j);
    for (i, j) in is.zip(js.skip(1).chain([source.text.len()])) {
        let mut child = Tree { data: Data(HashMap::new()), children: Vec::new() };
        child.data.insert::<Token>(Token { source: source.clone(), lo: i, hi: j });
        tree.children.push(child);
    }

    let mut passes: Vec<Box<dyn Fn(&mut Tree)>> = Vec::new();
    // tokenizing
    passes.push(Box::new(remove_comments));
    passes.push(Box::new(concat_alike_tokens(is_identifier)));
    passes.push(Box::new(concat_alike_tokens(is_operator)));
    passes.push(Box::new(remove_whitespace));
    passes.push(Box::new(integer_literals));
    // parsing
    passes.push(Box::new(match_brackets("(", ")")));
    passes.push(Box::new(match_brackets("{", "}")));
    passes.push(Box::new(parse_operator("?", Operator::Postfix)));
    passes.push(Box::new(parse_operator(":", Operator::Postfix)));
    passes.push(Box::new(parse_operator("~", Operator::Prefix)));
    passes.push(Box::new(parse_operator("$", Operator::Prefix)));
    passes.push(Box::new(parse_operator("-", Operator::Prefix)));
    passes.push(Box::new(parse_operator("+", Operator::InfixLeft)));
    passes.push(Box::new(parse_operator("=", Operator::InfixRight)));
    // flattening
    passes.push(Box::new(unroll_children("(", false)));
    passes.push(Box::new(unroll_children("-", true)));
    passes.push(Box::new(unroll_children("+", true)));
    // compiling
    passes.push(Box::new(compile_push));
    passes.push(Box::new(compile_move));
    passes.push(Box::new(compile_copy));
    passes.push(Box::new(compile_add));
    passes.push(Box::new(compile_neg));
    passes.push(Box::new(compile_jumpz));
    passes.push(Box::new(compile_label));

    for pass in &passes {
        pass(&mut tree)
    }

    // backend
    let code: Vec<_> = tree.children.into_iter().map(|mut child| {
        let token = child.data.get::<Token>().unwrap().clone();
        if !child.children.is_empty() {
            panic!("after compilation should have finished, there was a child of {}", token)
        }
        child.data.remove::<Box<dyn Instruction>>()
            .unwrap_or_else(|| panic!("no instruction for {}", token))
    }).collect();
    let mut i = Interpreter {
        labels: tree.data.remove::<Labels>().unwrap(),
        pc: 0,
        stack: Memory(Vec::new()),
        sp: -1,
    };
    while let Some(instruction) = code.get(i.pc as usize) {
        instruction.interpret(&mut i);
        i.pc += 1;

        print!("{:<32}\t", format!("{:?}", instruction));
        if i.sp > -1 {
            print!("{:?}", &i.stack.0[0..=i.sp as usize])
        }
        println!();
    }
}

fn remove_comments(tree: &mut Tree) {
    let mut in_comment = false;
    tree.children.retain(|child| {
        match child.data.get::<Token>().unwrap().deref() {
            "#" if !in_comment => in_comment = true,
            "\n" if in_comment => in_comment = false,
            _ => {},
        }
        !in_comment
    });
}

fn is_identifier(s: &str) -> bool {
    s.chars().all(|c| c.is_alphanumeric() || c == '-' || c == '_')
}

fn is_whitespace(s: &str) -> bool {
    s.chars().all(char::is_whitespace)
}

fn is_operator(s: &str) -> bool {
    s.chars().all(|c| !(c.is_alphanumeric() || c.is_whitespace() || "(){}[]".contains(c)))
}

fn concat_alike_tokens<F: Fn(&str) -> bool>(alike: F) -> impl Fn(&mut Tree) {
    move |tree: &mut Tree| {
        let mut i = 1;
        while i < tree.children.len() {
            let curr = tree.children[i].data.get::<Token>().unwrap();
            let prev = tree.children[i - 1].data.get::<Token>().unwrap();
            if alike(curr) && alike(prev) && curr.source == prev.source && curr.lo == prev.hi {
                let curr = tree.children.remove(i).data.remove::<Token>().unwrap();
                i -= 1;
                tree.children[i].data.get_mut::<Token>().unwrap().hi = curr.hi;
            }
            i += 1;
        }
    }
}

fn remove_whitespace(tree: &mut Tree) {
    tree.children.retain(|child| !is_whitespace(child.data.get::<Token>().unwrap()));
}

fn integer_literals(tree: &mut Tree) {
    for tree in &mut tree.children {
        if let Some(token) = tree.data.get::<Token>() {
            let mut chars = token.deref().chars().peekable();
            let is_negative = match chars.peek() {
                Some('-') => {
                    chars.next().unwrap();
                    true
                }
                _ => false,
            };
            if matches!(chars.peek(), Some('0'..='9')) {
                let base = match chars.peek() {
                    Some('1'..='9') => 10,
                    Some('0') => {
                        chars.next().unwrap();
                        match chars.next() {
                            None => 10,
                            Some('x') => 16,
                            Some('b') => 2,
                            Some(c) => panic!("unknown base prefix 0{} in {}", c, token),
                        }
                    },
                    Some(_) | None => unreachable!(),
                };
                let mut value = 0;
                for c in chars {
                    let digit = match c {
                        '0'..='9' => c as i64 - '0' as i64,
                        'a'..='f' => c as i64 - 'a' as i64 + 10,
                        'A'..='F' => c as i64 - 'A' as i64 + 10,
                        '_' => continue,
                        c => panic!("unknown digit {} in {}", c, token),
                    };
                    if digit >= base {
                        panic!("digit {} too large for base {} in {}", c, base, token)
                    }
                    value = base * value + digit;
                }
                tree.data.insert::<i64>(if is_negative { -value } else { value });
            }
        }
    }
}

fn match_brackets<'a>(open: &'a str, close: &'a str) -> impl Fn(&mut Tree) + 'a {
    move |tree: &mut Tree| {
        tree.children.iter_mut().for_each(match_brackets(open, close));
        let mut indices = Vec::new(); // stack of open bracket indices
        let mut i = 0;
        while i < tree.children.len() {
            let curr = tree.children[i].data.get::<Token>().unwrap();
            if curr.deref() == open {
                indices.push(i);
            } else if curr.deref() == close {
                let j = indices.pop().unwrap_or_else(|| panic!("extra {}", curr));
                let mut cs: Vec<Tree> = tree.children.drain(j + 1 ..= i).collect();
                cs.pop(); // remove closing bracket
                tree.children[j].children.append(&mut cs);
                i = j;
            }
            i += 1;
        }
        if let Some(j) = indices.pop() {
            panic!("extra {}", tree.children[j].data.get::<Token>().unwrap())
        }
    }
}

#[derive(Clone, Copy)]
enum Operator { Prefix, Postfix, InfixLeft, InfixRight }

fn parse_operator<'a>(name: &'a str, operator: Operator) -> impl Fn(&mut Tree) + 'a {
    use Operator::*;
    move |tree: &mut Tree| {
        tree.children.iter_mut().for_each(parse_operator(name, operator));
        let mut i = match operator {
            Postfix | InfixLeft => 0,
            Prefix | InfixRight => tree.children.len().wrapping_sub(1),
        };
        while i < tree.children.len() {
            let curr = tree.children[i].data.get::<Token>().unwrap().clone();
            if curr.deref() == name {
                let mut args = Vec::new();
                if matches!(operator, Postfix | InfixLeft | InfixRight) {
                    if i == 0 {
                        panic!("no argument for {}", curr)
                    }
                    args.push(tree.children.remove(i - 1));
                    i -= 1;
                }
                if matches!(operator, Prefix | InfixLeft | InfixRight) {
                    if i + 1 == tree.children.len() {
                        panic!("no argument for {}", curr)
                    }
                    args.push(tree.children.remove(i + 1));
                }
                tree.children[i].children.append(&mut args);
            }
            i = match operator {
                Postfix | InfixLeft => i + 1,
                Prefix | InfixRight => i.wrapping_sub(1),
            };
        }
    }
}

fn unroll_children<'a>(name: &'a str, keep_parent: bool) -> impl Fn(&mut Tree) + 'a {
    move |tree: &mut Tree| {
        tree.children.iter_mut().for_each(unroll_children(name, keep_parent));
        let mut i = 0;
        while i < tree.children.len() {
            if tree.children[i].data.get::<Token>().unwrap().deref() == name {
                let cs: Vec<_> = tree.children[i].children.drain(..).collect();
                let l = cs.len();
                tree.children.splice(i..i, cs);
                i += l;
                if keep_parent {
                    i += 1;
                } else {
                    tree.children.remove(i);
                }
            } else {
                i += 1;
            }
        }
    }
}

struct Memory(Vec<i64>);
impl std::ops::Index<i64> for Memory {
    type Output = i64;
    fn index(&self, i: i64) -> &i64 {
        &self.0[i as usize]
    }
}
impl std::ops::IndexMut<i64> for Memory {
    fn index_mut(&mut self, i: i64) -> &mut i64 {
        let i = i as usize;
        if i >= self.0.len() {
            self.0.resize(i + 1, 0)
        }
        &mut self.0[i]
    }
}

struct Labels(HashMap<String, i64>);

struct Interpreter {
    labels: Labels,
    pc: i64,
    stack: Memory,
    sp: i64,
}

trait Instruction: std::fmt::Debug {
    fn interpret(&self, i: &mut Interpreter);
}

#[derive(Debug)]
struct Push(i64);
impl Instruction for Push {
    fn interpret(&self, i: &mut Interpreter) {
        i.sp += 1;
        i.stack[i.sp] = self.0;
    }
}
fn compile_push(tree: &mut Tree) {
    for tree in &mut tree.children {
        if let Some(int) = tree.data.get::<i64>() {
            tree.data.insert::<Box<dyn Instruction>>(Box::new(Push(*int)));
        }
    }
}
#[derive(Debug)]
struct Move(i64);
impl Instruction for Move {
    fn interpret(&self, i: &mut Interpreter) {
        i.sp -= self.0;
    }
}
fn compile_move(tree: &mut Tree) {
    for tree in &mut tree.children {
        if let Some(token) = tree.data.get::<Token>() {
            if token.deref() == "~" {
                let i = tree.children.pop().unwrap().data.remove::<i64>().unwrap();
                tree.data.insert::<Box<dyn Instruction>>(Box::new(Move(i)));
            }
        }
    }
}
#[derive(Debug)]
struct Copy(i64);
impl Instruction for Copy {
    fn interpret(&self, i: &mut Interpreter) {
        i.stack[i.sp + 1] = i.stack[i.sp - self.0];
        i.sp += 1;
    }
}
fn compile_copy(tree: &mut Tree) {
    for tree in &mut tree.children {
        if let Some(token) = tree.data.get::<Token>() {
            if token.deref() == "$" {
                let i = tree.children.pop().unwrap().data.remove::<i64>().unwrap();
                tree.data.insert::<Box<dyn Instruction>>(Box::new(Copy(i)));
            }
        }
    }
}
#[derive(Debug)]
struct Add;
impl Instruction for Add {
    fn interpret(&self, i: &mut Interpreter) {
        i.sp -= 1;
        i.stack[i.sp] += i.stack[i.sp + 1];
    }
}
fn compile_add(tree: &mut Tree) {
    for tree in &mut tree.children {
        if let Some(token) = tree.data.get::<Token>() {
            if token.deref() == "+" || token.deref() == "add" {
                tree.data.insert::<Box<dyn Instruction>>(Box::new(Add));
            }
        }
    }
}
#[derive(Debug)]
struct Neg;
impl Instruction for Neg {
    fn interpret(&self, i: &mut Interpreter) {
        i.stack[i.sp] = -i.stack[i.sp];
    }
}
fn compile_neg(tree: &mut Tree) {
    for tree in &mut tree.children {
        if let Some(token) = tree.data.get::<Token>() {
            if token.deref() == "-" || token.deref() == "neg" {
                tree.data.insert::<Box<dyn Instruction>>(Box::new(Neg));
            }
        }
    }
}
#[derive(Debug)]
struct Jumpz(String);
impl Instruction for Jumpz {
    fn interpret(&self, i: &mut Interpreter) {
        if i.stack[i.sp] == 0 {
            i.pc = *i.labels.0.get(&self.0).unwrap_or_else(|| panic!("unknown label {}", self.0));
        }
        i.sp -= 1;
    }
}
fn compile_jumpz(tree: &mut Tree) {
    for tree in &mut tree.children {
        if let Some(token) = tree.data.get::<Token>() {
            if token.deref() == "?" {
                let s = tree.children.pop().unwrap().data.remove::<Token>().unwrap();
                tree.data.insert::<Box<dyn Instruction>>(Box::new(Jumpz(s.deref().to_owned())));
            }
        }
    }
}
#[derive(Debug)]
struct Label(String);
impl Instruction for Label {
    fn interpret(&self, _i: &mut Interpreter) {}
}
fn compile_label(tree: &mut Tree) {
    let mut labels = Labels(HashMap::new());
    compile_label(tree, &mut labels);
    tree.data.insert::<Labels>(labels);
    fn compile_label(tree: &mut Tree, labels: &mut Labels) {    
        for (i, tree) in tree.children.iter_mut().enumerate() {
            if let Some(token) = tree.data.get::<Token>() {
                if token.deref() == ":" {
                    let s = tree.children.pop().unwrap().data.remove::<Token>();
                    let s = s.unwrap().deref().to_owned();
                    labels.0.insert(s.clone(), i as i64);
                    tree.data.insert::<Box<dyn Instruction>>(Box::new(Label(s)));
                }
            }
        }
    }
}
