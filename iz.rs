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

use std::rc::Rc; // only needed to get around Any restrictions
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

use std::{any::{Any, TypeId}, collections::HashMap};
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

struct Tree {
    locals: Data,
    children: Vec<Tree>,
}

struct Context<'a> {
    globals: &'a mut Data,
    trees: &'a mut Vec<Tree>,
}

impl<'a> Context<'a> {
    fn for_each<F: Fn(&mut Context)>(&mut self, f: F) {
        for tree in &mut *self.trees {
            f(&mut Context {
                globals: self.globals,
                trees: &mut tree.children,
            });
        }
    }

    // requires globals::Passes and globals::NextPass to exist
    fn run(&mut self) {
        loop {
            let passes = self.globals.get::<Passes>().unwrap();
            let pass = match passes.passes.get(self.globals.get::<NextPass>().unwrap().0) {
                None => return,
                Some(rc) => rc.clone(),
            };
            pass(self);
            self.globals.get_mut::<NextPass>().unwrap().0 += 1;
        }
    }
}

struct NextPass(usize);

struct Passes {
    names: HashMap<String, usize>,
    passes: Vec<Rc<dyn Fn(&mut Context)>>,
}

impl Passes {
    fn push<F: Fn(&mut Context) + 'static>(&mut self, name: &str, pass: F) {
        self.names.insert(name.to_owned(), self.passes.len());
        self.passes.push(Rc::new(pass));
    }
}

fn main() {
    // frontend
    let args: Vec<String> = std::env::args().collect();
    let name = args.get(1).unwrap_or_else(|| panic!("missing .iz file")).to_owned();
    let text = std::fs::read_to_string(&name)
        .unwrap_or_else(|_| panic!("could not read {}", name));
    let source = Rc::new(Source { name, text });

    let mut context = Context { globals: &mut Data(HashMap::new()), trees: &mut Vec::new() };
    let is = source.text.char_indices().map(|(i, _)| i);
    let js = source.text.char_indices().map(|(j, _)| j);
    for (i, j) in is.zip(js.skip(1).chain([source.text.len()])) {
        let mut tree = Tree { locals: Data(HashMap::new()), children: Vec::new() };
        tree.locals.insert::<Token>(Token { source: source.clone(), lo: i, hi: j });
        context.trees.push(tree);
    }

    let mut passes = Passes { names: HashMap::new(), passes: Vec::new() };
    // flat
    passes.push("remove comments", remove_comments);
    passes.push("tokenize identifiers", concat_alike_tokens(is_identifier));
    passes.push("tokenize operators", concat_alike_tokens(is_operator));
    passes.push("remove whitespace", remove_whitespace);
    passes.push("parse integer literals", integer_literals);
    // tree
    passes.push("parse {}", match_brackets("{", "}"));
    passes.push("parse :?", parse_postfix(&[":", "?"]));
    passes.push("analyze types", type_check);
    passes.push("compile instructions", compile_instructions);
    // flat
    passes.push("interpret", interpret);

    context.globals.insert::<Passes>(passes);
    context.globals.insert::<NextPass>(NextPass(0));
    context.run();
}

fn remove_comments(context: &mut Context) {
    let mut in_comment = false;
    context.trees.retain(|tree| {
        if !tree.children.is_empty() {
            panic!("unexpected child of {}", tree.locals.get::<Token>().unwrap())
        }
        match tree.locals.get::<Token>().unwrap().deref() {
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

fn concat_alike_tokens<F: Fn(&str) -> bool>(alike: F) -> impl Fn(&mut Context) {
    move |context: &mut Context| {
        let mut i = 1;
        while i < context.trees.len() {
            if !context.trees[i].children.is_empty() {
                panic!("unexpected child of {}", context.trees[i].locals.get::<Token>().unwrap())
            }
            let curr = context.trees[i].locals.get::<Token>().unwrap();
            let prev = context.trees[i - 1].locals.get::<Token>().unwrap();
            if alike(curr) && alike(prev) && curr.source == prev.source && curr.lo == prev.hi {
                let curr = context.trees.remove(i).locals.remove::<Token>().unwrap();
                i -= 1;
                context.trees[i].locals.get_mut::<Token>().unwrap().hi = curr.hi;
            }
            i += 1;
        }
    }
}

fn remove_whitespace(context: &mut Context) {
    context.trees.retain(|tree| !is_whitespace(tree.locals.get::<Token>().unwrap()));
}

fn integer_literals(context: &mut Context) {
    for tree in &mut *context.trees {
        if !tree.children.is_empty() {
            panic!("unexpected child of {}", tree.locals.get::<Token>().unwrap())
        }
        if let Some(token) = tree.locals.get::<Token>() {
            let mut chars = token.deref().chars().peekable();
            let is_negative = match chars.peek() {
                Some('-') => {
                    chars.next().unwrap();
                    true
                }
                _ => false,
            };
            if matches!(chars.peek(), Some('0'..='9')) {
                let mut value = 0;
                let base = match chars.peek() {
                    Some('1'..='9') => 10,
                    Some('0') => {
                        chars.next().unwrap();
                        match chars.peek() {
                            None | Some('0'..='9') => 10,
                            Some('x') => {
                                chars.next().unwrap();
                                16
                            },
                            Some('b') => {
                                chars.next().unwrap();
                                2
                            },
                            Some(c) => panic!("unknown base prefix 0{} in {}", c, token),
                        }
                    },
                    Some(_) | None => unreachable!(),
                };
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
                tree.locals.insert::<i64>(if is_negative { -value } else { value });
            }
        }
    }
}

fn match_brackets<'a>(open: &'a str, close: &'a str) -> impl Fn(&mut Context) + 'a {
    move |context: &mut Context| {
        context.for_each(match_brackets(open, close));
        let mut indices = Vec::new(); // stack of open bracket indices
        let mut i = 0;
        while i < context.trees.len() {
            let curr = context.trees[i].locals.get::<Token>().unwrap();
            if curr.deref() == open {
                indices.push(i);
            } else if curr.deref() == close {
                let j = indices.pop().unwrap_or_else(|| panic!("extra {}", curr));
                let mut cs: Vec<Tree> = context.trees.drain(j + 1 ..= i).collect();
                cs.pop(); // remove closing bracket
                context.trees[j].children.append(&mut cs);
                i = j;
            }
            i += 1;
        }
        if let Some(j) = indices.pop() {
            panic!("extra {}", context.trees[j].locals.get::<Token>().unwrap())
        }
    }
}

fn parse_postfix<'a>(names: &'a [&'a str]) -> impl Fn(&mut Context) + 'a {
    move |context: &mut Context| {
        context.for_each(parse_postfix(names));
        let mut i = 0;
        while i < context.trees.len() {
            let curr = context.trees[i].locals.get::<Token>().unwrap().clone();
            if names.contains(&curr.deref()) {
                if i == 0 {
                    panic!("no argument for {}", curr)
                }
                let child = context.trees.remove(i - 1);
                i -= 1;
                context.trees[i].children.push(child);
            }
            i += 1;
        }
    }
}

#[derive(Debug)]
enum Instruction {
    Push(i64),
    Add,
    Jumpz(String),
    Label(String),
    Pc,
    Return,
    Sp,
    Read,
    Write,
}

#[derive(Debug, PartialEq)]
enum Type {
    Int,
    Ptr,
}

struct Effect {
    inputs: Vec<Type>,
    outputs: Vec<Type>,
}

impl Effect {
    fn new<I: IntoIterator<Item = Type>>(inputs: I, outputs: I) -> Effect {
        Effect { inputs: inputs.into_iter().collect(), outputs: outputs.into_iter().collect() }
    }

    fn compose(&mut self, mut other: Effect) {
        for input in other.inputs.into_iter().rev() {
            match self.outputs.pop() {
                Some(output) if input == output => {}
                Some(output) => panic!("expected {:?}, found {:?}", output, input),
                None => self.inputs.insert(0, input),
            }
        }
        self.outputs.append(&mut other.outputs);
    }
}

fn type_check(_context: &mut Context) {
    todo!()
}

fn compile_instructions(context: &mut Context) {
    context.for_each(compile_instructions);
    for tree in &mut *context.trees {
        let instruction = if let Some(int) = tree.locals.get::<i64>() {
            Some(Instruction::Push(*int))
        } else {
            let token = tree.locals.get::<Token>().unwrap();
            match token.deref() {
                "add" => Some(Instruction::Add),
                "?" => {
                    let s = tree.children.pop().unwrap().locals.remove::<Token>();
                    Some(Instruction::Jumpz(s.unwrap().deref().to_owned()))
                }
                ":" => {
                    let s = tree.children.pop().unwrap().locals.remove::<Token>();
                    Some(Instruction::Label(s.unwrap().deref().to_owned()))
                }
                "pc" => Some(Instruction::Pc),
                "return" => Some(Instruction::Return),
                "sp" => Some(Instruction::Sp),
                "read" => Some(Instruction::Read),
                "write" => Some(Instruction::Write),
                _ => None,
            }
        };
        if let Some(instruction) = instruction {
            tree.locals.insert::<Vec<Instruction>>(vec![instruction]);
        }
    }
}

// heap is stored in positive indices
// stack is stored in negative indices
struct Memory(HashMap<i64, i64>);
impl std::ops::Index<i64> for Memory {
    type Output = i64;
    fn index(&self, i: i64) -> &i64 {
        &self.0[&i]
    }
}
impl std::ops::IndexMut<i64> for Memory {
    fn index_mut(&mut self, i: i64) -> &mut i64 {
        self.0.entry(i).or_insert(0)
    }
}

fn interpret(context: &mut Context) {
    let code: Vec<_> = context.trees.into_iter().flat_map(|tree| {
        if !tree.children.is_empty() {
            panic!("unexpected child of {}", tree.locals.get::<Token>().unwrap())
        }
        tree.locals.remove::<Vec<Instruction>>()
            .unwrap_or_else(|| panic!("no instruction for {}", tree.locals.get::<Token>().unwrap()))
    }).collect();
    let mut labels: HashMap<String, usize> = HashMap::new();
    for (i, instruction) in code.iter().enumerate() {
        if let Instruction::Label(s) = instruction {
            labels.insert(s.clone(), i);
        }
    }
    let mut memory = Memory(HashMap::new());
    let mut pc: usize = 0;
    let mut sp: i64 = 0;
    while let Some(instruction) = code.get(pc as usize) {
        match instruction {
            Instruction::Push(int) => {
                sp -= 1;
                memory[sp] = *int;
            }
            Instruction::Add => {
                sp += 1;
                memory[sp] += memory[sp - 1];
            }
            Instruction::Jumpz(label) => {
                if memory[sp] == 0 {
                    pc = *labels.get(label).unwrap_or_else(|| panic!("unknown label {}", label));
                }
                sp += 1;
            },
            Instruction::Label(_label) => {},
            Instruction::Pc => {
                sp -= 1;
                memory[sp] = pc as i64;
            }
            Instruction::Return => {
                pc = memory[sp] as usize;
                sp += 1;
            }
            Instruction::Sp => {
                sp -= 1;
                memory[sp] = sp + 1;
            }
            Instruction::Read => memory[sp] = memory[memory[sp]],
            Instruction::Write => {
                let addr = memory[sp]; // borrow checker
                memory[addr] = memory[sp + 1];
                sp += 2;
            }
        }
        pc += 1;

        print!("{:<16}\t", format!("{:?}", instruction));
        (sp..0).rev().for_each(|i| print!("{:>8?}", memory[i]));
        println!();
    }
}
