use std::any::{Any, TypeId};
use std::collections::{HashMap, VecDeque};
use std::io::Write;
use std::ops::Deref;
use std::process::{Command, Stdio};
use std::rc::Rc;

#[derive(Clone)]
struct Token {
    source: Rc<Source>,
    lo: usize,
    hi: usize,
}

#[derive(PartialEq)]
struct Source {
    name: String,
    text: String,
}

impl Deref for Token {
    type Target = str;
    fn deref(&self) -> &str {
        &self.source.text[self.lo..self.hi]
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut row = 1;
        let mut col = 1;
        for (i, c) in self.source.text.char_indices() {
            match (i, c) {
                (i, _) if i == self.lo => break,
                (_, '\n') => {
                    row += 1;
                    col = 1;
                }
                _ => col += 1,
            }
        }
        write!(f, "{:?} at {}:{}:{}", self.deref(), self.source.name, row, col)
    }
}

struct Tree {
    token: Token,
    children: Vec<Tree>,
    contents: HashMap<TypeId, Box<dyn Any>>,
}

#[allow(dead_code)]
impl Tree {
    fn new(token: Token) -> Tree {
        Tree { token, children: Vec::new(), contents: HashMap::new() }
    }

    fn insert<T: 'static>(&mut self, value: T) -> Option<T> {
        self.contents.insert(TypeId::of::<T>(), Box::new(value)).map(|any| *any.downcast().unwrap())
    }

    fn remove<T: 'static>(&mut self) -> Option<T> {
        self.contents.remove(&TypeId::of::<T>()).map(|any| *any.downcast().unwrap())
    }

    fn get<T: 'static>(&self) -> Option<&T> {
        self.contents.get(&TypeId::of::<T>()).map(|any| any.downcast_ref().unwrap())
    }

    fn get_mut<T: 'static>(&mut self) -> Option<&mut T> {
        self.contents.get_mut(&TypeId::of::<T>()).map(|any| any.downcast_mut().unwrap())
    }
}

impl std::fmt::Display for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        fn fmt(tree: &Tree, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
            if !tree.token.deref().is_empty() {
                writeln!(f, "{}{}", "\t".repeat(depth), tree.token)?;
            }
            tree.children.iter().map(|child| fmt(child, f, depth + 1)).collect()
        }
        fmt(self, f, 0)
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let name = match args.get(1) {
        Some(name) => name.to_owned(),
        None => panic!("usage: pass a .iz file"),
    };
    let text = match std::fs::read_to_string(&name) {
        Ok(text) => text,
        Err(_) => panic!("could not read {}", name),
    };
    let source = Rc::new(Source { name, text });

    let mut tree = Tree::new(Token { source: source.clone(), lo: 0, hi: 0 });
    tree.insert(source.clone());
    let los = source.text.char_indices().map(|(i, _)| i);
    let his = source.text.char_indices().map(|(i, _)| i);
    for (lo, hi) in los.zip(his.skip(1).chain([source.text.len()])) {
        tree.children.push(Tree::new(Token { source: source.clone(), lo, hi }));
    }

    type Pass = Box<dyn Fn(&mut Tree)>;
    let mut passes: VecDeque<Pass> = VecDeque::new();
    // flat
    passes.push_back(Box::new(remove_comments));
    passes.push_back(Box::new(remove_whitespace));
    passes.push_back(Box::new(concat_tokens(is_identifier)));
    passes.push_back(Box::new(concat_tokens(is_operator)));
    passes.push_back(Box::new(parse_integers));
    // nested
    passes.push_back(Box::new(parse_brackets("(", ")")));
    passes.push_back(Box::new(parse_brackets("{", "}")));
    passes.push_back(Box::new(parse_brackets("[", "]")));
    passes.push_back(Box::new(parse_postfixes(&[":", "?", "&"])));
    // structured
    passes.push_back(Box::new(annotate_intrinsics));
    passes.push_back(Box::new(compute_types));
    passes.push_back(Box::new(compile_intrinsics_x64));
    passes.push_back(Box::new(compile_program_x64));

    tree.insert(passes);

    while let Some(pass) = tree.get_mut::<VecDeque<Pass>>().unwrap().pop_front() {
        pass(&mut tree);
    }
}

fn remove_comments(tree: &mut Tree) {
    let mut in_comment = false;
    tree.children.retain(|child| {
        match child.token.deref() {
            "#" if !in_comment => in_comment = true,
            "\n" if in_comment => in_comment = false,
            _ => {}
        }
        !in_comment
    });
}

fn is_identifier(s: &str) -> bool {
    s.chars().all(|c| c.is_alphanumeric() || c == '-' || c == '_')
}

fn is_whitespace(s: &str) -> bool {
    s.chars().all(|c| c.is_whitespace())
}

fn is_operator(s: &str) -> bool {
    !s.chars().any(|c| c.is_alphanumeric() || c.is_whitespace() || "(){}[]".contains(c))
}

fn remove_whitespace(tree: &mut Tree) {
    tree.children.retain(|child| !is_whitespace(child.token.deref()));
}

fn concat_tokens(f: impl Fn(&str) -> bool) -> impl Fn(&mut Tree) {
    move |tree: &mut Tree| {
        let mut i = 1;
        while i < tree.children.len() {
            if f(tree.children[i - 1].token.deref())
                && f(tree.children[i].token.deref())
                && tree.children[i - 1].token.source == tree.children[i].token.source
                && tree.children[i - 1].token.hi == tree.children[i].token.lo
            {
                tree.children[i - 1].token.hi = tree.children[i].token.hi;
                tree.children.remove(i);
            } else {
                i += 1;
            }
        }
    }
}

fn parse_integers(tree: &mut Tree) {
    for child in &mut tree.children {
        let chars: Vec<char> = child.token.deref().chars().collect();
        if !chars.is_empty() {
            let (chars, is_negative) = match chars[0] {
                '-' => (&chars[1..], true),
                _ => (&chars[..], false),
            };
            if let Some('0'..='9') = chars.get(0) {
                let (chars, base) = match chars {
                    ['0', 'x', ..] => (&chars[2..], 16),
                    ['0', 'b', ..] => (&chars[2..], 2),
                    _ => (chars, 10),
                };
                let value = chars.iter().fold(0, |value, c| {
                    let digit = match c {
                        '0'..='9' => *c as i64 - '0' as i64,
                        'a'..='f' => *c as i64 - 'a' as i64 + 10,
                        '_' => return value,
                        c => panic!("unknown digit {} in {}", c, child.token),
                    };
                    if digit >= base {
                        panic!("digit {} too large for base {} in {}", c, base, child.token)
                    }
                    base * value + digit
                });
                child.insert::<i64>(if is_negative { -value } else { value });
            }
        }
    }
}

fn parse_brackets<'a>(open: &'a str, close: &'a str) -> impl Fn(&mut Tree) + 'a {
    move |tree: &mut Tree| {
        tree.children.iter_mut().for_each(parse_brackets(open, close));

        let mut indices = Vec::new(); // stack of open bracket indices
        let mut i = 0;
        while i < tree.children.len() {
            let curr = &tree.children[i].token;
            if curr.deref() == open {
                indices.push(i);
            } else if curr.deref() == close {
                let j = indices.pop().unwrap_or_else(|| panic!("extra {}", curr));
                let mut cs: Vec<Tree> = tree.children.drain(j + 1..=i).collect();
                cs.pop(); // remove closing bracket
                tree.children[j].children.append(&mut cs);
                i = j;
            }
            i += 1;
        }
        if let Some(j) = indices.pop() {
            panic!("extra {}", tree.children[j].token)
        }
    }
}

fn parse_postfixes<'a>(names: &'a [&'a str]) -> impl Fn(&mut Tree) + 'a {
    move |tree: &mut Tree| {
        tree.children.iter_mut().for_each(parse_postfixes(names));

        let mut i = 0;
        while i < tree.children.len() {
            if names.contains(&tree.children[i].token.deref()) {
                if i == 0 {
                    panic!("no argument for {}", tree.children[i].token)
                }
                let child = tree.children.remove(i - 1);
                tree.children[i - 1].children.push(child);
            } else {
                i += 1;
            }
        }
    }
}

use Intrinsic::*;
#[derive(Debug, PartialEq)]
enum Intrinsic {
    Push(i64),
    Pop,
    Write,
    Read,
    Sp,
    Add,
    Mul,
    And,
    Or,
    Xor,
    Label(String),
    Jumpz(String),
    Call,
    Return,
    Func,
}

fn annotate_intrinsics(tree: &mut Tree) {
    tree.children.iter_mut().for_each(annotate_intrinsics);

    let intrinsic = match tree.token.deref() {
        _ if tree.get::<i64>().is_some() => Push(*tree.get::<i64>().unwrap()),
        "pop" => Pop,
        "read" => Read,
        "write" => Write,
        "sp" => Sp,
        "add" => Add,
        "mul" => Mul,
        "and" => And,
        "or" => Or,
        "xor" => Xor,
        ":" => Label(tree.children.remove(0).token.deref().to_string()),
        "?" => Jumpz(tree.children.remove(0).token.deref().to_string()),
        "call" => Call,
        "ret" => Return,
        "{" => Func,
        _ => return,
    };
    tree.insert(intrinsic);
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Effect {
    inputs: Vec<Type>,
    outputs: Vec<Type>,
}

macro_rules! effect {
    ($($inputs:expr)* ; $($outputs:expr)*) => (Effect {
        inputs: vec![$($inputs),*],
        outputs: vec![$($outputs),*],
    });
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Type {
    Int,
    Ptr,
    Fun(Effect),
    Ret,
}

impl Type {
    fn size(&self) -> usize {
        match self {
            Type::Int | Type::Ptr | Type::Fun(_) | Type::Ret => 8,
        }
    }
}

fn compute_types(tree: &mut Tree) {
    use Type::*;

    // tree.children.len() as a key holds the returned effect
    let mut inputs: HashMap<usize, Effect> = HashMap::from([(0, effect!(;))]);

    let mut i = 0;
    while i < tree.children.len() {
        let mut targets = vec![i + 1];
        let mut curr = match tree.children[i].get::<Intrinsic>() {
            Some(Push(_)) => effect!(; Int),
            Some(Pop) => match inputs[&i].outputs.last() {
                Some(t) if t.size() == 8 => effect!(t.clone() ;),
                Some(t) => {
                    panic!("expected a type of size 8, found {:?} for {}", t, tree.children[i])
                }
                None => panic!("could not infer type for {}", tree.children[i]),
            },
            Some(Read) => effect!(Ptr ; Int),
            Some(Write) => effect!(Int Ptr ;),
            Some(Sp) => effect!(; Ptr),
            Some(Add) => effect!(Int Int ; Int),
            Some(Mul) => effect!(Int Int ; Int),
            Some(And) => effect!(Int Int ; Int),
            Some(Or) => effect!(Int Int ; Int),
            Some(Xor) => effect!(Int Int ; Int),
            Some(Label(_)) => effect!(;),
            Some(Jumpz(s)) => {
                let labels = (0..tree.children.len()).filter(
                    |j| matches!(tree.children[*j].get::<Intrinsic>(), Some(Label(t)) if s == t),
                );
                let labels: Vec<usize> = labels.collect();
                let target = match labels.as_slice() {
                    [target] => target,
                    [] => panic!("could not find the matching label for {}", tree.children[i]),
                    _ => panic!("too many matching labels for {}", tree.children[i]),
                };
                targets.push(*target);
                effect!(Int ;)
            }
            Some(Call) => match inputs[&i].outputs.last() {
                Some(Fun(e)) => {
                    let mut e = e.clone();
                    e.inputs.push(Fun(e.clone()));
                    e
                }
                Some(t) => {
                    panic!("expected a function type, found {:?} for {}", t, tree.children[i])
                }
                None => panic!("could not infer type for {}", tree.children[i]),
            },
            Some(Return) => {
                targets = vec![tree.children.len()];
                effect!(Ret ;)
            }
            Some(Func) => {
                compute_types(&mut tree.children[i]);
                tree.children[i].remove::<Effect>().unwrap()
            }
            None => todo!("compute_types callback for non-intrinsics?"),
        };

        tree.children[i].insert(curr.clone());

        let mut next = inputs[&i].clone();
        while let Some(input) = curr.inputs.pop() {
            match next.outputs.pop() {
                Some(output) if input == output => {}
                Some(output) => {
                    panic!("expected {:?}, found {:?} for {}", output, input, tree.children[i])
                }
                None => next.inputs.insert(0, input.clone()),
            }
        }
        next.outputs.append(&mut curr.outputs);

        for target in targets {
            match inputs.get(&target) {
                Some(old) if *old == next => {}
                Some(_) => panic!("found branching types for {}", tree.children[target]),
                None => {
                    inputs.insert(target, next.clone());
                }
            }
        }

        i += 1;
    }

    match inputs.get(&tree.children.len()) {
        None => panic!("could not compute return type for {}", tree),
        Some(effect) => {
            tree.insert(effect!(; Fun(effect.clone())));
        }
    }
}

struct Assembly(String);
fn compile_intrinsics_x64(tree: &mut Tree) {
    tree.children.iter_mut().for_each(compile_intrinsics_x64);

    let assembly = match tree.get::<Intrinsic>() {
        Some(Push(int)) => format!("\tmovq ${}, %rax\n\tpushq %rax\n", int),
        Some(Pop) => format!("\tpopq %rax\n"),
        Some(Read) => {
            format!("\tmovq (%rsp), %rax\n\tmovq (%rax), %rax\n\tmovq %rax, (%rsp)\n")
        }
        Some(Write) => format!("\tpopq %rax\n\tpopq %rcx\n\tmovq %rcx, (%rax)\n"),
        Some(Sp) => format!("\tpushq %rsp\n"),
        Some(Add) => format!("\tpopq %rax\n\taddq %rax, (%rsp)\n"),
        Some(Mul) => format!("\tpopq %rax\n\tmulq %rax, (%rsp)\n"),
        Some(And) => format!("\tpopq %rax\n\tandq %rax, (%rsp)\n"),
        Some(Or) => format!("\tpopq %rax\n\torq %rax, (%rsp)\n"),
        Some(Xor) => format!("\tpopq %rax\n\txorq %rax, (%rsp)\n"),
        Some(Label(s)) => format!("{}:\n", s),
        Some(Jumpz(s)) => format!("\tpopq %rax\n\ttest %rax, %rax\n\tjz {}\n", s),
        Some(Call) => todo!(),
        Some(Return) => format!("\tret\n"),
        Some(Func) => todo!(),
        None => return,
    };
    tree.insert(Assembly(assembly));
}

fn compile_program_x64(tree: &mut Tree) {
    let name = match tree.token.source.name.rfind('.') {
        Some(i) => &tree.token.source.name[..i],
        None => "a.out",
    };

    let mut assembler = Command::new("gcc")
        .arg("-x")
        .arg("assembler-with-cpp")
        .arg("-nostdlib")
        .arg("-Wall")
        .arg("-g")
        .arg("-o")
        .arg(name)
        .arg("-")
        .stdin(Stdio::piped())
        .spawn()
        .unwrap();
    let stdin = assembler.stdin.as_mut().unwrap();

    write!(stdin, "#include <sys/syscall.h>\n\n\t.global _start\n_start:").unwrap();
    for child in &tree.children {
        match child.get::<Assembly>() {
            Some(Assembly(assembly)) => write!(stdin, "{}", assembly).unwrap(),
            _ => panic!("no assembly for {}", child),
        }
    }
    write!(stdin, "\tmovq $SYS_exit, %rax\n\tmovq $0, %rdi\n\tsyscall").unwrap();

    if !assembler.wait().unwrap().success() {
        panic!("assembler failed")
    }

    let output = Command::new(format!("./{}", name)).output().unwrap();
    println!(
        "status: {}\nstderr: {}\nstdout: {}",
        output.status,
        std::str::from_utf8(&output.stderr).unwrap(),
        std::str::from_utf8(&output.stdout).unwrap(),
    );
}
