use std::{
    any::{Any, TypeId},
    collections::{HashMap, VecDeque},
    ops::Deref,
    rc::Rc,
};

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

struct Pass {
    name: String,
    func: Box<dyn Fn(&mut Tree)>,
}

impl Pass {
    fn new<F: Fn(&mut Tree) + 'static>(name: &str, func: F) -> Pass {
        Pass { name: name.to_owned(), func: Box::new(func) }
    }

    fn run_passes(passes: VecDeque<Pass>, tree: &mut Tree) {
        tree.insert(passes);
        while let Some(pass) = tree.get_mut::<VecDeque<Pass>>().unwrap().pop_front() {
            (pass.func)(tree)
        }
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

    let mut passes = VecDeque::new();
    passes.push_back(Pass::new("remove comments", remove_comments));
    passes.push_back(Pass::new("remove whitespace", remove_whitespace));
    passes.push_back(Pass::new("concat identifiers", concat_tokens(is_identifier, is_identifier)));
    passes.push_back(Pass::new("concat operators", concat_tokens(is_operator, is_operator)));
    passes.push_back(Pass::new("parse integers", parse_integers));
    passes.push_back(Pass::new("parse :?&", parse_postfixes(&[":", "?", "&"])));
    passes.push_back(Pass::new("translate instructions", translate_instructions));
    passes.push_back(Pass::new("get instructions", get_instructions));
    passes.push_back(Pass::new("get labels", get_labels));
    passes.push_back(Pass::new("compile x86", compile_x86));
    Pass::run_passes(passes, &mut tree);
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
    s.chars().all(|c| !(c.is_alphanumeric() || c.is_whitespace() || "(){}[]".contains(c)))
}

fn remove_whitespace(tree: &mut Tree) {
    tree.children.retain(|child| !is_whitespace(child.token.deref()));
}

fn concat_tokens(f: impl Fn(&str) -> bool, g: impl Fn(&str) -> bool) -> impl Fn(&mut Tree) {
    move |tree: &mut Tree| {
        let mut i = 1;
        while i < tree.children.len() {
            if f(tree.children[i - 1].token.deref())
                && g(tree.children[i].token.deref())
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
                i -= 1;
                tree.children[i].children.push(child);
            }
            i += 1;
        }
    }
}

enum Instruction {
    Push(i64),
    Pop,
    Sp,
    Write,
    Read,
    Add,
    Mul,
    Ltz,
    Label(String),
    Jumpz(String),
    Addr(String),
    Goto,
}

fn translate_instructions(tree: &mut Tree) {
    for child in &mut tree.children {
        if child.get::<Instruction>().is_none() {
            let instruction = if let Some(int) = child.get::<i64>() {
                Instruction::Push(*int)
            } else {
                match child.token.deref() {
                    "pop" => Instruction::Pop,
                    "sp" => Instruction::Sp,
                    "write" => Instruction::Write,
                    "read" => Instruction::Read,
                    "add" => Instruction::Add,
                    "mul" => Instruction::Mul,
                    "ltz" => Instruction::Ltz,
                    ":" => {
                        let grandchild = child.children.pop().unwrap();
                        Instruction::Label(grandchild.token.deref().to_owned())
                    }
                    "?" => {
                        let grandchild = child.children.pop().unwrap();
                        Instruction::Jumpz(grandchild.token.deref().to_owned())
                    }
                    "&" => {
                        let grandchild = child.children.pop().unwrap();
                        Instruction::Addr(grandchild.token.deref().to_owned())
                    }
                    "goto" => Instruction::Goto,
                    _ => panic!("could not translate {}", child.token),
                }
            };
            child.insert(vec![instruction]);
        }
    }
}

// we want the tokens for the location information
struct Code(Vec<(Instruction, Token)>);

fn get_instructions(tree: &mut Tree) {
    let code = tree
        .children
        .drain(..)
        .flat_map(|mut child| match child.remove::<Vec<Instruction>>() {
            None => panic!("no instruction for tree\n{}", child),
            Some(instructions) => {
                if child.children.is_empty() {
                    instructions.into_iter().map(move |i| (i, child.token.clone()))
                } else {
                    panic!("tree had children\n{}", child)
                }
            }
        })
        .collect();
    tree.insert(Code(code));
}

// we want the tokens for the location information
struct Labels(HashMap<String, (usize, Token)>);

fn get_labels(tree: &mut Tree) {
    let code: &Code = tree.get().expect("expected tree to contain code");
    let mut labels = Labels(HashMap::new());
    for (pc, (instruction, token)) in code.0.iter().enumerate() {
        if let Instruction::Label(label) = instruction {
            let old = labels.0.insert(label.clone(), (pc, token.clone()));
            if let Some((_, old)) = old {
                panic!("label is declared twice:\n{}\n{}", token, old)
            }
        }
    }
    for (instruction, token) in &code.0 {
        if let Instruction::Jumpz(label) = instruction {
            if labels.0.get(label).is_none() {
                panic!("jumpz has no matching label: {}", token)
            }
        }
        if let Instruction::Addr(label) = instruction {
            if labels.0.get(label).is_none() {
                panic!("addr has no matching label: {}", token)
            }
        }
    }
    tree.insert(labels);
}

fn compile_x86(tree: &mut Tree) {
    let code: &Code = tree.get().expect("expected tree to contain code");
    let assembly: String = code
        .0
        .iter()
        .map(|(instruction, _)| match instruction {
            Instruction::Push(int) => format!("\tpushq ${}\n", int),
            Instruction::Pop => format!("\tpopq %rax\n"),
            Instruction::Sp => format!("\tpushq %rsp\n"),
            Instruction::Write => format!("\tpopq %rax\n\tpopq %rbx\n\tmovq %rbx, (%rax)\n"),
            Instruction::Read => format!("\tmovq (%rsp), %rax\n\tmovq (%rax), (%rsp)\n"),
            Instruction::Add => format!("\tpopq %rax\n\tadd %rax, (%rsp)\n"),
            Instruction::Mul => format!("\tpopq %rax\n\timul %rax, (%rsp)\n"),
            Instruction::Ltz => format!("TODO"),
            Instruction::Label(label) => format!("{}:\n", label),
            Instruction::Jumpz(label) => {
                format!("\tpopq %rax\n\ttest %rax, %rax\n\tjz {}\n", label)
            }
            Instruction::Addr(label) => format!("\tpushq {}\n", label),
            Instruction::Goto => format!("\tret\n"),
        })
        .collect();
    println!("{}", assembly);
}
