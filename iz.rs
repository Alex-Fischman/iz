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
    children: Vec<Tree>,
    contents: HashMap<TypeId, Box<dyn Any>>,
}

impl Tree {
    fn new() -> Tree {
        Tree { children: Vec::new(), contents: HashMap::new() }
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
            if let Some(token) = tree.get::<Token>() {
                writeln!(f, "{}{}", "\t".repeat(depth), token)?;
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

    let mut tree = Tree::new();
    tree.insert(source.clone());
    let los = source.text.char_indices().map(|(i, _)| i);
    let his = source.text.char_indices().map(|(i, _)| i);
    for (lo, hi) in los.zip(his.skip(1).chain([source.text.len()])) {
        let mut child = Tree::new();
        child.insert(Token { source: source.clone(), lo, hi });
        tree.children.push(child);
    }

    let mut passes = VecDeque::new();
    passes.push_back(Pass::new("remove comments", remove_comments));
    passes.push_back(Pass::new("remove whitespace", remove_whitespace));
    passes.push_back(Pass::new("concat identifiers", concat_alike_tokens(is_identifier)));
    passes.push_back(Pass::new("concat operators", concat_alike_tokens(is_operator)));
    passes.push_back(Pass::new("concat labels", concat_labels));
    passes.push_back(Pass::new("print tree", |tree| print!("{}", tree)));
    passes.push_back(Pass::new("get instructions", instructions));
    passes.push_back(Pass::new("get labels", labels));
    Pass::run_passes(passes, &mut tree);
}

fn remove_comments(tree: &mut Tree) {
    let mut in_comment = false;
    tree.children.retain(|child| {
        match child.get::<Token>().unwrap().deref() {
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
    tree.children.retain(|child| !is_whitespace(child.get::<Token>().unwrap()));
}

fn concat_alike_tokens<F: Fn(&str) -> bool>(alike: F) -> impl Fn(&mut Tree) {
    move |tree: &mut Tree| {
        let mut i = 1;
        while i < tree.children.len() {
            let prev = tree.children[i - 1].get::<Token>().unwrap();
            let curr = tree.children[i].get::<Token>().unwrap();
            if alike(prev) && alike(curr) && curr.source == prev.source && prev.hi == curr.lo {
                let curr = tree.children.remove(i).remove::<Token>().unwrap();
                i -= 1;
                tree.children[i].get_mut::<Token>().unwrap().hi = curr.hi;
            }
            i += 1;
        }
    }
}

fn concat_labels(tree: &mut Tree) {
    let mut i = 1;
    while i < tree.children.len() {
        let prev = tree.children[i - 1].get::<Token>().unwrap();
        let curr = tree.children[i].get::<Token>().unwrap();
        if curr.deref() == ":" {
            if is_identifier(prev) && curr.source == prev.source && prev.hi == curr.lo {
                let curr = tree.children.remove(i).remove::<Token>().unwrap();
                i -= 1;
                tree.children[i].get_mut::<Token>().unwrap().hi = curr.hi;
            }
        }
        i += 1;
    }
}

enum Instruction {
    Push(i64),
    Pop,
    Sp,
    Pc,
    Return,
    Write,
    Read,
    Add,
    Mul,
    Ltz,
    Jumpz(String),
    Label(String),
}

// we want the tokens for the location information
struct Code(Vec<(Instruction, Token)>);

fn instructions(tree: &mut Tree) {
    let code = tree
        .children
        .drain(..)
        .map(|mut child| match (child.remove::<Instruction>(), child.remove::<Token>()) {
            (None, _) => panic!("no instruction for tree\n{}", child),
            (_, None) => panic!("no token for tree\n{}", child),
            (Some(instruction), Some(token)) => {
                if child.children.is_empty() {
                    (instruction, token)
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

fn labels(tree: &mut Tree) {
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
    }
    tree.insert(labels);
}
