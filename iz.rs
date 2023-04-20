use std::{
    any::{Any, TypeId},
    collections::{HashMap, VecDeque},
    rc::Rc,
};

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

pub use std::ops::Deref;
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
    passes.push_back(Pass::new("printing backend", |tree| print!("{}", tree)));
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
            let curr = tree.children[i].get::<Token>().unwrap();
            let prev = tree.children[i - 1].get::<Token>().unwrap();
            if alike(curr) && alike(prev) && curr.source == prev.source && prev.hi == curr.lo {
                let curr = tree.children.remove(i).remove::<Token>().unwrap();
                i -= 1;
                tree.children[i].get_mut::<Token>().unwrap().hi = curr.hi;
            }
            i += 1;
        }
    }
}
