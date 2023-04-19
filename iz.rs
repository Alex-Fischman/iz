use std::{
    any::{Any, TypeId},
    collections::HashMap,
};

struct Token {
    source: std::rc::Rc<Source>,
    lo: usize,
    hi: usize,
}

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
        if self.lo == 0 && self.hi == 0 {
            return Ok(());
        }
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

struct Data(HashMap<TypeId, Box<dyn Any>>);

impl Data {
    fn insert<T: 'static>(&mut self, value: T) -> Option<T> {
        self.0.insert(TypeId::of::<T>(), Box::new(value)).map(|any| *any.downcast().unwrap())
    }

    fn remove<T: 'static>(&mut self) -> Option<T> {
        self.0.remove(&TypeId::of::<T>()).map(|any| *any.downcast().unwrap())
    }

    fn get<T: 'static>(&self) -> Option<&T> {
        self.0.get(&TypeId::of::<T>()).map(|any| any.downcast_ref().unwrap())
    }

    fn get_mut<T: 'static>(&mut self) -> Option<&mut T> {
        self.0.get_mut(&TypeId::of::<T>()).map(|any| any.downcast_mut().unwrap())
    }
}

struct Tree {
    children: Vec<Tree>,
    contents: Data,
}

impl Tree {
    fn new() -> Tree {
        Tree { children: Vec::new(), contents: Data(HashMap::new()) }
    }
}

impl std::fmt::Display for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        fn fmt(tree: &Tree, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
            writeln!(f, "{}{}", "\t".repeat(depth), tree.contents.get::<Token>().unwrap())?;
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
    let source = std::rc::Rc::new(Source { name, text });

    let mut tree = Tree::new();
    tree.contents.insert(Token { source: source.clone(), lo: 0, hi: 0 });

    let los = source.text.char_indices().map(|(i, _)| i);
    let his = source.text.char_indices().map(|(i, _)| i);
    for (lo, hi) in los.zip(his.skip(1).chain([source.text.len()])) {
        let mut child = Tree::new();
        child.contents.insert(Token { source: source.clone(), lo, hi });
        tree.children.push(child);
    }

    remove_comments(&mut tree);
    remove_whitespace(&mut tree);

    print!("{}", tree);
}

fn remove_comments(tree: &mut Tree) {
    let mut in_comment = false;
    tree.children.retain(|child| {
        match child.contents.get::<Token>().unwrap().deref() {
            "#" if !in_comment => in_comment = true,
            "\n" if in_comment => in_comment = false,
            _ => {}
        }
        !in_comment
    });
}

fn remove_whitespace(tree: &mut Tree) {
    tree.children.retain(|child| {
        !child.contents.get::<Token>().unwrap().deref().chars().next().unwrap().is_whitespace()
    });
}
