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
        if self.lo == self.hi {
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
        write!(f, "{} at {}:{}:{}", self.deref(), self.source.name, row, col)
    }
}

pub use data::*;
mod data {
    use std::{any::Any, any::TypeId, collections::HashMap};

    pub struct Data(HashMap<TypeId, Box<dyn Any>>);

    impl Data {
        pub fn new() -> Data {
            Data(HashMap::new())
        }

        pub fn insert<T: 'static>(&mut self, value: T) -> Option<T> {
            self.0.insert(TypeId::of::<T>(), Box::new(value)).map(|any| *any.downcast().unwrap())
        }

        pub fn remove<T: 'static>(&mut self) -> Option<T> {
            self.0.remove(&TypeId::of::<T>()).map(|any| *any.downcast().unwrap())
        }

        pub fn get<T: 'static>(&self) -> Option<&T> {
            self.0.get(&TypeId::of::<T>()).map(|any| any.downcast_ref().unwrap())
        }

        pub fn get_mut<T: 'static>(&mut self) -> Option<&mut T> {
            self.0.get_mut(&TypeId::of::<T>()).map(|any| any.downcast_mut().unwrap())
        }
    }
}

pub use context::*;
mod context {
    use {std::collections::HashMap, Data};

    #[derive(Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Node(usize);
    pub type Children = Vec<Node>;
    pub struct Context {
        next: Node,
        tree: HashMap<Node, Children>,
        data: HashMap<Node, Data>,
    }

    impl Context {
        pub fn new() -> Context {
            Context {
                next: Node(1),
                tree: HashMap::from([(Node(0), Vec::new())]),
                data: HashMap::from([(Node(0), Data::new())]),
            }
        }

        pub fn insert_node(&mut self, parent: Option<Node>) -> Node {
            let next = self.next;
            self.next.0 += 1;
            self.tree.insert(next, Vec::new());
            self.data.insert(next, Data::new());
            self.tree.get_mut(&parent.unwrap_or(Node(0))).unwrap().push(next);
            next
        }

        pub fn remove_node(&mut self, node: Node) -> (Children, Data) {
            self.tree.values_mut().for_each(|children| children.retain(|child| *child != node));
            (self.tree.remove(&node).unwrap(), self.data.remove(&node).unwrap())
        }

        pub fn insert<T: 'static>(&mut self, node: Node, value: T) -> Option<T> {
            self.data.get_mut(&node).unwrap().insert(value)
        }

        pub fn remove<T: 'static>(&mut self, node: Node) -> Option<T> {
            self.data.get_mut(&node).unwrap().remove()
        }

        pub fn get<T: 'static>(&mut self, node: Node) -> Option<&T> {
            self.data.get(&node).unwrap().get()
        }

        pub fn get_mut<T: 'static>(&mut self, node: Node) -> Option<&mut T> {
            self.data.get_mut(&node).unwrap().get_mut()
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
    let source = std::rc::Rc::new(Source { name, text });

    let mut context = Context::new();

    let los = source.text.char_indices().map(|(i, _)| i);
    let his = source.text.char_indices().map(|(i, _)| i);
    for (lo, hi) in los.zip(his.skip(1).chain([source.text.len()])) {
        let node = context.insert_node(None);
        context.insert(node, Token { source: source.clone(), lo, hi });
    }
}
