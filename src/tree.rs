use crate::token::Token;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

pub struct Tree {
    pub token: Token,
    pub children: Vec<Tree>,
    contents: HashMap<std::any::TypeId, Box<dyn std::any::Any>>,
}

impl Tree {
    pub fn new(token: Token) -> Tree {
        Tree {
            token,
            children: Vec::new(),
            contents: HashMap::new(),
        }
    }

    pub fn insert<T: 'static>(&mut self, value: T) -> Option<T> {
        self.contents
            .insert(std::any::TypeId::of::<T>(), Box::new(value))
            .map(|any| *any.downcast().unwrap())
    }

    pub fn remove<T: 'static>(&mut self) -> Option<T> {
        self.contents
            .remove(&std::any::TypeId::of::<T>())
            .map(|any| *any.downcast().unwrap())
    }

    pub fn get<T: 'static>(&self) -> Option<&T> {
        self.contents
            .get(&std::any::TypeId::of::<T>())
            .map(|any| any.downcast_ref().unwrap())
    }

    pub fn get_mut<T: 'static>(&mut self) -> Option<&mut T> {
        self.contents
            .get_mut(&std::any::TypeId::of::<T>())
            .map(|any| any.downcast_mut().unwrap())
    }
}

impl Display for Tree {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        fn fmt(tree: &Tree, f: &mut Formatter, depth: usize) -> FmtResult {
            if !tree.token.as_str().is_empty() {
                writeln!(f, "{}{}", "\t".repeat(depth), tree.token)?;
            }
            tree.children
                .iter()
                .try_for_each(|child| fmt(child, f, depth + 1))
        }
        fmt(self, f, 0)
    }
}
