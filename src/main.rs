use std::any::Any;
use std::fmt::Debug;

#[derive(Debug)]
struct Tree {
    data: Box<dyn Data>,
    children: Vec<Tree>,
}

trait Data: Any + Debug {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl<T: Any + Debug> Data for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl dyn Data {
    fn is<T: Any>(&self) -> bool {
        self.as_any().is::<T>()
    }

    fn xx<T: Any>(&self) -> Option<&T> {
        self.as_any().downcast_ref::<T>()
    }

    fn xxx<T: Any>(&mut self) -> Option<&mut T> {
        self.as_any_mut().downcast_mut::<T>()
    }
}

fn postorder<F: Fn(&mut Vec<Tree>) + Clone>(tree: &mut Tree, f: F) {
    for tree in &mut tree.children {
        postorder(tree, f.clone());
    }
    f(&mut tree.children);
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let file = args.get(1).expect("no file passed");
    let tokens = std::fs::read_to_string(file)
        .expect("could not read file")
        .chars()
        .map(|c| Tree {
            data: Box::new(c),
            children: vec![],
        })
        .collect();
    let mut tree = Tree {
        data: Box::new(()),
        children: tokens,
    };

    // group identifiers
    postorder(&mut tree, |trees| {
        let mut i = 0;
        while i < trees.len() {
            match trees[i].data.xx::<char>().cloned() {
                Some(c) if matches!(c, '_' | 'a' ..= 'z' | 'A' ..= 'Z' | '0' ..= '9') => {
                    if i == 0 || !trees[i - 1].data.is::<String>() {
                        trees[i].data = Box::new(c.to_string());
                    } else {
                        trees[i - 1].data.xxx::<String>().unwrap().push(c);
                        trees.remove(i);
                    }
                }
                _ => i += 1,
            }
        }
    });

    // remove whitespace
    postorder(&mut tree, |trees| {
        let mut i = 0;
        while i < trees.len() {
            match trees[i].data.xx::<char>() {
                Some(c) if c.is_whitespace() => {
                    trees.remove(i);
                }
                _ => i += 1,
            }
        }
    });

    println!("{tree:#?}");
}

#[allow(dead_code)]
#[derive(Debug)]
enum Op {
    Push(i64),
    Add,
    Neg,
    Ltz,
    Lbl(usize),
    Jmp,
}

#[allow(dead_code)]
type Stack = Vec<i64>;
