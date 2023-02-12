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

    // --------------------------------------------------------------------------

    // remove comments
    postorder(&mut tree, |trees| {
        let mut i = 0;
        while i < trees.len() {
            match trees[i].data.xx::<char>() {
                Some('#') => {
                    let mut j = i;
                    while j < trees.len() && trees[j].data.xx::<char>() != Some(&'\n') {
                        j += 1;
                    }
                    trees.drain(i..j);
                }
                _ => i += 1,
            }
        }
    });

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

    // parse integer literals
    postorder(&mut tree, |trees| {
        let mut i = 0;
        while i < trees.len() {
            match trees[i].data.xx::<String>().map(|s| s.parse::<i64>()) {
                Some(Ok(int)) => trees[i].data = Box::new(int),
                _ => i += 1,
            }
        }
    });

    // convert to ops
    postorder(&mut tree, |trees| {
        let mut i = 0;
        while i < trees.len() {
            if let Some(int) = trees[i].data.xx::<i64>() {
                trees[i].data = Box::new(Op::Psh(*int))
            } else if let Some(s) = trees[i].data.xx::<String>() {
                trees[i].data = Box::new(match s.as_str() {
                    "add" => Op::Add,
                    "neg" => Op::Neg,
                    "ltz" => Op::Ltz,
                    "lbl" => {
                        i += 1;
                        Op::Lbl(*trees[i].data.xx::<i64>().unwrap())
                    }
                    "jmp" => Op::Jmp,
                    _ => panic!("unknown op"),
                })
            } else {
                panic!("unknown op")
            }
            i += 1;
        }
    });

    // --------------------------------------------------------------------------

    // interpreter backend

    assert!(tree.data.is::<()>());
    let program: Vec<Op> = tree
        .children
        .iter()
        .map(|tree| {
            assert!(tree.children.is_empty());
            tree.data.xx::<Op>().unwrap().clone()
        })
        .collect();

    let mut labels = std::collections::HashMap::new();
    let mut i = 0;
    while i < program.len() {
        if let Op::Lbl(lbl) = program[i] {
            let old = labels.insert(lbl, i);
            assert!(old.is_none());
        }
        i += 1;
    }

    let mut stack = vec![];
    let mut pc = 0;
    while pc < program.len() {
        match program[pc] {
            Op::Psh(i) => stack.push(i),
            Op::Add => {
                let (a, b) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push(a + b);
            }
            Op::Neg => {
                let a = stack.pop().unwrap();
                stack.push(-a);
            }
            Op::Ltz => match stack.pop().unwrap() < 0 {
                true => stack.push(1),
                false => stack.push(0),
            },
            Op::Lbl(_) => {}
            Op::Jmp => {
                let lbl = stack.pop().unwrap();
                if stack.pop().unwrap() == 0 {
                    pc = *labels.get(&lbl).unwrap();
                }
            }
        }
        pc += 1;
    }

    println!("{stack:?}");
}

#[derive(Clone)]
enum Op {
    Psh(i64),
    Add,
    Neg,
    Ltz,
    Lbl(i64),
    Jmp,
}

impl Debug for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Op::Psh(i) => write!(f, "psh {i}"),
            Op::Add => write!(f, "add"),
            Op::Neg => write!(f, "neg"),
            Op::Ltz => write!(f, "ltz"),
            Op::Lbl(i) => write!(f, "lbl {i}"),
            Op::Jmp => write!(f, "jmp"),
        }
    }
}
