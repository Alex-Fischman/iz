#[derive(Debug)]
struct Tree {
    data: Box<dyn Data>,
    children: Vec<Tree>,
}

trait Data: std::fmt::Debug + std::any::Any {}
impl<T: std::fmt::Debug + std::any::Any> Data for T {}

impl dyn Data {
    fn is<T: std::any::Any>(&self) -> bool {
        self.type_id() == std::any::TypeId::of::<T>()
    }

    fn xx<T: std::any::Any>(&self) -> Option<&T> {
        if self.is::<T>() {
            unsafe { Some(&*(self as *const dyn Data as *const T)) }
        } else {
            None
        }
    }

    fn xxx<T: std::any::Any>(&mut self) -> Option<&mut T> {
        if self.is::<T>() {
            unsafe { Some(&mut *(self as *mut dyn Data as *mut T)) }
        } else {
            None
        }
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
                    if i == 0 {
                        trees[i].data = Box::new(c.to_string());
                    } else if let Some(s) = trees[i - 1].data.xxx::<String>() {
                        s.push(c);
                        trees.remove(i);
                    } else {
                        trees[i].data = Box::new(c.to_string());
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
