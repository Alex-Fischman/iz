use std::any::Any;

#[derive(Debug)]
struct Tree {
    data: Box<dyn Any>,
    children: Vec<Tree>,
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

    postorder(&mut tree, remove_whitespace);

    println!("{tree:#?}");
}

fn remove_whitespace(trees: &mut Vec<Tree>) {
    let mut i = 0;
    while i < trees.len() {
        match trees[i].data.downcast_ref::<char>() {
            Some(c) if c.is_whitespace() => {
                trees.remove(i);
            }
            _ => i += 1,
        }
    }
}

// #[derive(Clone, Debug, PartialEq)]
// enum Leaf {
//     CurlyLeft,
//     CurlyRight,
//     Identifier(String),
//     Integer(i64),
//     Whitespace(char),
//     Char(char),
// }

fn postorder<F: Fn(&mut Vec<Tree>) + Clone>(tree: &mut Tree, f: F) {
    for tree in &mut tree.children {
        postorder(tree, f.clone());
    }
    f(&mut tree.children);
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
