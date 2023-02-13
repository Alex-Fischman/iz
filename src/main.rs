#[derive(Debug, PartialEq)]
struct Tree {
    data: Data,
    children: Vec<Tree>,
}

#[derive(Debug, PartialEq)]
enum Data {
    Empty,
    Char(char),
    String(String),
    Int(i64),
    Op(Op),
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
            data: Data::Char(c),
            children: vec![],
        })
        .collect();
    let mut tree = Tree {
        data: Data::Empty,
        children: tokens,
    };

    // --------------------------------------------------------------------------

    // remove comments
    postorder(&mut tree, |trees| {
        let mut i = 0;
        while i < trees.len() {
            match trees[i].data {
                Data::Char('#') => {
                    let mut j = i;
                    while j < trees.len() && !matches!(trees[j].data, Data::Char('\n')) {
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
            match trees[i].data {
                Data::Char(c @ ('_' | 'a'..='z' | 'A'..='Z' | '0'..='9')) => {
                    if i == 0 {
                        trees[i].data = Data::String(c.to_string());
                    } else if let Data::String(s) = &mut trees[i - 1].data {
                        s.push(c);
                        trees.remove(i);
                    } else {
                        trees[i].data = Data::String(c.to_string());
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
            match trees[i].data {
                Data::Char(c) if c.is_whitespace() => {
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
            match &trees[i].data {
                Data::String(s) => match s.parse::<i64>() {
                    Ok(int) => trees[i].data = Data::Int(int),
                    _ => i += 1,
                },
                _ => i += 1,
            }
        }
    });

    // convert to ops
    postorder(&mut tree, |trees| {
        let mut i = 0;
        while i < trees.len() {
            if let Data::Int(int) = trees[i].data {
                trees[i].data = Data::Op(Op::Psh(int))
            } else if let Data::String(s) = &trees[i].data {
                trees[i].data = Data::Op(match s.as_str() {
                    "add" => Op::Add,
                    "neg" => Op::Neg,
                    "ltz" => Op::Ltz,
                    "lbl" => {
                        i += 1;
                        Op::Lbl(match &trees[i].data {
                            Data::Int(int) => *int,
                            data => panic!("expected int, found {data:?}"),
                        })
                    }
                    "jmp" => Op::Jmp,
                    op => panic!("unknown op {op:?}"),
                })
            } else {
                panic!("unknown op")
            }
            i += 1;
        }
    });

    // --------------------------------------------------------------------------

    // interpreter backend

    assert_eq!(tree.data, Data::Empty);
    let program: Vec<Op> = tree
        .children
        .into_iter()
        .map(|tree| {
            assert!(tree.children.is_empty());
            match tree.data {
                Data::Op(op) => op,
                data => panic!("expected op, found {data:?}"),
            }
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

#[derive(Clone, PartialEq)]
enum Op {
    Psh(i64),
    Add,
    Neg,
    Ltz,
    Lbl(i64),
    Jmp,
}

impl std::fmt::Debug for Op {
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
