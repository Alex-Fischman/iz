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

impl Tree {
    fn postorder<F: Fn(&mut Tree) + Clone>(&mut self, f: F) {
        for tree in &mut self.children {
            tree.postorder(f.clone());
        }
        f(self);
    }
}

#[derive(Clone, PartialEq)]
enum Op {
    Psh(i64),
    Dup,
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
            Op::Dup => write!(f, "dup"),
            Op::Add => write!(f, "add"),
            Op::Neg => write!(f, "neg"),
            Op::Ltz => write!(f, "ltz"),
            Op::Lbl(i) => write!(f, "lbl {i}"),
            Op::Jmp => write!(f, "jmp"),
        }
    }
}

fn main() {
    // file reading frontend
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

    let passes = [
        // tokenizer
        remove_comments,
        group_identifiers,
        remove_whitespace,
        integer_literals,
        // parser
        group_brackets,
        group_operators,
        unroll_operators,
        // analysis
        // transformation
        convert_to_ops,
    ];

    for pass in passes {
        pass(&mut tree)
    }

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

    // --------------------------------------------------------------------------

    // interpreter backend
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
            Op::Dup => {
                let a = stack.pop().unwrap();
                stack.push(a);
                stack.push(a);
            }
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

fn remove_comments(tree: &mut Tree) {
    let mut i = 0;
    while i < tree.children.len() {
        match tree.children[i].data {
            Data::Char('#') => {
                let mut j = i;
                while j < tree.children.len() && !matches!(tree.children[j].data, Data::Char('\n'))
                {
                    j += 1;
                }
                tree.children.drain(i..j);
            }
            _ => i += 1,
        }
    }
}

fn group_identifiers(tree: &mut Tree) {
    let mut i = 0;
    while i < tree.children.len() {
        match tree.children[i].data {
            Data::Char(c) if !c.is_whitespace() => {
                if i == 0 {
                    tree.children[i].data = Data::String(c.to_string());
                } else if let Data::String(s) = &mut tree.children[i - 1].data {
                    s.push(c);
                    tree.children.remove(i);
                } else {
                    tree.children[i].data = Data::String(c.to_string());
                }
            }
            _ => i += 1,
        }
    }
}

fn remove_whitespace(tree: &mut Tree) {
    let mut i = 0;
    while i < tree.children.len() {
        match tree.children[i].data {
            Data::Char(c) if c.is_whitespace() => {
                tree.children.remove(i);
            }
            _ => i += 1,
        }
    }
}

fn integer_literals(tree: &mut Tree) {
    let mut i = 0;
    while i < tree.children.len() {
        match &tree.children[i].data {
            Data::String(s) => match s.parse::<i64>() {
                Ok(int) => tree.children[i].data = Data::Int(int),
                _ => i += 1,
            },
            _ => i += 1,
        }
    }
}

fn group_brackets(tree: &mut Tree) {
    bracket_matcher(&mut tree.children, &mut 0, None);
    fn bracket_matcher(trees: &mut Vec<Tree>, i: &mut usize, target: Option<&str>) {
        let brackets = std::collections::HashMap::from([
            ("(".to_owned(), ")".to_owned()),
            ("{".to_owned(), "}".to_owned()),
            ("[".to_owned(), "]".to_owned()),
        ]);
        while *i < trees.len() {
            *i += 1;
            match &trees[*i - 1].data {
                Data::String(s) if s == ")" || s == "}" || s == "]" => match (s, target) {
                    (s, Some(t)) if s == t => return,
                    (s, _) => panic!("extra {s}"),
                },
                Data::String(s) if brackets.contains_key(s) => {
                    let start = *i;
                    bracket_matcher(trees, i, Some(brackets.get(s).unwrap()));
                    let mut children: Vec<Tree> = trees.drain(start..*i).collect();
                    children.pop();
                    *i = start;
                    assert!(trees[*i - 1].children.is_empty());
                    trees[*i - 1].children = children;
                }
                _ => {}
            }
            if let Some(s) = target {
                panic!("missing {s}");
            }
        }
    }
}

//                   name     func     left   right
type Operator<'a> = (&'a str, &'a str, usize, usize);
//                               right associativity
const OPERATORS: &[(&[Operator], bool)] = &[
    (&[("@", "nop", 1, 1)], false),
    (&[("-", "neg", 0, 1), ("not", "_not_", 0, 1)], true),
    (&[("*", "mul", 1, 1)], false),
    (&[("+", "add", 1, 1)], false),
    (
        &[
            ("==", "eq", 1, 1),
            ("!=", "ne", 1, 1),
            ("<", "lt", 1, 1),
            (">", "gt", 1, 1),
            ("<=", "le", 1, 1),
            (">=", "ge", 1, 1),
        ],
        false,
    ),
    (&[("and", "_and_", 1, 1), ("or", "_or_", 1, 1)], true),
    (&[("=", "=", 1, 1), (":=", ":=", 1, 1)], true),
    (&[("if", "_if_", 0, 2), ("while", "_while_", 0, 2)], true),
    (&[("else", "_else_", 1, 1)], true),
];

fn group_operators(tree: &mut Tree) {
    tree.postorder(|tree| {
        for (ops, right) in OPERATORS {
            let mut i = if *right {
                tree.children.len().wrapping_sub(1)
            } else {
                0
            };
            while let Some(child) = tree.children.get(i) {
                if let Data::String(s) = &child.data {
                    let s = s.clone();
                    if let Some(op) = ops.iter().find(|op| op.0 == s) {
                        if i < op.2 || i + op.3 >= tree.children.len() {
                            panic!("not enough operator arguments for {s}");
                        }
                        tree.children.remove(i);
                        let children: Vec<Tree> = tree.children.drain(i - op.2..i + op.3).collect();
                        i -= op.2;
                        tree.children.insert(
                            i,
                            Tree {
                                data: Data::String(s),
                                children,
                            },
                        );
                    }
                }
                i = if *right { i.wrapping_sub(1) } else { i + 1 }
            }
        }
    });
}

fn unroll_operators(tree: &mut Tree) {
    tree.postorder(|tree| {
        let mut i = 0;
        while i < tree.children.len() {
            if let Data::String(s) = &tree.children[i].data {
                if let Some(op) = OPERATORS
                    .iter()
                    .find_map(|(ops, _)| ops.iter().find(|op| op.0 == s))
                {
                    let mut children: Vec<Tree> = tree.children[i].children.drain(..).collect();
                    children.reverse();
                    let l = children.len();
                    children.push(Tree {
                        data: Data::String(op.1.to_owned()),
                        children: vec![],
                    });
                    tree.children.splice(i..=i, children);
                    i += l;
                }
            }
            i += 1;
        }
    });
}

fn convert_to_ops(tree: &mut Tree) {
    let ops: std::collections::HashMap<&str, &[Op]> =
        std::collections::HashMap::from([("not", &[Op::Neg, Op::Psh(1), Op::Add][..])]);
    tree.postorder(|tree| {
        let mut i = 0;
        while i < tree.children.len() {
            if let Data::Int(int) = tree.children[i].data {
                tree.children[i].data = Data::Op(Op::Psh(int))
            } else if let Data::String(s) = &tree.children[i].data {
                match s.as_str() {
                    "dup" => tree.children[i].data = Data::Op(Op::Dup),
                    "add" => tree.children[i].data = Data::Op(Op::Add),
                    "neg" => tree.children[i].data = Data::Op(Op::Neg),
                    "ltz" => tree.children[i].data = Data::Op(Op::Ltz),
                    "lbl" => {
                        tree.children.splice(
                            i..=i + 1,
                            [match &tree.children[i + 1].data {
                                Data::Int(int) => Tree {
                                    data: Data::Op(Op::Lbl(*int)),
                                    children: vec![],
                                },
                                data => panic!("expected int, found {data:?}"),
                            }],
                        );
                    }
                    "jmp" => tree.children[i].data = Data::Op(Op::Jmp),
                    op => match ops.get(op) {
                        Some(ops) => {
                            tree.children.splice(
                                i..=i,
                                ops.iter().map(|op| Tree {
                                    data: Data::Op(op.clone()),
                                    children: vec![],
                                }),
                            );
                            i += ops.len() - 1;
                        }
                        None => panic!("unknown op {op:?}"),
                    },
                }
            } else {
                panic!("unknown op")
            }
            i += 1;
        }
    });
}
