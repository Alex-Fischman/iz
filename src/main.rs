#[derive(Debug, PartialEq)]
struct Tree {
    data: Data,
    children: Vec<Tree>,
}

impl Tree {
    fn postorder<F: Fn(&mut Tree) + Clone>(&mut self, f: F) {
        for tree in &mut self.children {
            tree.postorder(f.clone());
        }
        f(self);
    }
}

#[derive(Debug, PartialEq)]
enum Data {
    Empty,
    Char(char),
    String(String),
    Int(i64),
    Op(Op),
}

impl Data {
    fn as_char(&self) -> char {
        match self {
            Data::Char(c) => *c,
            _ => panic!("expected char, found {self:?}"),
        }
    }

    fn as_string(&mut self) -> &mut String {
        match self {
            Data::String(s) => s,
            _ => panic!("expected string, found {self:?}"),
        }
    }

    fn first_char(&mut self) -> char {
        self.as_string().chars().next().unwrap()
    }

    fn as_int(&self) -> i64 {
        match self {
            Data::Int(i) => *i,
            _ => panic!("expected int, found {self:?}"),
        }
    }

    fn as_op(&self) -> &Op {
        match self {
            Data::Op(o) => o,
            _ => panic!("expected op, found {self:?}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Op {
    Push(i64),
    Copy(usize),
    Pull(usize),
    Add,
    Neg,
    Ltz,
    Mark(String),
    Cond(String),
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
        group_characters,
        remove_whitespace,
        // parser
        group_brackets,
        group_operators,
        unroll_operators,
        integer_literals,
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
            tree.data.as_op().clone()
        })
        .collect();

    // --------------------------------------------------------------------------

    // interpreter backend
    let mut marks = std::collections::HashMap::new();
    let mut i = 0;
    while i < program.len() {
        if let Op::Mark(mark) = &program[i] {
            let old = marks.insert(mark, i);
            assert!(old.is_none());
        }
        i += 1;
    }

    let mut stack: Vec<i64> = vec![];
    let mut pc = 0;
    while pc < program.len() {
        println!("{stack:?}");
        match &program[pc] {
            Op::Push(i) => stack.push(*i),
            Op::Copy(i) => {
                let a = stack.get(stack.len() - 1 - i).unwrap();
                stack.push(*a);
            }
            Op::Pull(i) => {
                stack.remove(stack.len() - 1 - i);
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
            Op::Mark(_) => {}
            Op::Cond(s) => {
                let a = stack.pop().unwrap();
                if a == 0 {
                    pc = *marks.get(&s).unwrap();
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
        if tree.children[i].data.as_char() == '#' {
            let mut j = i;
            while j < tree.children.len() && tree.children[j].data.as_char() != '\n' {
                j += 1;
            }
            tree.children.drain(i..j);
        } else {
            i += 1;
        }
    }
}

fn char_type(c: char) -> usize {
    match c {
        '_' | 'a'..='z' | 'A'..='Z' | '0'..='9' => 0,
        _ if c.is_whitespace() => 1,
        _ => 2,
    }
}

fn group_characters(tree: &mut Tree) {
    let mut i = 0;
    while i < tree.children.len() {
        let c = tree.children[i].data.as_char();
        if i != 0 && char_type(c) == char_type(tree.children[i - 1].data.first_char()) {
            tree.children[i - 1].data.as_string().push(c);
            tree.children.remove(i);
        } else {
            tree.children[i].data = Data::String(c.to_string());
            i += 1;
        }
    }
}

fn remove_whitespace(tree: &mut Tree) {
    let mut i = 0;
    while i < tree.children.len() {
        if tree.children[i].data.first_char().is_whitespace() {
            tree.children.remove(i);
        } else {
            i += 1;
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
            match trees[*i - 1].data.as_string().clone().as_str() {
                s @ (")" | "}" | "]") => match (s, target) {
                    (s, Some(t)) if s == t => return,
                    (s, _) => panic!("extra {s}"),
                },
                s if brackets.contains_key(s) => {
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

//                   name     func     left   right  unroll
type Operator<'a> = (&'a str, &'a str, usize, usize, bool);
//                               right associativity
const OPERATORS: &[(&[Operator], bool)] = &[
    (
        &[
            ("copy", "copy", 0, 1, false),
            ("pull", "pull", 0, 1, false),
            (":", "mark", 1, 0, false),
            ("cond", "cond", 0, 1, false),
        ],
        true,
    ),
    (
        &[("-", "neg", 0, 1, true), ("not", "_not_", 0, 1, true)],
        true,
    ),
    (&[("*", "mul", 1, 1, true)], false),
    (&[("+", "add", 1, 1, true)], false),
    (
        &[("and", "_and_", 1, 1, true), ("or", "_or_", 1, 1, true)],
        true,
    ),
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
                    if op.4 {
                        let mut children: Vec<Tree> = tree.children[i].children.drain(..).collect();
                        children.reverse();
                        let l = children.len();
                        children.push(Tree {
                            data: Data::String(op.1.to_owned()),
                            children: vec![],
                        });
                        tree.children.splice(i..=i, children);
                        i += l;
                    } else {
                        tree.children[i].data = Data::String(op.1.to_owned());
                    }
                }
            }
            i += 1;
        }
    });
}

fn integer_literals(tree: &mut Tree) {
    tree.postorder(|tree| match &tree.data {
        Data::Empty => {}
        Data::String(s) => {
            if let Ok(int) = s.parse::<i64>() {
                tree.data = Data::Int(int)
            }
        }
        data => panic!("expected empty or string, found {data:?}"),
    });
}

fn convert_to_ops(tree: &mut Tree) {
    let mut i = 0;
    while i < tree.children.len() {
        tree.children[i].data = Data::Op(match &tree.children[i].data {
            Data::Int(int) => Op::Push(*int),
            Data::String(s) => match s.as_str() {
                "copy" => Op::Copy(tree.children[i].children.remove(0).data.as_int() as usize),
                "pull" => Op::Pull(tree.children[i].children.remove(0).data.as_int() as usize),
                "add" => Op::Add,
                "neg" => Op::Neg,
                "ltz" => Op::Ltz,
                "mark" => Op::Mark(tree.children[i].children.remove(0).data.as_string().clone()),
                "cond" => Op::Cond(tree.children[i].children.remove(0).data.as_string().clone()),
                op => panic!("unknown op {op}"),
            },
            data => panic!("expected an int or string, found {data:?}"),
        });
        i += 1;
    }
}
