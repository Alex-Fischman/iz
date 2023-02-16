#[derive(Debug, PartialEq)]
struct Tree {
    data: Data,
    children: Vec<Tree>,
}

impl Tree {
    fn new(data: Data, children: Vec<Tree>) -> Tree {
        Tree { data, children }
    }

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

#[derive(Clone, Debug, PartialEq)]
enum Op {
    Push(i64),
    Move(i64),
    Copy(i64),
    Add,
    Neg,
    Ltz,
    Jz(i64),
}

struct Memory(Vec<i64>);

impl std::ops::Index<i64> for Memory {
    type Output = i64;
    fn index(&self, i: i64) -> &i64 {
        &self.0[i as usize]
    }
}

impl std::ops::IndexMut<i64> for Memory {
    fn index_mut(&mut self, i: i64) -> &mut i64 {
        let i = i as usize;
        if self.0.len() <= i {
            self.0.resize(i + 1, 0);
        }
        &mut self.0[i]
    }
}

fn main() {
    // frontend
    let args: Vec<String> = std::env::args().collect();
    let file = args.get(1).expect("no file passed");
    let tokens = std::fs::read_to_string(file)
        .expect("could not read file")
        .chars()
        .map(|c| Tree::new(Data::Char(c), vec![]))
        .collect();
    let mut tree = Tree::new(Data::Empty, tokens);

    // compiler
    let passes = [
        // tokenizer
        remove_comments,
        group_characters,
        remove_whitespace,
        // parser
        group_brackets,
        group_operators,
        unroll_operators,
        unroll_brackets,
        integer_literals,
        // analysis

        // transformation
        convert_to_ops,
    ];

    for pass in passes {
        pass(&mut tree)
    }

    // backend
    assert_eq!(tree.data, Data::Empty);
    let code: Vec<Op> = tree
        .children
        .into_iter()
        .map(|tree| {
            assert!(tree.children.is_empty());
            tree.data.as_op().clone()
        })
        .collect();

    let mut pc = 0;
    let mut data = Memory(vec![]);
    let mut sp = -1;

    while (pc as usize) < code.len() {
        print!("{:?}\t", code[pc as usize]);

        match &code[pc as usize] {
            Op::Push(i) => {
                data[sp + 1] = *i;
                sp += 1;
            }
            Op::Move(i) => sp -= i,
            Op::Copy(i) => {
                data[sp + 1] = data[sp - i];
                sp += 1;
            }
            Op::Add => {
                data[sp - 1] += data[sp];
                sp -= 1;
            }
            Op::Neg => data[sp] = -data[sp],
            Op::Ltz => data[sp] = (data[sp] < 0) as i64,
            Op::Jz(i) => {
                if data[sp] == 0 {
                    if *i < 0 {
                        todo!("syscalls?")
                    } else {
                        pc = i - 1;
                    }
                }
                sp -= 1;
            }
        }

        match sp {
            -1 => println!(),
            sp => println!("{:?}", &data.0[0..=sp as usize]),
        }

        pc += 1;
    }
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

fn group_characters(tree: &mut Tree) {
    fn char_type(c: char) -> usize {
        match c {
            '-' | '_' | 'a'..='z' | 'A'..='Z' | '0'..='9' => 0,
            _ if c.is_whitespace() => 1,
            '(' | ')' | '{' | '}' | '[' | ']' => 2,
            _ => 3,
        }
    }

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

const BRACKETS: &[(&str, &str)] = &[("(", ")"), ("{", "}"), ("[", "]")];

fn group_brackets(tree: &mut Tree) {
    bracket_matcher(&mut tree.children, &mut 0, None);
    fn bracket_matcher(trees: &mut Vec<Tree>, i: &mut usize, target: Option<&str>) {
        while *i < trees.len() {
            *i += 1;
            let s = trees[*i - 1].data.as_string().as_str();
            match (s, BRACKETS.iter().find(|(b, _)| *b == s)) {
                (")" | "}" | "]", _) => match target {
                    Some(t) if s == t => return,
                    _ => panic!("extra {s}"),
                },
                (_, Some((_, t))) => {
                    let start = *i;
                    bracket_matcher(trees, i, Some(t));
                    let mut children: Vec<Tree> = trees.drain(start..*i).collect();
                    children.pop();
                    *i = start;
                    assert!(trees[*i - 1].children.is_empty());
                    trees[*i - 1].children = children;
                }
                _ => {}
            }
        }
        if let Some(s) = target {
            panic!("missing {s}");
        }
    }
}

//                   name     func     left   right  unroll
type Operator<'a> = (&'a str, &'a str, usize, usize, bool);
//                               right associativity
const OPERATORS: &[(&[Operator], bool)] = &[
    (&[(":", ":", 1, 0, false)], false),
    (&[("?", "?", 1, 0, false)], false),
    (&[("~", "~", 0, 1, false)], true),
    (&[("$", "$", 0, 1, false)], true),
    (&[("-", "neg", 0, 1, true)], true),
    (&[("+", "add", 1, 1, true)], false),
];

fn group_operators(tree: &mut Tree) {
    tree.postorder(|tree| {
        for (ops, right) in OPERATORS {
            let mut i = if *right {
                tree.children.len().wrapping_sub(1)
            } else {
                0
            };
            while let Some(child) = tree.children.get_mut(i) {
                let s = child.data.as_string().clone();
                if let Some(op) = ops.iter().find(|op| op.0 == s) {
                    if i < op.2 || i + op.3 >= tree.children.len() {
                        panic!("not enough operator arguments for {s}");
                    }
                    tree.children.remove(i);
                    let children: Vec<Tree> = tree.children.drain(i - op.2..i + op.3).collect();
                    i -= op.2;
                    tree.children
                        .insert(i, Tree::new(Data::String(s), children));
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
            let s = tree.children[i].data.as_string();
            if let Some(op) = OPERATORS
                .iter()
                .find_map(|(ops, _)| ops.iter().find(|op| op.0 == s))
            {
                if op.4 {
                    let children: Vec<Tree> = tree.children[i].children.drain(..).collect();
                    let l = children.len();
                    tree.children.splice(i..i, children);
                    i += l;
                }
                tree.children[i].data = Data::String(op.1.to_owned());
            }
            i += 1;
        }
    });
}

fn unroll_brackets(tree: &mut Tree) {
    tree.postorder(|tree| {
        let mut i = 0;
        while i < tree.children.len() {
            if tree.children[i].data.as_string() == "(" {
                let cs: Vec<Tree> = tree.children[i].children.drain(..).collect();
                tree.children.splice(i..=i, cs);
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
    let mut labels = std::collections::HashMap::new();
    let mut i = 0;
    while i < tree.children.len() {
        if tree.children[i].data == Data::String(":".to_string()) {
            let label = tree.children[i].children.remove(0).data.as_string().clone();
            let old = labels.insert(label, i as i64);
            assert_eq!(old, None);
            tree.children.remove(i);
            continue;
        }
        i += 1;
    }

    let mut i = 0;
    while i < tree.children.len() {
        tree.children[i].data = Data::Op(match &tree.children[i].data {
            Data::Int(int) => Op::Push(*int),
            Data::String(s) => match s.as_str() {
                "~" => Op::Move(tree.children[i].children.remove(0).data.as_int()),
                "$" => Op::Copy(tree.children[i].children.remove(0).data.as_int()),
                "add" => Op::Add,
                "neg" => Op::Neg,
                "ltz" => Op::Ltz,
                "?" => {
                    let label = tree.children[i].children.remove(0).data.as_string().clone();
                    Op::Jz(*labels.get(&label).unwrap())
                }
                _ => panic!("expected an op, found {s}"),
            },
            data => panic!("expected an int or string, found {data:?}"),
        });
        i += 1;
    }
}
