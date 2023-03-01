use std::any::Any;
use std::collections::HashMap;

type Node = usize;

struct Tree {
    id: Node,
    children: Vec<Vec<Node>>,
    storages: HashMap<std::any::TypeId, Box<dyn Any>>,
}

impl Tree {
    fn new() -> Tree {
        Tree {
            id: 0,
            children: vec![],
            storages: HashMap::new(),
        }
    }

    fn new_node(&mut self, parent: Option<Node>) -> Node {
        let node = self.id;
        self.id += 1;
        self.children.push(vec![]);
        if let Some(parent) = parent {
            self.children[parent].push(node);
        }
        node
    }

    fn insert_storage<Data: Any>(&mut self) {
        let old = self.storages.insert(
            std::any::TypeId::of::<Data>(),
            Box::<HashMap<Node, Data>>::default(),
        );
        assert!(old.is_none())
    }

    fn remove_storage<Data: Any>(&mut self) {
        let old = self.storages.remove(&std::any::TypeId::of::<Data>());
        let old = old.unwrap().downcast::<HashMap<Node, Data>>().unwrap();
        assert!(old.is_empty())
    }

    fn get_storage<Data: Any>(&self) -> &HashMap<Node, Data> {
        self.storages
            .get(&std::any::TypeId::of::<Data>())
            .unwrap()
            .downcast_ref()
            .unwrap()
    }

    fn get_storage_mut<Data: Any>(&mut self) -> &mut HashMap<Node, Data> {
        self.storages
            .get_mut(&std::any::TypeId::of::<Data>())
            .unwrap()
            .downcast_mut()
            .unwrap()
    }

    fn insert<Data: Any>(&mut self, node: Node, data: Data) -> Option<Data> {
        self.get_storage_mut::<Data>().insert(node, data)
    }

    fn remove<Data: Any>(&mut self, node: Node) -> Option<Data> {
        self.get_storage_mut::<Data>().remove(&node)
    }

    fn get<Data: Any>(&self, node: Node) -> Option<&Data> {
        self.get_storage::<Data>().get(&node)
    }

    fn get_mut<Data: Any>(&mut self, node: Node) -> Option<&mut Data> {
        self.get_storage_mut::<Data>().get_mut(&node)
    }

    fn postorder<F: FnMut(&mut Tree, Node)>(&mut self, mut f: F) {
        postorder(self, 0, &mut f);
        fn postorder<F: FnMut(&mut Tree, Node)>(tree: &mut Tree, node: Node, f: &mut F) {
            for child in tree.children[node].clone() {
                postorder(tree, child, f)
            }
            f(tree, node)
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
    let text = std::fs::read_to_string(file).expect("could not read file");
    let mut tree = Tree::new();
    let root = tree.new_node(None);
    assert_eq!(root, 0);
    tree.insert_storage::<char>();
    for c in text.chars() {
        let node = tree.new_node(Some(0));
        tree.insert::<char>(node, c);
    }

    // compiler
    let passes = [
        remove_comments,
        chars_to_strings,
        remove_whitespace,
        group_brackets,
        group_operators,
        unroll_operators,
        substitute_macros,
        unroll_brackets,
        integer_literals,
        substitute_labels,
    ];
    for pass in passes {
        pass(&mut tree)
    }

    // backend
    let code: Vec<Op> = tree.children[0]
        .iter()
        .cloned()
        .map(|node| {
            if let Some(int) = tree.get::<i64>(node) {
                Op::Push(*int)
            } else if let Some(s) = tree.get::<String>(node) {
                match s.as_str() {
                    "~" => Op::Move(*tree.get(tree.children[node][0]).unwrap()),
                    "$" => Op::Copy(*tree.get(tree.children[node][0]).unwrap()),
                    "add" => Op::Add,
                    "neg" => Op::Neg,
                    "ltz" => Op::Ltz,
                    "?" => Op::Jz(*tree.get(tree.children[node][0]).unwrap()),
                    s => panic!("expected an op, found {s}"),
                }
            } else {
                panic!("expected an int or a string, found neither")
            }
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
    while i < tree.children[0].len() {
        if tree.get::<char>(tree.children[0][i]) == Some(&'#') {
            let mut j = i;
            while j < tree.children[0].len()
                && tree.remove::<char>(tree.children[0][j]) != Some('\n')
            {
                j += 1;
            }
            tree.insert(tree.children[0][j], '\n');
            tree.children[0].drain(i..j);
        } else {
            i += 1;
        }
    }
}

fn chars_to_strings(tree: &mut Tree) {
    fn is_bracket(c: char) -> bool {
        matches!(c, '(' | ')' | '{' | '}' | '[' | ']')
    }

    fn is_identifier(c: char) -> bool {
        matches!(c, '-' | '_' | 'a'..='z' | 'A'..='Z' | '0'..='9')
    }

    fn char_type(c: char) -> usize {
        match c {
            _ if is_identifier(c) => 0,
            _ if c.is_whitespace() => 1,
            _ if is_bracket(c) => 2,
            _ => 3, // operators
        }
    }

    tree.insert_storage::<String>();

    let mut i = 0;
    while i < tree.children[0].len() {
        let c = tree.remove::<char>(tree.children[0][i]).unwrap();
        if i == 0 {
            tree.insert(tree.children[0][i], c.to_string());
            i += 1;
        } else {
            let s = tree.get_mut::<String>(tree.children[0][i - 1]).unwrap();
            if !is_bracket(c) && char_type(c) == char_type(s.chars().next().unwrap()) {
                s.push(c);
                tree.children[0].remove(i);
            } else {
                tree.insert(tree.children[0][i], c.to_string());
                i += 1;
            }
        }
    }

    tree.remove_storage::<char>();
}

fn remove_whitespace(tree: &mut Tree) {
    let mut i = 0;
    while i < tree.children[0].len() {
        let s = tree.get::<String>(tree.children[0][i]).unwrap();
        if s.chars().next().unwrap().is_whitespace() {
            tree.children[0].remove(i);
        } else {
            i += 1;
        }
    }
}

fn group_brackets(tree: &mut Tree) {
    bracket_matcher(tree, &mut 0, None);
    fn bracket_matcher(tree: &mut Tree, i: &mut usize, target: Option<&str>) {
        while *i < tree.children[0].len() {
            let child = tree.children[0][*i];
            *i += 1;
            let s = tree.get::<String>(child).unwrap().clone();
            let mut handle_open_bracket = |t| {
                let start = *i;
                bracket_matcher(tree, i, Some(t));
                let mut cs: Vec<Node> = tree.children[0].drain(start..*i).collect();
                cs.pop();
                *i = start;
                let n = tree.children[0][*i - 1];
                assert!(tree.children[n].is_empty());
                tree.children[child] = cs;
            };
            match s.as_str() {
                "(" => handle_open_bracket(")"),
                "{" => handle_open_bracket("}"),
                "[" => handle_open_bracket("]"),
                ")" | "}" | "]" => match target {
                    Some(t) if s == t => return,
                    _ => panic!("extra {s}"),
                },
                _ => {}
            }
        }
        if let Some(s) = target {
            panic!("missing {s}");
        }
    }
}

fn unroll_brackets(tree: &mut Tree) {
    tree.postorder(|tree, node| {
        let mut i = 0;
        while i < tree.children[node].len() {
            let child = tree.children[node][i];
            if tree.get::<String>(child) == Some(&"(".to_owned()) {
                let cs: Vec<Node> = tree.children[child].drain(..).collect();
                tree.children[node].splice(i..=i, cs);
            }
            i += 1;
        }
    });
}

//                   name     func     left   right  unroll
type Operator<'a> = (&'a str, &'a str, usize, usize, bool);
//                               right associativity
const OPERATORS: &[(&[Operator], bool)] = &[
    (&[(":", ":", 1, 0, false)], false),
    (&[("?", "?", 1, 0, false)], false),
    (&[("~", "~", 0, 1, false)], true),
    (&[("$", "$", 0, 1, false)], true),
    (&[("-", "neg", 0, 1, true), ("!", "not", 0, 1, true)], true),
    (&[("+", "add", 1, 1, true)], false),
    (&[("macro", "macro", 0, 2, false)], true),
];

fn group_operators(tree: &mut Tree) {
    tree.postorder(|tree, node| {
        for (ops, right) in OPERATORS {
            let mut i = if *right {
                tree.children[node].len().wrapping_sub(1)
            } else {
                0
            };
            while let Some(child) = tree.children[node].get(i).copied() {
                let s = tree.get::<String>(child).unwrap().clone();
                if let Some(op) = ops.iter().find(|op| op.0 == s) {
                    if i < op.2 || i + op.3 >= tree.children[node].len() {
                        panic!("not enough operator arguments for {s}");
                    }
                    let mut cs: Vec<Node> = tree.children[node].drain(i + 1..=i + op.3).collect();
                    cs.extend(tree.children[node].drain(i - op.2..i));
                    i -= op.2;
                    tree.children[child] = cs;
                }
                i = if *right { i.wrapping_sub(1) } else { i + 1 }
            }
        }
    });
}

fn unroll_operators(tree: &mut Tree) {
    tree.postorder(|tree, node| {
        let mut i = 0;
        while i < tree.children[node].len() {
            let child = tree.children[node][i];
            let s = tree.get::<String>(child).unwrap();
            if let Some(op) = OPERATORS
                .iter()
                .find_map(|(ops, _)| ops.iter().find(|op| op.0 == s))
            {
                if op.4 {
                    let cs: Vec<Node> = tree.children[child].drain(..).collect();
                    let l = cs.len();
                    tree.children[node].splice(i..i, cs);
                    i += l;
                }
                tree.insert::<String>(child, op.1.to_owned());
            }
            i += 1;
        }
    });
}

fn integer_literals(tree: &mut Tree) {
    tree.insert_storage::<i64>();
    tree.postorder(|tree, node| {
        if let Some(s) = tree.get::<String>(node) {
            if let Ok(int) = s.parse::<i64>() {
                tree.remove::<String>(node);
                tree.insert::<i64>(node, int);
            }
        }
    });
}

fn substitute_macros(tree: &mut Tree) {
    let mut macros: HashMap<String, Node> = HashMap::new();
    tree.postorder(|tree, node| {
        let mut i = 0;
        while i < tree.children[node].len() {
            let child = tree.children[node][i];
            if tree.get::<String>(child) == Some(&"macro".to_owned()) {
                let key = tree.children[child][0];
                let value = tree.children[child][1];
                let old = macros.insert(tree.get::<String>(key).unwrap().clone(), value);
                assert_eq!(old, None);
                tree.children[node].remove(i);
            } else {
                i += 1;
            }
        }
    });
    tree.postorder(|tree, node| {
        let mut i = 0;
        while i < tree.children[node].len() {
            if let Some(s) = tree.get_mut::<String>(tree.children[node][i]) {
                if let Some(replacement) = macros.get(s) {
                    tree.children[node][i] = *replacement;
                }
            }
            i += 1;
        }
    });
}

fn substitute_labels(tree: &mut Tree) {
    let mut labels: HashMap<String, i64> = HashMap::new();
    tree.postorder(|tree, node| {
        let mut i = 0;
        while i < tree.children[node].len() {
            let child = tree.children[node][i];
            if tree.get::<String>(child) == Some(&":".to_owned()) {
                let key = tree.children[child].remove(0);
                let old = labels.insert(tree.remove::<String>(key).unwrap(), i as i64);
                assert_eq!(old, None);
                tree.children[node].remove(i);
            } else {
                i += 1;
            }
        }
    });
    tree.postorder(|tree, node| {
        let mut i = 0;
        while i < tree.children[node].len() {
            let child = tree.children[node][i];
            if let Some(s) = tree.get_mut::<String>(child) {
                if let Some(int) = labels.get(s) {
                    tree.insert::<i64>(child, *int);
                    tree.remove::<String>(child);
                }
            }
            i += 1;
        }
    });
}
