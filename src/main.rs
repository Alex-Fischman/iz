use std::any::{Any, TypeId};
use std::collections::HashMap;

type Node = usize;
type Storage = HashMap<Node, Box<dyn Any>>;

struct Tree {
    id: Node,
    nodes: HashMap<Node, Vec<Node>>,
    storages: HashMap<std::any::TypeId, Storage>,
}

impl Tree {
    fn new() -> Tree {
        Tree {
            id: 0,
            nodes: HashMap::new(),
            storages: HashMap::new(),
        }
    }

    fn node(&mut self, parent: Option<Node>) -> Node {
        let node = self.id;
        self.id += 1;
        self.nodes.insert(node, vec![]);
        if let Some(parent) = parent {
            self.nodes.get_mut(&parent).unwrap().push(node);
        }
        node
    }

    fn children(&mut self, parent: Node) -> &Vec<Node> {
        self.nodes.get(&parent).unwrap()
    }

    fn children_mut(&mut self, parent: Node) -> &mut Vec<Node> {
        self.nodes.get_mut(&parent).unwrap()
    }

    fn new_storage<Data: Any>(&mut self) {
        let old = self
            .storages
            .insert(std::any::TypeId::of::<Data>(), HashMap::new());
        assert!(old.is_none())
    }

    fn insert<Data: Any>(&mut self, node: Node, data: Data) -> Option<Data> {
        let storage = self.storages.get_mut(&TypeId::of::<Data>()).unwrap();
        Some(
            *storage
                .insert(node, Box::new(data))?
                .downcast::<Data>()
                .unwrap(),
        )
    }

    fn has<Data: Any>(&mut self, node: Node) -> bool {
        let storage = self.storages.get_mut(&TypeId::of::<Data>()).unwrap();
        storage.get(&node).is_some()
    }

    fn get_mut<Data: Any>(&mut self, node: Node) -> &mut Data {
        let storage = self.storages.get_mut(&TypeId::of::<Data>()).unwrap();
        storage.get_mut(&node).unwrap().downcast_mut().unwrap()
    }

    fn remove<Data: Any>(&mut self, node: Node) -> Data {
        let storage = self.storages.get_mut(&TypeId::of::<Data>()).unwrap();
        *storage.remove(&node).unwrap().downcast::<Data>().unwrap()
    }

    fn postorder<F: FnMut(&mut Tree, Node)>(&mut self, mut f: F) {
        postorder(self, 0, &mut f);
        fn postorder<F: FnMut(&mut Tree, Node)>(tree: &mut Tree, node: Node, f: &mut F) {
            for child in tree.children(node).clone() {
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
    let root = tree.node(None);
    assert_eq!(root, 0);
    tree.new_storage::<char>();
    for c in text.chars() {
        let node = tree.node(Some(0));
        tree.insert::<char>(node, c);
    }

    // compiler
    let passes = [
        remove_comments,
        group_characters,
        remove_whitespace,
        group_brackets,
        group_operators,
        unroll_operators,
        substitute_macros,
        unroll_brackets,
        integer_literals,
    ];

    for pass in passes {
        pass(&mut tree);
    }

    // backend
    let mut labels = HashMap::new();
    let mut children = tree.children(0).clone();
    let mut i = 0;
    while i < children.len() {
        if tree.has::<String>(children[i]) && tree.get_mut::<String>(children[i]) == ":" {
            let mut cs = tree.children_mut(children[i]).clone();
            let old = labels.insert(tree.remove::<String>(cs.remove(0)), i as i64);
            assert_eq!(old, None);
            children.remove(i);
            continue;
        }
        i += 1;
    }

    let code: Vec<Op> = children
        .into_iter()
        .map(|node| {
            if tree.has::<i64>(node) {
                Op::Push(tree.remove(node))
            } else if tree.has::<String>(node) {
                let mut cs = tree.children_mut(node).clone();
                match tree.remove::<String>(node).as_str() {
                    "~" => Op::Move(tree.remove(cs.remove(0))),
                    "$" => Op::Copy(tree.remove(cs.remove(0))),
                    "add" => Op::Add,
                    "neg" => Op::Neg,
                    "ltz" => Op::Ltz,
                    "?" => Op::Jz(*labels.get(&tree.remove::<String>(cs.remove(0))).unwrap()),
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
    let mut children = tree.children(0).clone();
    let mut i = 0;
    while i < children.len() {
        if *tree.get_mut::<char>(children[i]) == '#' {
            let mut j = i;
            while j < children.len() && *tree.get_mut::<char>(children[j]) != '\n' {
                j += 1;
            }
            children.drain(i..j);
        } else {
            i += 1;
        }
    }
    *tree.children_mut(0) = children;
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

    tree.new_storage::<String>();
    let mut children = tree.children(0).clone();
    let mut i = 0;
    while i < children.len() {
        let c = tree.remove::<char>(children[i]);
        if i == 0 {
            tree.insert(children[i], c.to_string());
            i += 1;
        } else {
            let s = tree.get_mut::<String>(children[i - 1]);
            if char_type(c) == char_type(s.chars().next().unwrap()) {
                s.push(c);
                children.remove(i);
            } else {
                tree.insert(children[i], c.to_string());
                i += 1;
            }
        }
    }
    *tree.children_mut(0) = children;
}

fn remove_whitespace(tree: &mut Tree) {
    let mut children = tree.children(0).clone();
    let mut i = 0;
    while i < children.len() {
        let s = tree.get_mut::<String>(children[i]);
        if s.chars().next().unwrap().is_whitespace() {
            children.remove(i);
        } else {
            i += 1;
        }
    }
    *tree.children_mut(0) = children;
}

fn group_brackets(tree: &mut Tree) {
    bracket_matcher(tree, &mut 0, None);
    fn bracket_matcher(tree: &mut Tree, i: &mut usize, target: Option<&str>) {
        while *i < tree.children(0).len() {
            *i += 1;
            let n = tree.children(0)[*i - 1];
            let s = tree.get_mut::<String>(n).clone();
            let mut handle_open_bracket = |t| {
                let start = *i;
                bracket_matcher(tree, i, Some(t));
                let mut children: Vec<Node> = tree.children_mut(0).drain(start..*i).collect();
                children.pop();
                *i = start;
                let n = tree.children(0)[*i - 1];
                assert!(tree.children(n).is_empty());
                *tree.children_mut(n) = children;
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
        let mut children = tree.children(node).clone();
        for (ops, right) in OPERATORS {
            let mut i = if *right {
                children.len().wrapping_sub(1)
            } else {
                0
            };
            while let Some(child) = children.get(i) {
                let s = tree.get_mut::<String>(*child).clone();
                if let Some(op) = ops.iter().find(|op| op.0 == s) {
                    if i < op.2 || i + op.3 >= children.len() {
                        panic!("not enough operator arguments for {s}");
                    }
                    let node = children.remove(i);
                    let cs: Vec<Node> = children.drain(i - op.2..i + op.3).collect();
                    i -= op.2;
                    children.insert(i, node);
                    *tree.children_mut(node) = cs;
                    tree.insert::<String>(node, s);
                }
                i = if *right { i.wrapping_sub(1) } else { i + 1 }
            }
        }
        *tree.children_mut(node) = children;
    });
}

fn unroll_operators(tree: &mut Tree) {
    tree.postorder(|tree, node| {
        let mut children = tree.children(node).clone();
        let mut i = 0;
        while i < children.len() {
            let s = tree.get_mut::<String>(children[i]);
            if let Some(op) = OPERATORS
                .iter()
                .find_map(|(ops, _)| ops.iter().find(|op| op.0 == s))
            {
                if op.4 {
                    let cs: Vec<Node> = tree.children_mut(children[i]).drain(..).collect();
                    let l = cs.len();
                    children.splice(i..i, cs);
                    i += l;
                }
                tree.insert::<String>(children[i], op.1.to_owned());
            }
            i += 1;
        }
        *tree.children_mut(node) = children;
    });
}

fn unroll_brackets(tree: &mut Tree) {
    tree.postorder(|tree, node| {
        let mut children = tree.children(node).clone();
        let mut i = 0;
        while i < children.len() {
            if tree.get_mut::<String>(children[i]) == "(" {
                let cs: Vec<Node> = tree.children_mut(children[i]).drain(..).collect();
                children.splice(i..=i, cs);
            }
            i += 1;
        }
        *tree.children_mut(node) = children;
    });
}

fn integer_literals(tree: &mut Tree) {
    tree.new_storage::<i64>();
    tree.postorder(|tree, node| {
        if tree.has::<String>(node) {
            if let Ok(int) = tree.get_mut::<String>(node).parse::<i64>() {
                tree.remove::<String>(node);
                tree.insert::<i64>(node, int);
            }
        }
    });
}

fn substitute_macros(tree: &mut Tree) {
    let mut macros: HashMap<String, Vec<Node>> = HashMap::new();
    tree.postorder(|tree, node| {
        let mut children = tree.children(node).clone();
        let mut i = 0;
        while i < children.len() {
            if tree.has::<String>(children[i]) && tree.get_mut::<String>(children[i]) == "macro" {
                let key = tree.children(children[i])[0];
                let value = tree.children(children[i])[1];
                macros.insert(
                    tree.get_mut::<String>(key).clone(),
                    tree.children(value).clone(),
                );
                children.remove(i);
            } else {
                i += 1;
            }
        }
        *tree.children_mut(node) = children;
    });
    tree.postorder(|tree, node| {
        if tree.has::<String>(node) {
            let s = tree.get_mut::<String>(node);
            if let Some(children) = macros.get(s) {
                *s = "(".to_owned();
                *tree.children_mut(node) = children.clone();
            }
        }
    });
}
