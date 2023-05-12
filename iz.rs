use std::{
    any::{Any, TypeId},
    collections::{HashMap, HashSet, VecDeque},
    io::Write,
    ops::Deref,
    process::{Command, Stdio},
    rc::Rc,
};

#[derive(Clone)]
struct Token {
    source: Rc<Source>,
    lo: usize,
    hi: usize,
}

#[derive(PartialEq)]
struct Source {
    name: String,
    text: String,
}

impl Deref for Token {
    type Target = str;
    fn deref(&self) -> &str {
        &self.source.text[self.lo..self.hi]
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut row = 1;
        let mut col = 1;
        for (i, c) in self.source.text.char_indices() {
            match (i, c) {
                (i, _) if i == self.lo => break,
                (_, '\n') => {
                    row += 1;
                    col = 1;
                }
                _ => col += 1,
            }
        }
        write!(f, "{:?} at {}:{}:{}", self.deref(), self.source.name, row, col)
    }
}

struct Tree {
    token: Token,
    children: Vec<Tree>,
    contents: HashMap<TypeId, Box<dyn Any>>,
}

#[allow(dead_code)]
impl Tree {
    fn new(token: Token) -> Tree {
        Tree { token, children: Vec::new(), contents: HashMap::new() }
    }

    fn insert<T: 'static>(&mut self, value: T) -> Option<T> {
        self.contents.insert(TypeId::of::<T>(), Box::new(value)).map(|any| *any.downcast().unwrap())
    }

    fn remove<T: 'static>(&mut self) -> Option<T> {
        self.contents.remove(&TypeId::of::<T>()).map(|any| *any.downcast().unwrap())
    }

    fn get<T: 'static>(&self) -> Option<&T> {
        self.contents.get(&TypeId::of::<T>()).map(|any| any.downcast_ref().unwrap())
    }

    fn get_mut<T: 'static>(&mut self) -> Option<&mut T> {
        self.contents.get_mut(&TypeId::of::<T>()).map(|any| any.downcast_mut().unwrap())
    }
}

impl std::fmt::Display for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        fn fmt(tree: &Tree, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
            if !tree.token.deref().is_empty() {
                writeln!(f, "{}{}", "\t".repeat(depth), tree.token)?;
            }
            tree.children.iter().map(|child| fmt(child, f, depth + 1)).collect()
        }
        fmt(self, f, 0)
    }
}

enum Pass {
    Name(String),
    Func(Box<dyn Fn(&mut Tree)>),
}

impl Pass {
    fn run_passes(passes: VecDeque<Pass>, tree: &mut Tree) {
        tree.insert(passes);
        while let Some(pass) = tree.get_mut::<VecDeque<Pass>>().unwrap().pop_front() {
            if let Pass::Func(func) = pass {
                Pass::postorder(&func, tree);
            }
        }
    }

    fn postorder(func: &impl Fn(&mut Tree), tree: &mut Tree) {
        tree.children.iter_mut().for_each(|child| Pass::postorder(func, child));
        func(tree);
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let name = match args.get(1) {
        Some(name) => name.to_owned(),
        None => panic!("usage: pass a .iz file"),
    };
    let text = match std::fs::read_to_string(&name) {
        Ok(text) => text,
        Err(_) => panic!("could not read {}", name),
    };
    let source = Rc::new(Source { name, text });

    let mut tree = Tree::new(Token { source: source.clone(), lo: 0, hi: 0 });
    tree.insert(source.clone());
    let los = source.text.char_indices().map(|(i, _)| i);
    let his = source.text.char_indices().map(|(i, _)| i);
    for (lo, hi) in los.zip(his.skip(1).chain([source.text.len()])) {
        tree.children.push(Tree::new(Token { source: source.clone(), lo, hi }));
    }

    let mut passes = VecDeque::new();
    passes.push_back(Pass::Name("chars".to_owned()));
    passes.push_back(Pass::Func(Box::new(remove_comments)));
    passes.push_back(Pass::Func(Box::new(remove_whitespace)));
    passes.push_back(Pass::Func(Box::new(concat_tokens(is_identifier))));
    passes.push_back(Pass::Func(Box::new(concat_tokens(is_operator))));
    passes.push_back(Pass::Name("tokens".to_owned()));
    passes.push_back(Pass::Func(Box::new(parse_integers)));
    passes.push_back(Pass::Func(Box::new(parse_brackets("(", ")"))));
    passes.push_back(Pass::Func(Box::new(parse_brackets("{", "}"))));
    passes.push_back(Pass::Func(Box::new(parse_brackets("[", "]"))));
    passes.push_back(Pass::Func(Box::new(parse_postfixes(&[":", "?", "&"]))));
    passes.push_back(Pass::Name("ast".to_owned()));
    passes.push_back(Pass::Func(Box::new(translate_instructions)));
    passes.push_back(Pass::Func(Box::new(get_labels)));
    passes.push_back(Pass::Func(Box::new(get_cfg)));
    passes.push_back(Pass::Func(Box::new(check_instructions)));
    passes.push_back(Pass::Name("code".to_owned()));
    passes.push_back(Pass::Func(Box::new(compile_x64)));
    Pass::run_passes(passes, &mut tree);
}

fn remove_comments(tree: &mut Tree) {
    let mut in_comment = false;
    tree.children.retain(|child| {
        match child.token.deref() {
            "#" if !in_comment => in_comment = true,
            "\n" if in_comment => in_comment = false,
            _ => {}
        }
        !in_comment
    });
}

fn is_identifier(s: &str) -> bool {
    s.chars().all(|c| c.is_alphanumeric() || c == '-' || c == '_')
}

fn is_whitespace(s: &str) -> bool {
    s.chars().all(|c| c.is_whitespace())
}

fn is_operator(s: &str) -> bool {
    s.chars().all(|c| !(c.is_alphanumeric() || c.is_whitespace() || "(){}[]".contains(c)))
}

fn remove_whitespace(tree: &mut Tree) {
    tree.children.retain(|child| !is_whitespace(child.token.deref()));
}

fn concat_tokens(f: impl Fn(&str) -> bool) -> impl Fn(&mut Tree) {
    move |tree: &mut Tree| {
        let mut i = 1;
        while i < tree.children.len() {
            if f(tree.children[i - 1].token.deref())
                && f(tree.children[i].token.deref())
                && tree.children[i - 1].token.source == tree.children[i].token.source
                && tree.children[i - 1].token.hi == tree.children[i].token.lo
            {
                tree.children[i - 1].token.hi = tree.children[i].token.hi;
                tree.children.remove(i);
            } else {
                i += 1;
            }
        }
    }
}

fn parse_integers(tree: &mut Tree) {
    let chars: Vec<char> = tree.token.deref().chars().collect();
    if !chars.is_empty() {
        let (chars, is_negative) = match chars[0] {
            '-' => (&chars[1..], true),
            _ => (&chars[..], false),
        };
        if let Some('0'..='9') = chars.get(0) {
            let (chars, base) = match chars {
                ['0', 'x', ..] => (&chars[2..], 16),
                ['0', 'b', ..] => (&chars[2..], 2),
                _ => (chars, 10),
            };
            let value = chars.iter().fold(0, |value, c| {
                let digit = match c {
                    '0'..='9' => *c as i64 - '0' as i64,
                    'a'..='f' => *c as i64 - 'a' as i64 + 10,
                    '_' => return value,
                    c => panic!("unknown digit {} in {}", c, tree.token),
                };
                if digit >= base {
                    panic!("digit {} too large for base {} in {}", c, base, tree.token)
                }
                base * value + digit
            });
            tree.insert::<i64>(if is_negative { -value } else { value });
        }
    }
}

fn parse_brackets<'a>(open: &'a str, close: &'a str) -> impl Fn(&mut Tree) + 'a {
    move |tree: &mut Tree| {
        let mut indices = Vec::new(); // stack of open bracket indices
        let mut i = 0;
        while i < tree.children.len() {
            let curr = &tree.children[i].token;
            if curr.deref() == open {
                indices.push(i);
            } else if curr.deref() == close {
                let j = indices.pop().unwrap_or_else(|| panic!("extra {}", curr));
                let mut cs: Vec<Tree> = tree.children.drain(j + 1..=i).collect();
                cs.pop(); // remove closing bracket
                tree.children[j].children.append(&mut cs);
                i = j;
            }
            i += 1;
        }
        if let Some(j) = indices.pop() {
            panic!("extra {}", tree.children[j].token)
        }
    }
}

fn parse_postfixes<'a>(names: &'a [&'a str]) -> impl Fn(&mut Tree) + 'a {
    move |tree: &mut Tree| {
        let mut i = 0;
        while i < tree.children.len() {
            if names.contains(&tree.children[i].token.deref()) {
                if i == 0 {
                    panic!("no argument for {}", tree.children[i].token)
                }
                let child = tree.children.remove(i - 1);
                tree.children[i - 1].children.push(child);
            } else {
                i += 1;
            }
        }
    }
}

// Instructions semantically target a VM with the following structure
// - Memory is represented as an array of 64-bit 2's complement signed integers
// - There are only two registers: a program counter `pc` and a stack pointer `sp`
//   - pc points to the next instruction to run
//   - sp points to the lowest address of the top element of the stack, which grows downward
enum Instruction {
    // Memory
    Push(i64), // increments sp, then writes an immediate there
    Pop,       // decrements sp
    Sp,        // pushes the value in sp onto the stack
    Write,     // first pops an address, then pops a value, then writes the value to the address
    Read,      // pops an address, then pushes the value at that address
    // Integer
    Add, // pops two values, then pushes their sum
    Mul, // pops two values, then pushes their product     (subsumes neg)
    And, // pops two values, then pushes their bitwise and (subsumes ltz)
    Or,  // pops two values, then pushes their bitwise or
    Xor, // pops two values, then pushes their bitwise xor (subsumes not)
    // Control
    Label(String), // does nothing except hold a label
    Jumpz(String), // pops a value, if the value is zero sets pc to the label
    Addr(String),  // pushes the address of the given label
    Goto,          // pops an address and sets pc to it
}

fn translate_instructions(tree: &mut Tree) {
    if tree.get::<Vec<Instruction>>().is_none() {
        let instruction = if let Some(int) = tree.remove::<i64>() {
            Instruction::Push(int)
        } else {
            match tree.token.deref() {
                "pop" => Instruction::Pop,
                "sp" => Instruction::Sp,
                "write" => Instruction::Write,
                "read" => Instruction::Read,
                "add" => Instruction::Add,
                "mul" => Instruction::Mul,
                "and" => Instruction::And,
                "or" => Instruction::Or,
                "xor" => Instruction::Xor,
                ":" => {
                    let grandchild = tree.children.pop().unwrap();
                    Instruction::Label(grandchild.token.deref().to_owned())
                }
                "?" => {
                    let grandchild = tree.children.pop().unwrap();
                    Instruction::Jumpz(grandchild.token.deref().to_owned())
                }
                "&" => {
                    let grandchild = tree.children.pop().unwrap();
                    Instruction::Addr(grandchild.token.deref().to_owned())
                }
                "goto" => Instruction::Goto,
                _ => return,
            }
        };
        tree.insert(vec![instruction]);
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct InstructionIndex {
    child: usize,
    index: usize,
}
struct Labels(HashMap<String, InstructionIndex>);
fn get_labels(tree: &mut Tree) {
    let mut labels = HashMap::new();
    for (i, child) in tree.children.iter().enumerate() {
        if let Some(instructions) = child.get::<Vec<Instruction>>() {
            for (j, instruction) in instructions.iter().enumerate() {
                if let Instruction::Label(label) = instruction {
                    let old = labels.insert(label.clone(), InstructionIndex { child: i, index: j });
                    if let Some(InstructionIndex { child: k, .. }) = old {
                        panic!(
                            "label {} is declared twice:\n{}\n{}",
                            label, child.token, tree.children[k].token
                        )
                    }
                }
            }
        }
    }
    let mut found = HashSet::new();
    for child in &tree.children {
        if let Some(instructions) = child.get::<Vec<Instruction>>() {
            for instruction in instructions {
                if let Instruction::Jumpz(label) = instruction {
                    if labels.get(label).is_some() {
                        found.insert(label);
                    } else {
                        panic!("jumpz has no matching label: {}", child.token)
                    }
                }
                if let Instruction::Addr(label) = instruction {
                    if labels.get(label).is_some() {
                        found.insert(label);
                    } else {
                        panic!("addr has no matching label: {}", child.token)
                    }
                }
            }
        }
    }
    let labels_set = labels.keys().collect::<HashSet<_>>();
    let diff: HashSet<_> = labels_set.difference(&found).collect();
    if !diff.is_empty() {
        panic!("unused labels: {:?}", diff)
    }
    tree.insert(Labels(labels));
}

struct Cfg(HashMap<InstructionIndex, Vec<InstructionIndex>>);
fn get_cfg(tree: &mut Tree) {
    let labels = &tree.get::<Labels>().unwrap().0;
    let mut cfg: HashMap<InstructionIndex, Vec<InstructionIndex>> = HashMap::new();
    for (i, child) in tree.children.iter().enumerate() {
        if let Some(instructions) = child.get::<Vec<Instruction>>() {
            for (j, instruction) in instructions.iter().enumerate() {
                let curr = InstructionIndex { child: i, index: j };
                let entry: &mut _ = cfg.entry(curr).or_default();
                let next = if j + 1 < instructions.len() {
                    InstructionIndex { child: i, index: j + 1 }
                } else {
                    InstructionIndex { child: i + 1, index: 0 }
                };
                match instruction {
                    Instruction::Jumpz(label) => {
                        entry.push(next);
                        entry.push(*labels.get(label).unwrap());
                    }
                    Instruction::Goto => todo!(),
                    _ => entry.push(next),
                }
            }
        }
    }
    tree.insert(Cfg(cfg));
}

fn check_instructions(tree: &mut Tree) {
    for child in &tree.children {
        if child.get::<Vec<Instruction>>().is_none() {
            panic!("could not translate to instructions: {}", child.token)
        } else if !child.children.is_empty() {
            panic!("tree had children\n{}", child)
        }
    }
}

fn compile_x64(tree: &mut Tree) {
    if tree.children.is_empty() {
        return;
    }

    let name = match tree.token.source.name.rfind('.') {
        Some(i) => &tree.token.source.name[..i],
        None => "a.out",
    };

    let mut assembler = Command::new("gcc")
        .arg("-x")
        .arg("assembler-with-cpp")
        .arg("-nostdlib")
        .arg("-Wall")
        .arg("-g")
        .arg("-o")
        .arg(name)
        .arg("-")
        .stdin(Stdio::piped())
        .spawn()
        .unwrap();
    let stdin = assembler.stdin.as_mut().unwrap();

    write!(stdin, "#include <sys/syscall.h>\n\n\t.global _start\n_start:").unwrap();
    for child in &tree.children {
        for instruction in child.get::<Vec<Instruction>>().unwrap() {
            match instruction {
                Instruction::Push(int) => write!(stdin, "\tmovq ${}, %rax\n\tpushq %rax\n", int),
                Instruction::Pop => write!(stdin, "\tpopq %rax\n"),
                Instruction::Sp => write!(stdin, "\tpushq %rsp\n"),
                Instruction::Write => {
                    write!(stdin, "\tpopq %rax\n\tpopq %rcx\n\tmovq %rcx, (%rax)\n")
                }
                Instruction::Read => {
                    write!(stdin, "\tmovq (%rsp), %rax\n\tmovq (%rax), %rax\n\tmovq %rax, (%rsp)\n")
                }
                Instruction::Add => write!(stdin, "\tpopq %rax\n\taddq %rax, (%rsp)\n"),
                Instruction::Mul => write!(stdin, "\tpopq %rax\n\tmulq %rax, (%rsp)\n"),
                Instruction::And => write!(stdin, "\tpopq %rax\n\tandq %rax, (%rsp)\n"),
                Instruction::Or => write!(stdin, "\tpopq %rax\n\torq %rax, (%rsp)\n"),
                Instruction::Xor => write!(stdin, "\tpopq %rax\n\txorq %rax, (%rsp)\n"),
                Instruction::Label(label) => write!(stdin, "{}:\n", label),
                Instruction::Jumpz(label) => {
                    write!(stdin, "\tpopq %rax\n\ttest %rax, %rax\n\tjz {}\n", label)
                }
                Instruction::Addr(label) => {
                    write!(stdin, "\tleaq {}(%rip), %rax\n\tpushq %rax\n", label)
                }
                Instruction::Goto => write!(stdin, "\tret\n"),
            }
            .unwrap();
        }
    }
    write!(stdin, "\tmovq $SYS_exit, %rax\n\tmovq $0, %rdi\n\tsyscall").unwrap();

    assembler.wait().unwrap();

    let output = Command::new(format!("./{}", name)).output().unwrap();
    println!(
        "status: {}\nstderr: {}\nstdout: {}",
        output.status,
        std::str::from_utf8(&output.stderr).unwrap(),
        std::str::from_utf8(&output.stdout).unwrap(),
    );
}
