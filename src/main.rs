use crate::pass::Passes;
use crate::token::{Source, Token, TokenKind};
use crate::tree::Tree;
use std::collections::HashMap;

mod pass;
mod token;
mod tree;

fn main() {
    let name = std::env::args()
        .collect::<Vec<String>>()
        .get(1)
        .unwrap_or_else(|| panic!("usage: pass a .iz file"))
        .to_string();
    let text = std::fs::read_to_string(&name).unwrap_or_else(|_| panic!("could not read {}", name));
    let source = std::rc::Rc::new(Source { name, text });

    let mut tree = Tree::new(Token {
        source: source.clone(),
        range: 0..0,
    });
    let los = source.text.char_indices().map(|(i, _)| i);
    let his = source.text.char_indices().map(|(i, _)| i);
    for (lo, hi) in los.zip(his.skip(1).chain([source.text.len()])) {
        tree.children.push(Tree::new(Token {
            source: source.clone(),
            range: lo..hi,
        }));
    }

    let mut passes = Passes::default();
    // flat
    passes.push(remove_comments);
    passes.push(remove_whitespace);
    passes.push(concat_tokens(|t| t.kind() == TokenKind::Identifier));
    passes.push(concat_tokens(|t| t.kind() == TokenKind::Operator));
    passes.push(parse_integers);
    // nested
    passes.push(parse_brackets("(", ")"));
    passes.push(parse_brackets("{", "}"));
    passes.push(parse_brackets("[", "]"));
    passes.push(parse_postfixes(&[":", "?", "&"]));
    // structured
    passes.push(annotate_intrinsics);
    passes.push(compute_types);
    passes.push(compile_intrinsics_x64);
    passes.push(compile_program_x64);

    tree.insert(passes);
    tree.run();
}

fn remove_comments(tree: &mut Tree) {
    let mut in_comment = false;
    tree.children.retain(|child| {
        match child.token.as_str() {
            "#" if !in_comment => in_comment = true,
            "\n" if in_comment => in_comment = false,
            _ => {}
        }
        !in_comment
    });
}

fn remove_whitespace(tree: &mut Tree) {
    tree.children
        .retain(|child| child.token.kind() != TokenKind::Whitespace);
}

fn concat_tokens(f: impl Fn(&Token) -> bool) -> impl Fn(&mut Tree) {
    move |tree: &mut Tree| {
        let mut i = 1;
        while i < tree.children.len() {
            if f(&tree.children[i - 1].token)
                && f(&tree.children[i].token)
                && tree.children[i - 1].token.source == tree.children[i].token.source
                && tree.children[i - 1].token.range.end == tree.children[i].token.range.start
            {
                tree.children[i - 1].token.range.end = tree.children[i].token.range.end;
                assert!(tree.children[i].children.is_empty());
                tree.children.remove(i);
            } else {
                i += 1;
            }
        }
    }
}

fn parse_integers(tree: &mut Tree) {
    for child in &mut tree.children {
        let chars: Vec<char> = child.token.as_str().chars().collect();
        if !chars.is_empty() {
            let (chars, is_negative) = match chars[0] {
                '-' => (&chars[1..], true),
                _ => (&chars[..], false),
            };
            if let Some('0'..='9') = chars.first() {
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
                        c => panic!("unknown digit {} in {}", c, child.token),
                    };
                    if digit >= base {
                        panic!("digit {} too large for base {} in {}", c, base, child.token)
                    }
                    base * value + digit
                });
                child.insert::<i64>(if is_negative { -value } else { value });
            }
        }
    }
}

fn parse_brackets<'a>(open: &'a str, close: &'a str) -> impl Fn(&mut Tree) + 'a {
    move |tree: &mut Tree| {
        tree.children
            .iter_mut()
            .for_each(parse_brackets(open, close));

        let mut indices = Vec::new(); // stack of open bracket indices
        let mut i = 0;
        while i < tree.children.len() {
            let curr = &tree.children[i].token;
            if curr.as_str() == open {
                indices.push(i);
            } else if curr.as_str() == close {
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
        tree.children.iter_mut().for_each(parse_postfixes(names));

        let mut i = 0;
        while i < tree.children.len() {
            if names.contains(&tree.children[i].token.as_str()) {
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

use Intrinsic::*;
#[derive(Debug, PartialEq)]
enum Intrinsic {
    Push(i64),
    Pop,
    Write,
    Read,
    Sp,
    Add,
    Mul,
    And,
    Or,
    Xor,
    Label(String),
    Jumpz(String),
    Call,
    Func,
}

fn annotate_intrinsics(tree: &mut Tree) {
    tree.children.iter_mut().for_each(annotate_intrinsics);

    let intrinsic = match tree.token.as_str() {
        _ if tree.get::<i64>().is_some() => Push(*tree.get::<i64>().unwrap()),
        "pop" => Pop,
        "read" => Read,
        "write" => Write,
        "sp" => Sp,
        "add" => Add,
        "mul" => Mul,
        "and" => And,
        "or" => Or,
        "xor" => Xor,
        ":" => Label(tree.children.remove(0).token.as_str().to_string()),
        "?" => Jumpz(tree.children.remove(0).token.as_str().to_string()),
        "call" => Call,
        "{" => Func,
        _ => return,
    };
    tree.insert(intrinsic);
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Effect {
    inputs: Vec<Type>,
    outputs: Vec<Type>,
}

macro_rules! effect {
    ($($inputs:expr)* ; $($outputs:expr)*) => (Effect {
        inputs: vec![$($inputs),*],
        outputs: vec![$($outputs),*],
    });
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Type {
    Int,
    Ptr,
    Fun(Effect),
}

impl Type {
    fn size(&self) -> usize {
        match self {
            Type::Int | Type::Ptr | Type::Fun(_) => 8,
        }
    }
}

fn compute_types(tree: &mut Tree) {
    use Type::*;

    // tree.children.len() as a key holds the returned effect
    let mut inputs: HashMap<usize, Effect> = HashMap::from([(0, effect!(;))]);

    let mut i = 0;
    while i < tree.children.len() {
        let mut targets = vec![i + 1];
        let mut curr = match tree.children[i].get::<Intrinsic>() {
            Some(Push(_)) => effect!(; Int),
            Some(Pop) => match inputs[&i].outputs.last() {
                Some(t) if t.size() == 8 => effect!(t.clone() ;),
                Some(t) => {
                    panic!(
                        "expected a type of size 8, found {:?} for {}",
                        t, tree.children[i]
                    )
                }
                None => panic!("could not infer type for {}", tree.children[i]),
            },
            Some(Read) => effect!(Ptr ; Int),
            Some(Write) => effect!(Int Ptr ;),
            Some(Sp) => effect!(; Ptr),
            Some(Add) => effect!(Int Int ; Int),
            Some(Mul) => effect!(Int Int ; Int),
            Some(And) => effect!(Int Int ; Int),
            Some(Or) => effect!(Int Int ; Int),
            Some(Xor) => effect!(Int Int ; Int),
            Some(Label(_)) => effect!(;),
            Some(Jumpz(s)) => {
                let labels = (0..tree.children.len()).filter(
                    |j| matches!(tree.children[*j].get::<Intrinsic>(), Some(Label(t)) if s == t),
                );
                let labels: Vec<usize> = labels.collect();
                let target = match labels.as_slice() {
                    [target] => target,
                    [] => panic!("could not find the matching label for {}", tree.children[i]),
                    _ => panic!("too many matching labels for {}", tree.children[i]),
                };
                targets.push(*target);
                effect!(Int ;)
            }
            Some(Call) => match inputs[&i].outputs.last() {
                Some(Fun(e)) => {
                    let mut e = e.clone();
                    e.inputs.push(Fun(e.clone()));
                    e
                }
                Some(t) => {
                    panic!(
                        "expected a function type, found {:?} for {}",
                        t, tree.children[i]
                    )
                }
                None => panic!("could not infer type for {}", tree.children[i]),
            },
            Some(Func) => {
                compute_types(&mut tree.children[i]);
                tree.children[i].remove::<Effect>().unwrap()
            }
            None => todo!(
                "compute_types callback for non-intrinsics? {}",
                tree.children[i]
            ),
        };

        tree.children[i].insert(curr.clone());

        let mut next = inputs[&i].clone();
        while let Some(input) = curr.inputs.pop() {
            match next.outputs.pop() {
                Some(output) if input == output => {}
                Some(output) => {
                    panic!(
                        "expected {:?}, found {:?} for {}",
                        output, input, tree.children[i]
                    )
                }
                None => next.inputs.insert(0, input.clone()),
            }
        }
        next.outputs.append(&mut curr.outputs);

        for target in targets {
            match inputs.get(&target) {
                Some(old) if *old == next => {}
                Some(_) => panic!("found branching types for {}", tree.children[target]),
                None => {
                    inputs.insert(target, next.clone());
                }
            }
        }

        i += 1;
    }

    match inputs.get(&tree.children.len()) {
        None => panic!("could not compute return type for {}", tree),
        Some(effect) => {
            tree.insert(effect!(; Fun(effect.clone())));
        }
    }
}

#[derive(Debug)]
struct Assembly {
    main: String,
    rest: String,
}
fn compile_intrinsics_x64(tree: &mut Tree) {
    fn compile_intrinsics_x64(tree: &mut Tree, labels: &mut usize) {
        use std::fmt::Write;

        let mut main = String::new();
        let mut rest = String::new();
        for child in &mut tree.children {
            main.push_str(&match child.get::<Intrinsic>() {
                Some(Push(int)) => format!("\tmovq ${}, %rax\n\tpushq %rax\n", int),
                Some(Pop) => "\tpopq %rax\n".to_string(),
                Some(Read) => {
                    "\tmovq (%rsp), %rax\n\tmovq (%rax), %rax\n\tmovq %rax, (%rsp)\n".to_string()
                }
                Some(Write) => "\tpopq %rax\n\tpopq %rcx\n\tmovq %rcx, (%rax)\n".to_string(),
                Some(Sp) => "\tpushq %rsp\n".to_string(),
                Some(Add) => "\tpopq %rax\n\taddq %rax, (%rsp)\n".to_string(),
                Some(Mul) => "\tpopq %rax\n\tmulq %rax, (%rsp)\n".to_string(),
                Some(And) => "\tpopq %rax\n\tandq %rax, (%rsp)\n".to_string(),
                Some(Or) => "\tpopq %rax\n\torq %rax, (%rsp)\n".to_string(),
                Some(Xor) => "\tpopq %rax\n\txorq %rax, (%rsp)\n".to_string(),
                Some(Label(s)) => format!("{}:\n", s),
                Some(Jumpz(s)) => format!("\tpopq %rax\n\ttestq %rax, %rax\n\tjz {}\n", s),
                Some(Call) => "\tpopq %rax\n\tcallq *%rax\n".to_string(),
                Some(Func) => {
                    compile_intrinsics_x64(child, labels);
                    let assembly = child.get::<Assembly>().unwrap();
                    let effect = match &child.get::<Effect>().unwrap().outputs[0] {
                        Type::Fun(e) => e,
                        _ => unreachable!(),
                    };
                    let inputs = effect.inputs.iter().fold(0, |a, t| a + t.size());
                    let outputs = effect.outputs.iter().fold(0, |a, t| a + t.size());

                    let mut prologue = String::new();
                    for _ in 0..(inputs / 8) {
                        write!(prologue, "\tmovq {}(%rsp), %rax\n\tpushq %rax\n", inputs).unwrap();
                    }

                    let mut epilogue = String::new();
                    writeln!(epilogue, "\tmovq {}(%rsp), %rcx", outputs).unwrap();
                    for i in 0..(outputs / 8) {
                        let offset = outputs - 8 - i * 8;
                        write!(
                            epilogue,
                            "\tmovq {}(%rsp), %rax\n\tmovq %rax, {}(%rsp)\n",
                            offset,
                            offset + inputs + 8
                        )
                        .unwrap();
                    }
                    writeln!(epilogue, "\taddq ${}, %rsp", inputs + 8).unwrap();
                    write!(epilogue, "\tpushq %rcx\n\tretq\n").unwrap();

                    *labels += 1;
                    let label = format!("_${}", *labels - 1);

                    write!(
                        rest,
                        "{}:\n{}{}{}\n\n{}",
                        label, prologue, assembly.main, epilogue, assembly.rest
                    )
                    .unwrap();
                    format!("\tleaq {}(%rip), %rax\n\tpushq %rax\n", label)
                }
                None => return,
            });
        }

        tree.insert(Assembly { main, rest });
    }
    compile_intrinsics_x64(tree, &mut 0)
}

fn compile_program_x64(tree: &mut Tree) {
    use std::io::Write;

    let name = match tree.token.source.name.rfind('.') {
        Some(i) => &tree.token.source.name[..i],
        None => "a.out",
    };

    let mut assembler = std::process::Command::new("gcc")
        .arg("-x")
        .arg("assembler-with-cpp")
        .arg("-nostdlib")
        .arg("-Wall")
        .arg("-g")
        .arg("-o")
        .arg(name)
        .arg("-")
        .stdin(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    let stdin = assembler.stdin.as_mut().unwrap();

    let assembly = tree.get::<Assembly>().unwrap();
    write!(
        stdin,
        "#include <sys/syscall.h>\n\n\t.global _start\n_start:\n"
    )
    .unwrap();
    write!(stdin, "{}", assembly.main).unwrap();
    write!(
        stdin,
        "\tmovq $SYS_exit, %rax\n\tmovq $0, %rdi\n\tsyscall\n\n"
    )
    .unwrap();
    write!(stdin, "{}", assembly.rest).unwrap();

    if !assembler.wait().unwrap().success() {
        panic!("assembler failed")
    }

    let output = std::process::Command::new(format!("./{}", name))
        .output()
        .unwrap();
    println!(
        "status: {}\nstderr: {}\nstdout: {}",
        output.status,
        std::str::from_utf8(&output.stderr).unwrap(),
        std::str::from_utf8(&output.stdout).unwrap(),
    );
}
