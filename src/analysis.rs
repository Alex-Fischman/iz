//! Functions to analyze the structure of a tree.

use crate::Tree;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

pub enum Intrinsic {
    Push(i64),
    Pop,
    Read,
    Write,
    Sp,
    Add,
    Label(String),
    Jumpz(String),
    Call,
    Func,
}

pub fn annotate_intrinsics(tree: &mut Tree) {
    tree.children.iter_mut().for_each(annotate_intrinsics);

    let intrinsic = match tree.token.as_str() {
        _ if tree.get::<i64>().is_some() => Intrinsic::Push(*tree.get::<i64>().unwrap()),
        "pop" => Intrinsic::Pop,
        "read" => Intrinsic::Read,
        "write" => Intrinsic::Write,
        "sp" => Intrinsic::Sp,
        "add" => Intrinsic::Add,
        ":" => Intrinsic::Label(tree.children.remove(0).token.as_str().to_string()),
        "?" => Intrinsic::Jumpz(tree.children.remove(0).token.as_str().to_string()),
        "call" => Intrinsic::Call,
        "{" => Intrinsic::Func,
        _ => return,
    };
    tree.insert(intrinsic);
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Effect {
    pub inputs: Vec<Type>,
    pub outputs: Vec<Type>,
}

macro_rules! effect {
    ($($inputs:expr)* ; $($outputs:expr)*) => (Effect {
        inputs: vec![$($inputs),*],
        outputs: vec![$($outputs),*],
    });
}

impl Display for Effect {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "(")?;
        for input in &self.inputs {
            write!(f, "{} ", input)?;
        }
        write!(f, "|")?;
        for output in &self.outputs {
            write!(f, " {}", output)?;
        }
        write!(f, ")")
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Ptr,
    Fun(Effect),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Ptr => write!(f, "Ptr"),
            Type::Fun(e) => write!(f, "Fun{}", e),
        }
    }
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Int | Type::Ptr | Type::Fun(_) => 8,
        }
    }
}

pub fn compute_types(tree: &mut Tree) {
    use Type::*;

    // tree.children.len() as a key holds the returned effect
    let mut inputs: HashMap<usize, Effect> = HashMap::from([(0, effect!(;))]);

    let mut i = 0;
    while i < tree.children.len() {
        let input = &inputs[&i];
        let token = &tree.children[i].token;
        let mut targets = vec![i + 1];
        let mut curr = match tree.children[i].get::<Intrinsic>() {
            Some(Intrinsic::Push(_)) => effect!(; Int),
            Some(Intrinsic::Pop) => match input.outputs.last() {
                Some(t) if t.size() == 8 => effect!(t.clone() ;),
                Some(t) => {
                    panic!("expected a type of size 8, found {} for {}", t, token)
                }
                None => panic!("could not infer type for {}", token),
            },
            Some(Intrinsic::Read) => effect!(Ptr ; Int),
            Some(Intrinsic::Write) => effect!(Int Ptr ;),
            Some(Intrinsic::Sp) => effect!(; Ptr),
            Some(Intrinsic::Add) => effect!(Int Int ; Int),
            Some(Intrinsic::Label(_)) => effect!(;),
            Some(Intrinsic::Jumpz(s)) => {
                let labels = (0..tree.children.len()).filter(
                    |j| matches!(tree.children[*j].get::<Intrinsic>(), Some(Intrinsic::Label(t)) if s == t),
                );
                let labels: Vec<usize> = labels.collect();
                let target = match labels.as_slice() {
                    [target] => target,
                    [] => panic!("could not find the matching label for {}", token),
                    _ => panic!("too many matching labels for {}", token),
                };
                targets.push(*target);
                effect!(Int ;)
            }
            Some(Intrinsic::Call) => match input.outputs.last() {
                Some(Fun(e)) => {
                    let mut e = e.clone();
                    e.inputs.push(Fun(e.clone()));
                    e
                }
                Some(t) => {
                    panic!("expected a function type, found {} for {}", t, token)
                }
                None => panic!("could not infer type for {}", token),
            },
            Some(Intrinsic::Func) => {
                compute_types(&mut tree.children[i]);
                tree.children[i].remove::<Effect>().unwrap()
            }
            None => unreachable!("compute_types requires the tree to have intrinsics"),
        };

        tree.children[i].insert(curr.clone());

        let mut next = input.clone();
        while let Some(input) = curr.inputs.pop() {
            match next.outputs.pop() {
                Some(output) if input == output => {}
                Some(output) => {
                    panic!(
                        "expected {}, found {} for {}",
                        output, input, tree.children[i].token
                    )
                }
                None => next.inputs.insert(0, input.clone()),
            }
        }
        next.outputs.append(&mut curr.outputs);

        for target in targets {
            match inputs.get(&target) {
                Some(old) if *old == next => {}
                Some(_) => panic!("found branching types for {}", tree.children[target].token),
                None => {
                    inputs.insert(target, next.clone());
                }
            }
        }

        i += 1;
    }

    match inputs.get(&tree.children.len()) {
        None => panic!("could not compute return type for {}", tree.token),
        Some(effect) => {
            tree.insert(effect!(; Fun(effect.clone())));
        }
    }
}
