use crate::token::Token;
use crate::token::TokenType;

pub fn parse(tokens: &Vec<Token>) -> Tree<Token> {
    let mut ast = Tree::new(Token("".to_string(), 0, TokenType::Other));

    let mut depth = 0;
    for t in tokens {
        if t.2 == TokenType::Closer {
            depth -= 1;
        } else {
            fn insert_last(t: &mut Tree<Token>, child: Tree<Token>, depth: usize) {
                if depth == 0 {
                    t.children.push(child);
                } else {
                    insert_last(t.children.last_mut().unwrap(), child, depth - 1);
                }
            }

            insert_last(&mut ast, Tree::new(t.clone()), depth);

            if t.2 == TokenType::Opener {
                depth += 1;
            }
        }
    }

    let operators = vec![
        Operator::new("=", 2, 1, 0, false),
        Operator::new(":", 2, 1, 1, false),
        Operator::new("->", 2, 1, 2, false),
        Operator::new("if", 3, 0, 3, false),
        Operator::new("||", 2, 1, 4, true),
        Operator::new("&&", 2, 1, 5, true),
        Operator::new("==", 2, 1, 6, true),
        Operator::new("!=", 2, 1, 6, true),
        Operator::new("<", 2, 1, 6, true),
        Operator::new(">", 2, 1, 6, true),
        Operator::new("<=", 2, 1, 6, true),
        Operator::new(">=", 2, 1, 6, true),
        Operator::new("|", 2, 1, 7, true),
        Operator::new("&", 2, 1, 7, true),
        Operator::new("+", 2, 1, 7, true),
        Operator::new("-", 2, 1, 7, true),
        Operator::new("*", 2, 1, 8, true),
        Operator::new("/", 2, 1, 8, true),
        Operator::new("%", 2, 1, 8, true),
        Operator::new("^", 2, 1, 9, true),
        Operator::new("!", 1, 0, 10, true),
        Operator::new(".", 2, 1, 11, true),
        Operator::new("@", 2, 1, 12, true),
    ];

    fn postorder(t: &mut Tree<Token>, operators: &Vec<Operator>) {
        for c in &mut t.children {
            postorder(c, operators);
        }
        
        let mut ops = Vec::new();
        for (i, c) in t.children.iter().enumerate() {
            for o in operators {
                if c.value.0 == o.name {
                    ops.push((i, o, c.value.1));
                }
            }
        }
        ops.sort_by(|(i, a, _), (j, b, _)| {
            if a.prec == b.prec {
                if j < i && a.left || i < j && !a.left {
                    std::cmp::Ordering::Greater
                } else {
                    std::cmp::Ordering::Less
                }
            } else {
                b.prec.cmp(&a.prec)
            }
        });
        for i in 0..ops.len() {
            let mut j = ops[i].0 - ops[i].1.pos;
            let mut remaining = ops[i].1.arity;
            while 0 < remaining {
                if ops[i].0 == j {
                    j += 1;
                } else {
                    let child = t.children.remove(j);
                    for (k, p) in ops.iter_mut().enumerate() {
                        if k >= i && p.0 >= j {
                            p.0 -= 1;
                        }
                    }
                    t.children[ops[i].0].children.push(child);
                    remaining -= 1;
                }
            }
        }
    }

    postorder(&mut ast, &operators);

    ast
}

struct Operator {
    name: String,
    arity: usize,
    pos: usize,
    prec: isize,
    left: bool,
}

impl Operator {
    fn new(name: &str, arity: usize, pos: usize, prec: isize, left: bool) -> Operator {
        Operator {
            name: name.to_string(),
            arity: arity,
            pos: pos,
            prec: prec,
            left: left,
        }
    }
}

#[derive(Clone)]
pub struct Tree<T> {
    pub value: T,
    pub children: Vec<Tree<T>>,
}

impl<T> Tree<T> {
    pub fn new(value: T) -> Tree<T> {
        Tree {
            value: value,
            children: Vec::new(),
        }
    }
}

use std::fmt::Debug;
use std::fmt::Formatter;
use std::fmt::Result;
impl<T: Debug> Debug for Tree<T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        fn preorder<T: Debug>(t: &Tree<T>, d: usize, f: &mut Formatter) -> Result {
            writeln!(f, "{}{:?}", "\t".repeat(d), t.value)?;
            for c in &t.children {
                preorder(c, d + 1, f)?;
            }
            Ok(())
        }
        preorder(self, 0, f)
    }
}

