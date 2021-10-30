use std::collections::HashMap;
use std::fmt::*;

fn main() {
    let parse_test = |a, b| {
        let (p, mut ops) = preprocess(a);
        assert_eq!(
            format!("{}", parse(tokenize(&p), &mut ops)),
            format!("({{ {})", b)
        )
    };
    parse_test("1", "1");
    parse_test("a + b", "(add a b)");
    parse_test("if a b", "(if_ a b)");
    parse_test("a + b + c", "(add (add a b) c)");
    parse_test("a = b = c", "(set a (set b c))");
    parse_test("1 + f = g = h * 3", "(set (add 1 f) (set g (mul h 3)))");
    parse_test("a + b * c * d + e", "(add (add a (mul (mul b c) d)) e)");
    parse_test("#op dot 15 l _ . _\na . b", "(call (call dot a) b)");
    parse_test("#op neg 14 p - _\n-a + b", "(add (call neg a) b)");
    parse_test(
        "#op neg 14 p - _\n- -1 * 2",
        "(mul (call neg (call neg 1)) 2)",
    );
    parse_test(
        "#op neg 14 p - _\n- -f + g",
        "(add (call neg (call neg f)) g)",
    );
    parse_test("(0(0))", "(( 0 (( 0))");
    parse_test("((0)(0))", "(( (( 0) (( 0))");
    parse_test("(((0)))", "(( (( (( 0)))");
    parse_test(
        "if a b else if c d else e",
        "(else_ (if_ a b) (else_ (if_ c d) e))",
    );
    parse_test(
        "a = if 0 b else c = d",
        "(set a (set (else_ (if_ 0 b) c) d))",
    );
    parse_test("  \" asdfs sadfasdf \"  ", " asdfs sadfasdf ");

    run_program("src/test.iz");
    run_program("src/scratch.iz");
}

fn run_program(f: &str) -> Expr {
    let (p, mut ops) = preprocess(&std::fs::read_to_string(f).unwrap());
    interpret(&parse(tokenize(&p), &mut ops))
}

type Operators = HashMap<(String, bool), (String, bool, u8, bool, bool)>;

fn new_op(ops: &mut Operators, n: &str, p: bool, f: &str, bp: u8, ar: bool, ri: bool) {
    ops.insert((n.to_string(), p), (f.to_string(), true, bp, ar, ri));
}

#[derive(Clone, Copy, PartialEq)]
struct FilePos(usize, usize);

impl Debug for FilePos {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

fn preprocess(s: &str) -> (Vec<(char, FilePos)>, Operators) {
    let mut operators = HashMap::new();

    let mut cur = FilePos(1, 1);
    let program: Vec<(char, FilePos)> = s
        .chars()
        .map(|c| {
            let out = (c, cur);
            cur = match c {
                '\n' => FilePos(cur.0 + 1, 1),
                _ => FilePos(cur.0, cur.1 + 1),
            };
            out
        })
        .collect();

    let program = program
        .split(|(c, _)| c == &'#')
        .map(|s| {
            let parts: Vec<&[(char, FilePos)]> = s.splitn(2, |(c, _)| c == &'\n').collect();
            let c = parts[0].iter().map(|(c, _)| c).collect::<String>();
            let c: Vec<&str> = c.split_whitespace().collect();
            if c.get(0) == Some(&"op") {
                let bp = c[2].parse::<u8>().unwrap();
                operators.insert(
                    (c[4 + (c[3] != "p") as usize].to_string(), c[3] == "p"),
                    (c[1].to_string(), false, bp, c.len() == 7, c[3] == "r"),
                );
                parts[1].to_vec()
            } else if c.get(0) == Some(&"include") {
                let f = &c[1][1..c[1].len() - 1];
                let (mut p, ops) = preprocess(&std::fs::read_to_string(f).unwrap());
                operators.extend(ops);
                p.extend_from_slice(parts[1]);
                p
            } else {
                s.to_vec()
            }
        })
        .flatten()
        .collect::<Vec<(char, FilePos)>>();
    (program, operators)
}

#[derive(Clone)]
struct Token(String, FilePos, TokenType);

use TokenType::*;
#[derive(Clone, Debug, PartialEq)]
enum TokenType {
    Alphabetic,
    Whitespace,
    Numeric,
    Opener,
    Closer,
    Str,
    Other,
}

impl PartialEq for Token {
    fn eq(&self, t: &Token) -> bool {
        self.0 == t.0
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.0)
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}@{:?}", self.0, self.1)
    }
}

fn tokenize(s: &[(char, FilePos)]) -> Vec<Token> {
    s.iter()
        .map(|(c, u)| {
            Token(
                c.to_string(),
                *u,
                match c {
                    _ if c.is_alphabetic() || c == &'_' => Alphabetic,
                    _ if c.is_whitespace() => Whitespace,
                    _ if c.is_numeric() => Numeric,
                    '(' | '{' | '[' => Opener,
                    ')' | '}' | ']' => Closer,
                    _ => Other,
                },
            )
        })
        .fold(
            (false, vec![]),
            |(mut in_s, mut acc): (bool, Vec<Token>), t| {
                match &*t.0 {
                    "\"" => match in_s {
                        true => in_s = false,
                        false => {
                            in_s = true;
                            acc.push(Token("".to_string(), t.1, TokenType::Str));
                        }
                    },
                    _ => match acc.last_mut() {
                        Some(s) if in_s => s.0.push_str(&t.0),
                        Some(s) if s.2 == t.2 && s.2 != Opener && s.2 != Closer => {
                            s.0.push_str(&t.0)
                        }
                        _ => acc.push(t),
                    },
                }
                (in_s, acc)
            },
        )
        .1
        .into_iter()
        .filter(|t| t.2 != Whitespace)
        .collect()
}

#[derive(Clone, PartialEq)]
struct S(Token, Vec<S>);

impl Display for S {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.1.is_empty() {
            write!(f, "{}", self.0)
        } else {
            write!(f, "({}", self.0)?;
            for s in &self.1 {
                write!(f, " {}", s)?;
            }
            write!(f, ")")
        }
    }
}

impl Debug for S {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.1.is_empty() {
            write!(f, "{:?}", self.0)
        } else {
            write!(f, "({:?}", self.0)?;
            for s in &self.1 {
                write!(f, " {:?}", s)?;
            }
            write!(f, ")")
        }
    }
}

struct Context<'a>(usize, &'a [Token], &'a Operators);

impl<'a> Context<'a> {
    fn get(&self) -> &Token {
        &self.1[self.0]
    }

    fn next(&mut self) {
        self.0 += 1;
    }
}

fn parse(mut tokens: Vec<Token>, operators: &mut Operators) -> S {
    new_op(operators, "print", true, "print", 1, false, true);
    new_op(operators, "assert", true, "assert", 1, true, true);
    new_op(operators, "=", false, "set", 2, true, true);
    new_op(operators, ":", false, "type", 3, true, true);
    new_op(operators, "->", false, "func", 4, true, true);
    new_op(operators, "if", true, "if_", 5, true, true);
    new_op(operators, "else", false, "else_", 5, true, true);
    new_op(operators, "==", false, "eq", 9, true, false);
    new_op(operators, ">", false, "gt", 9, true, false);
    new_op(operators, "<", false, "lt", 9, true, false);
    new_op(operators, "::", false, "cons", 10, true, true);
    new_op(operators, "+", false, "add", 11, true, false);
    new_op(operators, "-", false, "sub", 11, true, false);
    new_op(operators, "*", false, "mul", 12, true, false);
    new_op(operators, "/", false, "div", 12, true, false);
    new_op(operators, "%", false, "mod", 12, true, false);
    new_op(operators, "^", false, "pow", 13, true, false);
    new_op(operators, "@", false, "call", 16, true, false);

    fn get_lhs(func: &str, pos: FilePos, i: bool, v: Vec<S>) -> S {
        match i {
            true => S(Token(func.to_string(), pos, Alphabetic), v),
            false => v.into_iter().fold(
                S(Token(func.to_string(), pos, Alphabetic), vec![]),
                |acc, x| S(Token("call".to_string(), pos, Other), vec![acc, x]),
            ),
        }
    }

    fn expr(c: &mut Context, rbp: u8) -> S {
        let mut op = c.get().clone();
        c.next();
        let mut lhs = match c.2.get(&(op.0.clone(), true)) {
            Some((f, i, bp, false, _)) => get_lhs(f, op.1, *i, vec![expr(c, *bp)]),
            Some((f, i, bp, _, _)) => get_lhs(f, op.1, *i, vec![expr(c, *bp), expr(c, *bp)]),
            None if op.2 == Opener => {
                let mut v = vec![];
                while c.get().2 != Closer {
                    v.push(expr(c, 0));
                }
                c.next();
                S(op, v)
            }
            None => S(op, vec![]),
        };
        while c.0 < c.1.len() {
            op = c.get().clone();
            match c.2.get(&(op.0, false)) {
                Some((f, i, bp, true, ri)) if bp > &rbp => {
                    c.next();
                    lhs = get_lhs(f, op.1, *i, vec![lhs, expr(c, bp - *ri as u8)]);
                }
                Some((f, i, bp, false, _)) if bp > &rbp => {
                    c.next();
                    lhs = get_lhs(f, op.1, *i, vec![lhs]);
                }
                _ => break,
            }
        }
        lhs
    }

    tokens.insert(0, Token("{".to_string(), FilePos(0, 0), Opener));
    tokens.push(Token("}".to_string(), FilePos(0, 0), Closer));
    expr(&mut Context(0, &tokens, &operators), 0)
}

use Expr::*;
#[derive(Clone, PartialEq)]
enum Expr {
    Unit,
    Var(Token),
    Bool(bool),
    Num(f64),
    Opt(Option<Box<Expr>>),
    Cons(Box<Expr>, Box<Expr>),
    Empty,
    Function(Token, S, HashMap<String, Expr>),
    Str(Token),
}

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Unit => write!(f, "{{}}"),
            Var(s) => write!(f, "v{:?}", s),
            Bool(b) => write!(f, "{}", b),
            Num(n) => write!(f, "{}", n),
            Opt(Some(e)) => write!(f, "Some({:?})", e),
            Opt(None) => write!(f, "None"),
            Cons(a, b) => {
                write!(f, "[{:?}", a)?;
                let mut e = b;
                while let Cons(a, b) = &**e {
                    write!(f, " {:?}", a)?;
                    e = &b;
                }
                write!(f, "]")
            }
            Empty => write!(f, "[]"),
            Function(x, y, _) => write!(f, "{:?}->{:?}", x, y),
            Expr::Str(s) => write!(f, "{}", s.0),
        }
    }
}

struct Env<'a>(&'a mut HashMap<String, Expr>, Option<&'a Env<'a>>, FilePos);

impl<'a> Env<'a> {
    fn get(&self, s: &str) -> Option<Expr> {
        match self.0.get(s) {
            Some(e) => Some(e.clone()),
            None => self.1.and_then(|c| c.get(s)),
        }
    }
}

impl<'a> Debug for Env<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:#?}@{:?} ", self.0, self.2)?;
        match self.1 {
            Some(c) => write!(f, "{:?}", c),
            None => Ok(()),
        }
    }
}

fn interpret(s: &S) -> Expr {
    fn interpret_(s: &S, c: &mut Env) -> Expr {
        if s.0 .2 == Numeric {
            return Num(s.0 .0.parse().unwrap());
        }

        match &*s.0 .0 {
            "(" => interpret_(&s.1[0], c),
            "{" => {
                let mut hash_map = HashMap::new();
                let mut child = Env(&mut hash_map, Some(c), s.0 .1);
                s.1.iter().fold(Unit, |_, s| interpret_(s, &mut child))
            }
            "[" => s.1.iter().rev().fold(Empty, |acc, s| {
                Cons(Box::new(interpret_(s, c)), Box::new(acc))
            }),
            "assert" => {
                let left = interpret_(&s.1[0], c);
                let right = interpret_(&s.1[1], c);
                if left != right {
                    panic!("{:?}!={:?} {:?}", left, right, c);
                }
                Unit
            }
            "print" => {
                println!("{:?}", interpret_(&s.1[0], c));
                Unit
            }
            "set" => {
                fn destruct(e: &Expr, a: Expr, c: &mut Env) -> bool {
                    match (e, a) {
                        (Var(s), a) => {
                            if s.0 != "_" {
                                c.0.insert(s.0.clone(), a);
                            }
                            true
                        }
                        (Cons(e, f), Cons(a, b)) => destruct(e, *a, c) && destruct(f, *b, c),
                        (e, a) => e == &a,
                    }
                }
                let a = interpret_(&s.1[1], c);
                Bool(destruct(
                    &interpret_(&s.1[0], &mut Env(&mut HashMap::new(), None, s.0 .1)),
                    a,
                    c,
                ))
            }
            "func" => Function(s.1[0].0.clone(), s.1[1].clone(), c.0.clone()), // todo
            "if_" => match interpret_(&s.1[0], c) {
                Bool(true) => Opt(Some(Box::new(interpret_(&s.1[1], c)))),
                Bool(false) => Opt(None),
                _ => panic!("{:?} {:?}", s, c),
            },
            "else_" => match interpret_(&s.1[0], c) {
                Opt(Some(a)) => *a,
                Opt(None) => interpret_(&s.1[1], c),
                _ => panic!("{:?} {:?}", s, c),
            },
            "eq" => Bool(interpret_(&s.1[0], c) == interpret_(&s.1[1], c)),
            "gt" => match (interpret_(&s.1[0], c), interpret_(&s.1[1], c)) {
                (Num(a), Num(b)) => Bool(a > b),
                _ => panic!("{:?} {:?}", s, c),
            },
            "lt" => match (interpret_(&s.1[0], c), interpret_(&s.1[1], c)) {
                (Num(a), Num(b)) => Bool(a < b),
                _ => panic!("{:?} {:?}", s, c),
            },
            "cons" => Cons(
                Box::new(interpret_(&s.1[0], c)),
                Box::new(interpret_(&s.1[1], c)),
            ),
            "add" => match (interpret_(&s.1[0], c), interpret_(&s.1[1], c)) {
                (Num(a), Num(b)) => Num(a + b),
                _ => panic!("{:?} {:?}", s, c),
            },
            "sub" => match (interpret_(&s.1[0], c), interpret_(&s.1[1], c)) {
                (Num(a), Num(b)) => Num(a - b),
                _ => panic!("{:?} {:?}", s, c),
            },
            "mul" => match (interpret_(&s.1[0], c), interpret_(&s.1[1], c)) {
                (Num(a), Num(b)) => Num(a * b),
                _ => panic!("{:?} {:?}", s, c),
            },
            "div" => match (interpret_(&s.1[0], c), interpret_(&s.1[1], c)) {
                (Num(a), Num(b)) => Num(a / b),
                _ => panic!("{:?} {:?}", s, c),
            },
            "mod" => match (interpret_(&s.1[0], c), interpret_(&s.1[1], c)) {
                (Num(a), Num(b)) => Num(a % b),
                _ => panic!("{:?} {:?}", s, c),
            },
            "pow" => match (interpret_(&s.1[0], c), interpret_(&s.1[1], c)) {
                (Num(a), Num(b)) => Num(a.powf(b)),
                _ => panic!("{:?} {:?}", s, c),
            },
            "call" => match interpret_(&s.1[0], c) {
                Var(Token(t, _, _)) if t == "Some" => Opt(Some(Box::new(interpret_(&s.1[1], c)))),
                Function(x, y, mut z) => {
                    z.insert(x.0, interpret_(&s.1[1], c));
                    interpret_(&y, &mut Env(&mut z, Some(c), s.0.1))
                }
                o => panic!("{:?} {:?} {:?}", o, s, c),
            },
            "false" => Bool(false),
            "true" => Bool(true),
            "None" => Opt(None),
            o => match s.0 .2 {
                TokenType::Str => Expr::Str(s.0.clone()),
                _ => match c.get(o) {
                    Some(e) => e,
                    None => Var(s.0.clone()),
                },
            },
        }
    }

    interpret_(s, &mut Env(&mut HashMap::new(), None, FilePos(0, 0)))
}
