use std::collections::HashMap;
use std::fmt::*;

fn main() {
    let parse_test = |a, b| {
        let (p, mut ops) = preprocess(a);
        assert_eq!(
            format!("{}", parse(tokenize(p), &mut ops)),
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
    parse_test("  \" asdfs sadfasdf \"  ", "\" asdfs sadfasdf \"");

    run_program("src/test.iz");
    run_program("src/scratch.iz");
}

fn run_program(f: &str) {
    let (p, mut ops) = preprocess(&std::fs::read_to_string(f).unwrap());
    interpret(&parse(tokenize(p), &mut ops));
}

type Operators = HashMap<(String, bool), (String, bool, u8, bool, bool)>;

#[derive(Clone)]
struct Token(String, usize, usize, TokenType);

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
        self.0 == t.0 && self.3 == t.3
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

fn preprocess(s: &str) -> (Vec<Token>, Operators) {
    let mut row = 1;
    let mut col = 1;
    let mut program = Vec::with_capacity(s.len());
    for c in s.chars() {
        let t = match c {
            c if c.is_alphabetic() || c == '_' => Alphabetic,
            c if c.is_whitespace() => Whitespace,
            c if c.is_numeric() => Numeric,
            '(' | '{' | '[' => Opener,
            ')' | '}' | ']' => Closer,
            '\"' => TokenType::Str,
            _ => Other,
        };
        program.push(Token(c.to_string(), row, col, t));
        if c == '\n' {
            row += 1;
            col = 1
        } else {
            col += 1;
        }
    }
    let mut operators = HashMap::new();
    let program = program
        .split(|t| t.0 == "#")
        .map(|s| {
            if let Some(i) = s.iter().position(|t| t.0 == "\n") {
                let (command, rest) = s.split_at(i);
                let c = command.iter().map(|t| &*t.0).collect::<String>();
                let c: Vec<&str> = c.split_whitespace().collect();
                match c.get(0) {
                    Some(&"op") => {
                        let bp = c[2].parse::<u8>().unwrap();
                        operators.insert(
                            (c[4 + (c[3] != "p") as usize].to_string(), c[3] == "p"),
                            (c[1].to_string(), false, bp, c.len() == 7, c[3] == "r"),
                        );
                        rest.to_vec()
                    }
                    Some(&"include") => {
                        let f = &c[1][1..c[1].len() - 1];
                        let (mut p, ops) = preprocess(&std::fs::read_to_string(f).unwrap());
                        operators.extend(ops);
                        p.extend(rest.to_vec());
                        p
                    }
                    Some(c) => panic!("unrecognized command {:?}", c),
                    None => unreachable!(),
                }
            } else {
                s.to_vec()
            }
        })
        .flatten()
        .collect();
    (program, operators)
}

fn tokenize(s: Vec<Token>) -> Vec<Token> {
    s.into_iter()
        .fold(
            (false, vec![]),
            |(mut in_s, mut acc): (bool, Vec<Token>), t| {
                match &*t.0 {
                    "\"" => match in_s {
                        true => {
                            in_s = false;
                            acc.last_mut().unwrap().0.push('\"');
                        }
                        false => {
                            in_s = true;
                            acc.push(Token("\"".to_string(), t.1, t.2, t.3));
                        }
                    },
                    _ => match acc.last_mut() {
                        Some(s) if in_s => s.0.push_str(&t.0),
                        Some(s) if s.3 == t.3 && s.3 != Opener && s.3 != Closer => {
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
        .filter(|t| t.3 != Whitespace)
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
    let mut new_op = |n: &str, p: bool, f: &str, bp: u8, ar: bool, ri: bool| {
        operators.insert((n.to_string(), p), (f.to_string(), true, bp, ar, ri))
    };
    new_op("print", true, "print", 1, false, true);
    new_op("assert", true, "assert", 1, true, true);
    new_op("=", false, "set", 2, true, true);
    new_op(":", false, "type", 3, true, true);
    new_op("->", false, "func", 4, true, true);
    new_op("if", true, "if_", 5, true, true);
    new_op("else", false, "else_", 5, true, true);
    new_op("==", false, "eq", 9, true, false);
    new_op(">", false, "gt", 9, true, false);
    new_op("<", false, "lt", 9, true, false);
    new_op("::", false, "cons", 10, true, true);
    new_op("+", false, "add", 11, true, false);
    new_op("-", false, "sub", 11, true, false);
    new_op("*", false, "mul", 12, true, false);
    new_op("/", false, "div", 12, true, false);
    new_op("%", false, "mod", 12, true, false);
    new_op("^", false, "pow", 13, true, false);
    new_op("@", false, "call", 16, true, false);

    fn get_lhs(func: &str, row: usize, col: usize, i: bool, v: Vec<S>) -> S {
        let f = Token(func.to_string(), row, col, Other);
        match i {
            true => S(f, v),
            false => v.into_iter().fold(S(f, vec![]), |acc, x| {
                S(Token("call".to_string(), 0, 0, Other), vec![acc, x])
            }),
        }
    }

    fn expr(c: &mut Context, rbp: u8) -> S {
        let mut op = c.get().clone();
        c.next();
        let mut lhs = match c.2.get(&(op.0.clone(), true)) {
            Some((f, i, bp, false, _)) => get_lhs(f, op.1, op.2, *i, vec![expr(c, *bp)]),
            Some((f, i, bp, _, _)) => get_lhs(f, op.1, op.2, *i, vec![expr(c, *bp), expr(c, *bp)]),
            None if op.3 == Opener => {
                let mut v = vec![];
                while c.get().3 != Closer {
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
                    lhs = get_lhs(f, op.1, op.2, *i, vec![lhs, expr(c, bp - *ri as u8)]);
                }
                Some((f, i, bp, false, _)) if bp > &rbp => {
                    c.next();
                    lhs = get_lhs(f, op.1, op.2, *i, vec![lhs]);
                }
                _ => break,
            }
        }
        lhs
    }

    tokens.insert(0, Token("{".to_string(), 0, 0, Opener));
    tokens.push(Token("}".to_string(), 0, 0, Closer));
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

struct Env<'a>(HashMap<String, Expr>, Option<&'a Env<'a>>);

impl<'a> Env<'a> {
    fn new() -> Env<'a> {
        Env(HashMap::new(), None)
    }

    fn get(&self, s: &str) -> Option<Expr> {
        match self.0.get(s) {
            Some(e) => Some(e.clone()),
            None => self.1.and_then(|c| c.get(s)),
        }
    }

    fn child(&self) -> Env {
        Env(HashMap::new(), Some(self))
    }
}

impl<'a> Debug for Env<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:#?} ", self.0)?;
        match self.1 {
            Some(c) => write!(f, "{:?}", c),
            None => Ok(()),
        }
    }
}

fn interpret(s: &S) -> Expr {
    fn interpret_(s: &S, c: &mut Env) -> Expr {
        if s.0 .3 == Numeric {
            return Num(s.0 .0.parse().unwrap());
        }

        let get_lhs = || interpret_(&s.1[0], &mut c.child());
        let get_rhs = || interpret_(&s.1[1], &mut c.child());
        let get_num_bin_op = |f: fn(f64, f64) -> f64| match (get_lhs(), get_rhs()) {
            (Num(a), Num(b)) => Num(f(a, b)),
            _ => panic!("{:?} {:?}", s, c),
        };

        match &*s.0 .0 {
            "(" => interpret_(&s.1[0], c),
            "{" => {
                let mut child = c.child();
                s.1.iter().fold(Unit, |_, s| interpret_(s, &mut child))
            }
            "[" => s.1.iter().rev().fold(Empty, |acc, s| {
                Cons(Box::new(interpret_(s, &mut c.child())), Box::new(acc))
            }),
            "assert" => {
                if get_lhs() != get_rhs() {
                    panic!("{:?} != {:?} {:?}", get_lhs(), get_rhs(), c);
                }
                Unit
            }
            "print" => {
                println!("{:?}", get_lhs());
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
                Bool(destruct(
                    &interpret_(&s.1[0], &mut Env::new()),
                    get_rhs(),
                    c,
                ))
            }
            "func" => Function(s.1[0].0.clone(), s.1[1].clone(), c.0.clone()),
            "if_" => match interpret_(&s.1[0], c) {
                Bool(true) => Opt(Some(Box::new(interpret_(&s.1[1], &mut c.child())))),
                Bool(false) => Opt(None),
                _ => panic!("{:?} {:?}", s, c),
            },
            "else_" => match get_lhs() {
                Opt(Some(a)) => *a,
                Opt(None) => get_rhs(),
                _ => panic!("{:?} {:?}", s, c),
            },
            "eq" => Bool(get_lhs() == get_rhs()),
            "gt" => match (get_lhs(), get_rhs()) {
                (Num(a), Num(b)) => Bool(a > b),
                _ => panic!("{:?} {:?}", s, c),
            },
            "lt" => match (get_lhs(), get_rhs()) {
                (Num(a), Num(b)) => Bool(a < b),
                _ => panic!("{:?} {:?}", s, c),
            },
            "cons" => Cons(Box::new(get_lhs()), Box::new(get_rhs())),
            "add" => get_num_bin_op(|a, b| a + b),
            "sub" => get_num_bin_op(|a, b| a - b),
            "mul" => get_num_bin_op(|a, b| a * b),
            "div" => get_num_bin_op(|a, b| a / b),
            "mod" => get_num_bin_op(|a, b| a % b),
            "pow" => get_num_bin_op(|a, b| a.powf(b)),
            "call" => match get_lhs() {
                Var(Token(t, _, _, _)) if t == "Some" => Opt(Some(Box::new(get_rhs()))),
                Function(x, y, mut z) => {
                    z.insert(x.0, get_rhs());
                    interpret_(&y, &mut Env(z, Some(c)))
                }
                o => panic!("{:?} {:?} {:?}", o, s, c),
            },
            "false" => Bool(false),
            "true" => Bool(true),
            "None" => Opt(None),
            o => match s.0 .3 {
                TokenType::Str => Expr::Str(s.0.clone()),
                _ => match c.get(o) {
                    Some(e) => e,
                    None => Var(s.0.clone()),
                },
            },
        }
    }

    interpret_(s, &mut Env::new())
}
