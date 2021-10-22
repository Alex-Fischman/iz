use std::collections::HashMap;
use std::fmt::*;

fn main() {
    run_program("src/scratch.iz");

    let parse_test = |a, b| {
        let (p, mut ops) = preprocess(a);
        assert_eq!(
            format!("{:?}", parse(tokenize(&p), &mut ops)),
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

    run_program("src/test.iz");
}

fn run_program(f: &str) -> Expr {
    let (p, mut ops) = preprocess(&std::fs::read_to_string(f).unwrap());
    interpret(&parse(tokenize(&p), &mut ops))
}

type Operators = HashMap<(String, bool), (String, bool, u8, bool, bool)>;

fn new_op(ops: &mut Operators, n: &str, p: bool, f: &str, bp: u8, ar: bool, ri: bool) {
    ops.insert((n.to_string(), p), (f.to_string(), true, bp, ar, ri));
}

fn preprocess(s: &str) -> (String, Operators) {
    let mut operators = HashMap::new();
    let program = s
        .split('#')
        .map(|s| {
            let (a, b) = s.split_at(s.find('\n').unwrap_or(0));
            let c: Vec<&str> = a.split_whitespace().collect();
            if c.get(0) == Some(&"op") {
                let bp = c[2].parse::<u8>().unwrap();
                operators.insert(
                    (c[4 + (c[3] != "p") as usize].to_string(), c[3] == "p"),
                    (c[1].to_string(), false, bp, c.len() == 7, c[3] == "r"),
                );
                b.to_string()
            } else if c.get(0) == Some(&"include") {
                let (p, ops) = preprocess(&std::fs::read_to_string(c[1]).unwrap());
                operators.extend(ops);
                p + b
            } else {
                s.to_string()
            }
        })
        .collect::<Vec<String>>()
        .join("");
    (program, operators)
}

#[derive(Clone)]
struct Token(String, usize, TokenType);

use TokenType::*;
#[derive(Clone, Debug, PartialEq)]
enum TokenType {
    Alphabetic,
    Whitespace,
    Numeric,
    Opener,
    Closer,
    Other,
}

impl PartialEq for Token {
    fn eq(&self, t: &Token) -> bool {
        self.0 == t.0
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.0)
    }
}

fn tokenize(s: &str) -> Vec<Token> {
    s.chars()
        .enumerate()
        .map(|(i, c)| {
            Token(
                c.to_string(),
                i,
                match c {
                    _ if c.is_alphabetic() || c == '_' => Alphabetic,
                    _ if c.is_whitespace() => Whitespace,
                    _ if c.is_numeric() => Numeric,
                    '(' | '{' | '[' => Opener,
                    ')' | '}' | ']' => Closer,
                    _ => Other,
                },
            )
        })
        .fold(vec![], |mut acc: Vec<Token>, t| {
            match acc.last_mut() {
                Some(s) if s.2 == t.2 && s.2 != Opener && s.2 != Closer => s.0.push_str(&t.0),
                _ => acc.push(t),
            }
            acc
        })
        .into_iter()
        .filter(|t| t.2 != Whitespace)
        .collect()
}

#[derive(Clone, PartialEq)]
struct S(Token, Vec<S>);

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

struct ParseContext<'a>(usize, &'a [Token], &'a Operators);

impl<'a> ParseContext<'a> {
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
    new_op(operators, "?=", false, "try", 6, true, true);
    new_op(operators, "==", false, "eq", 9, true, false);
    new_op(operators, ">", false, "gt", 9, true, false);
    new_op(operators, "<", false, "lt", 9, true, false);
    new_op(operators, "::", false, "cons", 10, true, true);
    new_op(operators, "+", false, "add", 11, true, false);
    new_op(operators, "-", false, "sub", 11, true, false);
    new_op(operators, "*", false, "mul", 12, true, false);
    new_op(operators, "/", false, "div", 12, true, false);
    new_op(operators, "^", false, "pow", 13, true, false);
    new_op(operators, "@", false, "call", 16, true, false);

    fn get_lhs(func: &str, pos: usize, i: bool, v: Vec<S>) -> S {
        match i {
            true => S(Token(func.to_string(), pos, Alphabetic), v),
            false => v.into_iter().fold(
                S(Token(func.to_string(), pos, Alphabetic), vec![]),
                |acc, x| S(Token("call".to_string(), 0, Other), vec![acc, x]),
            ),
        }
    }

    fn expr(c: &mut ParseContext, rbp: u8) -> S {
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

    tokens.insert(0, Token("{".to_string(), 0, Opener));
    tokens.push(Token("}".to_string(), 0, Closer));
    expr(&mut ParseContext(0, &tokens, &operators), 0)
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
    Closure(S, S, InterContext),
    Function(S, S),
}

type InterContext = HashMap<String, Expr>;

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Unit => write!(f, "{{}}"),
            Var(s) => write!(f, "{:?}", s),
            Bool(b) => write!(f, "{}", b),
            Num(n) => write!(f, "{}", n),
            Opt(Some(e)) => write!(f, "Some({:?})", e),
            Opt(None) => write!(f, "None"),
            Cons(a, b) => {
                write!(f, " [{:?}", a)?;
                let mut e = b;
                while let Cons(a, b) = &**e {
                    write!(f, " {:?}", a)?;
                    e = &b;
                }
                write!(f, "]")
            }
            Empty => write!(f, "[]"),
            Closure(x, y, z) => write!(f, "{:?}->{:?}{:?}", x, y, z),
            Function(x, y) => write!(f, "{:?}->{:?}", x, y),
        }
    }
}

fn interpret(s: &S) -> Expr {
    fn destructure(s: &S, a: &Expr, c: &mut InterContext) -> bool {
        fn destructure_(e: &Expr, a: &Expr, c: &mut InterContext) -> bool {
            match (e, a) {
                (Var(s), a) => {
                    if s.0 != "_" {
                        c.insert(s.0.clone(), a.clone());
                    }
                    true
                }
                (Cons(e, f), Cons(a, b)) => destructure_(e, a, c) && destructure_(f, b, c),
                (e, a) => e == a,
            }
        }
        destructure_(&interpret_(s, &mut HashMap::new()), a, c)
    }

    fn search(tree: &S, string: &str) -> bool {
        tree.0 .0 == string || tree.1.iter().any(|s| search(s, string))
    }

    fn interpret_(s: &S, c: &mut InterContext) -> Expr {
        if s.0 .2 == Numeric {
            return Num(s.0 .0.parse().unwrap());
        }

        match &*s.0 .0 {
            "(" => match s.1.len() {
                1 => interpret_(&s.1[0], c),
                _ => panic!("tuple {:?}", s.1),
            },
            "{" => {
                let mut c = c.clone();
                s.1.iter().fold(Unit, |_, s| interpret_(s, &mut c))
            }
            "[" => s.1.iter().rev().fold(Empty, |acc, s| {
                Cons(Box::new(interpret_(s, c)), Box::new(acc))
            }),
            "assert" => {
                assert_eq!(interpret_(&s.1[0], c), interpret_(&s.1[1], c));
                Unit
            }
            "print" => {
                println!("{:?}", interpret_(&s.1[0], c));
                Unit
            }
            "set" => {
                let mut a = interpret_(&s.1[1], c);
                if let Closure(x, y, z) = &mut a {
                    z.insert(s.1[0].0 .0.to_string(), Function(x.clone(), y.clone()));
                }
                match destructure(&s.1[0], &a, c) {
                    true => a,
                    false => panic!("set {:?}", a),
                }
            }
            "func" => Closure(
                s.1[0].clone(),
                s.1[1].clone(),
                c.iter()
                    .filter(|(k, _)| search(s, k))
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect(),
            ),
            "if_" => match interpret_(&s.1[0], c) {
                Bool(true) => Opt(Some(Box::new(interpret_(&s.1[1], c)))),
                Bool(false) => Opt(None),
                _ => panic!("if_ {:?}", s.1),
            },
            "else_" => match interpret_(&s.1[0], c) {
                Opt(Some(a)) => *a,
                Opt(None) => interpret_(&s.1[1], c),
                _ => panic!("else_ {:?}", s.1),
            },
            "try" => Bool(destructure(&s.1[0], &interpret_(&s.1[1], c), c)),
            "eq" => Bool(interpret_(&s.1[0], c) == interpret_(&s.1[1], c)),
            "gt" => match (interpret_(&s.1[0], c), interpret_(&s.1[1], c)) {
                (Num(a), Num(b)) => Bool(a > b),
                _ => panic!("gt {:?}", s.1),
            },
            "lt" => match (interpret_(&s.1[0], c), interpret_(&s.1[1], c)) {
                (Num(a), Num(b)) => Bool(a < b),
                _ => panic!("lt {:?}", s.1),
            },
            "cons" => Cons(
                Box::new(interpret_(&s.1[0], c)),
                Box::new(interpret_(&s.1[1], c)),
            ),
            "add" => match (interpret_(&s.1[0], c), interpret_(&s.1[1], c)) {
                (Num(a), Num(b)) => Num(a + b),
                _ => panic!("add {:?}", s.1),
            },
            "sub" => match (interpret_(&s.1[0], c), interpret_(&s.1[1], c)) {
                (Num(a), Num(b)) => Num(a - b),
                _ => panic!("sub {:?}", s.1),
            },
            "mul" => match (interpret_(&s.1[0], c), interpret_(&s.1[1], c)) {
                (Num(a), Num(b)) => Num(a * b),
                _ => panic!("mul {:?}", s.1),
            },
            "div" => match (interpret_(&s.1[0], c), interpret_(&s.1[1], c)) {
                (Num(a), Num(b)) => Num(a / b),
                _ => panic!("div {:?}", s.1),
            },
            "pow" => match (interpret_(&s.1[0], c), interpret_(&s.1[1], c)) {
                (Num(a), Num(b)) => Num(a.powf(b)),
                _ => panic!("pow {:?}", s.1),
            },
            "call" => match interpret_(&s.1[0], c) {
                Var(Token(t, _, _)) if t == "Some" => Opt(Some(Box::new(interpret_(&s.1[1], c)))),
                Closure(x, y, mut z) => {
                    if !destructure(&x, &interpret_(&s.1[1], c), &mut z) {
                        panic!("call closure {:?} {:?} {:?}", x, y, z);
                    }
                    interpret_(&y, &mut z)
                }
                Function(x, y) => {
                    if !destructure(&x, &interpret_(&s.1[1], c), c) {
                        panic!("call function {:?} {:?}", x, y);
                    }
                    interpret_(&y, c)
                }
                _ => panic!("call {:?} {:?}", s.1, c),
            },
            "false" => Bool(false),
            "true" => Bool(true),
            "None" => Opt(None),
            o => match c.get(o) {
                Some(e) => e.clone(),
                None => Var(s.0.clone()),
            },
        }
    }

    interpret_(s, &mut HashMap::new())
}
