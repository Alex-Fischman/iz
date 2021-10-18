use std::collections::HashMap;
use std::fmt::*;

// todo: std.iz, built-in enums, remove Opt, built-in structs, remove Cons, Empty, Tuple
// todo: types, :

fn main() {
    let program = std::fs::read_to_string("scratch.iz").unwrap();
    let (program, operators) = preprocess(&program);
    // println!("{:?}", &parse(tokenize(&program), &operators));
    let program = interpret(&parse(tokenize(&program), &operators));
    println!("{:?}", program);

    let parse_test = |a, b| {
        assert_eq!(
            format!("{:?}", parse(tokenize(a), &HashMap::new())),
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
    parse_test("- !b", "(neg (not b))");
    parse_test("! -b", "(not (neg b))");
    parse_test("-a + b", "(add (neg a) b)");
    parse_test("!a . b", "(not (call (call dot a) b))");
    parse_test("- -1 * 2", "(mul (neg (neg 1)) 2)");
    parse_test("- -f . g", "(neg (neg (call (call dot f) g)))");
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

    // todo: custom op tests, for prefix and infix
    let inter_test = |a, b| assert_eq!(interpret(&parse(tokenize(a), &HashMap::new())), b);
    inter_test("(1 + 2 * 3 - 4) ^ 2 / -9", Num(-1.0));
    inter_test("!false && 1 == 1 && true != false || 6 <= 2", Bool(true));
    inter_test("if false 1 else if true 2 else 3", Num(2.0));

    inter_test("a = 3 b = 5 a + b", Num(8.0));
    inter_test("{ a = 1 } a", Var(Token::new("a")));
    inter_test("c = 1 { a = 2 } c", Num(1.0));
    inter_test("a = 1 a = 2 a + 1", Num(3.0));

    inter_test("(x -> x)@1", Num(1.0));
    inter_test("(x -> x * x)@16", Num(256.0));
    inter_test("(x -> x)@1 x", Var(Token::new("x")));
    inter_test("id = x -> x id@3", Num(3.0));
    inter_test("(a -> b -> a)@3@4", Num(3.0));
    inter_test("square=x->x*x dot=x->f->f@x dot@3@square", Num(9.0));
    inter_test("square=x->x*x dot=x->f->f@x 3.square", Num(9.0)); // stdlib
    inter_test("{x=2 y->x+y}@3", Num(5.0));

    inter_test("fst=(a b)->a fst@(3 5)", Num(3.0)); // stdlib
    inter_test("snd=(a b)->b snd@(3 5)", Num(5.0)); // stdlib
    inter_test("thr=(a b c)->c thr@(1 2 3 4)", Num(3.0)); // stdlib
    inter_test("(a b c d)=(2 3 5 7 11) c", Num(5.0));
    inter_test("fac=n->if n<=1 1 else n*fac@(n-1) fac@4", Num(24.0));

    inter_test("if (a ?= 1) a else 2", Num(1.0));
    inter_test("if a ?= 5 1 a", Num(5.0)); // todo
    inter_test("if (a b) ?= (1 2) a else 3", Num(1.0));
    inter_test("if (a b) ?= (1 2) b else 3", Num(2.0));
    inter_test("if (a b) ?= (1) a else 3", Num(3.0));
    inter_test("if (a b)?=(1 2 3) b else 4", Num(2.0));
    inter_test("if (a) ?= (1 2) a else 3", Tuple(vec![Num(1.0), Num(2.0)]));
    inter_test(
        "dot = x -> f -> f@x
         map = f -> l -> if x::xs?=l f@x::xs.map@f else []
         get = i -> l -> if !(x::xs?=l) false else if i==0 x else xs.get@(i - 1)
         [1 2 3 4 5].map@(x -> x * x).get@3",
        Num(16.0),
    ); // stdlib
}

type Operators<'a> = HashMap<(&'a str, bool), (&'a str, bool, u8, bool, bool)>;
//                            name  is_prefix  func   is_impl bp 2_args assoc_right

fn preprocess(s: &str) -> (String, Operators) {
    let mut operators = HashMap::new();
    let program = s
        .split('#')
        .map(|s| {
            let (a, b) = s.split_at(s.find('\n').unwrap_or(0));
            let c: Vec<&str> = a.split_whitespace().collect();
            if c.get(0) == Some(&"op") {
                let i = 4 + (c[3] != "p") as usize;
                let r = c.len() == 7;
                let p = c[2].parse::<u8>().unwrap();
                operators.insert((c[i], c[3] == "p"), (c[1], false, p, r, c[3] == "r"));
                b
            } else {
                s
            }
        })
        .collect::<Vec<&str>>()
        .join("");
    (program, operators)
}

#[derive(Clone)]
struct Token(String, usize, TokenType);

use TokenType::*;
#[derive(Clone, PartialEq)]
enum TokenType {
    Alphabetic,
    Whitespace,
    Numeric,
    Opener,
    Closer,
    Other,
}

impl Token {
    fn new(s: &str) -> Token {
        Token(s.to_string(), 0, Other)
    }
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

struct ParseContext<'a>(usize, &'a [Token], &'a Operators<'a>);

impl<'a> ParseContext<'a> {
    fn get(&self) -> &Token {
        &self.1[self.0]
    }

    fn next(&mut self) -> &Token {
        self.0 += 1;
        &self.1[self.0 - 1]
    }
}

fn parse(mut tokens: Vec<Token>, operators: &Operators) -> S {
    let mut operators = operators.clone();
    operators.insert(("=", false), ("set", true, 1, true, true));
    operators.insert((":", false), ("type", true, 2, true, true));
    operators.insert(("->", false), ("func", true, 3, true, true));

    operators.insert(("if", true), ("if_", true, 4, true, true));
    operators.insert(("else", false), ("else_", true, 4, true, true));

    operators.insert(("?=", false), ("try", true, 5, true, true));

    operators.insert(("||", false), ("or", true, 6, true, false));
    operators.insert(("&&", false), ("and", true, 7, true, false));

    operators.insert(("==", false), ("eq", true, 8, true, false));
    operators.insert(("!=", false), ("ne", true, 8, true, false));
    operators.insert((">", false), ("gt", true, 8, true, false));
    operators.insert(("<", false), ("lt", true, 8, true, false));
    operators.insert((">=", false), ("ge", true, 8, true, false));
    operators.insert(("<=", false), ("le", true, 8, true, false));

    operators.insert(("::", false), ("cons", true, 9, true, true));

    operators.insert(("+", false), ("add", true, 10, true, false));
    operators.insert(("-", false), ("sub", true, 10, true, false));
    operators.insert(("*", false), ("mul", true, 11, true, false));
    operators.insert(("/", false), ("div", true, 11, true, false));
    operators.insert(("^", false), ("pow", true, 12, true, false));

    operators.insert(("-", true), ("neg", true, 13, false, false));
    operators.insert(("!", true), ("not", true, 13, false, false));

    operators.insert((".", false), ("dot", false, 14, true, false)); // stdlib
    operators.insert(("@", false), ("call", true, 15, true, false));

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
        let mut op = c.next().clone();
        let mut lhs = match c.2.get(&(&op.0, true)) {
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
            match c.2.get(&(&op.0, false)) {
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
    Tuple(Vec<Expr>),
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
            Var(s) => write!(f, "v{:?}", s),
            Bool(b) => write!(f, "b{}", b),
            Num(n) => write!(f, "n{}", n),
            Opt(Some(e)) => write!(f, "Some({:?})", e),
            Opt(None) => write!(f, "None"),
            Tuple(v) => {
                write!(f, "(")?;
                if let Some(a) = v.get(0) {
                    write!(f, "{:?}", a)?;
                }
                for a in &v[1..] {
                    write!(f, " {:?}", a)?;
                }
                write!(f, ")")
            }
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
            Closure(x, y, z) => write!(f, "{:?}->{:?}{:?}", x, y, z),
            Function(x, y) => write!(f, "{:?}-r>{:?}", x, y),
        }
    }
}

fn interpret(s: &S) -> Expr {
    fn destructure(s: &S, a: Expr, c: &mut InterContext) -> bool {
        fn destructure_(e: &Expr, a: Expr, c: &mut InterContext) -> bool {
            match (e, a) {
                (Var(s), a) => {
                    c.insert(s.0.clone(), a);
                    true
                }
                (Tuple(e), Tuple(a)) => (0..e.len()).all(|i| match (e.get(i), a.get(i)) {
                    (Some(e), Some(a)) => destructure_(&e, a.clone(), c),
                    _ => false,
                }),
                (Cons(e, f), Cons(a, b)) => {
                    destructure_(e, *a.clone(), c) && destructure_(f, *b.clone(), c)
                }
                (e, a) => e == &a,
            }
        }
        destructure_(&interpret_(s, &mut HashMap::new()), a, c)
    }

    fn call(x: &S, y: &S, a: Expr, c: &mut InterContext) -> Expr {
        if !destructure(x, a, c) {
            panic!("call closure {:?} {:?} {:?}", x, y, c);
        }
        interpret_(y, c)
    }

    fn interpret_(s: &S, c: &mut InterContext) -> Expr {
        if s.0 .2 == Numeric {
            return Num(s.0 .0.parse().unwrap());
        }

        let snapshot = c.clone();
        let mut args = s.1.iter().map(|s| interpret_(s, c));
        match &*s.0 .0 {
            "(" => match args.len() {
                1 => args.next().unwrap(),
                _ => Tuple(args.collect()),
            },
            "{" => match args.last() {
                None => Unit,
                Some(e) => {
                    *c = snapshot;
                    e
                }
            },
            "[" => args
                .rev()
                .fold(Empty, |acc, b| Cons(Box::new(b), Box::new(acc))),
            "set" => {
                let mut a = args.skip(1).next().unwrap();
                if let Closure(x, y, z) = &mut a {
                    z.insert(s.1[0].0 .0.to_string(), Function(x.clone(), y.clone()));
                }
                match destructure(&s.1[0], a.clone(), c) {
                    true => a,
                    false => panic!("set {:?}", a),
                }
            }
            "func" => Closure(s.1[0].clone(), s.1[1].clone(), c.clone()),
            "if_" => match args.next().unwrap() {
                Bool(true) => Opt(Some(Box::new(args.next().unwrap()))),
                Bool(false) => Opt(None),
                a => panic!("if_ {:?} {:?}", a, args),
            },
            "else_" => match args.next().unwrap() {
                Opt(Some(a)) => *a,
                Opt(None) => args.next().unwrap(),
                a => panic!("else_ {:?} {:?}", a, args),
            },
            // todo: merge with eq?
            "try" => Bool(destructure(&s.1[0], args.skip(1).next().unwrap(), c)),
            "or" => match (args.next().unwrap(), args.next().unwrap()) {
                (Bool(a), Bool(b)) => Bool(a || b),
                a => panic!("or {:?} {:?}", a, args),
            },
            "and" => match (args.next().unwrap(), args.next().unwrap()) {
                (Bool(a), Bool(b)) => Bool(a && b),
                a => panic!("and {:?} {:?}", a, args),
            },
            "eq" => Bool(args.next().unwrap() == args.next().unwrap()),
            "ne" => Bool(args.next().unwrap() != args.next().unwrap()),
            "gt" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Bool(a > b),
                a => panic!("gt {:?} {:?}", a, args),
            },
            "lt" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Bool(a < b),
                a => panic!("lt {:?} {:?}", a, args),
            },
            "ge" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Bool(a >= b),
                a => panic!("ge {:?} {:?}", a, args),
            },
            "le" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Bool(a <= b),
                a => panic!("le {:?} {:?}", a, args),
            },
            "cons" => Cons(
                Box::new(args.next().unwrap()),
                Box::new(args.next().unwrap()),
            ),
            "add" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Num(a + b),
                a => panic!("add {:?} {:?}", a, args),
            },
            "sub" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Num(a - b),
                a => panic!("sub {:?} {:?}", a, args),
            },
            "mul" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Num(a * b),
                a => panic!("mul {:?} {:?}", a, args),
            },
            "div" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Num(a / b),
                a => panic!("div {:?} {:?}", a, args),
            },
            "pow" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Num(a.powf(b)),
                a => panic!("pow {:?} {:?}", a, args),
            },
            "neg" => match args.next().unwrap() {
                Num(a) => Num(-a),
                a => panic!("neg {:?} {:?}", a, args),
            },
            "not" => match args.next().unwrap() {
                Bool(a) => Bool(!a),
                a => panic!("not {:?} {:?}", a, args),
            },
            "call" => match args.next().unwrap() {
                Var(Token(s, _, _)) if s == "Some" => Opt(Some(Box::new(args.next().unwrap()))),
                Closure(x, y, mut z) => call(&x, &y, args.next().unwrap(), &mut z),
                Function(x, y) => call(&x, &y, args.next().unwrap(), c),
                a => panic!("call {:?} {:?} {:?}", a, args.next().unwrap(), c),
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
