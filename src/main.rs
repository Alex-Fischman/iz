use std::collections::HashMap;
use std::fmt::*;

fn main() {
    let program = std::fs::read_to_string("scratch.iz").unwrap();
    let (program, operators) = preprocess(&program);
    let program = interpret(&parse(&tokenize(&program), &operators));
    println!("{:?}", program);

    let parse_test = |a, b| {
        assert_eq!(
            format!("{:?}", parse(&tokenize(a), &HashMap::new())),
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
    parse_test("x[0][1]", "(idx (idx x 0) 1)");
    parse_test(
        "if a b else if c d else e",
        "(else_ (if_ a b) (else_ (if_ c d) e))",
    );
    parse_test(
        "a = if 0 b else c = d",
        "(set a (set (else_ (if_ 0 b) c) d))",
    );

    // todo: custom op tests, for prefix and infix
    let inter_test = |a, b| assert_eq!(interpret(&parse(&tokenize(a), &HashMap::new())), b);
    inter_test("(1 + 2 * 3 - 4) ^ 2 / -9", Num(-1.0));
    inter_test("!false && 1 == 1 && true != false || 6 <= 2", Bool(true));
    inter_test("[0 1 2 3 4 5 6][3]", Num(3.0));
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
}

type Pattern<'a> = Vec<Option<&'a str>>;
//                            name  is_prefix  func   is_impl bp  pattern  assoc_right
type Operators<'a> = HashMap<(&'a str, bool), (&'a str, bool, u8, Pattern<'a>, bool)>;

fn preprocess(s: &str) -> (String, Operators) {
    let mut operators = HashMap::new();
    let program = s
        .split('#')
        .map(|s| {
            let (a, b) = s.split_at(s.find('\n').unwrap_or(0));
            let c: Vec<&str> = a.split_whitespace().collect();
            if c.get(0) == Some(&"op") {
                let i = 4 + (c[3] != "p") as usize;
                let r = c[i + 1..]
                    .iter()
                    .map(|c| Some(*c).filter(|c| c != &"_"))
                    .collect();
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

fn parse(tokens: &[Token], operators: &Operators) -> S {
    let mut operators = operators.clone();
    operators.insert(("=", false), ("set", true, 1, vec![None], true));
    operators.insert((":", false), ("type", true, 2, vec![None], true));
    operators.insert(("->", false), ("Closure", true, 3, vec![None], true));

    operators.insert(("if", true), ("if_", true, 4, vec![None, None], false));
    operators.insert(("else", false), ("else_", true, 4, vec![None], true));

    operators.insert(("||", false), ("or", true, 5, vec![None], false));
    operators.insert(("&&", false), ("and", true, 6, vec![None], false));

    operators.insert(("==", false), ("eq", true, 7, vec![None], false));
    operators.insert(("!=", false), ("ne", true, 7, vec![None], false));
    operators.insert((">", false), ("gt", true, 7, vec![None], false));
    operators.insert(("<", false), ("lt", true, 7, vec![None], false));
    operators.insert((">=", false), ("ge", true, 7, vec![None], false));
    operators.insert(("<=", false), ("le", true, 7, vec![None], false));

    operators.insert(("[", false), ("idx", true, 8, vec![None, Some("]")], false));

    operators.insert(("+", false), ("add", true, 11, vec![None], false));
    operators.insert(("-", false), ("sub", true, 11, vec![None], false));
    operators.insert(("*", false), ("mul", true, 12, vec![None], false));
    operators.insert(("/", false), ("div", true, 12, vec![None], false));
    operators.insert(("^", false), ("pow", true, 13, vec![None], false));

    operators.insert(("-", true), ("neg", true, 14, vec![None], false));
    operators.insert(("!", true), ("not", true, 14, vec![None], false));

    operators.insert((".", false), ("dot", false, 15, vec![None], false)); // stdlib
    operators.insert(("@", false), ("call", true, 16, vec![None], false));

    fn pattern(c: &mut ParseContext, right: &Pattern, bp: u8) -> Vec<S> {
        right
            .iter()
            .filter_map(|i| match i {
                Some(s) => {
                    assert_eq!(&c.next().0, s);
                    None
                }
                None => Some(expr(c, bp)),
            })
            .collect()
    }

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
        let op = c.next().clone();
        let mut lhs = match c.2.get(&(&op.0, true)) {
            Some((func, i, bp, right, _)) => get_lhs(func, op.1, *i, pattern(c, &right, *bp)),
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
            let op = c.get().clone();
            match c.2.get(&(&op.0, false)) {
                Some((func, i, bp, right, assoc)) if bp > &rbp => {
                    c.next();
                    let mut v = pattern(c, &right, bp - *assoc as u8);
                    v.insert(0, lhs);
                    lhs = get_lhs(func, op.1, *i, v);
                }
                _ => break,
            }
        }
        lhs
    }
    let mut ts = tokens.to_vec();
    ts.insert(0, Token("{".to_string(), 0, Opener));
    ts.push(Token("}".to_string(), 0, Closer));
    expr(&mut ParseContext(0, &ts, &operators), 0)
}

use Expr::*;
#[derive(Clone, PartialEq)]
enum Expr {
    Unit,
    Var(Token),
    Bool(bool),
    Num(f64),
    Opt(Box<Option<Expr>>),
    Tuple(Vec<Expr>),
    List(Vec<Expr>),
    Closure(S, S, InterContext),
    Function(S, S),
}

type InterContext = HashMap<String, Expr>;

fn write_list<A: Debug, I: Iterator<Item = A>>(f: &mut Formatter, mut i: I, s: &str) -> Result {
    write!(f, "{}", &s[..1])?;
    if let Some(a) = i.next() {
        write!(f, "{:?}", a)?;
    }
    for a in i {
        write!(f, "{}{:?}", &s[1..s.len() - 1], a)?;
    }
    write!(f, "{}", &s[s.len() - 1..])
}

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Unit => write!(f, "()"),
            Var(s) => write!(f, "v{:?}", s),
            Bool(b) => write!(f, "b{}", b),
            Num(n) => write!(f, "n{}", n),
            Opt(o) => match &**o {
                Some(e) => write!(f, "Some({:?})", e),
                None => write!(f, "None"),
            },
            Tuple(v) => write_list(f, v.iter(), "( )"),
            List(v) => write_list(f, v.iter(), "[ ]"),
            Closure(x, y, z) => write!(f, "{:?}->{:?}{:?}", x, y, z),
            Function(x, y) => write!(f, "{:?}-r>{:?}", x, y),
        }
    }
}

// todo: types and :
fn interpret(s: &S) -> Expr {
    fn destructure(s: &S, c: &mut InterContext, a: Expr) {
        fn destructure_(e: &Expr, c: &mut InterContext, a: Expr) {
            match e {
                Var(s) => {
                    c.insert(s.0.clone(), a);
                }
                Tuple(e) => match a {
                    Tuple(a) => (0..e.len()).for_each(|i| destructure_(&e[i], c, a[i].clone())),
                    _ => panic!("destructure tuple {:?}", a),
                },
                // todo: destructure lists
                _ => panic!("destructure {:?} {:?} {:?}", e, a, c),
            };
        }
        destructure_(&interpret_(s, &mut HashMap::new()), c, a)
    }

    fn interpret_(s: &S, c: &mut InterContext) -> Expr {
        if s.0 .2 == Numeric {
            return Num(s.0 .0.parse().unwrap());
        }

        let mut argc = c.clone();
        let mut args = s.1.iter().map(|s| interpret_(s, &mut argc));
        match &*s.0 .0 {
            "(" => match args.len() {
                1 => args.next().unwrap(),
                _ => Tuple(args.collect()),
            },
            "{" => match args.last() {
                Some(e) => e,
                None => Unit,
            },
            "[" => List(args.collect()),
            "set" => {
                let mut a = args.skip(1).next().unwrap();
                if let Closure(x, y, z) = &mut a {
                    // i don't think we need to destructure here
                    z.insert(s.1[0].0 .0.to_string(), Function(x.clone(), y.clone()));
                }
                destructure(&s.1[0], c, a.clone());
                a
            }
            "Closure" => Closure(s.1[0].clone(), s.1[1].clone(), c.clone()),
            "if_" => match args.next().unwrap() {
                Bool(true) => Opt(Box::new(Some(args.next().unwrap()))),
                Bool(false) => Opt(Box::new(None)),
                _ => panic!("if_ {:?}", args),
            },
            "else_" => match args.next().unwrap() {
                Opt(a) => match *a {
                    Some(a) => a,
                    None => args.next().unwrap(),
                },
                _ => panic!("else_ {:?}", args),
            },
            "or" => match (args.next().unwrap(), args.next().unwrap()) {
                (Bool(a), Bool(b)) => Bool(a || b),
                _ => panic!("or {:?}", args),
            },
            "and" => match (args.next().unwrap(), args.next().unwrap()) {
                (Bool(a), Bool(b)) => Bool(a && b),
                _ => panic!("and {:?}", args),
            },
            "eq" => Bool(args.next().unwrap() == args.next().unwrap()),
            "ne" => Bool(args.next().unwrap() != args.next().unwrap()),
            "gt" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Bool(a > b),
                _ => panic!("gt {:?}", args),
            },
            "lt" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Bool(a < b),
                _ => panic!("lt {:?}", args),
            },
            "ge" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Bool(a >= b),
                _ => panic!("ge {:?}", args),
            },
            "le" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Bool(a <= b),
                _ => panic!("le {:?}", args),
            },
            "idx" => match (args.next().unwrap(), args.next().unwrap()) {
                (List(mut a), Num(i)) => a.remove(i as usize),
                _ => panic!("idx {:?}", args),
            },
            "add" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Num(a + b),
                _ => panic!("add {:?}", args),
            },
            "sub" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Num(a - b),
                _ => panic!("sub {:?}", args),
            },
            "mul" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Num(a * b),
                _ => panic!("mul {:?}", args),
            },
            "div" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Num(a / b),
                _ => panic!("div {:?}", args),
            },
            "pow" => match (args.next().unwrap(), args.next().unwrap()) {
                (Num(a), Num(b)) => Num(a.powf(b)),
                _ => panic!("pow {:?}", args),
            },
            "neg" => match args.next().unwrap() {
                Num(a) => Num(-a),
                _ => panic!("neg {:?}", args),
            },
            "not" => match args.next().unwrap() {
                Bool(a) => Bool(!a),
                _ => panic!("not {:?}", args),
            },
            "call" => match args.next().unwrap() {
                Closure(x, y, mut z) => {
                    destructure(&x, &mut z, args.next().unwrap());
                    interpret_(&y, &mut z)
                }
                Function(x, y) => {
                    let a = args.next().unwrap();
                    destructure(&x, &mut argc, a);
                    interpret_(&y, &mut argc)
                }
                a => panic!("call {:?} {:?} {:?}", a, args.next().unwrap(), argc),
            },
            "false" => Bool(false),
            "true" => Bool(true),
            o => match argc.get(o) {
                Some(e) => e.clone(),
                None => Var(s.0.clone()),
            },
        }
    }

    interpret_(s, &mut HashMap::new())
}
