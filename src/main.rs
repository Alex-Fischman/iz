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
    parse_test("!a . b", "(not (dot a b))");
    parse_test("- -1 * 2", "(mul (neg (neg 1)) 2)");
    parse_test("- -f . g", "(neg (neg (dot f g)))");
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

    let inter_test = |a, b| assert_eq!(interpret(&parse(&tokenize(a), &HashMap::new())), b);
    inter_test("(1 + 2 * 3 - 4) ^ 2 / -9", Num(-1.0));
    inter_test("!false && 1 == 1 && true != false || 6 <= 2", Bool(true));
    inter_test("[0 1 2 3 4 5 6][3]", Num(3.0));
    inter_test("if false 1 else if true 2 else 3", Num(2.0));
}

use std::collections::HashMap;
type Operators<'a> = HashMap<(&'a str, bool), (&'a str, u8, Vec<Option<&'a str>>, bool)>;

fn preprocess(s: &str) -> (String, Operators) {
    let mut operators = HashMap::new();
    let program = s
        .split('#')
        .map(|s| {
            let (a, b) = s.split_at(s.find('\n').unwrap_or(s.len()));
            let c: Vec<&str> = a.split_whitespace().collect();
            if c[0] == "op" {
                let i = 4 + (c[3] != "p") as usize;
                let p = c[i + 1..].iter().map(|c| Some(*c).filter(|c| c != &"_"));
                operators.insert(
                    (c[i], c[3] == "p"),
                    (c[1], c[2].parse::<u8>().unwrap(), p.collect(), c[3] == "r"),
                );
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
#[derive(Clone, Copy, Debug, PartialEq)]
enum TokenType {
    Alphabetic,
    Whitespace,
    Numeric,
    Opener,
    Closer,
    Other,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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
        .fold(Vec::new(), |mut acc: Vec<Token>, t| {
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

struct S(Token, Vec<S>);

impl std::fmt::Debug for S {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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

struct Context<'a>(usize, &'a [Token], &'a Operators<'a>);

impl<'a> Context<'a> {
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
    operators.insert(("=", false), ("set", 1, vec![None], true));
    operators.insert((":", false), ("type", 2, vec![None], true));
    operators.insert(("->", false), ("func", 3, vec![None], true));

    operators.insert(("if", true), ("if_", 4, vec![None, None], false));
    operators.insert(("else", false), ("else_", 4, vec![None], true));

    operators.insert(("||", false), ("or", 5, vec![None], false));
    operators.insert(("&&", false), ("and", 6, vec![None], false));

    operators.insert(("==", false), ("eq", 7, vec![None], false));
    operators.insert(("!=", false), ("ne", 7, vec![None], false));
    operators.insert((">", false), ("gt", 7, vec![None], false));
    operators.insert(("<", false), ("lt", 7, vec![None], false));
    operators.insert((">=", false), ("ge", 7, vec![None], false));
    operators.insert(("<=", false), ("le", 7, vec![None], false));

    operators.insert(("[", false), ("idx", 8, vec![None, Some("]")], false));

    operators.insert(("+", false), ("add", 11, vec![None], false));
    operators.insert(("-", false), ("sub", 11, vec![None], false));
    operators.insert(("*", false), ("mul", 12, vec![None], false));
    operators.insert(("/", false), ("div", 12, vec![None], false));
    operators.insert(("^", false), ("pow", 13, vec![None], false));

    operators.insert(("-", true), ("neg", 14, vec![None], false));
    operators.insert(("!", true), ("not", 14, vec![None], false));

    operators.insert((".", false), ("dot", 15, vec![None], false));
    operators.insert(("@", false), ("call", 16, vec![None], false));

    fn read_pattern(c: &mut Context, right: &[Option<&str>], bp: u8) -> Vec<S> {
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

    fn expr(c: &mut Context, rbp: u8) -> S {
        let op = c.next().clone();
        let mut lhs = match c.2.get(&(&op.0, true)) {
            Some((func, bp, right, _)) => S(
                Token(func.to_string(), op.1, Alphabetic),
                read_pattern(c, &right, *bp),
            ),
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
                Some((func, bp, right, assoc)) if bp > &rbp => {
                    c.next();
                    let mut v = read_pattern(c, &right, bp - *assoc as u8);
                    v.insert(0, lhs);
                    lhs = S(Token(func.to_string(), op.1, Alphabetic), v);
                }
                _ => break,
            }
        }
        lhs
    }
    let mut ts = tokens.to_vec();
    ts.insert(0, Token("{".to_string(), 0, Opener));
    ts.push(Token("}".to_string(), 0, Closer));
    expr(&mut Context(0, &ts, &operators), 0)
}

use Expr::*;
#[derive(Clone, PartialEq, PartialOrd)]
enum Expr {
    Bool(bool),
    Num(f64),
    Opt(Box<Option<Expr>>),
    List(Vec<Expr>),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Bool(b) => write!(f, "{}", b),
            Num(n) => write!(f, "{}", n),
            Opt(o) => match &**o {
                Some(e) => write!(f, "Some({:?})", e),
                None => write!(f, "None"),
            },
            List(v) => {
                for e in v {
                    write!(f, "{:?} ", e)?;
                }
                Ok(())
            }
        }
    }
}

// TODO: = : -> . @
fn interpret(s: &S) -> Expr {
    let args: Vec<Expr> = s.1.iter().map(|s| interpret(s)).collect();

    let bin_num_op = |f: &dyn Fn(f64, f64) -> f64| match args[..] {
        [Num(a), Num(b)] => Num(f(a, b)),
        _ => panic!("{:?}", s.0),
    };

    match s.0 .2 {
        Numeric => Num(s.0 .0.parse().unwrap()),
        Opener => match args.len() {
            0 => todo!(),
            1 => args[0].clone(),
            _ => List(args),
        },
        _ => match &*s.0 .0 {
            "if_" => match &args[..] {
                [Bool(a), b] => match a {
                    true => Opt(Box::new(Some(b.clone()))),
                    false => Opt(Box::new(None)),
                },
                _ => panic!("if_"),
            },
            "else_" => match &args[..] {
                [Opt(a), b] => match &**a {
                    Some(c) => c.clone(),
                    None => b.clone(),
                },
                _ => panic!("else_"),
            },
            "or" => match args[..] {
                [Bool(a), Bool(b)] => Bool(a || b),
                _ => panic!("or"),
            },
            "and" => match args[..] {
                [Bool(a), Bool(b)] => Bool(a && b),
                _ => panic!("and"),
            },
            "eq" => Bool(args[0] == args[1]),
            "ne" => Bool(args[0] != args[1]),
            "gt" => Bool(args[0] > args[1]),
            "lt" => Bool(args[0] < args[1]),
            "ge" => Bool(args[0] >= args[1]),
            "le" => Bool(args[0] <= args[1]),
            "idx" => match &args[..] {
                [List(a), Num(b)] => a[*b as usize].clone(),
                _ => panic!("idx"),
            },
            "add" => bin_num_op(&std::ops::Add::add),
            "sub" => bin_num_op(&std::ops::Sub::sub),
            "mul" => bin_num_op(&std::ops::Mul::mul),
            "div" => bin_num_op(&std::ops::Div::div),
            "pow" => bin_num_op(&f64::powf),
            "neg" => match args[..] {
                [Num(a)] => Num(-a),
                _ => panic!("neg"),
            },
            "not" => match args[..] {
                [Bool(a)] => Bool(!a),
                _ => panic!("not"),
            },
            "false" => Bool(false),
            "true" => Bool(true),
            _ => panic!("unimplemented: {:?} {:?}", s.0, args),
        },
    }
}
