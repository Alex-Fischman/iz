use std::collections::HashMap;
type Operators<'a> = HashMap<(&'a str, bool), (&'a str, u8, Vec<Option<&'a str>>, bool)>;

pub fn preprocess(s: &str) -> (String, Operators) {
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
            }
            b
        })
        .collect::<Vec<&str>>()
        .join("");
    (program, operators)
}

#[derive(Clone)]
pub struct Token(pub String, pub usize, pub TokenType);

use TokenType::*;
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType {
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

pub fn tokenize(s: &str) -> Vec<Token> {
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

pub struct S(Token, Vec<S>);

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

pub fn parse(tokens: &Vec<Token>, operators: &Operators) -> S {
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

    operators.insert(("|", false), ("or", 9, vec![None], false));
    operators.insert(("&", false), ("and", 10, vec![None], false));

    operators.insert(("+", false), ("add", 11, vec![None], false));
    operators.insert(("-", false), ("sub", 11, vec![None], false));
    operators.insert(("*", false), ("mul", 12, vec![None], false));
    operators.insert(("/", false), ("div", 12, vec![None], false));
    operators.insert(("%", false), ("mod", 12, vec![None], false));
    operators.insert(("^", false), ("pow", 13, vec![None], false));

    operators.insert(("-", true), ("neg", 14, vec![None], false));
    operators.insert(("!", true), ("not", 14, vec![None], false));

    operators.insert((".", false), ("dot", 15, vec![None], false));
    operators.insert(("@", false), ("call", 16, vec![None], false));

    fn read_pattern(c: &mut Context, right: &Vec<Option<&str>>, bp: u8) -> Vec<S> {
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
                Token(func.to_string(), op.1, op.2),
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
                    lhs = S(Token(func.to_string(), op.1, op.2), v);
                }
                _ => break,
            }
        }
        lhs
    }
    let mut ts = tokens.clone();
    ts.insert(0, Token("{".to_string(), 0, Opener));
    ts.push(Token("}".to_string(), 0, Closer));
    expr(&mut Context(0, &ts, &operators), 0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_test() {
        fn test(a: &str, b: &str) {
            assert_eq!(
                format!("{:?}", parse(&tokenize(a), &HashMap::new())),
                format!("({{ {})", b)
            );
        }

        test("1", "1");
        test("a + b", "(add a b)");
        test("if a b", "(if_ a b)");
        test("a + b + c", "(add (add a b) c)");
        test("a = b = c", "(set a (set b c))");
        test("1 + f = g = h * 3", "(set (add 1 f) (set g (mul h 3)))");
        test("a + b * c * d + e", "(add (add a (mul (mul b c) d)) e)");
        test("- !b", "(neg (not b))");
        test("! -b", "(not (neg b))");
        test("-a + b", "(add (neg a) b)");
        test("!a . b", "(not (dot a b))");
        test("- -1 * 2", "(mul (neg (neg 1)) 2)");
        test("- -f . g", "(neg (neg (dot f g)))");
        test("(0(0))", "(( 0 (( 0))");
        test("((0)(0))", "(( (( 0) (( 0))");
        test("(((0)))", "(( (( (( 0)))");
        test("x[0][1]", "(idx (idx x 0) 1)");
        test(
            "if a b else if c d else e",
            "(else_ (if_ a b) (else_ (if_ c d) e))",
        );
        test(
            "a = if 0 b else c = d",
            "(set a (set (else_ (if_ 0 b) c) d))",
        );
    }
}
