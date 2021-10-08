type Bp = u8;
type Pattern = Vec<Option<String>>;
type Assoc = bool;

type Prefixes = std::collections::HashMap<String, (String, Bp, Pattern)>;
type Affixes = std::collections::HashMap<String, (String, Bp, Pattern, Assoc)>;

pub fn preprocess(s: &str) -> (String, Prefixes, Affixes) {
    let mut prefixes = std::collections::HashMap::new();
    let mut affixes = std::collections::HashMap::new();
    let mut program = "".to_string();
    let mut last = 0;
    for (i, _) in s.match_indices("#") {
        let j = i + s[i..].find('\n').unwrap_or(s.len() - i);
        let mut comment = s[i..j].split_whitespace();
        if comment.next().unwrap_or("") == "#op" {
            let func = comment.next().unwrap().to_string();
            let bp = comment.next().unwrap().parse::<u8>().unwrap();
            let id = comment.next().unwrap();
            let name = comment.next().unwrap().to_string();
            let pattern = comment
                .map(|c| Some(c.to_string()).filter(|c| c != &"_"))
                .collect();
            if id == "p" {
                prefixes.insert(name, (func, bp, pattern));
            } else {
                affixes.insert(name, (func, bp, pattern, id == "r"));
            }
        }
        program.push_str(&s[last..i]);
        last = j;
    }
    program.push_str(&s[last..]);
    (program, prefixes, affixes)
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

impl Token {
    fn replace(&self, s: &str) -> Token {
        Token(s.to_string(), self.1, self.2)
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn tokenize(s: &str) -> Vec<Token> {
    s.chars()
        .enumerate()
        .map(|(i, c)| Token(c.to_string(), i, match c {
            _ if c.is_alphabetic() || c == '_' => Alphabetic,
            _ if c.is_whitespace() => Whitespace,
            _ if c.is_numeric() => Numeric,
            '(' | '{' | '[' => Opener,
            ')' | '}' | ']' => Closer,
            _ => Other,
        }))
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
        if self.0 .0.is_empty() {
            for s in &self.1 {
                write!(f, "{:?}", s)?;
            }
            Ok(())
        } else if self.1.is_empty() {
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

struct Tokens<'a>(usize, &'a [Token]);

impl<'a> Tokens<'a> {
    fn get(&self) -> &Token {
        &self.1[self.0]
    }

    fn advance(&mut self) {
        self.0 += 1
    }
}

pub fn parse(tokens: &Vec<Token>, prefixes: &Prefixes, affixes: &Affixes) -> S {
    let mut prefixes = prefixes.clone();
    prefixes.insert("-".to_string(), ("neg".to_string(), 5, vec![None]));
    prefixes.insert(
        "if".to_string(),
        (
            "if_".to_string(),
            2,
            vec![
                None,
                Some(";".to_string()),
                None,
                Some(";".to_string()),
                None,
            ],
        ),
    );

    let mut affixes = affixes.clone();
    affixes.insert("=".to_string(), ("eq".to_string(), 1, vec![None], true));
    affixes.insert("+".to_string(), ("add".to_string(), 3, vec![None], false));
    affixes.insert("-".to_string(), ("sub".to_string(), 3, vec![None], false));
    affixes.insert("*".to_string(), ("mul".to_string(), 4, vec![None], false));
    affixes.insert("!".to_string(), ("fac".to_string(), 6, vec![], false));
    affixes.insert(
        "[".to_string(),
        (
            "idx".to_string(),
            7,
            vec![None, Some("]".to_string())],
            false,
        ),
    );
    affixes.insert(".".to_string(), ("dot".to_string(), 8, vec![None], false));

    type Context<'a> = (&'a mut Tokens<'a>, &'a Prefixes, &'a Affixes);

    fn read_pattern(c: &mut Context, right: &Pattern, bp: Bp) -> Vec<S> {
        let mut v = vec![];
        for i in right {
            match i {
                Some(s) => {
                    assert_eq!(&c.0.get().0, s);
                    c.0.advance();
                }
                None => v.push(expr(c, bp)),
            }
        }
        v
    }

    fn expr(c: &mut Context, rbp: Bp) -> S {
        let mut op = c.0.get().clone();
        c.0.advance();
        let mut lhs = match c.1.get(&*op.0) {
            Some((func, bp, right)) => S(op.replace(func), read_pattern(c, &right, *bp)),
            None if op.2 == Opener => {
                let mut v = vec![];
                while c.0.get().2 != Closer {
                    v.push(expr(c, 0));
                }
                c.0.advance();
                S(op, v)
            }
            None => S(op, vec![]),
        };
        while c.0 .0 < c.0 .1.len() {
            op = c.0.get().clone();
            if let Some((func, bp, right, assoc)) = c.2.get(&*op.0) {
                if bp > &rbp {
                    c.0.advance();
                    let mut v = read_pattern(c, &right, bp - *assoc as u8);
                    v.insert(0, lhs);
                    lhs = S(op.replace(func), v);
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        lhs
    }
    let mut ts = vec![Token("".to_string(), 0, Opener); tokens.len() + 2];
    let l = ts.len() - 1;
    ts[l] = Token("END".to_string(), 0, Closer);
    ts[1..l].clone_from_slice(&tokens);
    expr(&mut (&mut Tokens(0, &ts), &prefixes, &affixes), 0)
}
