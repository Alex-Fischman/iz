type Bp = u8;
type Pattern = Vec<Option<String>>;
type Assoc = bool;

type Prefixes = std::collections::HashMap<String, (u8, Pattern)>;
type Affixes = std::collections::HashMap<String, (u8, Pattern, Assoc)>;

pub fn preprocess(s: &str) -> (String, Prefixes, Affixes) {
    let mut prefixes = std::collections::HashMap::new();
    let mut affixes = std::collections::HashMap::new();
    let mut program = "".to_string();
    let mut last = 0;
    for (start, _) in s.match_indices("#") {
        let end = start + s[start..].find('\n').unwrap();
        let comment: &Vec<&str> = &s[start..end].split(' ').filter(|s| !s.is_empty()).collect();
        if comment[0] == "#op" {
            let prec = comment[1].parse::<u8>().unwrap();
            let assoc = comment[2] == "r";
            if !assoc && comment[2] != "_" {
                let pattern = comment[3..].iter().map(|c| if c == &"_" {None} else {Some(c.to_string())}).collect();
                prefixes.insert(comment[2].to_string(), (prec, pattern));
            } else {
                let name_index = if assoc {4} else {3};
                let pattern = comment[name_index+1..].iter().map(|c| if c == &"_" {None} else {Some(c.to_string())}).collect();
                affixes.insert(comment[name_index].to_string(), (prec, pattern, assoc));
            }
        }
        program.push_str(&s[last..start]);
        last = end;
    }
    program.push_str(&s[last..]);
    (program, prefixes, affixes)
}

#[derive(Clone)]
pub struct Token(pub String, pub usize, pub TokenType);

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Alphabetic,
    Whitespace,
    Numeric,
    Opener,
    Closer,
    Other,
}

impl Token {
    fn catagorize(c: char) -> TokenType {
        match c {
            _ if c.is_alphabetic() || c == '_' => TokenType::Alphabetic,
            _ if c.is_whitespace() => TokenType::Whitespace,
            _ if c.is_numeric() => TokenType::Numeric,
            '(' | '{' | '[' => TokenType::Opener,
            ')' | '}' | ']' => TokenType::Closer,
            _ => TokenType::Other
        }
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
        .map(|(i, c)| Token(c.to_string(), i, Token::catagorize(c)))
        .fold(Vec::new(), |mut acc: Vec<Token>, t| {
            match acc.last_mut() {
                Some(s) if s.2 == t.2 && s.2 != TokenType::Opener && s.2 != TokenType::Closer => {
                    s.0.push_str(&t.0)
                }
                _ => acc.push(t),
            }
            acc
        })
        .into_iter()
        .filter(|t| t.2 != TokenType::Whitespace)
        .collect()
}

pub fn parse(tokens: &Vec<Token>, prefixes: &Prefixes, affixes: &Affixes) -> S {
    let mut prefixes = prefixes.clone();
    prefixes.insert("-".to_string(), (5, vec![]));
    prefixes.insert("if".to_string(), (2, vec![Some(";".to_string()), None, Some(";".to_string()), None]));

    let mut affixes = affixes.clone();
    affixes.insert("=".to_string(), (1, vec![None], true));
    affixes.insert("+".to_string(), (3, vec![None], false));
    affixes.insert("*".to_string(), (4, vec![None], false));
    affixes.insert("!".to_string(), (6, vec![], false));
    affixes.insert("[".to_string(), (7, vec![None, Some("]".to_string())], false));
    affixes.insert(".".to_string(), (8, vec![None], false));

    type Context<'a> = (&'a mut Tokens<'a>, &'a Prefixes, &'a Affixes);

    fn get_right(init: S, c: &mut Context, right: &Pattern, bp: Bp, op: Token) -> S {
        let mut v = vec![init];
        for i in right {
            match i {
                Some(s) => {
                    assert_eq!(&c.0.get().0, s);
                    c.0.advance();
                }
                None => v.push(expr(c, bp)),
            }
        }
        S(op, v)
    }

    fn expr(c: &mut Context, rbp: Bp) -> S {
        let mut op = c.0.get().clone();
        c.0.advance();
        let mut lhs = match c.1.get(&*op.0) {
            Some((bp, right)) => get_right(expr(c, *bp), c, &right, *bp, op),
            None if op.2 == TokenType::Opener => {
                let out = S(op, vec![expr(c, 0)]);
                assert_eq!(c.0.get().2, TokenType::Closer);
                c.0.advance();
                out
            }
            None => S(op, vec![]),
        };
        while c.0 .0 < c.0 .1.len() {
            op = c.0.get().clone();
            if let Some((bp, right, assoc)) = c.2.get(&*op.0) {
                if bp > &rbp {
                    c.0.advance();
                    lhs = get_right(lhs, c, &right, bp - *assoc as u8, op);
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        lhs
    }
    expr(&mut (&mut Tokens(0, &tokens), &prefixes, &affixes), 0)
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

struct Tokens<'a>(usize, &'a [Token]);

impl<'a> Tokens<'a> {
    fn get(&self) -> &Token {
        &self.1[self.0]
    }

    fn advance(&mut self) {
        self.0 += 1
    }
}
