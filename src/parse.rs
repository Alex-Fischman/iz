use crate::token::Token;
use crate::token::TokenType;

pub fn parse(tokens: &Vec<Token>) -> S {
    // let _operators = vec![
    //     Operator::new("=", 2, 1, 0, false),
    //     Operator::new(":", 2, 1, 1, false),
    //     Operator::new("->", 2, 1, 2, false),
    //     Operator::new("if", 3, 0, 3, false),
    //     Operator::new("||", 2, 1, 4, true),
    //     Operator::new("&&", 2, 1, 5, true),
    //     Operator::new("==", 2, 1, 6, true),
    //     Operator::new("!=", 2, 1, 6, true),
    //     Operator::new("<", 2, 1, 6, true),
    //     Operator::new(">", 2, 1, 6, true),
    //     Operator::new("<=", 2, 1, 6, true),
    //     Operator::new(">=", 2, 1, 6, true),
    //     Operator::new("|", 2, 1, 7, true),
    //     Operator::new("&", 2, 1, 7, true),
    //     Operator::new("+", 2, 1, 7, true),
    //     Operator::new("-", 2, 1, 7, true),
    //     Operator::new("*", 2, 1, 8, true),
    //     Operator::new("/", 2, 1, 8, true),
    //     Operator::new("%", 2, 1, 8, true),
    //     Operator::new("^", 2, 1, 9, true),
    //     Operator::new("!", 1, 0, 10, true),
    //     Operator::new(".", 2, 1, 11, true),
    //     Operator::new("@", 2, 1, 12, true),
    // ];

    fn parse_expr(tokens: &mut Tokens, min_bp: u8) -> S {
        let mut lhs = match tokens.peek() {
            None => panic!("no tokens to start"),
            Some(t) if t.2 == TokenType::Opener => {
                let op = t.clone();
                tokens.next();
                let mut v = vec![];
                while let Some(_) = match tokens.peek() {
                    Some(t) if t.2 != TokenType::Closer => Some(t),
                    _ => None,
                } {
                    v.push(parse_expr(tokens, 0));
                }
                tokens.next();
                S(op, v)
            },
            Some(t) => {
                match prefix_binding_power(&t.0) {
                    Some(Bp(r_bp, bps)) => {
                        let op = t.clone();
                        tokens.next();

                        let mut v = vec![parse_expr(tokens, r_bp)];

                        bps.iter().for_each(|bp| {
                            if let Some(bp) = bp {
                                v.push(parse_expr(tokens, *bp));
                            } else {
                                tokens.next();
                            }
                        });

                        S(op, v)
                    }
                    None => {
                        let op = t.clone();
                        tokens.next();
                        S(op, vec![])
                    },
                }
            },
        };

        loop {
            match tokens.peek() {
                Some(t) => match infix_binding_power(&t.0) {
                    Some(Bp(l_bp, bps)) if l_bp > min_bp => {
                        let op = t.clone();

                        tokens.next();

                        let mut v = vec![lhs];

                        bps.iter().for_each(|bp| {
                            if let Some(bp) = bp {
                                v.push(parse_expr(tokens, *bp));
                            } else {
                                tokens.next();
                            }
                        });

                        lhs = S(op, v);
                    }
                    _ => break,
                },
                None => break,
            }
        }

        lhs
    }

    parse_expr(&mut Tokens(&tokens, 0), 0)
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

struct Tokens<'a>(&'a [Token], usize);

impl<'a> Tokens<'a> {
    fn peek(&self) -> Option<&Token> {
        self.0.get(self.1)
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = &'a Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.1 += 1;
        self.0.get(self.1 - 1)
    }
}

struct Bp(u8, Vec<Option<u8>>);

fn prefix_binding_power(op: &str) -> Option<Bp> {
    Some(match op {
        "-" => Bp(11, vec![]),
        "if" => Bp(4, vec![None, Some(0), None, Some(3)]),
        _ => return None,
    })
}

fn infix_binding_power(op: &str) -> Option<Bp> {
    Some(match op {
        "=" => Bp(2, vec![Some(1)]),
        "?" => Bp(4, vec![Some(0), None, Some(3)]),
        "==" => Bp(5, vec![Some(6)]),
        "+" | "-" => Bp(7, vec![Some(8)]),
        "*" | "/" => Bp(9, vec![Some(10)]),
        "!" => Bp(13, vec![]),
        "[" => Bp(15, vec![Some(0), None]),
        "." => Bp(18, vec![Some(17)]),
        _ => return None,
    })
}
