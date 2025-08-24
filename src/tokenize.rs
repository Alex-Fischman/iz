use crate::*;

#[allow(missing_docs)]
#[derive(Debug, PartialEq)]
pub enum Side {
    Left,
    Right,
}

#[allow(missing_docs)]
#[derive(Debug, PartialEq)]
pub enum Bracket {
    Paren,
    Curly,
    Square,
}

enum Char {
    Bracket(Bracket, Side),
    DoubleQuote,
    SingleQuote,
    Backslash,
    Comment,
    Whitespace,
    Symbol,
}

fn char_type(c: char) -> Char {
    match c {
        '(' => Char::Bracket(Bracket::Paren, Side::Left),
        ')' => Char::Bracket(Bracket::Paren, Side::Right),
        '{' => Char::Bracket(Bracket::Curly, Side::Left),
        '}' => Char::Bracket(Bracket::Curly, Side::Right),
        '[' => Char::Bracket(Bracket::Square, Side::Left),
        ']' => Char::Bracket(Bracket::Square, Side::Right),
        '"' => Char::DoubleQuote,
        '\'' => Char::SingleQuote,
        '\\' => Char::Backslash,
        '#' => Char::Comment,
        c if c.is_whitespace() => Char::Whitespace,
        _ => Char::Symbol,
    }
}

/// The different types of tokens in an `iz` program.
#[derive(Debug, PartialEq)]
pub enum Token {
    /// One of `(`, `)`, `{`, `}`, `[`, or `]`.
    Bracket(Bracket, Side),
    /// A string, which has been parsed with escape codes.
    String(String),
    /// A character, which has been parsed with escape codes.
    Char(char),
    /// Everything else, which is whitespace-separated.
    Symbol,
}

impl Source {
    fn peek_char(&self, idx: usize) -> Option<char> {
        self.text[idx..].chars().next()
    }

    fn skip_whitespace(&self, idx: &mut usize) {
        while let Some(c) = self.peek_char(*idx) {
            match char_type(c) {
                Char::Comment => loop {
                    match self.peek_char(*idx) {
                        None | Some('\n') => break,
                        Some(c) => *idx += c.len_utf8(),
                    }
                },
                Char::Whitespace => *idx += c.len_utf8(),
                _ => break,
            }
        }
    }

    fn next_char(&self, idx: &mut usize) -> Option<char> {
        let c = self.peek_char(*idx)?;
        *idx += c.len_utf8();
        Some(c)
    }

    /// Get the token at byte position `idx` in the text.
    pub fn next_token(&self, mut idx: usize) -> Result<Option<(Span, Token)>> {
        let escape = |c, span: Span| match c {
            't' => Ok('\t'),
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            '"' => Ok('"'),
            '\'' => Ok('\''),
            '\\' => Ok('\\'),
            _ => err!(self, span, "expected escape character, got {c}"),
        };

        self.skip_whitespace(&mut idx);

        let mut span = Span { lo: idx, hi: idx };
        let Some(c) = self.next_char(&mut span.hi) else {
            return Ok(None);
        };

        let token = match char_type(c) {
            Char::Bracket(b, s) => Token::Bracket(b, s),
            Char::DoubleQuote => {
                let mut string = String::new();
                let mut in_escape = false;
                loop {
                    let Some(c) = self.next_char(&mut span.hi) else {
                        return err!(self, span, "expected \", got end of file");
                    };

                    match char_type(c) {
                        _ if in_escape => {
                            string.push(escape(c, span)?);
                            in_escape = false;
                        }
                        Char::DoubleQuote => break,
                        Char::Backslash => in_escape = true,
                        _ => string.push(c),
                    }
                }
                Token::String(string)
            }
            Char::SingleQuote => {
                let Some(c) = self.next_char(&mut span.hi) else {
                    return err!(self, span, "expected character, got end of file");
                };

                let token = match c {
                    '\\' => {
                        let Some(c) = self.next_char(&mut span.hi) else {
                            return err!(self, span, "expected escape character, got end of file");
                        };
                        escape(c, span)?
                    }
                    '\'' => {
                        return err!(self, span, "expected character, got closing \'");
                    }
                    c => c,
                };

                let Some(c) = self.next_char(&mut span.hi) else {
                    return err!(self, span, "expected \', got end of file");
                };
                let '\'' = c else {
                    return err!(self, span, "expected \', got {c}");
                };

                Token::Char(token)
            }
            Char::Backslash => return err!(self, span, "found a \\ outside of quotes"),
            Char::Comment | Char::Whitespace => unreachable!(),
            Char::Symbol => {
                while let Some(c) = self.peek_char(span.hi) {
                    match char_type(c) {
                        Char::Bracket(..) | Char::Whitespace => break,
                        Char::DoubleQuote | Char::SingleQuote | Char::Backslash | Char::Comment => {
                            span.hi += c.len_utf8();
                            return err!(self, span, "expected symbol, got {c}");
                        }
                        Char::Symbol => span.hi += c.len_utf8(),
                    }
                }

                Token::Symbol
            }
        };
        Ok(Some((span, token)))
    }
}

#[test]
fn test() -> Result<()> {
    fn tokenize(s: &str) -> Result<Vec<Token>> {
        let source = text!(s);
        let mut idx = 0;
        let mut tokens = Vec::new();
        while let Some((span, token)) = source.next_token(idx)? {
            idx = span.hi;
            tokens.push(token);
        }
        Ok(tokens)
    }

    assert_eq!(
        tokenize("(}[")?,
        vec![
            Token::Bracket(Bracket::Paren, Side::Left),
            Token::Bracket(Bracket::Curly, Side::Right),
            Token::Bracket(Bracket::Square, Side::Left),
        ]
    );

    assert_eq!(
        tokenize("\"1 2\t3\n\\\\\"")?,
        vec![Token::String(String::from("1 2\t3\n\\"))],
    );
    // TODO: failing string tests

    assert_eq!(tokenize("'c'")?, vec![Token::Char('c')]);
    assert_eq!(tokenize(" 'c' ")?, vec![Token::Char('c')]);
    assert_eq!(
        tokenize("'").unwrap_err(),
        "error at TEST:1:1: expected character, got end of file\n'"
    );
    assert_eq!(
        tokenize("''").unwrap_err(),
        "error at TEST:1:1: expected character, got closing '\n''"
    );
    assert_eq!(
        tokenize("'a").unwrap_err(),
        "error at TEST:1:1: expected ', got end of file\n'a"
    );
    assert_eq!(
        tokenize("'aa'").unwrap_err(),
        "error at TEST:1:1: expected ', got a\n'aa"
    );

    assert_eq!(
        tokenize("\\").unwrap_err(),
        "error at TEST:1:1: found a \\ outside of quotes\n\\"
    );

    assert_eq!(tokenize("#")?, vec![]);
    assert_eq!(tokenize("# a a")?, vec![]);
    assert_eq!(tokenize("# a a\n")?, vec![]);
    assert_eq!(tokenize("# a a\na")?, vec![Token::Symbol]);
    assert_eq!(tokenize("a # a a\na")?, vec![Token::Symbol, Token::Symbol]);
    assert_eq!(tokenize("'#'")?, vec![Token::Char('#')]);

    assert_eq!(tokenize("asdf")?, vec![Token::Symbol]);
    assert_eq!(tokenize("asdf fdsa")?, vec![Token::Symbol, Token::Symbol]);
    assert_eq!(
        tokenize("'c' asdf 'c'")?,
        vec![Token::Char('c'), Token::Symbol, Token::Char('c')]
    );
    assert_eq!(
        tokenize("asdf'").unwrap_err(),
        "error at TEST:1:1: expected symbol, got '\nasdf'"
    );

    assert_eq!(
        tokenize("f(x)")?,
        vec![
            Token::Symbol,
            Token::Bracket(Bracket::Paren, Side::Left),
            Token::Symbol,
            Token::Bracket(Bracket::Paren, Side::Right)
        ]
    );

    Ok(())
}
