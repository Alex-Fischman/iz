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
    Paren(Side),
    Curly(Side),
    Square(Side),
}

enum CharType {
    Bracket(Bracket),
    DoubleQuote,
    SingleQuote,
    Comment,
    Whitespace,
    Symbol,
}

fn char_type(c: char) -> CharType {
    match c {
        '(' => CharType::Bracket(Bracket::Paren(Side::Left)),
        ')' => CharType::Bracket(Bracket::Paren(Side::Right)),
        '{' => CharType::Bracket(Bracket::Curly(Side::Left)),
        '}' => CharType::Bracket(Bracket::Curly(Side::Right)),
        '[' => CharType::Bracket(Bracket::Square(Side::Left)),
        ']' => CharType::Bracket(Bracket::Square(Side::Right)),
        '"' => CharType::DoubleQuote,
        '\'' => CharType::SingleQuote,
        '#' => CharType::Comment,
        c if c.is_whitespace() => CharType::Whitespace,
        _ => CharType::Symbol,
    }
}

/// The different types of tokens in an `iz` program.
#[derive(Debug, PartialEq)]
pub enum Token {
    /// One of `(`, `)`, `{`, `}`, `[`, or `]`.
    Bracket(Bracket),
    /// A string, which has been parsed with escape codes.
    String(String),
    /// A character, which has been parsed with escape codes.
    Char(char),
    /// Everything else, which is whitespace-separated.
    Symbol,
}

impl Source {
    fn next_char(&self, idx: usize) -> Option<char> {
        self.text[idx..].chars().next()
    }

    fn skip_whitespace(&self, mut idx: usize) -> usize {
        while let Some(c) = self.next_char(idx) {
            match char_type(c) {
                CharType::Comment => loop {
                    match self.next_char(idx) {
                        None | Some('\n') => break,
                        Some(c) => idx += c.len_utf8(),
                    }
                },
                CharType::Whitespace => idx += c.len_utf8(),
                _ => break,
            }
        }
        idx
    }

    /// Get the token at byte position `idx` in the text.
    fn next_token(&self, idx: usize) -> Result<Option<(Span, Token)>> {
        let idx = self.skip_whitespace(idx);
        let Some(c) = self.next_char(idx) else {
            return Ok(None);
        };
        let mut span = Span {
            lo: idx,
            hi: idx + c.len_utf8(),
        };

        let token = match char_type(c) {
            CharType::Bracket(b) => Token::Bracket(b),
            CharType::DoubleQuote => todo!("Token::String"),
            CharType::SingleQuote => {
                let Some(c) = self.next_char(span.hi) else {
                    return err!(self, span, "expected character, got end of file");
                };
                span.hi += c.len_utf8();

                let token = Token::Char(match c {
                    '\\' => todo!(),
                    '\'' => {
                        return err!(self, span, "expected character, got closing \'");
                    }
                    c => c,
                });

                let Some(c) = self.next_char(span.hi) else {
                    return err!(self, span, "expected \', got end of file");
                };
                span.hi += c.len_utf8();
                let '\'' = c else {
                    return err!(self, span, "expected \', got {c}");
                };

                token
            }
            CharType::Comment | CharType::Whitespace => unreachable!(),
            CharType::Symbol => {
                while let Some(c) = self.next_char(span.hi) {
                    span.hi += c.len_utf8();

                    match char_type(c) {
                        CharType::Bracket(_) | CharType::Whitespace => break,
                        CharType::DoubleQuote | CharType::SingleQuote | CharType::Comment => {
                            return err!(self, span, "expected symbol, got {c}");
                        }
                        CharType::Symbol => {}
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

    assert_eq!(text!("'c'").next_token(0)?.unwrap().1, Token::Char('c'));
    assert_eq!(text!(" 'c' ").next_token(0)?.unwrap().1, Token::Char('c'));
    assert_eq!(
        text!("'").next_token(0).unwrap_err(),
        "error at TEST:1:1: expected character, got end of file\n'"
    );
    assert_eq!(
        text!("''").next_token(0).unwrap_err(),
        "error at TEST:1:1: expected character, got closing '\n''"
    );
    assert_eq!(
        text!("'a").next_token(0).unwrap_err(),
        "error at TEST:1:1: expected ', got end of file\n'a"
    );
    assert_eq!(
        text!("'aa'").next_token(0).unwrap_err(),
        "error at TEST:1:1: expected ', got a\n'aa"
    );

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

    Ok(())
}
