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

enum CharType {
    Bracket(Bracket, Side),
    DoubleQuote,
    SingleQuote,
    Backslash,
    Comment,
    Whitespace,
    Symbol,
}

fn char_type(c: char) -> CharType {
    match c {
        '(' => CharType::Bracket(Bracket::Paren, Side::Left),
        ')' => CharType::Bracket(Bracket::Paren, Side::Right),
        '{' => CharType::Bracket(Bracket::Curly, Side::Left),
        '}' => CharType::Bracket(Bracket::Curly, Side::Right),
        '[' => CharType::Bracket(Bracket::Square, Side::Left),
        ']' => CharType::Bracket(Bracket::Square, Side::Right),
        '"' => CharType::DoubleQuote,
        '\'' => CharType::SingleQuote,
        '\\' => CharType::Backslash,
        '#' => CharType::Comment,
        c if c.is_whitespace() => CharType::Whitespace,
        _ => CharType::Symbol,
    }
}

/// The different types of tokens in an `iz` program.
#[derive(Debug, PartialEq)]
pub enum TokenType {
    /// One of `(`, `)`, `{`, `}`, `[`, or `]`.
    Bracket(Bracket, Side),
    /// A string, which has been parsed with escape codes.
    String(String),
    /// A character, which has been parsed with escape codes.
    Char(char),
    /// A comment (used for pretty-printing and preprocessing).
    Comment,
    /// Everything else, which is whitespace-separated.
    Symbol,
}

/// One token of an `iz` program. See `Source::next_token`.
#[derive(Debug, PartialEq)]
pub struct Token {
    pub span: Span,
    pub tag: TokenType,
}

impl Source {
    fn peek_char(&self, idx: usize) -> Option<char> {
        self.text[idx..].chars().next()
    }

    fn skip_whitespace(&self, idx: &mut usize) {
        while let Some(c) = self.peek_char(*idx) {
            if c.is_whitespace() {
                *idx += c.len_utf8();
            } else {
                break;
            }
        }
    }

    fn next_char(&self, idx: &mut usize) -> Option<char> {
        let c = self.peek_char(*idx)?;
        *idx += c.len_utf8();
        Some(c)
    }

    /// Get the token at byte position `idx` in the text.
    pub fn next_token(&self, mut idx: usize) -> Result<Option<Token>> {
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

        let tag = match char_type(c) {
            CharType::Bracket(b, s) => TokenType::Bracket(b, s),
            CharType::DoubleQuote => {
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
                        CharType::DoubleQuote => break,
                        CharType::Backslash => in_escape = true,
                        _ => string.push(c),
                    }
                }
                TokenType::String(string)
            }
            CharType::SingleQuote => {
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

                TokenType::Char(token)
            }
            CharType::Backslash => return err!(self, span, "found a \\ outside of quotes"),
            CharType::Comment => {
                while let Some(c) = self.next_char(&mut span.hi) {
                    if c == '\n' {
                        break;
                    }
                }
                TokenType::Comment
            }
            CharType::Whitespace => unreachable!("whitespace should have been skipped"),
            CharType::Symbol => {
                while let Some(c) = self.peek_char(span.hi) {
                    match char_type(c) {
                        CharType::Bracket(..) | CharType::Whitespace => break,
                        CharType::DoubleQuote
                        | CharType::SingleQuote
                        | CharType::Backslash
                        | CharType::Comment => {
                            span.hi += c.len_utf8();
                            return err!(self, span, "expected symbol, got {c}");
                        }
                        CharType::Symbol => span.hi += c.len_utf8(),
                    }
                }

                TokenType::Symbol
            }
        };

        Ok(Some(Token { span, tag }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn collect_tokens(source: &Source) -> Result<Vec<(&str, TokenType)>> {
        let mut out = Vec::new();
        let mut idx = 0;
        while let Some(Token { span, tag }) = source.next_token(idx)? {
            idx = span.hi;
            out.push((span.string(source), tag));
        }
        Ok(out)
    }

    macro_rules! token {
        (Left Paren) => {
            TokenType::Bracket(Bracket::Paren, Side::Left)
        };
        (Right Paren) => {
            TokenType::Bracket(Bracket::Paren, Side::Right)
        };
        (Left Curly) => {
            TokenType::Bracket(Bracket::Curly, Side::Left)
        };
        (Right Curly) => {
            TokenType::Bracket(Bracket::Curly, Side::Right)
        };
        (Left Square) => {
            TokenType::Bracket(Bracket::Square, Side::Left)
        };
        (Right Square) => {
            TokenType::Bracket(Bracket::Square, Side::Right)
        };
        (String $s:literal) => {
            TokenType::String(String::from($s))
        };
        (Char $c:literal) => {
            TokenType::Char($c)
        };
        (Comment) => {
            TokenType::Comment
        };
        (Symbol) => {
            TokenType::Symbol
        };
    }

    macro_rules! test {
        ($s:literal, $(($span:expr, $($token:tt)*)),* $(,)?) => {{
            let source = text!($s);
            let tokens = collect_tokens(&source)?;
            assert_eq!(tokens, [$(($span, token!($($token)*)),)*])
        }};
        ($s:literal, $err:literal) => {{
            let source = text!($s);
            let error = collect_tokens(&source).unwrap_err();
            assert_eq!(error, $err);
        }};
    }

    #[test]
    fn test() -> Result<()> {
        test!(
            "(}[",
            ("(", Left Paren),
            ("}", Right Curly),
            ("[", Left Square),
        );

        test!(
            "\"1 2\t3\n\\\\\"",
            (
                "\"1 2\t3\n\\\\\"",
                String "1 2\t3\n\\"
            )
        );
        // TODO: failing string tests

        test!("'c'", ("'c'", Char 'c'));
        test!(" 'c' ", ("'c'", Char 'c'));
        test!(
            "'",
            "error at TEST:1:1: expected character, got end of file\n'"
        );
        test!(
            "''",
            "error at TEST:1:1: expected character, got closing '\n''"
        );
        test!("'a", "error at TEST:1:1: expected ', got end of file\n'a");
        test!("'aa'", "error at TEST:1:1: expected ', got a\n'aa");

        test!("\\", "error at TEST:1:1: found a \\ outside of quotes\n\\");

        test!("#", ("#", Comment));
        test!("# a a", ("# a a", Comment));
        test!("# a a\n", ("# a a\n", Comment));
        test!("# a a\na", ("# a a\n", Comment), ("a", Symbol));
        test!("a #\na", ("a", Symbol), ("#\n", Comment), ("a", Symbol));
        test!("'#'", ("'#'", Char '#'));

        test!("asdf", ("asdf", Symbol));
        test!("asdf fdsa", ("asdf", Symbol), ("fdsa", Symbol));
        test!(
            "'c' asdf 'c'",
            ("'c'", Char 'c'),
            ("asdf", Symbol),
            ("'c'", Char 'c')
        );
        test!("asdf'", "error at TEST:1:1: expected symbol, got '\nasdf'");

        test!(
            "f(x)",
            ("f", Symbol),
            ("(", Left Paren),
            ("x", Symbol),
            (")", Right Paren)
        );

        Ok(())
    }
}
