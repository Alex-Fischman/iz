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
    Digit,
    Letter,
    Sigil,
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
        c if c.is_ascii_digit() => CharType::Digit,
        c if c.is_alphabetic() => CharType::Letter,
        '_' => CharType::Letter,
        _ => CharType::Sigil,
    }
}

#[derive(Debug, PartialEq)]
pub struct Number {
    // TODO
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
    /// A numeric literal, like `1` or `-2.5`.
    Number(Number),
    /// A name, like `x` or `name_with_underscores`.
    Identifier,
    /// An operator, like `+` or `.`.
    Operator,
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

    fn next_char(&self, span: &mut Span) -> Option<char> {
        let c = self.peek_char(span.hi)?;
        span.hi += c.len_utf8();
        Some(c)
    }

    fn next_escape(&self, span: &mut Span) -> Result<char> {
        debug_assert!(span.string(self).ends_with('\\'));
        match self.next_char(span) {
            Some('t') => Ok('\t'),
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('"') => Ok('"'),
            Some('\'') => Ok('\''),
            Some('\\') => Ok('\\'),
            Some(c) => err!(self, span, "expected escape character, got {c}"),
            None => err!(self, span, "expected escape character, got end of file"),
        }
    }

    /// Get the token at byte position `idx` in the text.
    pub fn next_token(&self, mut idx: usize) -> Result<Option<Token>> {
        self.skip_whitespace(&mut idx);

        let mut span = Span { lo: idx, hi: idx };
        let Some(c) = self.next_char(&mut span) else {
            return Ok(None);
        };

        let tag = match char_type(c) {
            CharType::Bracket(b, s) => TokenType::Bracket(b, s),
            CharType::DoubleQuote => {
                let mut string = String::new();
                loop {
                    match self.next_char(&mut span) {
                        Some('"') => break,
                        Some('\\') => string.push(self.next_escape(&mut span)?),
                        Some(c) => string.push(c),
                        None => return err!(self, span, "expected \", got end of file"),
                    }
                }
                TokenType::String(string)
            }
            CharType::SingleQuote => {
                let c = match self.next_char(&mut span) {
                    Some('\'') => return err!(self, span, "expected character, got closing \'"),
                    Some('\\') => self.next_escape(&mut span)?,
                    Some(c) => c,
                    None => return err!(self, span, "expected character, got end of file"),
                };
                match self.next_char(&mut span) {
                    Some('\'') => TokenType::Char(c),
                    Some(_) => return err!(self, span, "expected \', got {c}"),
                    None => return err!(self, span, "expected \', got end of file"),
                }
            }
            CharType::Backslash => return err!(self, span, "found a \\ outside of quotes"),
            CharType::Comment => {
                while let Some(c) = self.next_char(&mut span) {
                    if c == '\n' {
                        break;
                    }
                }
                TokenType::Comment
            }
            CharType::Whitespace => unreachable!("whitespace should have been skipped"),
            CharType::Digit => todo!("int and float literals"),
            CharType::Letter => {
                while let Some(c) = self.peek_char(span.hi) {
                    use CharType::*;
                    match char_type(c) {
                        Digit | Letter => span.hi += c.len_utf8(),
                        Bracket(..) | Whitespace | Sigil => break,
                        DoubleQuote | SingleQuote | Backslash | Comment => {
                            span.hi += c.len_utf8();
                            return err!(self, span, "expected letter, got {c}");
                        }
                    }
                }
                TokenType::Identifier
            }
            CharType::Sigil => {
                while let Some(c) = self.peek_char(span.hi) {
                    use CharType::*;
                    match char_type(c) {
                        Sigil => span.hi += c.len_utf8(),
                        Bracket(..) | Whitespace | Digit | Letter => break,
                        DoubleQuote | SingleQuote | Backslash | Comment => {
                            span.hi += c.len_utf8();
                            return err!(self, span, "expected letter, got {c}");
                        }
                    }
                }
                TokenType::Operator
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
        (Identifier) => {
            TokenType::Identifier
        };
        (Operator) => {
            TokenType::Operator
        };
    }

    macro_rules! test {
        ($name:ident, $s:literal, $(($span:expr, $($token:tt)*)),* $(,)?) => {
            #[test]
            fn $name() -> Result<()> {
                let source = text!($s);
                let tokens = collect_tokens(&source)?;
                assert_eq!(tokens, [$(($span, token!($($token)*)),)*]);
                Ok(())
            }
        };
        ($name:ident, $s:literal, $err:literal) => {
            #[test]
            fn $name() -> Result<()> {
                let source = text!($s);
                let error = collect_tokens(&source).unwrap_err();
                assert_eq!(error, $err);
                Ok(())
            }
        };
    }

    test! {
        brackets,
        "(}[",
        ("(", Left Paren),
        ("}", Right Curly),
        ("[", Left Square),
    }

    test! {
        string_escapes,
        "\"A B\tC\n\\\\\"",
        (
            "\"A B\tC\n\\\\\"",
            String "A B\tC\n\\"
        )
    }
    // TODO: failing string tests

    test! {char_basic, "'c'", ("'c'", Char 'c')}
    test! {char_spaces, " 'c' ", ("'c'", Char 'c')}
    test! {
        char_no_char,
        "'",
        "error at TEST:1:1: expected character, got end of file\n'"
    }
    test! {
        char_empty,
        "''",
        "error at TEST:1:1: expected character, got closing '\n''"
    }
    test! {
        char_no_end,
        "'a",
        "error at TEST:1:1: expected ', got end of file\n'a"
    }
    test! {
        char_two_chars,
        "'aa'",
        "error at TEST:1:1: expected ', got a\n'aa"
    }

    test! {
        bare_backslash,
        "\\",
        "error at TEST:1:1: found a \\ outside of quotes\n\\"
    }

    test! {empty_comment, "#", ("#", Comment)}
    test! {full_comment, "# a a", ("# a a", Comment)}
    test! {full_comment_n, "# a a\n", ("# a a\n", Comment)}
    test! {
        comment_symbol,
        "# a a\na",
        ("# a a\n", Comment),
        ("a", Identifier)
    }
    test! {
        symbol_comment_symbol,
        "a #\na",
        ("a", Identifier),
        ("#\n", Comment),
        ("a", Identifier)
    }
    test! {hashtag_char, "'#'", ("'#'", Char '#')}
    test! {hashtag_string, "\"#\"", ("\"#\"", String "#")}

    test! {symbol, "asdf", ("asdf", Identifier)}
    test! {symbols, "asdf fdsa", ("asdf", Identifier), ("fdsa", Identifier)}
    test! {
        char_symbol_char,
        "'c' asdf 'c'",
        ("'c'", Char 'c'),
        ("asdf", Identifier),
        ("'c'", Char 'c')
    }
    test! {
        symbol_into_char,
        "asdf'",
        "error at TEST:1:1: expected letter, got '\nasdf'"
    }

    test! {
        f_of_x,
        "f(x)",
        ("f", Identifier),
        ("(", Left Paren),
        ("x", Identifier),
        (")", Right Paren)
    }
    test! {
        x_plus_y,
        "x + y",
        ("x", Identifier),
        ("+", Operator),
        ("y", Identifier),
    }
    test! {
        a_dot_b,
        "a.b",
        ("a", Identifier),
        (".", Operator),
        ("b", Identifier),
    }
}
