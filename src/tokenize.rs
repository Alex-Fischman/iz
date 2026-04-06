use crate::*;

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Side {
    Left,
    Right,
}

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq)]
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

/// The different types of tokens in an `iz` program.
#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    /// One of `(`, `)`, `{`, `}`, `[`, or `]`.
    Bracket(Bracket, Side),
    /// A string, which has been parsed with escape codes.
    String(String),
    /// A character, which has been parsed with escape codes.
    Char(char),
    /// A comment (used for pretty-printing and preprocessing).
    Comment,
    /// A numeric literal, like `1` or `0x1F`.
    Number(u64),
    /// A name, like `x` or `name_with_underscores`.
    Identifier,
    /// An operator, like `+` or `.`.
    Operator,
}

/// One token of an `iz` program. See `State::tokenize`.
#[derive(Debug, PartialEq)]
pub struct Token {
    /// The location of this token.
    pub span: Span,
    /// The type of this token.
    pub tag: TokenType,
}

impl Span {
    fn peek_char(&self, state: &State) -> Option<char> {
        state[self.src].text[self.hi..].chars().next()
    }

    fn next_char(&mut self, state: &State) -> Option<char> {
        let c = self.peek_char(state)?;
        self.hi += c.len_utf8();
        Some(c)
    }

    fn next_escape(&mut self, state: &State) -> Result<char> {
        debug_assert!(self.string(state).ends_with('\\'));
        match self.next_char(state) {
            Some('t') => Ok('\t'),
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('"') => Ok('"'),
            Some('\'') => Ok('\''),
            Some('\\') => Ok('\\'),
            Some(c) => err!(state, self, "expected escape character, got {c}"),
            None => err!(state, self, "expected escape character, got end of file"),
        }
    }
}

impl State {
    /// Add all of the `Token`s in one `Source` to the `State`.
    pub fn tokenize(
        &mut self,
        src: SourceId,
        tokens: TableId<Token>,
        parent: NodeId,
    ) -> Result<()> {
        use CharType::*;

        let mut idx = 0;
        loop {
            let mut span = Span {
                src,
                lo: idx,
                hi: idx,
            };

            while let Some(c) = span.peek_char(self) {
                if c.is_whitespace() {
                    span.lo += c.len_utf8();
                    span.hi += c.len_utf8();
                } else {
                    break;
                }
            }

            let Some(c) = span.next_char(self) else { break };

            let tag = match char_type(c) {
                CharType::Bracket(b, s) => TokenType::Bracket(b, s),
                CharType::DoubleQuote => {
                    let mut string = String::new();
                    loop {
                        match span.next_char(self) {
                            Some('"') => break,
                            Some('\\') => string.push(span.next_escape(self)?),
                            Some(c) => string.push(c),
                            None => return err!(self, span, "expected \", got end of file"),
                        }
                    }
                    TokenType::String(string)
                }
                CharType::SingleQuote => {
                    let c = match span.next_char(self) {
                        Some('\'') => {
                            return err!(self, span, "expected character, got closing \'");
                        }
                        Some('\\') => span.next_escape(self)?,
                        Some(c) => c,
                        None => return err!(self, span, "expected character, got end of file"),
                    };
                    match span.next_char(self) {
                        Some('\'') => TokenType::Char(c),
                        Some(c) => return err!(self, span, "expected \', got {c}"),
                        None => return err!(self, span, "expected \', got end of file"),
                    }
                }
                CharType::Backslash => return err!(self, span, "found a \\ outside of quotes"),
                CharType::Comment => {
                    while let Some(c) = span.next_char(self) {
                        if c == '\n' {
                            break;
                        }
                    }
                    TokenType::Comment
                }
                CharType::Whitespace => unreachable!("whitespace should have been skipped"),
                CharType::Digit => {
                    let radix = match (c, span.peek_char(self)) {
                        ('0', Some('b')) => 2,
                        ('0', Some('x')) => 16,
                        _ => 10,
                    };
                    if radix == 10 {
                        // start the loop at the first digit
                        span.hi -= c.len_utf8();
                    } else {
                        // consume the `b` or the `x`
                        span.hi += c.len_utf8();
                    }

                    let mut value = 0;
                    while let Some(c) = span.peek_char(self) {
                        match char_type(c) {
                            Bracket(..) | Whitespace | Sigil => break,
                            _ => span.hi += c.len_utf8(),
                        }
                        if c == '_' {
                            continue;
                        }

                        let digit = match char_type(c) {
                            Digit => u64::from(u32::from(c) - u32::from('0')),
                            Letter => u64::from(u32::from(c) - u32::from('A') + 10),
                            _ => return err!(self, span, "expected digit, got {c}"),
                        };
                        if digit >= radix {
                            return err!(self, span, "base {radix} does not contain digit {c}");
                        }

                        value = (value * radix) + digit;
                    }

                    TokenType::Number(value)
                }
                CharType::Letter => {
                    while let Some(c) = span.peek_char(self) {
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
                    while let Some(c) = span.peek_char(self) {
                        match char_type(c) {
                            Sigil => span.hi += c.len_utf8(),
                            Bracket(..) | Whitespace | Digit | Letter => break,
                            DoubleQuote | SingleQuote | Backslash | Comment => {
                                span.hi += c.len_utf8();
                                return err!(self, span, "expected sigil, got {c}");
                            }
                        }
                    }
                    TokenType::Operator
                }
            };

            let node = self.add_node(parent);
            self[tokens].insert(node, Token { span, tag });

            idx = span.hi;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn collect_tokens(state: &mut State, src: SourceId) -> Result<Vec<(String, TokenType)>> {
        let tokens = state.add_table::<Token>();
        let () = state.tokenize(src, tokens, State::ROOT)?;

        let mut postorder = state.postorder(State::ROOT);
        let mut out = Vec::new();
        while let Some(node) = postorder.next(state)? {
            match state[tokens].get(node) {
                None => assert!(node == State::ROOT),
                Some(Token { span, tag }) => out.push((span.string(state).to_owned(), tag.clone())),
            }
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
        (Number $n:literal) => {
            TokenType::Number($n)
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
                let (mut state, src) = State::new(source);
                let tokens = collect_tokens(&mut state, src)?;
                assert_eq!(tokens, [$((String::from($span), token!($($token)*)),)*]);
                Ok(())
            }
        };
        ($name:ident, $s:literal, $err:literal) => {
            #[test]
            fn $name() -> Result<()> {
                let source = text!($s);
                let (mut state, src) = State::new(source);
                let error = collect_tokens(&mut state, src).unwrap_err();
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
        "error at text!:1:1: expected character, got end of file\n'"
    }
    test! {
        char_empty,
        "''",
        "error at text!:1:1: expected character, got closing '\n''"
    }
    test! {
        char_no_end,
        "'a",
        "error at text!:1:1: expected ', got end of file\n'a"
    }
    test! {
        char_two_chars,
        "'aa'",
        "error at text!:1:1: expected ', got a\n'aa"
    }

    test! {
        bare_backslash,
        "\\",
        "error at text!:1:1: found a \\ outside of quotes\n\\"
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

    test! {thirty, "30", ("30", Number 30)}
    test! {thirty_hex, "0x1E", ("0x1E", Number 30)}
    test! {thirty_binary, "0b0001_1110", ("0b0001_1110", Number 30)}
    test! {binary_two, "0b2", "error at text!:1:1: base 2 does not contain digit 2\n0b2"}
    test! {binary_a, "0bA", "error at text!:1:1: base 2 does not contain digit A\n0bA"}

    test! {ident, "asdf", ("asdf", Identifier)}
    test! {idents, "asdf fdsa", ("asdf", Identifier), ("fdsa", Identifier)}
    test! {
        char_ident_char,
        "'c' asdf 'c'",
        ("'c'", Char 'c'),
        ("asdf", Identifier),
        ("'c'", Char 'c')
    }
    test! {
        ident_into_char,
        "asdf'",
        "error at text!:1:1: expected letter, got '\nasdf'"
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
