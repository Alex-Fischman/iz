use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Clone)]
pub struct Token {
    pub source: std::rc::Rc<Source>,
    pub range: std::ops::Range<usize>,
}

#[derive(PartialEq)]
pub struct Source {
    pub name: String,
    pub text: String,
}

impl Token {
    pub fn as_str(&self) -> &str {
        &self.source.text[self.range.clone()]
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let mut row = 1;
        let mut col = 1;
        for (i, c) in self.source.text.char_indices() {
            match (i, c) {
                (i, _) if i == self.range.start => break,
                (_, '\n') => {
                    row += 1;
                    col = 1;
                }
                _ => col += 1,
            }
        }
        write!(f, "{:?} at {}:{row}:{col}", self.as_str(), self.source.name)
    }
}

#[derive(PartialEq)]
pub enum TokenKind {
    Whitespace,
    Identifier,
    Bracket,
    Operator,
}

impl Token {
    pub fn kind(&self) -> TokenKind {
        if matches!(self.as_str(), "(" | ")" | "{" | "}" | "[" | "]") {
            return TokenKind::Bracket;
        }
        assert!(!self.as_str().chars().any(|c| "(){}[]".contains(c)));

        if self.as_str().chars().all(char::is_whitespace) {
            return TokenKind::Whitespace;
        }
        assert!(!self.as_str().chars().any(char::is_whitespace));

        if self.as_str().chars().any(char::is_alphanumeric) {
            TokenKind::Identifier
        } else {
            TokenKind::Operator
        }
    }
}
