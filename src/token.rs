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
enum TokenKind {
    Whitespace,
    Identifier,
    Bracket,
    Operator,
}

impl TokenKind {
    fn classify(s: &str) -> TokenKind {
        if matches!(s, "(" | ")" | "{" | "}" | "[" | "]") {
            return TokenKind::Bracket;
        }
        assert!(!s.chars().any(|c| "(){}[]".contains(c)));

        if s.chars().all(char::is_whitespace) {
            return TokenKind::Whitespace;
        }
        assert!(!s.chars().any(char::is_whitespace));

        if s.chars().any(char::is_alphanumeric) {
            TokenKind::Identifier
        } else {
            TokenKind::Operator
        }
    }
}

impl Token {
    pub fn is_whitespace(&self) -> bool {
        TokenKind::classify(self.as_str()) == TokenKind::Whitespace
    }

    pub fn is_identifier(&self) -> bool {
        TokenKind::classify(self.as_str()) == TokenKind::Identifier
    }

    pub fn is_operator(&self) -> bool {
        TokenKind::classify(self.as_str()) == TokenKind::Operator
    }
}
