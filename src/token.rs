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

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn tokenize(s: &str) -> Vec<Token> {
    s.chars()
        .enumerate()
        .map(|(i, c)| {
            Token(
                c.to_string(),
                i,
                if c.is_alphabetic() || c == '_' {
                    TokenType::Alphabetic
                } else if c.is_whitespace() {
                    TokenType::Whitespace
                } else if c.is_numeric() {
                    TokenType::Numeric
                } else if c == '(' || c == '{' || c == '[' {
                    TokenType::Opener
                } else if c == ')' || c == '}' || c == ']' {
                    TokenType::Closer
                } else {
                    TokenType::Other
                },
            )
        })
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
