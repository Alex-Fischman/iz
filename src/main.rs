//! A compiler for the Iz programming language.

#![deny(clippy::all, clippy::pedantic, missing_docs)]

pub use std::fmt::{Debug, Display, Formatter};

mod state;
mod store;
pub use state::*;
pub use store::*;

/// Global `Result` alias for ease of use. See the `err!` macro.
pub type Result<T> = std::result::Result<T, String>;

/// Constructs an error message from the given state, span,
/// and format arguments.
#[macro_export]
macro_rules! err {
    ($state:expr, $span:expr, $($fmt:tt)*) => {
        Err(format!(
            "error at {}: {}\n{}",
            $span.location($state),
            format!($($fmt)*),
            $span.string($state),
        ))
    };
}

fn main() {
    std::process::exit(match cmd() {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{e}");
            1
        }
    })
}

fn cmd() -> Result<()> {
    let args = std::env::args().collect::<Vec<String>>();
    let name = args.get(1).ok_or("usage: pass a .iz file")?.to_string();
    let text = std::fs::read_to_string(&name).map_err(|_| format!("could not read {name}"))?;
    run(Source { name, text })
}

/// Represents a file containing source code.
pub struct Source {
    /// The file name.
    pub name: String,
    /// The file contents.
    pub text: String,
}

/// A snippet of a `Source`.
#[derive(Clone, Copy)]
pub struct Span {
    /// Which source file this span refers to.
    pub source: Index<Source>,
    /// The byte index of the first character in the snippet.
    pub lo: usize,
    /// The byte index after the last character in the snippet.
    pub hi: usize,
}

impl Span {
    /// Get the text from the snippet.
    #[must_use]
    pub fn string(self, state: &State) -> &str {
        &state.sources[self.source].text[self.lo..self.hi]
    }

    /// Assert that the snippet is a single `char` and return it.
    /// # Panics
    /// Will panic if this `Span` contains more or less than 1 `char`.
    #[must_use]
    pub fn single_char(self, state: &State) -> char {
        assert_eq!(self.string(state).chars().count(), 1);
        self.string(state).chars().next().unwrap()
    }

    /// Get the location of the snippet.
    #[must_use]
    pub fn location(&self, state: &State) -> String {
        let source = &state.sources[self.source];
        let mut row = 1;
        let mut col = 1;
        for (i, c) in source.text.char_indices() {
            match (i, c) {
                (i, _) if i == self.lo => break,
                (_, '\n') => {
                    row += 1;
                    col = 1;
                }
                _ => col += 1,
            }
        }
        format!("{}:{row}:{col}", source.name)
    }
}

#[allow(clippy::unnecessary_wraps)]
fn run(source: Source) -> Result<()> {
    let mut state = State::default();
    let source = state.sources.push(source);
    let text = &state.sources[source].text;

    let los = text.char_indices().map(|(i, _)| i);
    let his = los.clone().skip(1).chain([text.len()]);
    let spans = los.zip(his).map(|(lo, hi)| Span { source, lo, hi });
    let mut spans = spans.peekable();

    while let Some(mut span) = spans.next() {
        assert_eq!(span.string(&state).chars().count(), 1);
        let tag = match span.single_char(&state) {
            c if c.is_whitespace() => continue,
            '#' => {
                while let Some(s) = spans.peek() {
                    if s.single_char(&state) == '\n' {
                        break;
                    }
                    spans.next();
                }
                continue;
            }
            '(' => Tag::Opener(")"),
            '{' => Tag::Opener("}"),
            '[' => Tag::Opener("]"),
            ')' | '}' | ']' => Tag::Closer,
            '"' => {
                let mut in_escape = false;
                let mut string = String::new();
                loop {
                    let Some(s) = spans.next() else {
                        return err!(&state, span, "no matching end quote");
                    };
                    if in_escape {
                        let c = match s.single_char(&state) {
                            '"' => '"',
                            '\\' => '\\',
                            'n' => '\n',
                            't' => '\t',
                            _ => return err!(&state, s, "unknown escape character"),
                        };
                        string.push(c);
                        in_escape = false;
                    } else {
                        match s.single_char(&state) {
                            '"' => {
                                span.hi = s.hi;
                                break;
                            }
                            '\\' => in_escape = true,
                            c => string.push(c),
                        }
                    }
                }
                Tag::String(string)
            }
            _ => {
                while let Some(s) = spans.peek() {
                    let c = s.single_char(&state);
                    let is_special = matches!(c, '#' | '(' | ')' | '{' | '}' | '[' | ']' | '"');
                    if c.is_whitespace() || is_special {
                        break;
                    }
                    span.hi = s.hi;
                    spans.next();
                }
                Tag::Identifier
            }
        };
        state.nodes.push_child(ROOT, tag, span);
    }

    let mut child = state.nodes[ROOT].head;
    while let Some(i) = child.unpack() {
        let span = state.nodes[i].span;
        println!("{}\t{}", span.location(&state), span.string(&state));
        child = state.nodes[i].next;
    }

    Ok(())
}
