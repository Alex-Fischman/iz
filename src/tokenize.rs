use crate::*;

/// Like `str::char_indices` but doesn't hold a reference to the string.
struct Spans {
    /// Which source to use. Should not change.
    source: usize,
    /// Index of `lo` for the current span.
    lo: usize,
}

impl Spans {
    fn peek(&self, state: &State) -> Option<Span> {
        let text = &state.sources[self.source].text[self.lo..];
        Some(Span {
            lo: self.lo,
            hi: self.lo + text.chars().next()?.len_utf8(),
            source: self.source,
        })
    }

    fn next(&mut self, state: &State) -> Option<Span> {
        let span = self.peek(state);
        if let Some(span) = span {
            self.lo = span.hi;
        }
        span
    }
}

/// Tokenizes the given source file, adding its tokens as children to
/// the given parent node.
#[allow(clippy::missing_errors_doc, clippy::missing_panics_doc)]
pub fn tokenize(state: &mut State, src: Source, parent: usize) -> Result<()> {
    let tag_opener = state.push_tag::<&str>("opener")?;
    let tag_closer = state.push_tag::<()>("closer")?;
    let tag_string = state.push_tag::<String>("string")?;
    let tag_char = state.push_tag::<char>("char")?;
    let tag_identifier = state.push_tag::<()>("identifier")?;

    let source = state.sources.len();
    state.sources.push(src);

    let mut spans = Spans { source, lo: 0 };

    while let Some(mut span) = spans.next(state) {
        assert_eq!(span.string(state).chars().count(), 1);
        _ = match span.single_char(state) {
            c if c.is_whitespace() => continue,
            '#' => {
                while let Some(s) = spans.next(state) {
                    if s.single_char(state) == '\n' {
                        break;
                    }
                }
                continue;
            }
            '(' => state.push_child(parent, tag_opener, span, Box::new(")")),
            '{' => state.push_child(parent, tag_opener, span, Box::new("}")),
            '[' => state.push_child(parent, tag_opener, span, Box::new("]")),
            ')' | '}' | ']' => state.push_child(parent, tag_closer, span, Box::new(())),
            '"' => {
                let mut in_escape = false;
                let mut string = String::new();
                loop {
                    let Some(s) = spans.next(state) else {
                        return err!(state, span, "no matching end quote");
                    };
                    if in_escape {
                        string.push(escape_char(s, state)?);
                        in_escape = false;
                    } else {
                        match s.single_char(state) {
                            '"' => {
                                span.hi = s.hi;
                                break;
                            }
                            '\\' => in_escape = true,
                            c => string.push(c),
                        }
                    }
                }
                state.push_child(parent, tag_string, span, Box::new(string))
            }
            '\'' => {
                let e = err!(state, span, "no matching end quote");
                let Some(s) = spans.next(state) else { return e };
                span.hi = s.hi;

                let c = match s.single_char(state) {
                    '\'' => return err!(state, span, "empty char literal"),
                    '\\' => {
                        let Some(s) = spans.next(state) else { return e };
                        escape_char(s, state)?
                    }
                    c => c,
                };

                let Some(s) = spans.next(state) else { return e };
                let '\'' = s.single_char(state) else { return e };
                span.hi = s.hi;

                state.push_child(parent, tag_char, span, Box::new(c))
            }
            _ => {
                while let Some(s) = spans.peek(state) {
                    match s.single_char(state) {
                        c if c.is_whitespace() => break,
                        '#' | '(' | ')' | '{' | '}' | '[' | ']' | '"' | '\'' => break,
                        _ => {}
                    }
                    span.hi = s.hi;
                    spans.next(state);
                }
                state.push_child(parent, tag_identifier, span, Box::new(()))
            }
        }
    }

    Ok(())
}

fn escape_char(s: Span, state: &State) -> Result<char> {
    Ok(match s.single_char(state) {
        '"' => '"',
        '\\' => '\\',
        'n' => '\n',
        't' => '\t',
        _ => return err!(state, s, "unknown escape character"),
    })
}

#[test]
fn test() -> Result<()> {
    fn run(text: &str) -> Result<Vec<String>> {
        let name = String::new();
        let text = text.to_owned();
        let source = Source { name, text };
        let mut state = State::default();

        tokenize(&mut state, source, ROOT)?;

        let mut actual = Vec::new();
        let mut child = state.nodes[ROOT].head;
        while let Some(i) = child.unpack() {
            actual.push(state.nodes[i].span.string(&state).to_owned());
            child = state.nodes[i].next;
        }

        Ok(actual)
    }

    assert_eq!(run("1 + (2 + 3)")?, ["1", "+", "(", "2", "+", "3", ")"]);
    assert_eq!(
        run("ys = (map f xs)")?,
        ["ys", "=", "(", "map", "f", "xs", ")"]
    );
    assert_eq!(run("1+2")?, ["1+2"]);
    assert_eq!(run("a(s)df")?, ["a", "(", "s", ")", "df"]);
    assert_eq!(run("1\n23\t456")?, ["1", "23", "456"]);
    assert_eq!(run("#a\nb#")?, ["b"]);
    assert_eq!(run("'#'")?, ["'#'"]);
    assert_eq!(run("\'a\'b\"c d\"e")?, ["\'a\'", "b", "\"c d\"", "e"]);
    assert_eq!(
        run(" \" # this is a comment \n \" ")?,
        ["\" # this is a comment \n \""]
    );
    assert_eq!(
        run("'").unwrap_err(),
        "error at :1:1: no matching end quote\n'"
    );
    assert_eq!(
        run("''").unwrap_err(),
        "error at :1:1: empty char literal\n''"
    );
    assert_eq!(
        run("'a").unwrap_err(),
        "error at :1:1: no matching end quote\n'"
    );
    assert_eq!(
        run("'a ").unwrap_err(),
        "error at :1:1: no matching end quote\n'"
    );

    Ok(())
}
