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
    let tag_opener = state.push_tag::<&str>("Opener");
    let tag_closer = state.push_tag::<()>("Closer");
    let tag_string = state.push_tag::<String>("String");
    let tag_identifier = state.push_tag::<()>("Identifier");

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
                        let c = match s.single_char(state) {
                            '"' => '"',
                            '\\' => '\\',
                            'n' => '\n',
                            't' => '\t',
                            _ => return err!(&state, s, "unknown escape character"),
                        };
                        string.push(c);
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
            _ => {
                while let Some(s) = spans.peek(state) {
                    let c = s.single_char(state);
                    let is_special = matches!(c, '#' | '(' | ')' | '{' | '}' | '[' | ']' | '"');
                    if c.is_whitespace() || is_special {
                        break;
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
