use crate::*;

/// Tokenizes the given source file, adding its tokens as children to
/// the given parent node.
#[allow(clippy::missing_errors_doc, clippy::missing_panics_doc)]
pub fn tokenize(state: &mut State, src: Source, parent: usize) -> Result<()> {
    let source = state.sources.len();
    state.sources.push(Source {
        name: src.name,
        text: src.text.clone(),
    });

    let los = src.text.char_indices().map(|(i, _)| i);
    let his = los.clone().skip(1).chain([src.text.len()]);
    let spans = los.zip(his).map(|(lo, hi)| Span { source, lo, hi });
    let mut spans = spans.peekable();

    while let Some(mut span) = spans.next() {
        assert_eq!(span.string(state).chars().count(), 1);
        let tag = match span.single_char(state) {
            c if c.is_whitespace() => continue,
            '#' => {
                while let Some(s) = spans.peek() {
                    if s.single_char(state) == '\n' {
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
                Tag::String(string)
            }
            _ => {
                while let Some(s) = spans.peek() {
                    let c = s.single_char(state);
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
        state.push_child(parent, tag, span);
    }

    Ok(())
}
