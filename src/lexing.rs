//! Functions to reorganize and combine lists of tokens, given as single-level trees.

use crate::{Token, Tree};

pub fn remove_comments(tree: &mut Tree) {
    let mut in_comment = false;
    tree.children.retain(|child| {
        match child.token.as_str() {
            "#" if !in_comment => in_comment = true,
            "\n" if in_comment => in_comment = false,
            _ => {}
        }
        !in_comment
    });
}

pub fn remove_whitespace(tree: &mut Tree) {
    tree.children.retain(|child| !child.token.is_whitespace());
}

pub fn concat_tokens(f: impl Fn(&Token) -> bool) -> impl Fn(&mut Tree) {
    move |tree: &mut Tree| {
        let mut i = 1;
        while i < tree.children.len() {
            if f(&tree.children[i - 1].token)
                && f(&tree.children[i].token)
                && tree.children[i - 1].token.source == tree.children[i].token.source
                && tree.children[i - 1].token.range.end == tree.children[i].token.range.start
            {
                tree.children[i - 1].token.range.end = tree.children[i].token.range.end;
                assert!(tree.children[i].is_empty());
                tree.children.remove(i);
            } else {
                i += 1;
            }
        }
    }
}

pub fn parse_integers(tree: &mut Tree) {
    for child in &mut tree.children {
        let chars: Vec<char> = child.token.as_str().chars().collect();
        let (chars, is_negative) = match chars[0] {
            '-' => (&chars[1..], true),
            _ => (&chars[..], false),
        };
        if let Some('0'..='9') = chars.first() {
            let (chars, base) = match chars {
                ['0', 'x', ..] => (&chars[2..], 16),
                ['0', 'b', ..] => (&chars[2..], 2),
                _ => (chars, 10),
            };
            let value = chars.iter().fold(0, |value, c| {
                let digit = match c {
                    '0'..='9' => *c as i64 - '0' as i64,
                    'a'..='f' => *c as i64 - 'a' as i64 + 10,
                    '_' => return value,
                    c => panic!("unknown digit {} in {}", c, child.token),
                };
                if digit >= base {
                    panic!("digit {} too large for base {} in {}", c, base, child.token)
                }
                base * value + digit
            });
            child.insert::<i64>(if is_negative { -value } else { value });
        }
    }
}