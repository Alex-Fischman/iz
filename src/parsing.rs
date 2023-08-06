//! Functions to reorganize a tree without changing its tokens.

use crate::Tree;

pub fn parse_brackets<'a>(open: &'a str, close: &'a str) -> impl Fn(&mut Tree) + 'a {
    move |tree: &mut Tree| {
        tree.children
            .iter_mut()
            .for_each(parse_brackets(open, close));

        let mut openers = Vec::new();
        let mut i = 0;
        while i < tree.children.len() {
            if tree.children[i].token.as_str() == open {
                openers.push(i);
            } else if tree.children[i].token.as_str() == close {
                let opener = openers
                    .pop()
                    .unwrap_or_else(|| panic!("extra {}", tree.children[i].token));

                tree.children.remove(i);
                let cs: Vec<Tree> = tree.children.drain(opener + 1..i).collect();
                tree.children[opener].children.extend(cs);
                i = opener;
            }
            i += 1;
        }
        if let Some(opener) = openers.pop() {
            panic!("extra {}", tree.children[opener].token)
        }
    }
}

pub fn parse_postfixes<'a>(names: &'a [&'a str]) -> impl Fn(&mut Tree) + 'a {
    move |tree: &mut Tree| {
        tree.children.iter_mut().for_each(parse_postfixes(names));

        let mut i = 0;
        while i < tree.children.len() {
            if names.contains(&tree.children[i].token.as_str()) {
                if i == 0 {
                    panic!("no argument for {}", tree.children[i].token)
                }
                let child = tree.children.remove(i - 1);
                tree.children[i - 1].children.push(child);
            } else {
                i += 1;
            }
        }
    }
}
