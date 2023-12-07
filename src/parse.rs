//! Parsing passes

use crate::*;

/// Parse matching bracket tokens
pub fn brackets<'a>(open: &'a str, close: &'a str) -> Pass<'a> {
    Box::new(move |tree: &mut Tree, parent: Node| {
        tree.run_pass_over_children(parent, &brackets(open, close))?;

        let mut openers: Vec<(Node, usize)> = vec![];
        let mut i = 0;
        while i < tree.get_children(parent).len() {
            let child = tree.get_children(parent)[i];
            if tree.get_slice(child).str() == open {
                openers.push((child, i));
            } else if tree.get_slice(child).str() == close {
                let (opener_node, opener_index) = openers
                    .pop()
                    .ok_or(format!("extra {}", tree.get_slice(child)))?;

                let opener = tree.get_slice(opener_node);
                let closer = tree.get_slice(child);
                tree.set_slice(opener_node, Slice::new(opener.source, opener.lo, closer.hi));

                tree.get_children_mut(parent).remove(i);
                let grandchildren: Vec<_> = tree
                    .get_children_mut(parent)
                    .drain(opener_index + 1..i)
                    .collect();
                tree.get_children_mut(opener_node).extend(grandchildren);
                i = opener_index;
            }
            i += 1;
        }
        if let Some((opener_node, _)) = openers.pop() {
            return Err(format!("extra {}", tree.get_slice(opener_node)));
        }

        Ok(())
    })
}
