use crate::*;

impl State {
    /// Matches `Side::Left` and `Side::Right` `TokenType::Bracket`s in the
    /// children of the `root`, turning a flat list of tokens into a nested tree.
    pub fn bracket(&mut self, tokens: TableId<Token>, root: NodeId) -> Result<()> {
        let mut stack = Vec::new();
        let mut next = self[root].head.into();

        while let Some(node) = next {
            next = self[node].next.into();

            let TokenType::Bracket(bracket, side) = &self[tokens][node].tag else {
                continue;
            };

            match side {
                Side::Left => stack.push((node, *bracket)),
                Side::Right => {
                    let (right, right_bracket) = (node, *bracket);

                    let Some((left, left_bracket)) = stack.pop() else {
                        return err!(self, self[tokens][node].span, "unmatched close bracket");
                    };

                    // set left span to encompass all of the intervening nodes
                    self[tokens][left].span.hi = self[tokens][right].span.hi;

                    // check that the bracket types match
                    if left_bracket != right_bracket {
                        return err!(self, self[tokens][left].span, "mismatched brackets");
                    }

                    // if there are any contents:
                    if self[left].next.unwrap() != right {
                        let head = self[left].next.unwrap();
                        let last = self[right].prev.unwrap();

                        // move contents into `left`
                        self[left].head = OptionNodeId::some(head);
                        self[left].last = OptionNodeId::some(last);

                        // break pointers out of contents
                        self[head].prev = OptionNodeId::NONE;
                        self[last].next = OptionNodeId::NONE;
                    }

                    // fix up `left` pointer into contents
                    self[left].next = self[right].next;

                    // remove `right` from children/sibling lists
                    if let Some(next) = next {
                        // fix up `next` if we're not at the end
                        debug_assert!(self[next].prev.unwrap() == right);
                        self[next].prev = OptionNodeId::some(left);
                    } else {
                        // fix up `root` if we are at the end
                        debug_assert!(self[root].last.unwrap() == right);
                        self[root].last = OptionNodeId::some(left);
                    }
                }
            }
        }

        if let Some((node, _)) = stack.pop() {
            return err!(self, self[tokens][node].span, "unmatched open bracket");
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, PartialEq)]
    enum Tree {
        Leaf(String),
        Limb(LimbType, Vec<Tree>),
    }

    #[derive(Debug, PartialEq)]
    enum LimbType {
        Root,
        Bracket(Bracket),
    }

    macro_rules! node {
        [$leaf:ident] => { Tree::Leaf(stringify!($leaf).to_owned()) };
        [($($args:tt)*)] => { Tree::Limb(LimbType::Bracket(Bracket::Paren), vec![$(node![$args]),*]) };
        [{$($args:tt)*}] => { Tree::Limb(LimbType::Bracket(Bracket::Curly), vec![$(node![$args]),*]) };
        [[$($args:tt)*]] => { Tree::Limb(LimbType::Bracket(Bracket::Square), vec![$(node![$args]),*]) };
    }

    macro_rules! tree {
        [$($args:tt)*] => { Tree::Limb(LimbType::Root, vec![$(node!($args)),*]) }
    }

    fn state_to_tree(state: &State, tokens: TableId<Token>, parent: NodeId) -> Tree {
        let limb_type = match state[tokens].get(parent).map(|token| &token.tag) {
            None => LimbType::Root,
            Some(TokenType::Bracket(bracket, Side::Left)) => LimbType::Bracket(*bracket),
            Some(_) => return Tree::Leaf(state[tokens][parent].span.string(state).to_owned()),
        };

        let mut children = Vec::new();
        let mut child = state[parent].head;
        while let Some(node) = child.into() {
            children.push(state_to_tree(state, tokens, node));
            child = state[node].next;
        }

        Tree::Limb(limb_type, children)
    }

    macro_rules! test {
        ($name:ident, $s:literal, $err:literal) => {
            #[test]
            fn $name() -> Result<()> {
                let source = text!($s);
                let (mut state, src) = State::new(source);
                let tokens = state.add_table::<Token>();
                state.tokenize(src, tokens, State::ROOT).unwrap();
                let error = state.bracket(tokens, State::ROOT).unwrap_err();
                assert_eq!(error, $err);
                Ok(())
            }
        };
        ($name:ident, $s:literal, $($tree:tt)*) => {
            #[test]
            fn $name() -> Result<()> {
                let source = text!($s);
                let (mut state, src) = State::new(source);
                let tokens = state.add_table::<Token>();
                state.tokenize(src, tokens, State::ROOT).unwrap();
                state.bracket(tokens, State::ROOT).unwrap();
                assert_eq!(state_to_tree(&state, tokens, State::ROOT), tree!($($tree)*));
                Ok(())
            }
        };
    }

    test! {ident, "a", a}
    test! {unit, "()", ()}
    test! {unit_ident, "()a", () a}
    test! {paren_unit_ident,  "(()a)", (() a)}
    test! {ident_paren_idents_ident, "a (b c) d", a (b c) d}
    test! {lisp, "(eq y (map f xs))", (eq y (map f xs))}
    test! {matching, "({[]}[()])", ({[]}[()])}
    test! {matching_again, "([()(())])()", ([()(())])()}
    test! {unmatched_open, "{{}", "error at text!:1:1: unmatched open bracket\n{"}
    test! {unmatched_close, "[]][", "error at text!:1:3: unmatched close bracket\n]"}
    test! {mismatched, "({)}", "error at text!:1:2: mismatched brackets\n{)"}
}
