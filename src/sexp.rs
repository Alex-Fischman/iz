use crate::*;

impl State {
    /// Replaces the `Token`s for parenthesized expressions with their first child.
    pub fn sexp(&mut self, tokens: TableId<Token>, root: NodeId) -> Result<()> {
        let mut postorder = self.postorder(root);

        while let Some(node) = postorder.next(self)? {
            let Some(Token {
                span: _,
                tag: TokenType::Bracket(Bracket::Paren, Side::Left),
            }) = self[tokens].get(node)
            else {
                continue;
            };

            let Some(head) = self[node].head.into() else {
                continue;
            };

            // copy the token from `head` to `node`
            self[tokens][node] = self[tokens][head].clone();

            // fix up the left pointer into `head`
            self[node].head = self[head].next;

            // fix up the right pointer into `head`
            if let Some(next) = self[head].next.into() {
                // fix up `next` if we're not at the end
                debug_assert!(self[next].prev.unwrap() == head);
                self[next].prev = OptionNodeId::NONE;
            } else {
                // fix up `node` if we are at the end
                debug_assert!(self[node].last.unwrap() == head);
                self[node].last = OptionNodeId::NONE;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn collect_tokens_postorder(state: &mut State, tokens: TableId<Token>) -> Result<Vec<String>> {
        let mut postorder = state.postorder(State::ROOT);
        let mut result = Vec::new();
        while let Some(node) = postorder.next(state)? {
            match state[tokens].get(node) {
                None => assert!(node == State::ROOT),
                Some(Token { span, tag: _ }) => result.push(span.string(state).to_owned()),
            }
        }
        Ok(result)
    }

    // TODO: zero, one, and two children unit tests

    #[test]
    fn postorder() -> Result<()> {
        let source = text!("(add (add 1 2) 3)");
        let (mut state, src) = State::new(source);
        let tokens = state.add_table::<Token>();
        state.tokenize(src, tokens, State::ROOT)?;
        state.bracket(tokens, State::ROOT)?;
        assert_eq!(
            collect_tokens_postorder(&mut state, tokens)?,
            [
                "add",
                "add",
                "1",
                "2",
                "(add 1 2)",
                "3",
                "(add (add 1 2) 3)"
            ]
        );
        state.sexp(tokens, State::ROOT)?;
        assert_eq!(
            collect_tokens_postorder(&mut state, tokens)?,
            ["1", "2", "add", "3", "add"]
        );
        Ok(())
    }
}
