use crate::*;

impl State {
    fn get_token_of_next_child(
        &self,
        tokens: TableId<Token>,
        node: NodeId,
        next_child: &mut OptionNodeId,
    ) -> Result<&Token> {
        let Some(child) = (*next_child).into() else {
            return err!(self, self[tokens][node].span, "missing child");
        };
        let Some(token) = self[tokens].get(child) else {
            return err!(self, self[tokens][node].span, "missing token for child");
        };
        *next_child = self[child].next;
        Ok(token)
    }

    /// Parses tuples into instructions.
    pub fn codegen(
        &mut self,
        tokens: TableId<Token>,
        root: NodeId,
    ) -> Result<TableId<Instruction>> {
        let instructions = self.add_table::<Instruction>();
        let mut postorder = self.postorder(root);

        while let Some(node) = postorder.next(self)? {
            let Some(Token { span, .. }) = self[tokens].get(node) else {
                continue;
            };

            let mut next_child = self[node].head;

            let word = |state: &State, next_child: &mut OptionNodeId| {
                let Token { span, tag } = self.get_token_of_next_child(tokens, node, next_child)?;
                match tag {
                    TokenType::Number(number) => Ok(Word(*number)),
                    _ => err!(state, span, "expected number"),
                }
            };

            let reg = |state: &State, next_child: &mut OptionNodeId| -> Result<Register> {
                let Token { span, .. } = self.get_token_of_next_child(tokens, node, next_child)?;
                let register = match span.string(state).split_at(2) {
                    ("pc", "") => Register::Pc,
                    ("sp", "") => Register::Sp,
                    ("gp", suffix) => match suffix.parse::<usize>() {
                        Ok(number) => Register::Gp(number),
                        Err(error) => err!(state, span, "{error}")?,
                    },
                    (_, _) => err!(state, span, "expected register")?,
                };
                Ok(register)
            };

            let instruct = |state: &mut State, instruction| match state[instructions]
                .insert(node, instruction)
            {
                None => Ok(()),
                Some(old_instruction) => err!(
                    state,
                    state[tokens][node].span,
                    "parsed as {instruction:?} but also as {old_instruction:?}"
                ),
            };

            match span.string(self) {
                "imm" => {
                    let imm = word(self, &mut next_child)?;
                    let dst = reg(self, &mut next_child)?;
                    instruct(self, Instruction::Imm { imm, dst })?;
                }
                "mov" => {
                    let src = reg(self, &mut next_child)?;
                    let dst = reg(self, &mut next_child)?;
                    instruct(self, Instruction::Mov { src, dst })?;
                }
                "add" => {
                    let src = (reg(self, &mut next_child)?, reg(self, &mut next_child)?);
                    let dst = reg(self, &mut next_child)?;
                    instruct(self, Instruction::Add { src, dst })?;
                }
                "load" => {
                    let loc = reg(self, &mut next_child)?;
                    let dst = reg(self, &mut next_child)?;
                    instruct(self, Instruction::Load { loc, dst })?;
                }
                "store" => {
                    let src = reg(self, &mut next_child)?;
                    let loc = reg(self, &mut next_child)?;
                    instruct(self, Instruction::Store { src, loc })?;
                }
                _ => {}
            }
        }

        Ok(instructions)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static PC: Register = Register::Pc;
    static SP: Register = Register::Sp;
    static GP0: Register = Register::Gp(0);
    static GP1: Register = Register::Gp(1);
    static GP2: Register = Register::Gp(2);

    macro_rules! instr {
        ($imm:literal -> $dst:ident) => {
            Instruction::Imm {
                imm: Word($imm),
                dst: $dst,
            }
        };
        ($src:ident -> $dst:ident) => {
            Instruction::Mov {
                src: $src,
                dst: $dst,
            }
        };
        ($x:ident + $y:ident -> $z:ident) => {
            Instruction::Add {
                src: ($x, $y),
                dst: $z,
            }
        };
        (mem[$loc:ident] -> $dst:ident) => {
            Instruction::Load {
                loc: $loc,
                dst: $dst,
            }
        };
        ($src:ident -> mem[$loc:ident]) => {
            Instruction::Store {
                src: $src,
                loc: $loc,
            }
        };
    }

    fn run_test(source: Source, expected: &[Instruction]) -> Result<()> {
        let (mut state, src) = State::new(source);
        let tokens = state.add_table::<Token>();
        state.tokenize(src, tokens, State::ROOT)?;
        state.bracket(tokens, State::ROOT)?;
        state.sexp(tokens, State::ROOT)?;
        let instructions = state.codegen(tokens, State::ROOT)?;
        let mut program = Vec::new();
        let mut children = state.children(State::ROOT);
        while let Some(child) = children.next(&state)? {
            program.push(state[instructions][child]);
        }
        assert_eq!(program, expected);
        Ok(())
    }

    #[test]
    fn basic() -> Result<()> {
        run_test(text!("(imm 17 gp0)"), &[instr!(17 -> GP0)])?;
        run_test(text!("(mov gp0 gp1)"), &[instr!(GP0 -> GP1)])?;
        run_test(text!("(add gp0 gp1 gp2)"), &[instr!(GP0 + GP1 -> GP2)])?;
        run_test(text!("(load sp gp2)"), &[instr!(mem[SP] -> GP2)])?;
        run_test(text!("(store pc gp1)"), &[instr!(PC -> mem[GP1])])?;
        run_test(
            text!("(imm 17 gp0) (mov gp0 gp1)"),
            &[instr!(17 -> GP0), instr!(GP0 -> GP1)],
        )
    }
}
