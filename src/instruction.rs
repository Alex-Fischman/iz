use crate::*;

/// A unit of memory on the `Machine`.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Word(pub u64);

impl std::ops::Add for Word {
    type Output = Word;

    fn add(self, other: Word) -> Word {
        Word(self.0 + other.0)
    }
}

impl std::ops::Sub for Word {
    type Output = Word;

    fn sub(self, other: Word) -> Word {
        Word(self.0 - other.0)
    }
}

/// The state of the virtual machine that `Instruction`s run on.
#[derive(Debug, PartialEq)]
pub struct Machine {
    /// The program counter.
    pc: Word,
    /// The stack pointer.
    sp: Word,
    /// The general purpose register file.
    gp: Vec<Word>,
    /// The memory unit.
    memory: Memory,
}

/// One of a small number of fast memory locations.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Register {
    /// The program counter.
    Pc,
    /// The stack pointer.
    Sp,
    /// A general purpose register.
    Gp(usize),
}

impl Index<Register> for Machine {
    type Output = Word;
    fn index(&self, register: Register) -> &Word {
        match register {
            Register::Pc => &self.pc,
            Register::Sp => &self.sp,
            Register::Gp(i) => &self.gp[i],
        }
    }
}

impl IndexMut<Register> for Machine {
    fn index_mut(&mut self, register: Register) -> &mut Word {
        match register {
            Register::Pc => &mut self.pc,
            Register::Sp => &mut self.sp,
            Register::Gp(i) => &mut self.gp[i],
        }
    }
}

/// The `Word`-addressable memory available to the program.
#[derive(Debug, Default, PartialEq)]
pub struct Memory(HashMap<Word, Word>);

impl Index<Word> for Memory {
    type Output = Word;

    fn index(&self, word: Word) -> &Word {
        &self.0[&word]
    }
}

impl IndexMut<Word> for Memory {
    fn index_mut(&mut self, word: Word) -> &mut Word {
        self.0.entry(word).or_default()
    }
}

/// One instruction in the intermediate representation.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Instruction {
    /// Set the value of a register.
    Imm {
        /// The immediate value to write.
        imm: Word,
        /// The register to set.
        dst: Register,
    },
    /// Copy a value from one register to another.
    Mov {
        /// The register to copy out of.
        src: Register,
        /// The register to copy into.
        dst: Register,
    },
    /// Add two registers.
    Add {
        /// The two source registers.
        src: (Register, Register),
        /// The register to store the sum in.
        dst: Register,
    },
    /// Subtract two registers.
    Sub {
        /// The two source registers.
        src: (Register, Register),
        /// The register to store the sum in.
        dst: Register,
    },
    /// Read a value from memory.
    Load {
        /// The location to read from.
        loc: Register,
        /// The register to write the value into.
        dst: Register,
    },
    /// Write a value into memory.
    Store {
        /// The value to write.
        src: Register,
        /// The location to write into.
        loc: Register,
    },
}

/// A list of `Instruction`s, along with some metadata.
#[derive(Debug)]
pub struct Program {
    /// A list of `Instruction`s.
    pub instructions: Vec<Instruction>,
}

impl Program {
    fn gp_registers(&self) -> impl std::iter::Iterator<Item = usize> {
        self.instructions
            .iter()
            .flat_map(|instruction| match instruction {
                Instruction::Imm { imm: _, dst } => vec![dst],
                Instruction::Mov { src, dst } => vec![src, dst],
                Instruction::Add { src: (x, y), dst } | Instruction::Sub { src: (x, y), dst } => {
                    vec![x, y, dst]
                }
                Instruction::Load { loc, dst } => vec![loc, dst],
                Instruction::Store { src, loc } => vec![src, loc],
            })
            .filter_map(|register| match register {
                Register::Pc | Register::Sp => None,
                Register::Gp(i) => Some(i),
            })
            .copied()
    }

    fn gp_registers_mut(&mut self) -> impl std::iter::Iterator<Item = &mut usize> {
        self.instructions
            .iter_mut()
            .flat_map(|instruction| match instruction {
                Instruction::Imm { imm: _, dst } => vec![dst],
                Instruction::Mov { src, dst } => vec![src, dst],
                Instruction::Add { src: (x, y), dst } | Instruction::Sub { src: (x, y), dst } => {
                    vec![x, y, dst]
                }
                Instruction::Load { loc, dst } => vec![loc, dst],
                Instruction::Store { src, loc } => vec![src, loc],
            })
            .filter_map(|register| match register {
                Register::Pc | Register::Sp => None,
                Register::Gp(i) => Some(i),
            })
    }

    /// Get the index of the highest `Register::Gp` in this `Program`.
    #[must_use]
    pub fn gp_register_count(&self) -> usize {
        match self.gp_registers().max() {
            None => 0,
            Some(max) => max + 1,
        }
    }

    /// Increment all `Register::Gp`s in this `Program` by the given amount.
    pub fn gp_register_incr(&mut self, incr: usize) {
        self.gp_registers_mut().for_each(|i| *i += incr);
    }

    /// Run a program on a new `Machine`, and return the final state.
    #[must_use]
    pub fn execute(&self) -> Machine {
        let mut machine = Machine {
            pc: Word(0),
            sp: Word(u64::MAX),
            gp: vec![Word(0); self.gp_register_count()],
            memory: Memory::default(),
        };

        #[allow(clippy::cast_possible_truncation)]
        while let Some(instruction) = self.instructions.get(machine[Register::Pc].0 as usize) {
            machine[Register::Pc].0 += 1;
            match *instruction {
                Instruction::Imm { imm, dst } => machine[dst] = imm,
                Instruction::Mov { src, dst } => machine[dst] = machine[src],
                Instruction::Add { src: (x, y), dst } => machine[dst] = machine[x] + machine[y],
                Instruction::Sub { src: (x, y), dst } => machine[dst] = machine[x] - machine[y],
                Instruction::Load { loc, dst } => {
                    let loc = machine[loc];
                    machine[dst] = machine.memory[loc];
                }
                Instruction::Store { src, loc } => {
                    let loc = machine[loc];
                    machine.memory[loc] = machine[src];
                }
            }
        }

        machine
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    pub static PC: Register = Register::Pc;
    pub static SP: Register = Register::Sp;
    pub static GP0: Register = Register::Gp(0);
    pub static GP1: Register = Register::Gp(1);
    pub static GP2: Register = Register::Gp(2);

    /// Sublanguage for intuitively writing `Instruction`s.
    #[macro_export]
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
        ($x:ident - $y:ident -> $z:ident) => {
            Instruction::Sub {
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

    #[test]
    fn execute() {
        let program = Program {
            instructions: vec![
                instr!(1 -> GP0),
                instr!(SP - GP0 -> SP),
                instr!(GP0 + GP0 -> GP1),
                instr!(GP1 -> mem[SP]),
                instr!(GP0 -> GP2),
                instr!(mem[SP] -> GP2),
                instr!(GP0 + GP1 -> GP1),
            ],
        };
        let machine = program.execute();
        assert_eq!(
            machine,
            Machine {
                pc: Word(7),
                sp: Word(u64::MAX - 1),
                gp: vec![Word(1), Word(3), Word(2)],
                memory: Memory(HashMap::from([(Word(u64::MAX - 1), Word(2))])),
            }
        );
    }
}
