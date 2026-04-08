use crate::*;

/// A unit of memory on the `Machine`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Word(u64);

impl std::ops::Add for Word {
    type Output = Word;
    fn add(self, other: Word) -> Word {
        Word(self.0 + other.0)
    }
}

/// The state of the virtual machine that `Instruction`s run on.
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
#[derive(Default)]
pub struct Memory(Vec<Vec<Word>>);

fn word_to_address(Word(word): Word) -> (usize, usize) {
    #[allow(clippy::cast_lossless, clippy::cast_possible_truncation)]
    ((((word >> 32) as u32) as usize), ((word as u32) as usize))
}

fn address_to_word(hi: usize, lo: usize) -> Word {
    #[allow(clippy::cast_lossless, clippy::cast_possible_truncation)]
    Word(((hi as u32 as u64) << 32) | (lo as u32 as u64))
}

impl Index<Word> for Memory {
    type Output = Word;
    fn index(&self, word: Word) -> &Word {
        let (hi, lo) = word_to_address(word);
        &self.0[hi][lo]
    }
}

impl IndexMut<Word> for Memory {
    fn index_mut(&mut self, word: Word) -> &mut Word {
        let (hi, lo) = word_to_address(word);
        &mut self.0[hi][lo]
    }
}

impl Memory {
    /// Allocate a new region of memory with the given size.
    pub fn allocate(&mut self, size: Word) -> Word {
        let out = address_to_word(self.0.len(), 0);
        #[allow(clippy::cast_possible_truncation)]
        self.0.push(vec![Word(0); size.0 as usize]);
        out
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
    /// Add two registers together.
    Add {
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
pub struct Program {
    instructions: Vec<Instruction>,
}

impl Program {
    fn gp_registers(&self) -> impl std::iter::Iterator<Item = usize> {
        self.instructions
            .iter()
            .flat_map(|instruction| match instruction {
                Instruction::Imm { imm: _, dst } => vec![dst],
                Instruction::Mov { src, dst } => vec![src, dst],
                Instruction::Add { src: (x, y), dst } => vec![x, y, dst],
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
                Instruction::Add { src: (x, y), dst } => vec![x, y, dst],
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
        self.gp_registers().max().unwrap_or(0)
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
            sp: Word(0),
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
