use crate::*;

/// A unit of memory on the `Machine`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Word(u64);

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
    /// Copy a value from one register to another.
    Mov {
        /// The register to copy out of.
        src: Register,
        /// The register to copy into.
        dst: Register,
    },
    /// Read a value from memory.
    Load {
        /// The location to read from.
        src: Register,
        /// The register to write the value into.
        dst: Register,
    },
}

impl Machine {
    /// Run one `Instruction` on this `Machine`.
    pub fn execute(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::Mov { src, dst } => self[dst] = self[src],
            Instruction::Load { src, dst } => self[dst] = self.memory[self[src]],
        }
    }
}
