use crate::*;

/// A unit of memory on the `Machine`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Word(pub u64);

impl std::ops::Add for Word {
    type Output = Word;
    fn add(self, other: Word) -> Word {
        Word(self.0 + other.0)
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
pub struct Memory {
    stack: Vec<Word>,
    heap: Vec<Vec<Word>>,
}

enum Address {
    Stack { index: usize },
    Heap { allocation: usize, index: usize },
}

impl From<Word> for Address {
    fn from(Word(word): Word) -> Address {
        #[allow(clippy::cast_lossless, clippy::cast_possible_truncation)]
        let (region, index) = ((((word >> 32) as u32) as usize), ((word as u32) as usize));
        if region == 0 {
            Address::Stack { index }
        } else {
            Address::Heap {
                allocation: region - 1,
                index,
            }
        }
    }
}

impl From<Address> for Word {
    fn from(address: Address) -> Word {
        let (region, index) = match address {
            Address::Stack { index } => (0, index),
            Address::Heap { allocation, index } => (allocation + 1, index),
        };
        #[allow(clippy::cast_lossless, clippy::cast_possible_truncation)]
        Word(((region as u32 as u64) << 32) | (index as u32 as u64))
    }
}

impl Index<Word> for Memory {
    type Output = Word;

    fn index(&self, word: Word) -> &Word {
        match word.into() {
            Address::Stack { index } => &self.stack[index],
            Address::Heap { allocation, index } => &self.heap[allocation][index],
        }
    }
}

impl IndexMut<Word> for Memory {
    fn index_mut(&mut self, word: Word) -> &mut Word {
        match word.into() {
            Address::Stack { index } => {
                if self.stack.len() <= index {
                    self.stack.resize(index + 1, Word(0));
                }
                &mut self.stack[index]
            }
            Address::Heap { allocation, index } => &mut self.heap[allocation][index],
        }
    }
}

impl Memory {
    /// Allocate a new region of memory with the given size.
    pub fn allocate(&mut self, size: Word) -> Word {
        let address = Address::Heap {
            allocation: self.heap.len(),
            index: 0,
        };
        #[allow(clippy::cast_possible_truncation)]
        self.heap.push(vec![Word(0); size.0 as usize]);
        address.into()
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
            sp: Address::Stack { index: 0 }.into(),
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

    // TODO: test `Program::execute`
}
