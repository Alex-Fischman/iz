type Register = u8;
type Label = u64;
type Data = i64;

const MEM_SIZE: usize = 16;
type Memory = [Data; MEM_SIZE];

const REG_SIZE: usize = 16;
struct Registers {
    data: [Data; REG_SIZE],
}

/*
Registers:
0:  zero
1:  program counter
2:  stack pointer
3:  general 0
4:  general 1
*/

impl Registers {
    fn new() -> Registers {
        Registers {
            data: [0; REG_SIZE],
        }
    }

    fn pc(&mut self) -> &mut Data {
        &mut self.data[1]
    }
}

impl std::ops::Index<Register> for Registers {
    type Output = Data;
    fn index(&self, i: Register) -> &Self::Output {
        if i == 0 {
            &0
        } else {
            &self.data[i as usize]
        }
    }
}

impl std::ops::IndexMut<Register> for Registers {
    fn index_mut(&mut self, i: Register) -> &mut Self::Output {
        &mut self.data[i as usize]
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum I {
    LBL(Label),
    JMP(Register, Data),
    BEQ(Register, Register, Label),
    BNE(Register, Register, Label),
    BLT(Register, Register, Label),
    LOA(Register, Data, Register),
    STO(Register, Register, Data),
    ADD(Register, Register, Register),
    ADI(Register, Data, Register),
    SUB(Register, Register, Register),
    SUI(Register, Data, Register),
    SHF(Register, Register, Register),
    SHI(Register, Data, Register),
    XOR(Register, Register, Register),
    XOI(Register, Data, Register),
    ORR(Register, Register, Register),
    ORI(Register, Data, Register),
    AND(Register, Register, Register),
    ANI(Register, Data, Register),
}

#[allow(dead_code)]
pub fn execute(program: Vec<I>) -> Memory {
    let mut memory = [0; MEM_SIZE];
    let mut registers = Registers::new();
    let mut labels = std::collections::HashMap::new();
    for (i, p) in program.iter().enumerate() {
        if let I::LBL(l) = p {
            labels.insert(l, i as Data);
        }
    }

    println!("{:?}", labels);

    while (*registers.pc() as usize) < program.len() {
        match &program[*registers.pc() as usize] {
            I::LBL(_) => {}
            I::JMP(s, o) => *registers.pc() = registers[*s] + o - 1,
            I::BEQ(a, b, l) => {
                if a == b {
                    *registers.pc() = labels[l] - 1;
                }
            }
            I::BNE(a, b, l) => {
                if a != b {
                    *registers.pc() = labels[l] - 1;
                }
            }
            I::BLT(a, b, l) => {
                if a < b {
                    *registers.pc() = labels[l] - 1;
                }
            }
            I::LOA(s, o, r) => registers[*r] = memory[(registers[*s] + o) as usize],
            I::STO(a, s, o) => memory[(registers[*s] + o) as usize] = registers[*a],
            I::ADD(a, b, r) => registers[*r] = registers[*a] + registers[*b],
            I::ADI(a, b, r) => registers[*r] = registers[*a] + b,
            I::SUB(a, b, r) => registers[*r] = registers[*a] - registers[*b],
            I::SUI(a, b, r) => registers[*r] = registers[*a] - b,
            I::SHF(a, b, r) => registers[*r] = registers[*a] << registers[*b],
            I::SHI(a, b, r) => registers[*r] = registers[*a] << b,
            I::XOR(a, b, r) => registers[*r] = registers[*a] ^ registers[*b],
            I::XOI(a, b, r) => registers[*r] = registers[*a] ^ b,
            I::ORR(a, b, r) => registers[*r] = registers[*a] | registers[*b],
            I::ORI(a, b, r) => registers[*r] = registers[*a] | b,
            I::AND(a, b, r) => registers[*r] = registers[*a] & registers[*b],
            I::ANI(a, b, r) => registers[*r] = registers[*a] & b,
        }
        *registers.pc() += 1;
    }

    memory
}
