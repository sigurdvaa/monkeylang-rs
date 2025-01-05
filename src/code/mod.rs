use std::fmt::Display;

pub type Instruction = u8;

pub struct Definition {
    // TODO: remove unused field?
    _opcode: Opcode,
    // TODO: rework def to vec?
    operand_widths: [u32; 2],
}

#[derive(Debug)]
pub struct OpcodeError(u8);

impl std::error::Error for OpcodeError {}

impl Display for OpcodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "unknown opcode byte: {:#x}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    Constant,
    Add,
    Pop,
    Sub,
    Mul,
    Div,
    True,
    False,
    Eq,
    NotEq,
    Gt,
    Lt,
    Minus,
    Bang,
    Jump,
    JumpNotTrue,
    Null,
    GetGlobal,
    SetGlobal,
    Array,
    Hash,
    Index,
    Call,
    Return,
    ReturnValue,
    GetLocal,
    SetLocal,
    EnumLength,
}

impl TryFrom<u8> for Opcode {
    type Error = OpcodeError;

    fn try_from(op: u8) -> Result<Self, Self::Error> {
        match op {
            0 if 0 == Self::Constant as u8 => Ok(Self::Constant),
            1 if 1 == Self::Add as u8 => Ok(Self::Add),
            2 if 2 == Self::Pop as u8 => Ok(Self::Pop),
            3 if 3 == Self::Sub as u8 => Ok(Self::Sub),
            4 if 4 == Self::Mul as u8 => Ok(Self::Mul),
            5 if 5 == Self::Div as u8 => Ok(Self::Div),
            6 if 6 == Self::True as u8 => Ok(Self::True),
            7 if 7 == Self::False as u8 => Ok(Self::False),
            8 if 8 == Self::Eq as u8 => Ok(Self::Eq),
            9 if 9 == Self::NotEq as u8 => Ok(Self::NotEq),
            10 if 10 == Self::Gt as u8 => Ok(Self::Gt),
            11 if 11 == Self::Lt as u8 => Ok(Self::Lt),
            12 if 12 == Self::Minus as u8 => Ok(Self::Minus),
            13 if 13 == Self::Bang as u8 => Ok(Self::Bang),
            14 if 14 == Self::Jump as u8 => Ok(Self::Jump),
            15 if 15 == Self::JumpNotTrue as u8 => Ok(Self::JumpNotTrue),
            16 if 16 == Self::Null as u8 => Ok(Self::Null),
            17 if 17 == Self::GetGlobal as u8 => Ok(Self::GetGlobal),
            18 if 18 == Self::SetGlobal as u8 => Ok(Self::SetGlobal),
            19 if 19 == Self::Array as u8 => Ok(Self::Array),
            20 if 20 == Self::Hash as u8 => Ok(Self::Hash),
            21 if 21 == Self::Index as u8 => Ok(Self::Index),
            22 if 22 == Self::Call as u8 => Ok(Self::Call),
            23 if 23 == Self::Return as u8 => Ok(Self::Return),
            24 if 24 == Self::ReturnValue as u8 => Ok(Self::ReturnValue),
            25 if 25 == Self::GetLocal as u8 => Ok(Self::GetLocal),
            26 if 26 == Self::SetLocal as u8 => Ok(Self::SetLocal),
            _ => Err(OpcodeError(op)),
        }
    }
}

const DEFINITIONS: &[&Definition; Opcode::EnumLength as usize] = &[
    &Definition {
        _opcode: Opcode::Constant,
        operand_widths: [2, 0],
    },
    &Definition {
        _opcode: Opcode::Add,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::Pop,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::Sub,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::Mul,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::Div,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::True,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::False,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::Eq,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::NotEq,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::Gt,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::Lt,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::Minus,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::Bang,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::Jump,
        operand_widths: [2, 0],
    },
    &Definition {
        _opcode: Opcode::JumpNotTrue,
        operand_widths: [2, 0],
    },
    &Definition {
        _opcode: Opcode::Null,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::GetGlobal,
        operand_widths: [2, 0],
    },
    &Definition {
        _opcode: Opcode::SetGlobal,
        operand_widths: [2, 0],
    },
    &Definition {
        _opcode: Opcode::Array,
        operand_widths: [2, 0],
    },
    &Definition {
        _opcode: Opcode::Hash,
        operand_widths: [2, 0],
    },
    &Definition {
        _opcode: Opcode::Index,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::Call,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::Return,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::ReturnValue,
        operand_widths: [0, 0],
    },
    &Definition {
        _opcode: Opcode::GetLocal,
        operand_widths: [1, 0],
    },
    &Definition {
        _opcode: Opcode::SetLocal,
        operand_widths: [1, 0],
    },
];

pub fn make_ins(opcode: Opcode, operands: &[usize]) -> Vec<Instruction> {
    // TODO: replace const with method on enum?
    let def = DEFINITIONS[opcode.clone() as usize];
    let ins_len = 1 + def.operand_widths.iter().sum::<u32>();

    let mut ins = vec![opcode as Instruction];
    for (i, operand) in operands.iter().enumerate() {
        // TODO: improve the logic for building ins? and check len?
        let witdh = def.operand_widths[i];
        match witdh {
            2 => ins.extend((*operand as u16).to_be_bytes()),
            1 => ins.push(*operand as u8),
            0 => (),
            _ => todo!(),
        }
    }

    assert_eq!(
        ins.len(),
        ins_len as usize,
        "ERROR: instruction has expected length, got {}, want {}",
        ins.len(),
        ins_len
    );

    ins
}

pub fn read_u16_as_usize(ins: &[Instruction]) -> usize {
    u16::from_be_bytes(
        ins[..2]
            .try_into()
            .expect("not able to convert big-endian bytes to u16"),
    ) as usize
}

#[cfg(test)]
pub mod tests {
    use super::*;

    pub trait ReadInstructions {
        fn to_string(&self) -> String;
    }

    impl ReadInstructions for [Instruction] {
        fn to_string(&self) -> String {
            let mut buffer = String::new();

            let mut i = 0;
            while i < self.len() {
                let op =
                    Opcode::try_from(self[i]).expect("ERROR: failed to print instructions, {e}");
                let def = DEFINITIONS[op.clone() as usize];
                let (operands, read) = read_operands(def, &self[i + 1..]);
                buffer.push_str(&format!("{i:0>4} {op:?}",));
                if !operands.is_empty() {
                    buffer.push(' ');
                    buffer.push_str(
                        &operands
                            .iter()
                            .map(|i| i.to_string())
                            .collect::<Vec<_>>()
                            .join(","),
                    )
                }
                buffer.push('\n');
                i += 1 + read;
            }

            buffer
        }
    }

    fn read_operands(def: &Definition, ins: &[Instruction]) -> (Vec<usize>, usize) {
        let mut operands = vec![];
        let mut offset = 0;
        for width in def.operand_widths {
            match width {
                2 => operands.push(read_u16_as_usize(&ins[offset..])),
                1 => operands.push(ins[offset] as usize),
                0 => (),
                _ => todo!("{width:?}"),
            }
            offset += width as usize;
        }
        (operands, offset)
    }

    #[test]
    fn test_make() {
        let tests = [
            (
                Opcode::Constant,
                vec![65534],
                vec![Opcode::Constant as u8, 255, 254],
            ),
            (Opcode::Add, vec![], vec![Opcode::Add as u8]),
            (
                Opcode::GetLocal,
                vec![255],
                vec![Opcode::GetLocal as u8, 255],
            ),
        ];
        for (test_opcode, test_operands, test_value) in tests {
            let instruction = make_ins(test_opcode, &test_operands);
            assert_eq!(instruction.len(), test_value.len());
            assert_eq!(instruction, test_value);
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions = [
            make_ins(Opcode::Constant, &[1]),
            make_ins(Opcode::Constant, &[2]),
            make_ins(Opcode::Constant, &[65535]),
            make_ins(Opcode::Add, &[]),
            make_ins(Opcode::Pop, &[]),
            make_ins(Opcode::GetLocal, &[1]),
        ]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();
        let expected = concat!(
            "0000 Constant 1\n",
            "0003 Constant 2\n",
            "0006 Constant 65535\n",
            "0009 Add\n",
            "0010 Pop\n",
            "0011 GetLocal 1\n",
        );
        assert_eq!(instructions.to_string(), expected);
    }

    #[test]
    fn test_read_operands() {
        let tests = [(Opcode::Constant, &[65535], 2)];
        for (test_opcode, test_operands, test_read) in tests {
            let ins = make_ins(test_opcode.clone(), test_operands);
            let def = DEFINITIONS[test_opcode as usize];
            let (operands_read, bytes_read) = read_operands(def, ins[1..].into());
            assert_eq!(bytes_read, test_read);
            assert_eq!(operands_read, test_operands);
        }
    }
}
