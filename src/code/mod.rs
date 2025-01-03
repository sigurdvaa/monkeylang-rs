pub type Instruction = u8;

pub trait Instructions {
    fn to_string(&self) -> String;
}

impl Instructions for [Instruction] {
    fn to_string(&self) -> String {
        let mut buffer = String::new();

        let mut i = 0;
        while i < self.len() {
            let op = Opcode::try_from(self[i]).expect("ERROR: failed to print instructions, {e}");
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

pub struct Definition {
    opcode: Opcode,
    operand_widths: [u32; 2],
}

#[derive(Debug, Clone)]
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
    EnumLength,
}

impl TryFrom<u8> for Opcode {
    type Error = String;

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
            _ => Err(format!("invalid opcode byte: {op:?}")),
        }
    }
}

const DEFINITIONS: &[&Definition; Opcode::EnumLength as usize] = &[
    &Definition {
        opcode: Opcode::Constant,
        operand_widths: [2, 0],
    },
    &Definition {
        opcode: Opcode::Add,
        operand_widths: [0, 0],
    },
    &Definition {
        opcode: Opcode::Pop,
        operand_widths: [0, 0],
    },
    &Definition {
        opcode: Opcode::Sub,
        operand_widths: [0, 0],
    },
    &Definition {
        opcode: Opcode::Mul,
        operand_widths: [0, 0],
    },
    &Definition {
        opcode: Opcode::Div,
        operand_widths: [0, 0],
    },
    &Definition {
        opcode: Opcode::True,
        operand_widths: [0, 0],
    },
    &Definition {
        opcode: Opcode::False,
        operand_widths: [0, 0],
    },
    &Definition {
        opcode: Opcode::Eq,
        operand_widths: [0, 0],
    },
    &Definition {
        opcode: Opcode::NotEq,
        operand_widths: [0, 0],
    },
    &Definition {
        opcode: Opcode::Gt,
        operand_widths: [0, 0],
    },
    &Definition {
        opcode: Opcode::Lt,
        operand_widths: [0, 0],
    },
    &Definition {
        opcode: Opcode::Minus,
        operand_widths: [0, 0],
    },
    &Definition {
        opcode: Opcode::Bang,
        operand_widths: [0, 0],
    },
    &Definition {
        opcode: Opcode::Jump,
        operand_widths: [2, 0],
    },
    &Definition {
        opcode: Opcode::JumpNotTrue,
        operand_widths: [2, 0],
    },
    &Definition {
        opcode: Opcode::Null,
        operand_widths: [0, 0],
    },
];

pub fn make_ins(opcode: Opcode, operands: &[usize]) -> Vec<Instruction> {
    let def = DEFINITIONS[opcode.clone() as usize];
    let ins_len = 1 + def.operand_widths.iter().sum::<u32>();

    let mut ins = vec![opcode as Instruction];
    for (i, operand) in operands.iter().enumerate() {
        // TODO: improve the logic for building ins? and check len?
        let witdh = def.operand_widths[i];
        match witdh {
            2 => ins.extend((*operand as u16).to_be_bytes()),
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

pub fn read_operands(def: &Definition, ins: &[Instruction]) -> (Vec<usize>, usize) {
    let mut operands = vec![];
    let mut offset = 0;
    for width in def.operand_widths {
        match width {
            2 => operands.push(read_u16_as_usize(&ins[offset..])),
            0 => (),
            _ => todo!("{width:?}"),
        }
        offset += width as usize;
    }
    (operands, offset)
}

pub fn read_u16_as_usize(ins: &[Instruction]) -> usize {
    u16::from_be_bytes(
        ins[..2]
            .try_into()
            .expect("not able to convert big-endian bytes to u16"),
    ) as usize
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make() {
        let tests = [
            (
                Opcode::Constant,
                vec![65534],
                vec![Opcode::Constant as u8, 255, 254],
            ),
            (Opcode::Add, vec![], vec![Opcode::Add as u8]),
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
