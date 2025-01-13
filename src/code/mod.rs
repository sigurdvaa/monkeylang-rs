use std::fmt::Display;

pub type Instruction = u8;

#[derive(Debug, PartialEq)]
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
    GetBuiltin,
    Closure,
    GetFree,
    CurrentClosure,
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
            27 if 27 == Self::GetBuiltin as u8 => Ok(Self::GetBuiltin),
            28 if 28 == Self::Closure as u8 => Ok(Self::Closure),
            29 if 29 == Self::GetFree as u8 => Ok(Self::GetFree),
            30 if 30 == Self::CurrentClosure as u8 => Ok(Self::CurrentClosure),
            _ => Err(OpcodeError(op)),
        }
    }
}

impl Opcode {
    fn widths(&self) -> &'static [u8] {
        match self {
            Self::Constant => &[2],
            Self::Jump => &[2],
            Self::JumpNotTrue => &[2],
            Self::GetGlobal => &[2],
            Self::SetGlobal => &[2],
            Self::Array => &[2],
            Self::Hash => &[2],
            Self::Call => &[1],
            Self::GetLocal => &[1],
            Self::SetLocal => &[1],
            Self::GetBuiltin => &[1],
            Self::Closure => &[2, 1],
            Self::GetFree => &[1],
            _ => &[],
        }
    }
}

pub fn make_ins(opcode: Opcode, operands: &[usize]) -> Vec<Instruction> {
    let def = opcode.widths();
    let ins_len = 1 + def.iter().sum::<u8>();

    if def.len() != operands.len() {
        panic!("Opcode with too many operands, opcode {opcode:?}, operands {operands:?}",);
    }
    let mut ins = vec![opcode.clone() as Instruction];
    for (i, operand) in operands.iter().enumerate() {
        let witdh = def[i];
        match witdh {
            2 => ins.extend((*operand as u16).to_be_bytes()),
            1 => ins.push(*operand as Instruction),
            _ => panic!("Operand width not supported, opcode {opcode:?}, def {def:?}",),
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
                let op = Opcode::try_from(self[i])
                    .unwrap_or_else(|e| panic!("ERROR: failed to print instructions, {e}"));
                let def = op.widths();
                let (operands, read) = read_operands(def, &self[i + 1..]);
                buffer.push_str(&format!("{i:0>4} {op:?}",));
                if !operands.is_empty() {
                    buffer.push(' ');
                    buffer.push_str(
                        &operands
                            .iter()
                            .map(|i| i.to_string())
                            .collect::<Vec<_>>()
                            .join(" "),
                    )
                }
                buffer.push('\n');
                i += 1 + read;
            }

            buffer
        }
    }

    fn read_operands(def: &[u8], ins: &[Instruction]) -> (Vec<usize>, usize) {
        let mut operands = vec![];
        let mut offset = 0;
        for width in def {
            match width {
                2 => operands.push(read_u16_as_usize(&ins[offset..])),
                1 => operands.push(ins[offset] as usize),
                0 => (),
                _ => unreachable!(),
            }
            offset += *width as usize;
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
            (
                Opcode::Closure,
                vec![65534, 255],
                vec![Opcode::Closure as u8, 255, 254, 255],
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
            make_ins(Opcode::Closure, &[65535, 255]),
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
            "0013 Closure 65535 255\n",
        );
        assert_eq!(instructions.to_string(), expected);
    }

    #[test]
    fn test_read_operands() {
        let tests = [
            (Opcode::Constant, vec![65535], 2),
            (Opcode::Add, vec![], 0),
            (Opcode::GetLocal, vec![0], 1),
            (Opcode::Closure, vec![65535, 255], 3),
        ];
        for (test_opcode, test_operands, test_read) in tests {
            let ins = make_ins(test_opcode.clone(), &test_operands);
            let def = test_opcode.widths();
            let (operands_read, bytes_read) = read_operands(def, ins[1..].into());
            assert_eq!(bytes_read, test_read);
            assert_eq!(operands_read, test_operands);
        }
    }
}
