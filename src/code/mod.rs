pub type Instruction = u8;

trait Instructions {
    fn to_string(&self) -> String;
}

impl Instructions for [Instruction] {
    fn to_string(&self) -> String {
        let mut buffer = vec![];

        let mut i = 0;
        while i < self.len() {
            let op = Opcode::try_from(self[i]).expect("ERROR: failed to print instructions, {e}");
            let def = DEFINITIONS[op.clone() as usize];
            let (operands, read) = read_operands(def, &self[i + 1..]);
            buffer.push(format!(
                "{i:0>4} {op:?} {}\n",
                operands
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            ));
            i += 1 + read;
        }

        buffer.join("")
    }
}

pub struct Definition {
    opcode: Opcode,
    operand_widths: [u32; 2],
}

#[derive(Debug, Clone)]
pub enum Opcode {
    Constant,
    EnumLength,
}

impl TryFrom<u8> for Opcode {
    type Error = String;

    fn try_from(op: u8) -> Result<Self, Self::Error> {
        match op {
            0 => Ok(Self::Constant),
            _ => Err(format!("invalid opcode byte: {op:?}")),
        }
    }
}

const DEFINITIONS: &[&Definition; Opcode::EnumLength as usize] = &[&Definition {
    opcode: Opcode::Constant,
    operand_widths: [2, 0],
}];

pub fn make_ins(opcode: Opcode, operands: &[usize]) -> Vec<Instruction> {
    let def = DEFINITIONS[opcode.clone() as usize];
    let ins_len = 1 + def.operand_widths.iter().sum::<u32>();

    let mut ins = vec![opcode as Instruction];
    for (i, operand) in operands.iter().enumerate() {
        // TODO: improve the logic for building ins? and check len?
        let witdh = def.operand_widths[i];
        match witdh {
            2 => ins.extend((*operand as u16).to_be_bytes()),
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
    dbg!(&ins);
    for width in def.operand_widths {
        match width {
            2 => operands.push(u16::from_be_bytes(
                ins[offset..offset + 2]
                    .try_into()
                    .expect("not able to convert be bytes to u16"),
            ) as usize),
            0 => (),
            _ => todo!("{width:?}"),
        }
        offset += width as usize;
    }

    (operands, offset)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make() {
        let tests = [(
            Opcode::Constant,
            vec![65534],
            vec![Opcode::Constant as u8, 255, 254],
        )];
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
        ]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();
        let expected = concat!(
            "0000 Constant 1\n",
            "0003 Constant 2\n",
            "0006 Constant 65535\n",
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
