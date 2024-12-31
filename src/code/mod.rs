pub type Instructions = Vec<u8>;
pub type OpcodeType = u8;

pub struct Definition {
    opcode: Opcode,
    operand_widths: [u32; 2],
}

#[derive(Debug, Clone)]
pub enum Opcode {
    Constant,
    EnumLength,
}

const DEFINITIONS: &[&Definition; Opcode::EnumLength as usize] = &[&Definition {
    opcode: Opcode::Constant,
    operand_widths: [2, 0],
}];

// impl Opcode {
//     // TODO: change to hashmap? or can it be made static?
//     pub fn get_definitions() -> [Definition; Opcode::EnumLength as usize] {
//         [Definition {
//             opcode: Opcode::Constant,
//             operand_widths: vec![2],
//         }]
//     }
//
//     // TODO: remove?
//     pub fn lookup(opcode: Self) -> Result<Definition, String> {
//         todo!()
//     }
// }

pub fn make(opcode: Opcode, operands: &[u16]) -> Instructions {
    let def = DEFINITIONS[opcode.clone() as usize];
    let ins_len = 1 + def.operand_widths.iter().sum::<u32>();

    let mut ins = vec![opcode as OpcodeType];
    for (i, operand) in operands.iter().enumerate() {
        // TODO: improve the logic for building ins? and check len?
        let witdh = def.operand_widths[i];
        match witdh {
            2 => ins.extend(operand.to_be_bytes()),
            _ => todo!(),
        }
    }

    // TODO: remove assertion?
    assert_eq!(
        ins.len(),
        ins_len as usize,
        "instruction don't have the expected length, got {}, want {}",
        ins.len(),
        ins_len
    );
    ins
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make() {
        let tests = [(
            Opcode::Constant,
            vec![65534],
            vec![Opcode::Constant as u8, 255u8, 254u8],
        )];
        for (test_opcode, test_operands, test_value) in tests {
            let instruction = make(test_opcode, &test_operands);
            assert_eq!(instruction.len(), test_value.len());
            assert_eq!(instruction, test_value);
        }
    }
}
