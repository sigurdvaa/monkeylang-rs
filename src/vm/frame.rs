use super::Instruction;

#[derive(Debug)]
pub struct Frame {
    pub instructions: Vec<Instruction>,
    pub ip: usize,
}

impl Frame {
    pub fn new(instructions: Vec<Instruction>) -> Self {
        Self {
            instructions,
            ip: 0,
        }
    }
}

