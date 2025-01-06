use crate::{code::Instruction, object::ClosureObj};
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Frame {
    pub closure: Rc<ClosureObj>,
    pub ip: usize,
    pub bp: usize,
}

impl Frame {
    pub fn new(func: Rc<ClosureObj>, bp: usize) -> Self {
        Self {
            closure: func,
            ip: 0,
            bp,
        }
    }

    pub fn ins(&self) -> &[Instruction] {
        &self.closure.func.instructions
    }
}
