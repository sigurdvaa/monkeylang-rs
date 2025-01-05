use crate::object::CompiledFunctionObj;
use std::rc::Rc;

#[derive(Debug)]
pub struct Frame {
    // TODO: can we replace this with ref or Rc?
    pub func: Rc<CompiledFunctionObj>,
    pub ip: usize,
    pub bp: usize,
}

impl Frame {
    pub fn new(func: Rc<CompiledFunctionObj>, bp: usize) -> Self {
        Self { func, ip: 0, bp }
    }
}
