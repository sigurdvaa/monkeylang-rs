use crate::code::{read_u16_as_usize, Instruction, Opcode};
use crate::compiler::{Bytecode, Compiler};
use crate::object::Object;

const STACK_SIZE: usize = 2048;

#[derive(Debug)]
pub enum VmError {
    InvalidInstruction(String),
    StackOverflow(String),
    InvalidStackAccess(String),
}

pub struct Vm {
    constants: Vec<Object>,
    instructions: Vec<Instruction>,
    stack: [Option<Object>; STACK_SIZE],
    sp: usize,
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: [const { None }; STACK_SIZE],
            sp: 0,
        }
    }

    pub fn stack_top(&self) -> Option<&Object> {
        match self.sp {
            0 => None,
            _ => self.stack[self.sp - 1].as_ref(),
        }
    }

    fn push(&mut self, obj: Object) -> Result<(), VmError> {
        if self.sp >= STACK_SIZE {
            return Err(VmError::StackOverflow(format!(
                "failed to push object on the stack: {obj:?}"
            )));
        }

        self.stack[self.sp].replace(obj);
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> Result<Object, VmError> {
        dbg!(&self.stack[..5]);
        self.sp -= 1;
        match self.stack[self.sp].take() {
            Some(obj) => Ok(obj),
            None => Err(VmError::InvalidStackAccess(format!(
                "No object found when popping the stack at sp: {}",
                self.sp
            ))),
        }
    }

    pub fn run(&mut self) -> Result<Object, VmError> {
        let mut ip = 0;
        let mut last_pop = Object::None;

        while ip < self.instructions.len() {
            let op =
                Opcode::try_from(self.instructions[ip]).map_err(VmError::InvalidInstruction)?;

            match op {
                Opcode::Constant => {
                    // TODO: use def and width to read and increment ip?
                    let idx = read_u16_as_usize(&self.instructions[ip + 1..]);
                    ip += 2;
                    self.push(self.constants[idx].clone())?;
                }
                Opcode::Add => {
                    let right = self.pop()?;
                    let left = self.pop()?;
                    if let (Object::Integer(left), Object::Integer(right)) = (left, right) {
                        let result = Object::new_integer(left.value + right.value);
                        self.push(result)?;
                    } else {
                        todo!();
                    }
                }
                Opcode::Pop => {
                    last_pop = self.pop()?;
                }
                _ => todo!("{op:?}"),
            }

            ip += 1;
        }

        Ok(last_pop)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::tests::parse_program;

    use super::*;

    fn run_vm_test(input: &str, statements: usize, value: Object) {
        let prog = parse_program(input, statements);
        let mut compiler = Compiler::new();
        let _ = compiler
            .compile_program(&prog)
            .map_err(|e| panic!("compiler error: {e:?}"));
        let mut vm = Vm::new(compiler.bytecode());
        let result = vm.run().unwrap_or_else(|e| panic!("vm error: {e:?}"));
        assert_eq!(
            result, value,
            "unexpected result, got {result:?}, want {value:?}"
        );
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = [("1", 1), ("2", 2), ("1 + 2", 3)];

        for (test_input, test_value) in tests {
            run_vm_test(test_input, 1, Object::new_integer(test_value));
        }
    }
}
