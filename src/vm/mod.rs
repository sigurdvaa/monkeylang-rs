use crate::code::{read_u16_as_usize, Instruction, Opcode};
use crate::compiler::Bytecode;
use crate::object::Object;

const STACK_SIZE: usize = 2048;
const GLOBALS_SIZE: usize = 65536;

#[derive(Debug)]
pub enum VmError {
    InvalidInstruction(String),
    StackOverflow(String),
    InvalidStackAccess(String),
    InvalidType(String),
    InvalidOperator(String),
    UndefinedGlobalsIndex(usize),
}

pub struct Vm {
    constants: Vec<Object>,
    instructions: Vec<Instruction>,
    stack: [Option<Object>; STACK_SIZE],
    sp: usize,
    globals: Vec<Option<Object>>,
}

impl Vm {
    pub fn new(bytecode: Option<Bytecode>) -> Self {
        let (constants, instructions) = match bytecode {
            Some(bytecode) => (bytecode.constants.clone(), bytecode.instructions.clone()),
            None => (vec![], vec![]),
        };
        Self {
            constants,
            instructions,
            stack: [const { None }; STACK_SIZE],
            sp: 0,
            globals: vec![None; GLOBALS_SIZE],
        }
    }

    pub fn soft_reset(&mut self, bytecode: Bytecode) {
        self.constants = bytecode.constants.clone();
        self.instructions = bytecode.instructions.clone();
        self.stack = [const { None }; STACK_SIZE];
        self.sp = 0;
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
        self.sp -= 1;
        match self.stack[self.sp].take() {
            Some(obj) => Ok(obj),
            None => Err(VmError::InvalidStackAccess(format!(
                "No object found when popping the stack at sp: {}",
                self.sp
            ))),
        }
    }

    fn execute_binary_integer_operation(
        &mut self,
        op: Opcode,
        left: isize,
        right: isize,
    ) -> Result<(), VmError> {
        let value = match op {
            Opcode::Add => left + right,
            Opcode::Sub => left - right,
            Opcode::Mul => left * right,
            Opcode::Div => left / right,
            _ => {
                return Err(VmError::InvalidOperator(format!(
                    "unknown integer operator: {op:?}",
                )))
            }
        };
        self.push(Object::new_integer(value))?;
        Ok(())
    }

    fn execute_binary_operation(&mut self, op: Opcode) -> Result<(), VmError> {
        let right = self.pop()?;
        let left = self.pop()?;
        match (&left, &right) {
            (Object::Integer(left), Object::Integer(right)) => {
                self.execute_binary_integer_operation(op, left.value, right.value)
            }
            _ => Err(VmError::InvalidType(format!(
                "unsupported types for binary operation: {} {}",
                left.kind(),
                right.kind()
            ))),
        }
    }

    fn execute_integer_comparison(
        &mut self,
        op: Opcode,
        left: isize,
        right: isize,
    ) -> Result<(), VmError> {
        let value = match op {
            Opcode::Eq => left == right,
            Opcode::NotEq => left != right,
            Opcode::Gt => left > right,
            Opcode::Lt => left < right,
            _ => {
                return Err(VmError::InvalidOperator(format!(
                    "unknown integer comparison: {op:?}",
                )))
            }
        };
        self.push(Object::new_boolean(value))?;
        Ok(())
    }

    fn execute_boolean_comparison(
        &mut self,
        op: Opcode,
        left: bool,
        right: bool,
    ) -> Result<(), VmError> {
        let value = match op {
            Opcode::Eq => left == right,
            Opcode::NotEq => left != right,
            _ => {
                return Err(VmError::InvalidOperator(format!(
                    "unknown boolean comparison: {op:?}",
                )))
            }
        };
        self.push(Object::new_boolean(value))?;
        Ok(())
    }

    fn execute_comparison(&mut self, op: Opcode) -> Result<(), VmError> {
        let right = self.pop()?;
        let left = self.pop()?;
        match (&left, &right) {
            (Object::Integer(left), Object::Integer(right)) => {
                self.execute_integer_comparison(op, left.value, right.value)
            }
            (Object::Boolean(left), Object::Boolean(right)) => {
                self.execute_boolean_comparison(op, left.value, right.value)
            }
            _ => Err(VmError::InvalidType(format!(
                "unknown operator: {op:?} ({} {})",
                left.kind(),
                right.kind()
            ))),
        }
    }

    fn execute_bang_operator(&mut self) -> Result<(), VmError> {
        let operand = self.pop()?;
        self.push(Object::new_boolean(!operand.is_truthy()))
        // TODO: keep obj truthiness or use only Object::Boolean?
        // match &operand {
        //     Object::Null => self.push(Object::new_boolean(true)),
        //     Object::Integer(obj) => self.push(Object::new_boolean(obj.value < 1)),
        //     Object::Boolean(obj) => self.push(Object::new_boolean(!obj.value)),
        //     _ => Err(VmError::InvalidType(format!(
        //         "unsupported type for bang prefix operator: {}",
        //         operand.kind()
        //     ))),
        // }
    }

    fn execute_minus_operator(&mut self) -> Result<(), VmError> {
        let operand = self.pop()?;
        match &operand {
            Object::Integer(obj) => self.push(Object::new_integer(-obj.value)),
            _ => Err(VmError::InvalidType(format!(
                "unsupported type for minus prefix operator: {}",
                operand.kind()
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
                Opcode::Bang => self.execute_bang_operator()?,
                Opcode::Minus => self.execute_minus_operator()?,
                Opcode::True => self.push(Object::new_boolean(true))?,
                Opcode::False => self.push(Object::new_boolean(false))?,
                Opcode::Null => self.push(Object::new_null())?,
                Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                    self.execute_binary_operation(op)?
                }
                Opcode::Eq | Opcode::NotEq | Opcode::Gt | Opcode::Lt => {
                    self.execute_comparison(op)?
                }
                Opcode::Pop => {
                    last_pop = self.pop()?;
                }
                Opcode::Jump => {
                    let pos = read_u16_as_usize(&self.instructions[ip + 1..]);
                    ip = pos - 1;
                }
                Opcode::JumpNotTrue => {
                    let pos = read_u16_as_usize(&self.instructions[ip + 1..]);
                    ip += 2;
                    let condition = self.pop()?;
                    if !condition.is_truthy() {
                        ip = pos - 1;
                    }
                }
                Opcode::Constant => {
                    // TODO: use def and width to read and increment ip?
                    let idx = read_u16_as_usize(&self.instructions[ip + 1..]);
                    ip += 2;
                    self.push(self.constants[idx].clone())?;
                }
                Opcode::GetGlobal => {
                    let idx = read_u16_as_usize(&self.instructions[ip + 1..]);
                    ip += 2;
                    match &self.globals[idx] {
                        Some(obj) => self.push(obj.clone())?,
                        None => return Err(VmError::UndefinedGlobalsIndex(idx)),
                    }
                }
                Opcode::SetGlobal => {
                    let idx = read_u16_as_usize(&self.instructions[ip + 1..]);
                    ip += 2;
                    let obj = self.pop()?;
                    self.globals[idx] = Some(obj);
                }
                Opcode::EnumLength => unreachable!(),
            }

            ip += 1;
        }

        Ok(last_pop)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::Compiler;
    use crate::parser::tests::parse_program;

    fn run_vm_test(input: &str, statements: usize, value: Object) {
        let prog = parse_program(input, statements);
        let mut compiler = Compiler::new();
        let _ = compiler
            .compile_program(&prog)
            .map_err(|e| panic!("compiler error: {e:?}"));
        let mut vm = Vm::new(Some(compiler.bytecode()));
        let result = vm.run().unwrap_or_else(|e| panic!("vm error: {e:?}"));
        assert_eq!(
            result, value,
            "unexpected result, got {result:?}, want {value:?}"
        );
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = [
            ("1", 1),
            ("2", 2),
            ("1 + 2", 3),
            ("1 - 2", -1),
            ("1 * 2", 2),
            ("4 / 2", 2),
            ("50 / 2 * 2 + 10 - 5", 55),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("5 * (2 + 10)", 60),
            ("-5", -5),
            ("-10", -10),
            ("-50 + 100 + -50", 0),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        for (test_input, test_value) in tests {
            run_vm_test(test_input, 1, Object::new_integer(test_value));
        }
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
            ("!(if (false) { 5; })", true),
        ];
        for (test_input, test_value) in tests {
            run_vm_test(test_input, 1, Object::new_boolean(test_value));
        }
    }

    #[test]
    fn test_conditionals() {
        let tests = [
            ("if (true) { 10 }", Object::new_integer(10)),
            ("if (true) { 10 } else { 20 }", Object::new_integer(10)),
            ("if (false) { 10 } else { 20 } ", Object::new_integer(20)),
            ("if (1) { 10 }", Object::new_integer(10)),
            ("if (1 < 2) { 10 }", Object::new_integer(10)),
            ("if (1 < 2) { 10 } else { 20 }", Object::new_integer(10)),
            ("if (1 > 2) { 10 } else { 20 }", Object::new_integer(20)),
            ("if (1 > 2) { 10 }", Object::new_null()),
            ("if (false) { 10 }", Object::new_null()),
            (
                "if ((if (false) { 10 })) { 10 } else { 20 }",
                Object::new_integer(20),
            ),
        ];
        for (test_input, test_value) in tests {
            run_vm_test(test_input, 1, test_value);
        }
    }

    #[test]
    fn test_global_let_statements() {
        let tests = [
            ("let one = 1; one", 2, 1),
            ("let one = 1; let two = 2; one + two", 3, 3),
            ("let one = 1; let two = one + one; one + two", 3, 3),
        ];
        for (test_input, test_stmts, test_value) in tests {
            run_vm_test(test_input, test_stmts, Object::new_integer(test_value));
        }
    }
}
