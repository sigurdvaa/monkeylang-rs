use crate::code::{read_u16_as_usize, Instruction, Opcode, OpcodeError};
use crate::compiler::Bytecode;
use crate::object::{Array, HashKeyData, HashKeyError, HashObj, Integer, Object};
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

const STACK_SIZE: usize = 2048;
const GLOBALS_SIZE: usize = 65536;

#[derive(Debug)]
pub enum VmError {
    InvalidInstruction(OpcodeError),
    InvalidHashKey(HashKeyError),
    StackOverflow(Object),
    InvalidStackAccess(usize),
    InvalidGlobalsIndex(usize),
    InvalidBooleanOperator(Opcode),
    InvalidStringOperator(Opcode),
    InvalidIntegerOperator(Opcode),
    InvalidBinaryTypes(&'static str, &'static str),
    InvalidComparisonTypes(&'static str, &'static str),
    InvalidIndexTypes(&'static str, &'static str),
    InvalidPrefixType(&'static str),
}

impl std::error::Error for VmError {}

impl Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidInstruction(operr) => write!(f, "invalid instruction: {operr}"),
            Self::InvalidHashKey(keyerr) => write!(f, "invalid hash key: {keyerr}"),
            Self::StackOverflow(obj) => write!(f, "stack overflow, can't push object: {obj:?}"),
            Self::InvalidGlobalsIndex(idx) => {
                write!(f, "invalid globals access, no object at index {idx}")
            }
            Self::InvalidStackAccess(idx) => {
                write!(f, "invalid stack access, no object at index {idx}")
            }
            Self::InvalidBooleanOperator(op) => write!(f, "invalid boolean operator: {op:?}"),
            Self::InvalidStringOperator(op) => write!(f, "invalid string operator: {op:?}"),
            Self::InvalidIntegerOperator(op) => write!(f, "invalid integer operator: {op:?}"),
            Self::InvalidBinaryTypes(left, right) => {
                write!(f, "unsupported types for binary operation: {left} {right}")
            }
            Self::InvalidComparisonTypes(left, right) => {
                write!(
                    f,
                    "unsupported types for comparison operation: {left} {right}"
                )
            }
            Self::InvalidIndexTypes(left, idx) => {
                write!(f, "unsupported types for index operation: {left}[{idx}]")
            }
            Self::InvalidPrefixType(operand) => {
                write!(f, "unsupported type for minus prefix operator: {operand}",)
            }
        }
    }
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
            return Err(VmError::StackOverflow(obj));
        }

        self.stack[self.sp].replace(obj);
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> Result<Object, VmError> {
        self.sp -= 1;
        match self.stack[self.sp].take() {
            Some(obj) => Ok(obj),
            None => Err(VmError::InvalidStackAccess(self.sp)),
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
            _ => return Err(VmError::InvalidIntegerOperator(op)),
        };
        self.push(Object::new_integer(value))?;
        Ok(())
    }

    fn execute_binary_string_operation(
        &mut self,
        op: Opcode,
        left: &str,
        right: &str,
    ) -> Result<(), VmError> {
        let value = match op {
            Opcode::Add => String::from_iter([left, right]),
            _ => return Err(VmError::InvalidStringOperator(op)),
        };
        self.push(Object::new_string(value))?;
        Ok(())
    }

    fn execute_binary_operation(&mut self, op: Opcode) -> Result<(), VmError> {
        let right = self.pop()?;
        let left = self.pop()?;
        match (&left, &right) {
            (Object::Integer(left), Object::Integer(right)) => {
                self.execute_binary_integer_operation(op, left.value, right.value)
            }
            (Object::String(left), Object::String(right)) => {
                self.execute_binary_string_operation(op, &left.value, &right.value)
            }
            _ => Err(VmError::InvalidBinaryTypes(left.kind(), right.kind())),
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
            _ => return Err(VmError::InvalidIntegerOperator(op)),
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
            _ => return Err(VmError::InvalidBooleanOperator(op)),
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
            _ => Err(VmError::InvalidComparisonTypes(left.kind(), right.kind())),
        }
    }

    fn execute_bang_operator(&mut self) -> Result<(), VmError> {
        let operand = self.pop()?;
        self.push(Object::new_boolean(!operand.is_truthy()))
    }

    fn execute_minus_operator(&mut self) -> Result<(), VmError> {
        let operand = self.pop()?;
        match &operand {
            Object::Integer(obj) => self.push(Object::new_integer(-obj.value)),
            _ => Err(VmError::InvalidPrefixType(operand.kind())),
        }
    }

    fn execute_array_index(left: &Array, index: &Integer) -> Rc<Object> {
        left.get(index.value as usize)
            .unwrap_or(&Rc::new(Object::new_null()))
            .clone()
    }

    fn execute_hash_index(left: &HashObj, index: &HashKeyData) -> Rc<Object> {
        match left.get(index) {
            Some((_key, value)) => value.clone(),
            None => Rc::new(Object::new_null()),
        }
    }

    fn execute_index_expression(&mut self) -> Result<(), VmError> {
        let idx = self.pop()?;
        let left = self.pop()?;
        let obj = match (&left, &idx) {
            (Object::Array(obj), Object::Integer(idx)) => Self::execute_array_index(obj, idx),
            (Object::Hash(obj), _) => {
                // TODO: does the cache still work?
                let hash_key = idx.hash_key().map_err(VmError::InvalidHashKey)?;
                Self::execute_hash_index(obj, &hash_key)
            }
            _ => return Err(VmError::InvalidIndexTypes(left.kind(), idx.kind())),
        };
        self.push((*obj).clone())
    }

    fn build_array(&mut self, start_idx: usize, end_idx: usize) -> Result<Object, VmError> {
        // TODO: can we remove Rc from array?
        let mut array = Vec::with_capacity(end_idx - start_idx);
        for i in start_idx..end_idx {
            match self.stack[i].take() {
                Some(obj) => array.push(Rc::new(obj)),
                None => return Err(VmError::InvalidStackAccess(i)),
            }
        }
        Ok(Object::Array(array))
    }

    fn build_hash(&mut self, start_idx: usize, end_idx: usize) -> Result<Object, VmError> {
        // TODO: can we remove Rc from hash?
        let mut hash = HashMap::with_capacity(end_idx - start_idx);
        for i in (start_idx..end_idx).step_by(2) {
            let key = match self.stack[i].take() {
                Some(obj) => obj,
                None => return Err(VmError::InvalidStackAccess(i)),
            };
            let value = match self.stack[i + 1].take() {
                Some(obj) => obj,
                None => return Err(VmError::InvalidStackAccess(i)),
            };
            let hash_key = key.hash_key().map_err(VmError::InvalidHashKey)?;
            let pair = (Rc::new(key), Rc::new(value));
            hash.insert(hash_key, pair);
        }
        Ok(Object::Hash(hash))
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
                        _ => return Err(VmError::InvalidGlobalsIndex(idx)),
                    }
                }
                Opcode::SetGlobal => {
                    let idx = read_u16_as_usize(&self.instructions[ip + 1..]);
                    ip += 2;
                    let obj = self.pop()?;
                    self.globals[idx] = Some(obj);
                }
                Opcode::Array => {
                    let len = read_u16_as_usize(&self.instructions[ip + 1..]);
                    ip += 2;
                    let array = self.build_array(self.sp - len, self.sp)?;
                    self.sp -= len;
                    self.push(array)?;
                }
                Opcode::Hash => {
                    let len = read_u16_as_usize(&self.instructions[ip + 1..]);
                    ip += 2;
                    let hash = self.build_hash(self.sp - len, self.sp)?;
                    self.sp -= len;
                    self.push(hash)?;
                }
                Opcode::Index => self.execute_index_expression()?,
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
        let result = vm.run().unwrap_or_else(|e| panic!("vm error:\n {e}"));
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

    #[test]
    fn test_string_expressions() {
        let tests = [
            (r#""monkey""#, "monkey"),
            (r#""mon" + "key""#, "monkey"),
            (r#""mon" + "key" + "banana""#, "monkeybanana"),
        ];
        for (test_input, test_value) in tests {
            run_vm_test(test_input, 1, Object::new_string(test_value.into()));
        }
    }

    #[test]
    fn test_array_literals() {
        let tests = [
            ("[]", vec![]),
            ("[1, 2, 3]", vec![1, 2, 3]),
            ("[1 + 2, 3 * 4, 5 + 6]", vec![3, 12, 11]),
        ];
        for (test_input, test_value) in tests {
            let mut test_array = vec![];
            for v in test_value {
                test_array.push(Rc::new(Object::new_integer(v)));
            }
            run_vm_test(test_input, 1, Object::Array(test_array));
        }
    }

    #[test]
    fn test_hash_literals() {
        let tests = [
            ("{}", vec![]),
            ("{1: 2, 2:3}", vec![(1, 2), (2, 3)]),
            ("{1 + 1: 2 * 2, 3 + 3: 4 * 4}", vec![(2, 4), (6, 16)]),
        ];
        for (test_input, test_value) in tests {
            let mut test_hash = HashMap::new();
            for (k, v) in test_value {
                let key = Object::new_integer(k);
                let value = Object::new_integer(v);
                let hash = key.hash_key().expect("couldn't generate hash key");
                let pair = (Rc::new(key), Rc::new(value));
                test_hash.insert(hash, pair);
            }
            run_vm_test(test_input, 1, Object::Hash(test_hash));
        }
    }

    #[test]
    fn test_index_expressions() {
        let tests = [
            ("[1, 2, 3][1]", Object::new_integer(2)),
            ("[1, 2, 3][0 + 2]", Object::new_integer(3)),
            ("[[1, 1, 1]][0][0]", Object::new_integer(1)),
            ("[][0]", Object::new_null()),
            ("[1, 2, 3][99]", Object::new_null()),
            ("[1][-1]", Object::new_null()),
            ("{1: 1, 2: 2}[1]", Object::new_integer(1)),
            ("{1: 1, 2: 2}[2]", Object::new_integer(2)),
            ("{1: 1}[0]", Object::new_null()),
            ("{}[0]", Object::new_null()),
        ];
        for (test_input, test_value) in tests {
            run_vm_test(test_input, 1, test_value);
        }
    }
}
