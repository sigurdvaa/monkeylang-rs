mod frame;
#[cfg(test)]
mod tests;

use crate::code::{read_u16_as_usize, Opcode, OpcodeError};
use crate::compiler::Bytecode;
use crate::object::{
    builtins, Array, BuiltinFunction, ClosureObj, CompiledFunctionObj, HashKeyData, HashKeyError,
    HashObj, Integer, Object,
};
use frame::Frame;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

const STACK_SIZE: usize = 2048;
const GLOBALS_SIZE: usize = 65536;
const FRAMES_SIZE: usize = 1024;

#[derive(Debug, PartialEq)]
pub enum VmError {
    InvalidInstruction(OpcodeError),
    InvalidHashKey(HashKeyError),
    StackOverflow(Object),
    FramesOverflow(Frame),
    InvalidStackAccess(usize),
    InvalidFramesAccess(usize),
    InvalidGlobalsIndex(usize),
    InvalidBooleanOperator(Opcode),
    InvalidStringOperator(Opcode),
    InvalidIntegerOperator(Opcode),
    InvalidBinaryTypes(&'static str, &'static str),
    InvalidComparisonTypes(&'static str, &'static str),
    InvalidIndexTypes(&'static str, &'static str),
    InvalidPrefixType(&'static str),
    InvalidFunctionCall(usize, Object),
    InvalidClosure(usize, Object),
    WrongNumberArguments(usize, usize),
}

impl std::error::Error for VmError {}

impl Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidInstruction(operr) => write!(f, "invalid instruction: {operr}"),
            Self::InvalidHashKey(keyerr) => write!(f, "invalid hash key: {keyerr}"),
            Self::StackOverflow(obj) => write!(f, "stack overflow, can't push object: {obj:?}"),
            Self::FramesOverflow(frame) => {
                write!(f, "frames overflow, can't push frame: {frame:?}")
            }
            Self::InvalidGlobalsIndex(idx) => {
                write!(f, "invalid globals access, no object at index {idx}")
            }
            Self::InvalidStackAccess(idx) => {
                write!(f, "invalid stack access, no object at index {idx}")
            }
            Self::InvalidFramesAccess(idx) => {
                write!(f, "invalid frames access, no frame at index {idx}")
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
            Self::InvalidFunctionCall(sp, obj) => {
                write!(
                    f,
                    "invalid function call at stack pointer {sp}, of non-function object: {obj:?}",
                )
            }
            Self::InvalidClosure(idx, obj) => {
                write!(
                    f,
                    "invalid function constant at index {idx} used for closure, got non-function object: {obj:?}",
                )
            }
            Self::WrongNumberArguments(want, got) => {
                write!(f, "wrong number of arguments, want={want}, got={got}",)
            }
        }
    }
}

pub struct Vm {
    constants: Vec<Object>,
    // TODO: wrap obj in Rc? avoid cloning when pushing locals
    stack: [Option<Object>; STACK_SIZE],
    // TODO: verify that all obj has been taken from the stack when we decrement sp
    sp: usize,
    globals: Vec<Option<Object>>,
    builtins: &'static [(&'static str, Object)],
    // TODO: wrap obj in Rc? avoid cloning when pushing globals
    frames: [Option<Frame>; FRAMES_SIZE],
    fp: usize,
}

impl Vm {
    pub fn new(bytecode: Option<Bytecode>) -> Self {
        let (constants, instructions) = match bytecode {
            Some(bytecode) => (bytecode.constants.clone(), bytecode.instructions.clone()),
            None => (vec![], vec![]),
        };
        let mut new = Self {
            constants,
            stack: [const { None }; STACK_SIZE],
            sp: 0,
            globals: vec![None; GLOBALS_SIZE],
            builtins: builtins::get_all(),
            frames: [const { None }; FRAMES_SIZE],
            fp: 1,
        };
        let func = Rc::new(CompiledFunctionObj {
            instructions,
            num_locals: 0,
            num_parameters: 0,
        });
        let closure = Rc::new(ClosureObj { func, free: vec![] });
        let frame = Frame::new(closure, 0);
        new.frames[0].replace(frame);
        new
    }

    pub fn soft_reset(&mut self, bytecode: Bytecode) {
        self.constants = bytecode.constants.clone();
        self.stack = [const { None }; STACK_SIZE];
        self.sp = 0;
        self.frames = [const { None }; FRAMES_SIZE];
        self.fp = 1;

        let func = Rc::new(CompiledFunctionObj {
            instructions: bytecode.instructions,
            num_locals: 0,
            num_parameters: 0,
        });
        let closure = Rc::new(ClosureObj { func, free: vec![] });
        let frame = Frame::new(closure, 0);
        self.frames[0].replace(frame);
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

    fn curr_frame_running(&mut self) -> bool {
        match &mut self.frames[self.fp - 1] {
            Some(frame) => frame.ip < frame.closure.func.instructions.len(),
            None => panic!("main frame missing"),
        }
    }

    fn curr_frame(&mut self) -> &mut Frame {
        match &mut self.frames[self.fp - 1] {
            Some(frame) => frame,
            None => panic!("main frame missing"),
        }
    }

    fn push_frame(&mut self, frame: Frame) -> Result<(), VmError> {
        if self.fp >= FRAMES_SIZE {
            return Err(VmError::FramesOverflow(frame));
        }

        self.frames[self.fp].replace(frame);
        self.fp += 1;

        Ok(())
    }

    fn pop_frame(&mut self) -> Result<Frame, VmError> {
        self.fp -= 1;
        match self.frames[self.fp].take() {
            Some(frame) => Ok(frame),
            None => Err(VmError::InvalidFramesAccess(self.fp)),
        }
    }

    fn push_closure(&mut self, idx: usize, num_free: usize) -> Result<(), VmError> {
        match &self.constants[idx] {
            Object::CompiledFunction(func) => {
                let mut free = Vec::with_capacity(num_free);
                for idx in self.sp - num_free..self.sp {
                    match self.stack[idx].take() {
                        Some(obj) => free.push(obj),
                        None => return Err(VmError::InvalidStackAccess(idx)),
                    };
                }
                self.sp -= num_free;
                let closure = ClosureObj {
                    func: func.clone(),
                    free,
                };
                self.push(Object::Closure(Rc::new(closure)))?;
            }
            _ => return Err(VmError::InvalidClosure(idx, self.constants[idx].clone())),
        }
        Ok(())
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
            .unwrap_or(&Rc::new(Object::Null))
            .clone()
    }

    fn execute_hash_index(left: &HashObj, index: &HashKeyData) -> Rc<Object> {
        match left.get(index) {
            Some((_key, value)) => value.clone(),
            None => Rc::new(Object::Null),
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

    fn execute_call(&mut self, num_args: usize) -> Result<(), VmError> {
        match &self.stack[self.sp - 1 - num_args] {
            Some(Object::Closure(closure)) => self.call_closure(closure.clone(), num_args)?,
            Some(Object::Builtin(func)) => self.call_builtin(*func, num_args)?,
            Some(obj) => return Err(VmError::InvalidFunctionCall(self.sp - 1, obj.clone())),
            None => return Err(VmError::InvalidStackAccess(self.sp - 1 - num_args)),
        }
        Ok(())
    }

    fn call_closure(&mut self, closure: Rc<ClosureObj>, num_args: usize) -> Result<(), VmError> {
        if closure.func.num_parameters != num_args {
            return Err(VmError::WrongNumberArguments(
                closure.func.num_parameters,
                num_args,
            ));
        }

        let frame = Frame::new(closure.clone(), self.sp - num_args);
        self.sp += closure.func.num_locals;
        self.push_frame(frame)?;
        Ok(())
    }

    fn call_builtin(&mut self, func: BuiltinFunction, num_args: usize) -> Result<(), VmError> {
        // TODO: convert stack to Rc to avoid creating Rc and cloning internal objects here?
        let mut args = vec![];
        for idx in self.sp - num_args..self.sp {
            match &self.stack[idx] {
                Some(obj) => args.push(Rc::new(obj.clone())),
                None => return Err(VmError::InvalidStackAccess(idx)),
            }
        }
        self.sp -= num_args + 1;
        let result = func(&args).as_ref().clone();
        self.push(result)?;
        Ok(())
    }

    pub fn run(&mut self) -> Result<Object, VmError> {
        let mut ins;
        let mut ip;
        let mut last_pop = Object::None;

        while self.curr_frame_running() {
            let frame = self.curr_frame();
            ip = frame.ip;
            ins = frame.ins();

            // TODO: make this lookup using u8 as index into array instead?
            let op = Opcode::try_from(ins[ip]).map_err(VmError::InvalidInstruction)?;
            match op {
                Opcode::Bang => {
                    self.execute_bang_operator()?;
                    self.curr_frame().ip += 1;
                }
                Opcode::Minus => {
                    self.execute_minus_operator()?;
                    self.curr_frame().ip += 1;
                }
                Opcode::True => {
                    self.push(Object::new_boolean(true))?;
                    self.curr_frame().ip += 1;
                }
                Opcode::False => {
                    self.push(Object::new_boolean(false))?;
                    self.curr_frame().ip += 1;
                }
                Opcode::Null => {
                    self.push(Object::Null)?;
                    self.curr_frame().ip += 1;
                }
                Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                    self.execute_binary_operation(op)?;
                    self.curr_frame().ip += 1;
                }
                Opcode::Eq | Opcode::NotEq | Opcode::Gt | Opcode::Lt => {
                    self.execute_comparison(op)?;
                    self.curr_frame().ip += 1;
                }
                Opcode::Pop => {
                    last_pop = self.pop()?;
                    self.curr_frame().ip += 1;
                }
                Opcode::Jump => {
                    let pos = read_u16_as_usize(&ins[ip + 1..]);
                    self.curr_frame().ip = pos;
                }
                Opcode::JumpNotTrue => {
                    let pos = read_u16_as_usize(&ins[ip + 1..]);
                    self.curr_frame().ip += 3;
                    let condition = self.pop()?;
                    if !condition.is_truthy() {
                        self.curr_frame().ip = pos;
                    }
                }
                Opcode::Constant => {
                    // TODO: use def and width to read and increment ip?
                    let idx = read_u16_as_usize(&ins[ip + 1..]);
                    self.curr_frame().ip += 3;
                    self.push(self.constants[idx].clone())?;
                }
                Opcode::GetGlobal => {
                    let idx = read_u16_as_usize(&ins[ip + 1..]);
                    self.curr_frame().ip += 3;
                    match &self.globals[idx] {
                        Some(obj) => self.push(obj.clone())?,
                        _ => return Err(VmError::InvalidGlobalsIndex(idx)),
                    }
                }
                Opcode::SetGlobal => {
                    let idx = read_u16_as_usize(&ins[ip + 1..]);
                    self.curr_frame().ip += 3;
                    let obj = self.pop()?;
                    self.globals[idx] = Some(obj);
                }
                Opcode::GetLocal => {
                    let idx = ins[ip + 1] as usize;
                    let frame = self.curr_frame();
                    frame.ip += 2;
                    let bp = frame.bp;
                    match &self.stack[bp + idx] {
                        Some(obj) => self.push(obj.clone())?,
                        _ => return Err(VmError::InvalidStackAccess(bp + idx)),
                    }
                }
                Opcode::SetLocal => {
                    let idx = ins[ip + 1] as usize;
                    let obj = self.pop()?;
                    let frame = self.curr_frame();
                    frame.ip += 2;
                    self.stack[frame.bp + idx].replace(obj);
                }
                Opcode::GetBuiltin => {
                    let idx = ins[ip + 1] as usize;
                    self.curr_frame().ip += 2;
                    let func = self.builtins[idx].1.clone();
                    self.push(func)?;
                }
                Opcode::Array => {
                    let len = read_u16_as_usize(&ins[ip + 1..]);
                    self.curr_frame().ip += 3;
                    let array = self.build_array(self.sp - len, self.sp)?;
                    self.sp -= len;
                    self.push(array)?;
                }
                Opcode::Hash => {
                    let len = read_u16_as_usize(&ins[ip + 1..]);
                    self.curr_frame().ip += 3;
                    let hash = self.build_hash(self.sp - len, self.sp)?;
                    self.sp -= len;
                    self.push(hash)?;
                }
                Opcode::Index => {
                    self.execute_index_expression()?;
                    self.curr_frame().ip += 1;
                }
                Opcode::Call => {
                    let num_args = ins[ip + 1] as usize;
                    self.curr_frame().ip += 2;
                    self.execute_call(num_args)?;
                }
                Opcode::Closure => {
                    let idx = read_u16_as_usize(&ins[ip + 1..]);
                    let num_free = ins[ip + 3] as usize;
                    self.curr_frame().ip += 4;
                    self.push_closure(idx, num_free)?;
                }
                Opcode::Return => {
                    let frame = self.pop_frame()?;
                    self.sp = frame.bp;
                    self.pop()?;
                    self.push(Object::Null)?;
                }
                Opcode::ReturnValue => {
                    let value = self.pop()?;
                    let frame = self.pop_frame()?;
                    self.sp = frame.bp;
                    self.pop()?;
                    self.push(value)?;
                }
                Opcode::GetFree => {
                    let idx = ins[ip + 1] as usize;
                    let frame = self.curr_frame();
                    frame.ip += 2;
                    let obj = frame.closure.free[idx].clone();
                    self.push(obj)?;
                }
                Opcode::CurrentClosure => {
                    let frame = self.curr_frame();
                    frame.ip += 1;
                    let closure = frame.closure.clone();
                    self.push(Object::Closure(closure))?;
                }
                Opcode::EnumLength => unreachable!(),
            }
        }

        Ok(last_pop)
    }
}
