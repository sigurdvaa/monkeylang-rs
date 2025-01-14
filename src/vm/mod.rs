#[cfg(test)]
mod tests;

use crate::code::{read_u16_as_usize, Opcode, OpcodeError};
use crate::compiler::Bytecode;
use crate::object::{
    builtins, Array, BuiltinFunction, ClosureObj, CompiledFunctionObj, Engine, HashKeyData,
    HashKeyError, HashObj, Integer, Object,
};
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

const STACK_SIZE: usize = 32768;
const GLOBALS_SIZE: usize = 65536;
const FRAMES_SIZE: usize = STACK_SIZE / 2;

#[derive(Debug, PartialEq)]
pub enum VmError {
    InvalidInstruction(OpcodeError),
    InvalidHashKey(HashKeyError),
    StackOverflow(Rc<Object>),
    FramesOverflow(Frame),
    InvalidStackAccess(usize),
    InvalidFramesAccess(usize),
    InvalidGlobalsIndex(usize),
    InvalidBooleanOperator(Opcode),
    InvalidStringOperator(Opcode),
    InvalidIntegerOperator(Opcode),
    InvalidNullOperator(Opcode),
    InvalidBinaryTypes(&'static str, &'static str),
    InvalidComparisonTypes(&'static str, &'static str),
    InvalidIndexTypes(&'static str, &'static str),
    InvalidPrefixType(&'static str),
    InvalidExitCode(Rc<Object>),
    InvalidFunctionCall(usize, Rc<Object>),
    InvalidClosure(usize, Rc<Object>),
    WrongNumberArguments(usize, usize),
    BuiltinFunction(String),
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
            Self::InvalidNullOperator(op) => write!(f, "invalid null operator: {op:?}"),
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
                write!(f, "unsupported type for minus prefix operator: {operand}")
            }
            Self::InvalidExitCode(operand) => {
                write!(f, "invalid exit code: {operand} ({})", operand.kind())
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
            Self::BuiltinFunction(err) => {
                write!(f, "{err}",)
            }
        }
    }
}

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
}

pub struct Vm {
    constants: Vec<Rc<Object>>,
    stack: [Option<Rc<Object>>; STACK_SIZE],
    sp: usize,
    globals: Vec<Option<Rc<Object>>>,
    builtins: Vec<(&'static str, Rc<Object>)>,
    frames: [Option<Frame>; FRAMES_SIZE],
    fp: usize,
    obj_true: Rc<Object>,
    obj_false: Rc<Object>,
    obj_null: Rc<Object>,
    obj_none: Rc<Object>,
}

impl Engine for Vm {
    fn call_func(&mut self, func: Rc<Object>, args: &[Rc<Object>]) -> Rc<Object> {
        if let Err(e) = self.push_stack(func.clone()) {
            return Rc::new(Object::Error(e.to_string()));
        }
        for arg in args {
            if let Err(e) = self.push_stack(arg.clone()) {
                return Rc::new(Object::Error(e.to_string()));
            }
        }
        match func.as_ref() {
            Object::Closure(_) => {
                // TODO: can some of this be more constant?
                let func = Rc::new(CompiledFunctionObj {
                    instructions: vec![Opcode::Call as u8, args.len() as u8],
                    num_locals: 0,
                    num_parameters: 0,
                });
                let closure = Rc::new(ClosureObj { func, free: vec![] });
                let frame = Frame::new(closure, self.sp - args.len());
                if let Err(e) = self.push_frame(frame) {
                    return Rc::new(Object::Error(e.to_string()));
                }
                if let Err(err) = self.run() {
                    return Rc::new(Object::Error(err.to_string()));
                }
                self.pop_stack()
                    .unwrap_or_else(|e| Rc::new(Object::Error(e.to_string())))
            }
            Object::Builtin(func) => {
                if let Err(e) = self.call_builtin(*func, args.len()) {
                    return Rc::new(Object::Error(e.to_string()));
                }
                self.pop_stack()
                    .unwrap_or_else(|e| Rc::new(Object::Error(e.to_string())))
            }
            _ => unreachable!(),
        }
    }

    fn get_obj_null(&self) -> Rc<Object> {
        self.obj_null.clone()
    }

    fn get_obj_none(&self) -> Rc<Object> {
        self.obj_none.clone()
    }
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
            obj_true: Rc::new(Object::new_boolean(true)),
            obj_false: Rc::new(Object::new_boolean(false)),
            obj_null: Rc::new(Object::Null),
            obj_none: Rc::new(Object::None),
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

    fn push_stack(&mut self, obj: Rc<Object>) -> Result<(), VmError> {
        if self.sp >= STACK_SIZE {
            return Err(VmError::StackOverflow(obj));
        }
        self.stack[self.sp].replace(obj.clone());
        self.sp += 1;
        Ok(())
    }

    fn pop_stack(&mut self) -> Result<Rc<Object>, VmError> {
        self.sp -= 1;
        match self.stack[self.sp].take() {
            Some(obj) => Ok(obj),
            None => Err(VmError::InvalidStackAccess(self.sp)),
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
        match self.constants[idx].as_ref() {
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
                self.push_stack(Rc::new(Object::Closure(Rc::new(closure))))
            }
            _ => Err(VmError::InvalidClosure(idx, self.constants[idx].clone())),
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
        self.push_stack(Rc::new(Object::new_integer(value)))
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
        self.push_stack(Rc::new(Object::new_string(value)))
    }

    fn execute_binary_operation(&mut self, op: Opcode) -> Result<(), VmError> {
        let right = self.pop_stack()?;
        let left = self.pop_stack()?;
        match (left.as_ref(), right.as_ref()) {
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
        match value {
            true => self.push_stack(self.obj_true.clone()),
            false => self.push_stack(self.obj_false.clone()),
        }
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
        match value {
            true => self.push_stack(self.obj_true.clone()),
            false => self.push_stack(self.obj_false.clone()),
        }
    }

    fn execute_null_comparison(&mut self, op: Opcode, other: &Object) -> Result<(), VmError> {
        let truth = matches!(other, Object::Null);
        let value = match op {
            Opcode::Eq => truth,
            Opcode::NotEq => !truth,
            _ => return Err(VmError::InvalidNullOperator(op.clone())),
        };
        match value {
            true => self.push_stack(self.obj_true.clone()),
            false => self.push_stack(self.obj_false.clone()),
        }
    }

    fn execute_comparison(&mut self, op: Opcode) -> Result<(), VmError> {
        let right = self.pop_stack()?;
        let left = self.pop_stack()?;
        match (left.as_ref(), right.as_ref()) {
            (Object::Integer(left), Object::Integer(right)) => {
                self.execute_integer_comparison(op, left.value, right.value)
            }
            (Object::Boolean(left), Object::Boolean(right)) => {
                self.execute_boolean_comparison(op, left.value, right.value)
            }
            (Object::Null, other) | (other, Object::Null) => {
                self.execute_null_comparison(op, other)
            }
            _ => Err(VmError::InvalidComparisonTypes(left.kind(), right.kind())),
        }
    }

    fn execute_bang_operator(&mut self) -> Result<(), VmError> {
        let operand = self.pop_stack()?;
        match operand.is_truthy() {
            true => self.push_stack(self.obj_false.clone()),
            false => self.push_stack(self.obj_true.clone()),
        }
    }

    fn execute_minus_operator(&mut self) -> Result<(), VmError> {
        let operand = self.pop_stack()?;
        match operand.as_ref() {
            Object::Integer(obj) => self.push_stack(Rc::new(Object::new_integer(-obj.value))),
            _ => Err(VmError::InvalidPrefixType(operand.kind())),
        }
    }

    fn execute_exit(&mut self) -> Result<(), VmError> {
        let operand = self.pop_stack()?;
        match operand.as_ref() {
            Object::Integer(obj) if i32::try_from(obj.value).is_ok() => {
                std::process::exit(obj.value as i32)
            }
            _ => Err(VmError::InvalidExitCode(operand)),
        }
    }

    fn execute_array_index(&self, left: &Array, index: &Integer) -> Rc<Object> {
        match left.get(index.value as usize) {
            Some(value) => value.clone(),
            None => self.obj_null.clone(),
        }
    }

    fn execute_hash_index(&self, left: &HashObj, index: &HashKeyData) -> Rc<Object> {
        match left.get(index) {
            Some((_key, value)) => value.clone(),
            None => self.obj_null.clone(),
        }
    }

    fn execute_index_expression(&mut self) -> Result<(), VmError> {
        let idx = self.pop_stack()?;
        let left = self.pop_stack()?;
        let obj = match (left.as_ref(), idx.as_ref()) {
            (Object::Array(obj), Object::Integer(idx)) => self.execute_array_index(obj, idx),
            (Object::Hash(obj), _) => {
                let hash_key = idx.hash_key().map_err(VmError::InvalidHashKey)?;
                self.execute_hash_index(obj, &hash_key)
            }
            _ => return Err(VmError::InvalidIndexTypes(left.kind(), idx.kind())),
        };
        self.push_stack(obj)
    }

    fn build_array(&mut self, start_idx: usize, end_idx: usize) -> Result<Object, VmError> {
        let mut array = Vec::with_capacity(end_idx - start_idx);
        for i in start_idx..end_idx {
            match self.stack[i].take() {
                Some(obj) => array.push(obj),
                None => return Err(VmError::InvalidStackAccess(i)),
            }
        }
        Ok(Object::Array(array))
    }

    fn build_hash(&mut self, start_idx: usize, end_idx: usize) -> Result<Object, VmError> {
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
            let pair = (key, value);
            hash.insert(hash_key, pair);
        }
        Ok(Object::Hash(hash))
    }

    fn execute_call(&mut self, frame: Frame, num_args: usize) -> Result<Frame, VmError> {
        match &self.stack[self.sp - 1 - num_args] {
            Some(obj) => match obj.as_ref() {
                Object::Closure(closure) => self.call_closure(frame, closure.clone(), num_args),
                Object::Builtin(func) => {
                    self.call_builtin(*func, num_args)?;
                    Ok(frame)
                }
                _ => Err(VmError::InvalidFunctionCall(
                    self.sp - 1 - num_args,
                    obj.clone(),
                )),
            },
            None => Err(VmError::InvalidStackAccess(self.sp - 1 - num_args)),
        }
    }

    fn call_closure(
        &mut self,
        frame: Frame,
        closure: Rc<ClosureObj>,
        num_args: usize,
    ) -> Result<Frame, VmError> {
        if closure.func.num_parameters != num_args {
            return Err(VmError::WrongNumberArguments(
                closure.func.num_parameters,
                num_args,
            ));
        }
        let new_frame = Frame::new(closure.clone(), self.sp - num_args);
        self.sp = new_frame.bp + closure.func.num_locals;
        self.push_frame(frame)?;
        Ok(new_frame)
    }

    fn call_builtin(&mut self, func: BuiltinFunction, num_args: usize) -> Result<(), VmError> {
        let mut args = vec![];
        for idx in self.sp - num_args..self.sp {
            match self.stack[idx].take() {
                Some(obj) => args.push(obj),
                None => return Err(VmError::InvalidStackAccess(idx)),
            }
        }
        self.sp -= num_args;
        let result = func(&args, self);
        if let Object::Error(err) = result.as_ref() {
            return Err(VmError::BuiltinFunction(err.clone()));
        }

        self.stack[self.sp - 1].replace(result); // replaces called func with result
        Ok(())
    }

    pub fn run(&mut self) -> Result<Rc<Object>, VmError> {
        let mut last_pop = self.obj_none.clone();
        let mut frame = self.pop_frame()?;
        loop {
            let ins = &frame.closure.func.instructions;
            if frame.ip >= ins.len() {
                break;
            };

            let op = Opcode::try_from(ins[frame.ip]).map_err(VmError::InvalidInstruction)?;
            match op {
                Opcode::Bang => {
                    frame.ip += 1;
                    self.execute_bang_operator()?;
                }
                Opcode::Minus => {
                    frame.ip += 1;
                    self.execute_minus_operator()?;
                }
                Opcode::True => {
                    frame.ip += 1;
                    self.push_stack(self.obj_true.clone())?;
                }
                Opcode::False => {
                    frame.ip += 1;
                    self.push_stack(self.obj_false.clone())?;
                }
                Opcode::Null => {
                    frame.ip += 1;
                    self.push_stack(self.obj_null.clone())?;
                }
                Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                    frame.ip += 1;
                    self.execute_binary_operation(op)?;
                }
                Opcode::Eq | Opcode::NotEq | Opcode::Gt | Opcode::Lt => {
                    frame.ip += 1;
                    self.execute_comparison(op)?;
                }
                Opcode::Pop => {
                    frame.ip += 1;
                    last_pop = self.pop_stack()?;
                }
                Opcode::Jump => {
                    let pos = read_u16_as_usize(&ins[frame.ip + 1..]);
                    frame.ip = pos;
                }
                Opcode::JumpNotTrue => {
                    let pos = read_u16_as_usize(&ins[frame.ip + 1..]);
                    frame.ip += 3;
                    let condition = self.pop_stack()?;
                    if !condition.is_truthy() {
                        frame.ip = pos;
                    }
                }
                Opcode::Constant => {
                    let idx = read_u16_as_usize(&ins[frame.ip + 1..]);
                    frame.ip += 3;
                    self.push_stack(self.constants[idx].clone())?;
                }
                Opcode::GetGlobal => {
                    let idx = read_u16_as_usize(&ins[frame.ip + 1..]);
                    frame.ip += 3;
                    match &self.globals[idx] {
                        Some(obj) => self.push_stack(obj.clone())?,
                        _ => return Err(VmError::InvalidGlobalsIndex(idx)),
                    }
                }
                Opcode::SetGlobal => {
                    let idx = read_u16_as_usize(&ins[frame.ip + 1..]);
                    frame.ip += 3;
                    let obj = self.pop_stack()?;
                    self.globals[idx] = Some(obj);
                }
                Opcode::GetLocal => {
                    let idx = ins[frame.ip + 1] as usize;
                    frame.ip += 2;
                    match &self.stack[frame.bp + idx] {
                        Some(obj) => self.push_stack(obj.clone())?,
                        _ => return Err(VmError::InvalidStackAccess(frame.bp + idx)),
                    }
                }
                Opcode::SetLocal => {
                    let idx = ins[frame.ip + 1] as usize;
                    frame.ip += 2;
                    let obj = self.pop_stack()?;
                    self.stack[frame.bp + idx].replace(obj);
                }
                Opcode::GetBuiltin => {
                    let idx = ins[frame.ip + 1] as usize;
                    frame.ip += 2;
                    let func = self.builtins[idx].1.clone();
                    self.push_stack(func)?;
                }
                Opcode::Array => {
                    let len = read_u16_as_usize(&ins[frame.ip + 1..]);
                    frame.ip += 3;
                    let array = self.build_array(self.sp - len, self.sp)?;
                    self.sp -= len;
                    self.push_stack(Rc::new(array))?;
                }
                Opcode::Hash => {
                    let len = read_u16_as_usize(&ins[frame.ip + 1..]);
                    frame.ip += 3;
                    let hash = self.build_hash(self.sp - len, self.sp)?;
                    self.sp -= len;
                    self.push_stack(Rc::new(hash))?;
                }
                Opcode::Index => {
                    frame.ip += 1;
                    self.execute_index_expression()?;
                }
                Opcode::Call => {
                    let num_args = ins[frame.ip + 1] as usize;
                    frame.ip += 2;
                    frame = self.execute_call(frame, num_args)?;
                }
                Opcode::Closure => {
                    let idx = read_u16_as_usize(&ins[frame.ip + 1..]);
                    let num_free = ins[frame.ip + 3] as usize;
                    frame.ip += 4;
                    self.push_closure(idx, num_free)?;
                }
                Opcode::Return => {
                    self.sp = frame.bp;
                    self.stack[self.sp - 1].replace(self.obj_null.clone());
                    frame = self.pop_frame()?;
                }
                Opcode::ReturnValue => {
                    let value = self.pop_stack()?;
                    self.sp = frame.bp;
                    self.stack[self.sp - 1].replace(value);
                    frame = self.pop_frame()?;
                }
                Opcode::GetFree => {
                    let idx = ins[frame.ip + 1] as usize;
                    frame.ip += 2;
                    let obj = frame.closure.free[idx].clone();
                    self.push_stack(obj)?;
                }
                Opcode::CurrentClosure => {
                    frame.ip += 1;
                    let closure = frame.closure.clone();
                    self.push_stack(Rc::new(Object::Closure(closure)))?;
                }
                Opcode::Exit => self.execute_exit()?,
            }
        }
        Ok(last_pop)
    }
}
