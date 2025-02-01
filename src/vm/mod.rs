#[cfg(test)]
mod tests;

use crate::code::{read_u16_as_usize, Opcode, OpcodeError};
use crate::compiler::Bytecode;
use crate::object::{
    builtins, BuiltinFunction, ClosureObj, CompiledFunctionObj, Engine, HashKeyError, Object,
    ObjectUtil,
};
use std::collections::{HashMap, VecDeque};
use std::fmt::Display;
use std::rc::Rc;

const STACK_SIZE: usize = 32768;
const GLOBALS_SIZE: usize = 65536;
const FRAMES_SIZE: usize = STACK_SIZE / 2;
const RCPOOL_SIZE: usize = 1024;

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
    builtins: Vec<Rc<Object>>,
    frames: [Option<Frame>; FRAMES_SIZE],
    fp: usize,
    objutil: ObjectUtil,
    rcpool: VecDeque<Rc<Object>>,
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
        let obj = match func.as_ref() {
            Object::Closure(_) => {
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
        };
        self.return_rcobj(func);
        obj
    }

    fn get_objutil(&mut self) -> &mut ObjectUtil {
        &mut self.objutil
    }

    fn get_obj_null(&self) -> Rc<Object> {
        self.objutil.obj_null.clone()
    }

    fn get_obj_none(&self) -> Rc<Object> {
        self.objutil.obj_none.clone()
    }

    #[inline(always)]
    fn get_rcobj(&mut self, obj: Object) -> Rc<Object> {
        match self.rcpool.pop_front() {
            None => Rc::new(obj),
            Some(mut rc) => {
                *Rc::get_mut(&mut rc).expect("rc in use") = obj;
                rc
            }
        }
    }

    #[inline(always)]
    fn return_rcobj(&mut self, rc: Rc<Object>) {
        if Rc::strong_count(&rc) == 1 && self.rcpool.len() < RCPOOL_SIZE {
            self.rcpool.push_back(rc);
        }
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
            builtins: builtins::get_all().into_iter().map(|item| item.1).collect(),
            frames: [const { None }; FRAMES_SIZE],
            fp: 1,
            objutil: ObjectUtil::new(),
            rcpool: VecDeque::with_capacity(RCPOOL_SIZE),
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
        if let Some(obj) = self.stack[self.sp].replace(obj) {
            self.return_rcobj(obj);
        }
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
                let closure = self.get_rcobj(Object::Closure(Rc::new(closure)));
                self.push_stack(closure)
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
        let obj = self.get_rcobj(Object::Integer(value));
        self.push_stack(obj)
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
        let obj = self.get_rcobj(Object::String(value));
        self.push_stack(obj)
    }

    fn execute_binary_operation(&mut self, op: Opcode) -> Result<(), VmError> {
        let right = self.pop_stack()?;
        let left = self.pop_stack()?;
        match (left.as_ref(), right.as_ref()) {
            (Object::Integer(left), Object::Integer(right)) => {
                self.execute_binary_integer_operation(op, *left, *right)?;
            }
            (Object::String(left), Object::String(right)) => {
                self.execute_binary_string_operation(op, left, right)?;
            }
            _ => return Err(VmError::InvalidBinaryTypes(left.kind(), right.kind())),
        }
        self.return_rcobj(right);
        self.return_rcobj(left);
        Ok(())
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
            true => self.push_stack(self.objutil.obj_true.clone()),
            false => self.push_stack(self.objutil.obj_false.clone()),
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
            true => self.push_stack(self.objutil.obj_true.clone()),
            false => self.push_stack(self.objutil.obj_false.clone()),
        }
    }

    fn execute_null_comparison(&mut self, op: Opcode, other: &Object) -> Result<(), VmError> {
        let truth = matches!(other, Object::Null);
        let value = match op {
            Opcode::Eq => truth,
            Opcode::NotEq => !truth,
            _ => return Err(VmError::InvalidNullOperator(op)),
        };
        match value {
            true => self.push_stack(self.objutil.obj_true.clone()),
            false => self.push_stack(self.objutil.obj_false.clone()),
        }
    }

    fn execute_comparison(&mut self, op: Opcode) -> Result<(), VmError> {
        let right = self.pop_stack()?;
        let left = self.pop_stack()?;
        match (left.as_ref(), right.as_ref()) {
            (Object::Integer(left), Object::Integer(right)) => {
                self.execute_integer_comparison(op, *left, *right)?;
            }
            (Object::Boolean(left), Object::Boolean(right)) => {
                self.execute_boolean_comparison(op, *left, *right)?;
            }
            (Object::Null, other) | (other, Object::Null) => {
                self.execute_null_comparison(op, other)?;
            }
            _ => return Err(VmError::InvalidComparisonTypes(left.kind(), right.kind())),
        }
        self.return_rcobj(right);
        self.return_rcobj(left);
        Ok(())
    }

    fn execute_bang_operator(&mut self) -> Result<(), VmError> {
        let operand = self.pop_stack()?;
        match operand.is_truthy() {
            true => self.push_stack(self.objutil.obj_false.clone())?,
            false => self.push_stack(self.objutil.obj_true.clone())?,
        };
        self.return_rcobj(operand);
        Ok(())
    }

    fn execute_minus_operator(&mut self) -> Result<(), VmError> {
        let operand = self.pop_stack()?;
        match operand.as_ref() {
            Object::Integer(value) => {
                let obj = self.get_rcobj(Object::Integer(-value));
                self.push_stack(obj)?;
            }
            _ => return Err(VmError::InvalidPrefixType(operand.kind())),
        }
        self.return_rcobj(operand);
        Ok(())
    }

    fn execute_exit(&mut self) -> Result<(), VmError> {
        let operand = self.pop_stack()?;
        match operand.as_ref() {
            Object::Integer(value) if i32::try_from(*value).is_ok() => {
                std::process::exit(*value as i32)
            }
            _ => Err(VmError::InvalidExitCode(operand)),
        }
    }

    fn execute_index_expression(&mut self) -> Result<(), VmError> {
        let idx = self.pop_stack()?;
        let left = self.pop_stack()?;
        let obj = match (left.as_ref(), idx.as_ref()) {
            (Object::Array(obj), Object::Integer(idx)) => match obj.get(*idx as usize) {
                Some(value) => value.clone(),
                None => self.objutil.obj_null.clone(),
            },
            (Object::Hash(obj), _) => {
                let hash_key = self
                    .objutil
                    .hash_key(&idx)
                    .map_err(VmError::InvalidHashKey)?;
                match obj.get(&hash_key) {
                    Some((_key, value)) => value.clone(),
                    None => self.objutil.obj_null.clone(),
                }
            }
            _ => return Err(VmError::InvalidIndexTypes(left.kind(), idx.kind())),
        };
        self.return_rcobj(idx);
        self.return_rcobj(left);
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
            let hash_key = self
                .objutil
                .hash_key(&key)
                .map_err(VmError::InvalidHashKey)?;
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
        let num_locals = closure.func.num_locals;
        let new_frame = Frame::new(closure, self.sp - num_args);
        self.sp = new_frame.bp + num_locals;
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

        if let Some(obj) = self.stack[self.sp - 1].replace(result) {
            self.return_rcobj(obj);
        }
        Ok(())
    }

    pub fn run(&mut self) -> Result<Rc<Object>, VmError> {
        let mut last_pop = self.objutil.obj_none.clone();
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
                    self.push_stack(self.objutil.obj_true.clone())?;
                }
                Opcode::False => {
                    frame.ip += 1;
                    self.push_stack(self.objutil.obj_false.clone())?;
                }
                Opcode::Null => {
                    frame.ip += 1;
                    self.push_stack(self.objutil.obj_null.clone())?;
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
                    self.return_rcobj(last_pop);
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
                    self.return_rcobj(condition);
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
                    if let Some(obj) = self.globals[idx].replace(obj) {
                        self.return_rcobj(obj);
                    }
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
                    if let Some(obj) = self.stack[frame.bp + idx].replace(obj) {
                        self.return_rcobj(obj);
                    }
                }
                Opcode::GetBuiltin => {
                    let idx = ins[frame.ip + 1] as usize;
                    frame.ip += 2;
                    let func = self.builtins[idx].clone();
                    self.push_stack(func)?;
                }
                Opcode::Array => {
                    let len = read_u16_as_usize(&ins[frame.ip + 1..]);
                    frame.ip += 3;
                    let array = self.build_array(self.sp - len, self.sp)?;
                    self.sp -= len;
                    let obj = self.get_rcobj(array);
                    self.push_stack(obj)?
                }
                Opcode::Hash => {
                    let len = read_u16_as_usize(&ins[frame.ip + 1..]);
                    frame.ip += 3;
                    let hash = self.build_hash(self.sp - len, self.sp)?;
                    self.sp -= len;
                    let obj = self.get_rcobj(hash);
                    self.push_stack(obj)?
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
                    // cleanup stack after call
                    for idx in frame.bp..self.sp {
                        if let Some(obj) = self.stack[idx].take() {
                            self.return_rcobj(obj);
                        }
                    }

                    self.sp = frame.bp;
                    if let Some(obj) =
                        self.stack[self.sp - 1].replace(self.objutil.obj_null.clone())
                    {
                        self.return_rcobj(obj);
                    }
                    frame = self.pop_frame()?;
                }
                Opcode::ReturnValue => {
                    let value = self.pop_stack()?;

                    // cleanup stack after call
                    for idx in frame.bp..self.sp {
                        if let Some(obj) = self.stack[idx].take() {
                            self.return_rcobj(obj);
                        }
                    }

                    self.sp = frame.bp;
                    if let Some(obj) = self.stack[self.sp - 1].replace(value) {
                        self.return_rcobj(obj);
                    }
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
                    let obj = self.get_rcobj(Object::Closure(frame.closure.clone()));
                    self.push_stack(obj)?;
                }
                Opcode::Exit => self.execute_exit()?,
                Opcode::Break => {
                    let pos = read_u16_as_usize(&ins[frame.ip + 1..]);
                    frame.ip = pos;
                }
            }
        }
        Ok(last_pop)
    }
}
