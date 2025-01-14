pub mod builtins;

use crate::ast::{BlockStatement, Expression, IdentifierLiteral};
use crate::code::Instruction;
use crate::evaluator::Env;
use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::rc::Rc;

pub trait Engine {
    fn call_func(&mut self, func: Rc<Object>, args: &[Rc<Object>]) -> Rc<Object>;
    fn get_obj_null(&self) -> Rc<Object>;
    fn get_obj_none(&self) -> Rc<Object>;
}

pub type Integer = IntegerObj;
pub type BuiltinFunction = fn(&[Rc<Object>], &mut dyn Engine) -> Rc<Object>;
pub type Array = Vec<Rc<Object>>;
pub type HashObj = HashMap<HashKeyData, (Rc<Object>, Rc<Object>)>;
type HashKey = RefCell<Option<HashKeyData>>;

#[derive(Debug, PartialEq)]
pub struct HashKeyError(&'static str);

impl std::error::Error for HashKeyError {}

impl Display for HashKeyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type not supported as hash key: {}", self.0)
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct IntegerObj {
    pub value: isize,
    hash: HashKey,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct BooleanObj {
    pub value: bool,
    pub hash: HashKey,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct StringObj {
    pub value: String,
    hash: HashKey,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionObj {
    pub parameters: Vec<IdentifierLiteral>,
    pub body: BlockStatement,
    pub env: Env,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompiledFunctionObj {
    pub instructions: Vec<Instruction>,
    pub num_locals: usize,
    pub num_parameters: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ClosureObj {
    pub func: Rc<CompiledFunctionObj>,
    pub free: Vec<Rc<Object>>,
}

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Clone)]
pub struct HashKeyData {
    pub kind: &'static str,
    pub value: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Null,
    None,
    Integer(IntegerObj),
    Boolean(BooleanObj),
    Return(Rc<Self>),
    Error(String),
    Function(FunctionObj),
    CompiledFunction(Rc<CompiledFunctionObj>),
    String(StringObj),
    Builtin(BuiltinFunction),
    Array(Array),
    Hash(HashObj),
    Quote(Expression),
    Macro(FunctionObj),
    Closure(Rc<ClosureObj>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::String(obj) => write!(f, "{}", obj.value),
            _ => write!(f, "{}", self.inspect()),
        }
    }
}

impl Object {
    pub fn new_boolean(value: bool) -> Self {
        match value {
            true => Object::Boolean(BooleanObj {
                value: true,
                hash: RefCell::new(Some(HashKeyData {
                    kind: "BOOLEAN",
                    value: 1,
                })),
            }),

            false => Object::Boolean(BooleanObj {
                value: false,
                hash: RefCell::new(Some(HashKeyData {
                    kind: "BOOLEAN",
                    value: 0,
                })),
            }),
        }
    }

    pub fn new_integer(value: isize) -> Self {
        Self::Integer(IntegerObj {
            value,
            hash: RefCell::new(None),
        })
    }

    pub fn new_string(value: String) -> Self {
        Self::String(StringObj {
            value,
            hash: RefCell::new(None),
        })
    }

    pub fn kind(&self) -> &'static str {
        match self {
            Self::Null => "NULL",
            Self::None => "NONE",
            Self::Integer(_) => "INTEGER",
            Self::Boolean(_) => "BOOLEAN",
            Self::Return(_) => "RETURN",
            Self::Error(_) => "ERROR",
            Self::Function(_) => "FUNCTION",
            Self::CompiledFunction(_) => "COMPILED_FUNCTION",
            Self::String(_) => "STRING",
            Self::Builtin(_) => "BUILTIN",
            Self::Array(_) => "ARRAY",
            Self::Hash(_) => "HASH",
            Self::Quote(_) => "QUOTE",
            Self::Macro(_) => "MACRO",
            Self::Closure(_) => "CLOSURE",
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Integer(obj) => obj.value > 0,
            Self::String(obj) => !obj.value.is_empty(),
            Self::Array(value) => !value.is_empty(),
            Self::Hash(value) => !value.is_empty(),
            Self::Boolean(obj) => obj.value,
            _ => false,
        }
    }
    pub fn inspect(&self) -> String {
        match self {
            Self::Null => "null".into(),
            Self::None => "".into(),
            Self::Integer(obj) => obj.value.to_string(),
            Self::Boolean(obj) => obj.value.to_string(),
            Self::Return(value) => value.inspect(),
            Self::Error(value) => format!("ERROR: {value}"),
            Self::Function(func) => format!(
                "fn({}) {{\n{}\n}}",
                &func
                    .parameters
                    .iter()
                    .map(|i| i.value.as_str())
                    .collect::<Vec<_>>()
                    .join(", "),
                &func.body.to_string()
            ),
            Self::CompiledFunction(func) => format!("compiledfn[{func:p}]"),
            Self::Closure(func) => format!("closure[{func:p}]"),
            Self::Macro(func) => format!(
                "macro({}) {{\n{}\n}}",
                &func
                    .parameters
                    .iter()
                    .map(|i| i.value.as_str())
                    .collect::<Vec<_>>()
                    .join(", "),
                &func.body.to_string()
            ),
            Self::String(obj) => format!("\"{}\"", obj.value.replace("\"", "\\\"")),
            Self::Builtin(_) => "builtin function".into(),
            Self::Array(value) => format!(
                "[{}]",
                value
                    .iter()
                    .map(|i| i.inspect())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Hash(value) => {
                format!(
                    "{{{}}}",
                    &value
                        .values()
                        .map(|(key, value)| format!("{}: {}", key.inspect(), value.inspect()))
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            }
            Self::Quote(expr) => format!("QUOTE({expr})"),
        }
    }

    fn set_hash_key(
        &self,
        mut hash: RefMut<Option<HashKeyData>>,
        kind: &'static str,
        value: usize,
    ) -> HashKeyData {
        let hash_key = HashKeyData { kind, value };
        *hash = Some(hash_key.clone());
        hash_key
    }

    pub fn hash_key(&self) -> Result<HashKeyData, HashKeyError> {
        // TODO: avoid collisions
        // TODO: improve cache, currently only works with bindings (attached to obj)
        match self {
            Self::Boolean(obj) => {
                if let Some(hash_key) = obj.hash.borrow().as_ref() {
                    return Ok(hash_key.clone());
                }
                Ok(self.set_hash_key(obj.hash.borrow_mut(), self.kind(), obj.value as usize))
            }
            Self::Integer(obj) => {
                if let Some(hash_key) = obj.hash.borrow().as_ref() {
                    return Ok(hash_key.clone());
                }
                Ok(self.set_hash_key(obj.hash.borrow_mut(), self.kind(), obj.value as usize))
            }
            Self::String(obj) => {
                if let Some(hash_key) = obj.hash.borrow().as_ref() {
                    return Ok(hash_key.clone());
                }
                Ok(self.set_hash_key(
                    obj.hash.borrow_mut(),
                    self.kind(),
                    obj.value.chars().map(|c| c as usize).sum(),
                ))
            }
            _ => Err(HashKeyError(self.kind())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_str_obj(value: &str) -> Object {
        let obj = Object::new_string(value.into());
        let _ = obj.hash_key();
        obj
    }

    #[test]
    fn test_string_hash_key() {
        let hello1 = create_str_obj("Hello World!");
        let hello2 = create_str_obj("Hello World!");
        let diff1 = create_str_obj("My name is sig");
        let diff2 = create_str_obj("My name is sig");

        assert_eq!(hello1.hash_key(), hello2.hash_key());
        assert_eq!(diff1.hash_key(), diff2.hash_key());
        assert_ne!(hello1.hash_key(), diff1.hash_key());
    }
}
