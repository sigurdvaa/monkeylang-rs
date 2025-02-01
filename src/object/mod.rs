pub mod builtins;

use crate::ast::{BlockStatement, Expression, IdentifierLiteral};
use crate::code::Instruction;
use crate::evaluator::Env;
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::rc::Rc;

const HASHKEY_CACHE_SIZE: usize = 65535;

pub trait Engine {
    fn call_func(&mut self, func: Rc<Object>, args: &[Rc<Object>]) -> Rc<Object>;
    fn get_objutil(&mut self) -> &mut ObjectUtil;
    fn get_obj_null(&self) -> Rc<Object>;
    fn get_obj_none(&self) -> Rc<Object>;
    fn get_rc(&mut self, obj: Object) -> Rc<Object>;
    fn return_rc(&mut self, rc: Rc<Object>);
}

pub type Integer = isize;
pub type BuiltinFunction = fn(&[Rc<Object>], &mut dyn Engine) -> Rc<Object>;
pub type Array = Vec<Rc<Object>>;
pub type Hash = HashMap<HashKey, (Rc<Object>, Rc<Object>)>;

#[derive(Debug, PartialEq)]
pub struct HashKeyError(&'static str);

impl std::error::Error for HashKeyError {}

impl Display for HashKeyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type not supported as hash key: {}", self.0)
    }
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

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Copy, Clone)]
pub struct HashKey {
    pub kind: &'static str,
    pub value: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Null,
    None,
    Integer(Integer),
    Boolean(bool),
    Return(Rc<Self>),
    Break(Rc<Self>),
    Error(String),
    Function(Box<FunctionObj>),
    CompiledFunction(Rc<CompiledFunctionObj>),
    String(String),
    Builtin(BuiltinFunction),
    Array(Array),
    Hash(Hash),
    Quote(Box<Expression>),
    Macro(Box<FunctionObj>),
    Closure(Rc<ClosureObj>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::String(value) => write!(f, "{}", value),
            _ => write!(f, "{}", self.inspect()),
        }
    }
}

impl Object {
    pub fn kind(&self) -> &'static str {
        match self {
            Self::Null => "NULL",
            Self::None => "NONE",
            Self::Integer(_) => "INTEGER",
            Self::Boolean(_) => "BOOLEAN",
            Self::Return(_) => "RETURN",
            Self::Break(_) => "BREAK",
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
            Self::Integer(value) => *value > 0,
            Self::String(value) => !value.is_empty(),
            Self::Array(value) => !value.is_empty(),
            Self::Hash(value) => !value.is_empty(),
            Self::Boolean(value) => *value,
            _ => false,
        }
    }
    pub fn inspect(&self) -> String {
        match self {
            Self::Null => "null".into(),
            Self::None => "".into(),
            Self::Integer(value) => value.to_string(),
            Self::Boolean(value) => value.to_string(),
            Self::Return(value) => value.inspect(),
            Self::Break(value) => value.inspect(),
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
            Self::String(value) => format!("\"{}\"", value.replace("\"", "\\\"")),
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
}

pub struct ObjectUtil {
    pub obj_true: Rc<Object>,
    pub obj_false: Rc<Object>,
    pub obj_null: Rc<Object>,
    pub obj_none: Rc<Object>,
    hashkey_cache_str: HashMap<String, HashKey>,
    hashkey_cache_int: HashMap<isize, HashKey>,
    hashkey_true: HashKey,
    hashkey_false: HashKey,
    hashkey_null: HashKey,
}

impl ObjectUtil {
    pub fn new() -> Self {
        Self {
            obj_true: Rc::new(Object::Boolean(true)),
            obj_false: Rc::new(Object::Boolean(false)),
            obj_null: Rc::new(Object::Null),
            obj_none: Rc::new(Object::None),
            hashkey_cache_str: HashMap::with_capacity(HASHKEY_CACHE_SIZE),
            hashkey_cache_int: HashMap::with_capacity(HASHKEY_CACHE_SIZE),
            hashkey_true: HashKey {
                kind: Object::Boolean(true).kind(),
                value: 1,
            },
            hashkey_false: HashKey {
                kind: Object::Boolean(false).kind(),
                value: 0,
            },
            hashkey_null: HashKey {
                kind: Object::Null.kind(),
                value: 0,
            },
        }
    }

    pub fn hash_key(&mut self, obj: &Rc<Object>) -> Result<HashKey, HashKeyError> {
        match obj.as_ref() {
            Object::Null => Ok(self.hashkey_null),
            Object::Boolean(value) => match value {
                true => Ok(self.hashkey_true),
                false => Ok(self.hashkey_false),
            },
            Object::Integer(value) => {
                if let Some(hash_key) = self.hashkey_cache_int.get(value) {
                    return Ok(*hash_key);
                }
                let hash_key = HashKey {
                    kind: obj.kind(),
                    value: *value as usize,
                };
                if self.hashkey_cache_int.len() > HASHKEY_CACHE_SIZE {
                    self.hashkey_cache_int.clear();
                }
                self.hashkey_cache_int.insert(*value, hash_key);
                Ok(hash_key)
            }
            Object::String(ref value) => {
                if let Some(hash_key) = self.hashkey_cache_str.get(value) {
                    return Ok(*hash_key);
                }
                let hash_key = HashKey {
                    kind: obj.kind(),
                    value: value.chars().map(|c| c as usize).sum(),
                };
                if self.hashkey_cache_str.len() > HASHKEY_CACHE_SIZE {
                    self.hashkey_cache_str.clear();
                }
                self.hashkey_cache_str.insert(value.to_string(), hash_key);
                Ok(hash_key)
            }
            _ => Err(HashKeyError(obj.kind())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_hash_key() {
        let hello1 = Rc::new(Object::String("Hello World!".into()));
        let hello2 = Rc::new(Object::String("Hello World!".into()));
        let diff1 = Rc::new(Object::String("My name is sig".into()));
        let diff2 = Rc::new(Object::String("My name is sig".into()));

        let mut objutil = ObjectUtil::new();

        assert_eq!(objutil.hash_key(&hello1), objutil.hash_key(&hello2));
        assert_eq!(objutil.hash_key(&diff1), objutil.hash_key(&diff2));
        assert_ne!(objutil.hash_key(&hello1), objutil.hash_key(&diff1));
    }
}
