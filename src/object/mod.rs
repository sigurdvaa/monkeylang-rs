pub mod environment;

use crate::ast::{BlockStatement, IdentifierLiteral};
use environment::Env;
use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

pub type Integer = IntegerObj;
pub type BuiltinFunction = fn(&[Rc<Object>]) -> Rc<Object>;
pub type Array = Vec<Rc<Object>>;
pub type HashObj = HashMap<HashKeyData, (Rc<Object>, Rc<Object>)>;
type HashKey = RefCell<Option<HashKeyData>>;

#[derive(Debug, PartialEq, PartialOrd)]
pub struct IntegerObj {
    pub value: isize,
    hash: HashKey,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct BooleanObj {
    pub value: bool,
    hash: HashKey,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct StringObj {
    pub value: String,
    hash: HashKey,
}

#[derive(Debug, PartialEq)]
pub struct FunctionObj {
    pub parameters: Vec<IdentifierLiteral>,
    pub body: BlockStatement,
    pub env: Env,
}

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Clone)]
pub struct HashKeyData {
    kind: String,
    value: usize,
}

#[derive(Debug, PartialEq)]
pub enum Object {
    Null,
    Integer(IntegerObj),
    Boolean(BooleanObj),
    Return(Rc<Self>),
    Error(String),
    Function(FunctionObj),
    String(StringObj),
    Builtin(BuiltinFunction),
    Array(Array),
    Hash(HashObj),
}

impl Object {
    pub fn new_boolean(value: bool) -> Self {
        Self::Boolean(BooleanObj {
            value,
            hash: RefCell::new(None),
        })
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

    pub fn kind(&self) -> &str {
        match self {
            Self::Null => "NULL",
            Self::Integer(_) => "INTEGER",
            Self::Boolean(_) => "BOOLEAN",
            Self::Return(_) => "RETURN",
            Self::Error(_) => "ERROR",
            Self::Function(_) => "FUNCTION",
            Self::String(_) => "STRING",
            Self::Builtin(_) => "BUILTIN",
            Self::Array(_) => "ARRAY",
            Self::Hash(_) => "HASH",
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Self::Null => "null".into(),
            Self::Integer(obj) => obj.value.to_string(),
            Self::Boolean(obj) => obj.value.to_string(),
            Self::Return(value) => value.inspect(),
            Self::Error(value) => format!("ERROR: {value}"),
            Self::Function(func) => {
                let mut buffer = String::from("fn(");
                buffer.push_str(
                    &func
                        .parameters
                        .iter()
                        .map(|i| i.value.as_str())
                        .collect::<Vec<_>>()
                        .join(", "),
                );
                buffer.push_str(") {\n");
                buffer.push_str(&func.body.to_string());
                buffer.push_str("\n}");
                buffer
            }
            Self::String(obj) => format!("\"{}\"", obj.value.to_owned()),
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
        }
    }

    fn set_hash_key(
        &self,
        mut hash: RefMut<Option<HashKeyData>>,
        kind: String,
        value: usize,
    ) -> HashKeyData {
        let hash_key = HashKeyData { kind, value };
        *hash = Some(hash_key.clone());
        hash_key
    }

    pub fn hash_key(&self) -> Result<HashKeyData, Rc<Object>> {
        // TODO: avoid collisions
        // TODO: improve cache
        match self {
            Self::Boolean(obj) => {
                if let Some(hash_key) = obj.hash.borrow().as_ref() {
                    return Ok(hash_key.clone());
                }
                Ok(self.set_hash_key(
                    obj.hash.borrow_mut(),
                    self.kind().into(),
                    obj.value as usize,
                ))
            }
            Self::Integer(obj) => {
                if let Some(hash_key) = obj.hash.borrow().as_ref() {
                    return Ok(hash_key.clone());
                }
                Ok(self.set_hash_key(
                    obj.hash.borrow_mut(),
                    self.kind().into(),
                    obj.value as usize,
                ))
            }
            Self::String(obj) => {
                if let Some(hash_key) = obj.hash.borrow().as_ref() {
                    return Ok(hash_key.clone());
                }
                Ok(self.set_hash_key(
                    obj.hash.borrow_mut(),
                    self.kind().into(),
                    obj.value.chars().map(|c| c as usize).sum(),
                ))
            }
            _ => Err(Rc::new(Object::Error(format!(
                "Hash not implemented for {}",
                self.kind()
            )))),
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
