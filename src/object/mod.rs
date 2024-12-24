pub mod environment;

use crate::ast::{BlockStatement, IdentifierLiteral};
use environment::Env;
use std::collections::HashMap;
use std::rc::Rc;

pub type Integer = isize;
pub type BuiltinFunction = fn(&[Rc<Object>]) -> Rc<Object>;
pub type Array = Vec<Rc<Object>>;
pub type Hash = HashMap<HashKey, (Rc<Object>, Rc<Object>)>;

#[derive(Debug, PartialEq)]
pub struct FunctionObject {
    pub parameters: Vec<IdentifierLiteral>,
    pub body: BlockStatement,
    pub env: Env,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct HashKey {
    kind: String,
    value: usize,
}

#[derive(Debug, PartialEq)]
pub enum Object {
    Null,
    Integer(Integer),
    Boolean(bool),
    Return(Rc<Self>),
    Error(String),
    Function(FunctionObject),
    String(String),
    Builtin(BuiltinFunction),
    Array(Array),
    Hash(Hash),
}

impl Object {
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
            Self::Integer(value) => value.to_string(),
            Self::Boolean(value) => value.to_string(),
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
            Self::String(value) => value.to_owned(),
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
                let mut buffer = String::new();
                for (key, value) in value.values() {
                    buffer.push_str(&format!("{}: {}", key.inspect(), value.inspect()));
                }
                format!("{{{buffer}}}")
            }
        }
    }

    pub fn hash_key(&self) -> Result<HashKey, Object> {
        // TODO: add cache
        // TODO: avoid collisions
        match self {
            Self::Boolean(value) => Ok(HashKey {
                kind: self.kind().into(),
                value: *value as usize,
            }),
            Self::Integer(value) => Ok(HashKey {
                kind: self.kind().into(),
                value: *value as usize,
            }),
            Self::String(value) => Ok(HashKey {
                kind: self.kind().into(),
                value: value.chars().map(|c| c as usize).sum(),
            }),
            _ => Err(Object::Error(format!(
                "Hash not implemented for {}",
                self.kind()
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_hash_key() {
        let hello1 = Object::String("Hello World!".into());
        let hello2 = Object::String("Hello World!".into());
        let diff1 = Object::String("My name is sig".into());
        let diff2 = Object::String("My name is sig".into());

        assert_eq!(hello1.hash_key(), hello2.hash_key());
        assert_eq!(diff1.hash_key(), diff2.hash_key());
        assert_ne!(hello1.hash_key(), diff1.hash_key());
    }
}
