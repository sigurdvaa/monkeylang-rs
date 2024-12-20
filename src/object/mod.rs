pub mod environment;

use crate::ast::{BlockStatement, IdentifierLiteral};
use environment::Env;
use std::rc::Rc;

pub type Integer = isize;
pub type BuiltinFunction = fn(&[Rc<Object>]) -> Rc<Object>;
pub type Array = Vec<Rc<Object>>;

#[derive(Debug, PartialEq)]
pub struct FunctionObject {
    pub parameters: Vec<IdentifierLiteral>,
    pub body: BlockStatement,
    pub env: Env,
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
        }
    }
}
