pub mod environment;

use crate::ast::{BlockStatement, IdentifierLiteral};
use environment::Env;

pub type Integer = isize;

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionObject {
    pub parameters: Vec<IdentifierLiteral>,
    pub body: BlockStatement,
    pub env: Env,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Null,
    Integer(Integer),
    Boolean(bool),
    Return(Box<Self>),
    Error(String),
    Function(FunctionObject),
    String(String),
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
        }
    }
}
