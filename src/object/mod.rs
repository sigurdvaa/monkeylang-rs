pub type Integer = isize;

#[derive(Debug, PartialEq)]
pub enum Object {
    Null,
    Integer(Integer),
    Boolean(bool),
    Return(Box<Self>),
    Error(String),
}

impl Object {
    pub fn kind(&self) -> &str {
        match self {
            Self::Null => "NULL",
            Self::Integer(_) => "INTEGER",
            Self::Boolean(_) => "BOOLEAN",
            Self::Return(_) => "RETURN",
            Self::Error(_) => "ERROR",
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Self::Null => "null".into(),
            Self::Integer(value) => value.to_string(),
            Self::Boolean(value) => value.to_string(),
            Self::Return(value) => value.inspect(),
            Self::Error(value) => format!("ERROR: {value}"),
        }
    }
}
