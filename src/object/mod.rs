#[derive(Debug, PartialEq)]
pub enum Object {
    Null,
    Integer(isize),
    Boolean(bool),
}

impl Object {
    pub fn kind(&self) -> &str {
        match self {
            Self::Null => "null",
            Self::Integer(_) => "integer",
            Self::Boolean(_) => "boolean",
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Self::Null => "null".into(),
            Self::Integer(value) => format!("{value}"),
            Self::Boolean(value) => format!("{value}"),
        }
    }
}
