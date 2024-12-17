use crate::lexer::{Token, TokenKind};
use std::fmt;

#[derive(PartialEq, PartialOrd, Debug)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    // Todo: add call
    // Call,
}

impl From<&TokenKind> for Precedence {
    fn from(value: &TokenKind) -> Self {
        match value {
            TokenKind::Eq => Self::Equals,
            TokenKind::NotEq => Self::Equals,
            TokenKind::Lt => Self::LessGreater,
            TokenKind::Gt => Self::LessGreater,
            TokenKind::Plus => Self::Sum,
            TokenKind::Minus => Self::Sum,
            TokenKind::Slash => Self::Product,
            TokenKind::Asterisk => Self::Product,
            _ => Self::Lowest,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Bang,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Gt,
    Lt,
    Eq,
    NotEq,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Bang => write!(f, "!"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Asterisk => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Gt => write!(f, ">"),
            Self::Lt => write!(f, "<"),
            Self::Eq => write!(f, "=="),
            Self::NotEq => write!(f, "!="),
        }
    }
}

impl TryFrom<&str> for Operator {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            v if v == Self::Bang.to_string() => Ok(Self::Bang),
            v if v == Self::Plus.to_string() => Ok(Self::Plus),
            v if v == Self::Minus.to_string() => Ok(Self::Minus),
            v if v == Self::Asterisk.to_string() => Ok(Self::Asterisk),
            v if v == Self::Slash.to_string() => Ok(Self::Slash),
            v if v == Self::Gt.to_string() => Ok(Self::Gt),
            v if v == Self::Lt.to_string() => Ok(Self::Lt),
            v if v == Self::Eq.to_string() => Ok(Self::Eq),
            v if v == Self::NotEq.to_string() => Ok(Self::NotEq),
            _ => Err("Unknown operator"),
        }
    }
}

#[derive(Debug)]
pub enum ExpressionKind {
    Boolean {
        value: bool,
    },
    Identifier {
        value: String,
    },
    Prefix {
        operator: Operator,
        right: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
    IntegerLiteral {
        value: usize,
    },
}

impl PartialEq for ExpressionKind {
    fn eq(&self, other: &Self) -> bool {
        use ExpressionKind::*;
        match (self, other) {
            (Boolean { value: a }, Boolean { value: b }) => a == b,
            (Identifier { value: a }, Identifier { value: b }) => a == b,
            (
                Prefix {
                    operator: a_op,
                    right: a_rhs,
                },
                Prefix {
                    operator: b_op,
                    right: b_rhs,
                },
            ) => a_op == b_op && *a_rhs == *b_rhs,
            (
                Infix {
                    left: a_lhs,
                    operator: a_op,
                    right: a_rhs,
                },
                Infix {
                    left: b_lhs,
                    operator: b_op,
                    right: b_rhs,
                },
            ) => a_op == b_op && *a_lhs == *b_lhs && *a_rhs == *b_rhs,
            (IntegerLiteral { value: a }, IntegerLiteral { value: b }) => a == b,
            _ => false,
        }
    }
}

impl fmt::Display for ExpressionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Boolean { value } => write!(f, "{value}"),
            Self::Identifier { value } => write!(f, "{value}"),
            Self::IntegerLiteral { value } => write!(f, "{value}"),
            Self::Prefix { operator, right } => write!(f, "({operator}{right})"),
            Self::Infix {
                left,
                operator,
                right,
            } => write!(f, "({left} {operator} {right})"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Expression {
    pub token: Token,
    pub kind: ExpressionKind,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug)]
pub enum Statement {
    Let {
        token: Token,
        name: Expression,
        value: Expression,
    },
    Return {
        token: Token,
        value: Expression,
    },
    Expression {
        token: Token,
        value: Expression,
    },
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let { token, name, value } => {
                write!(f, "{} {} = {};", token.literal, name, value)
            }
            Statement::Return { token, value } => {
                write!(f, "{} {};", token.literal, value)
            }
            Statement::Expression { token: _, value } => {
                write!(f, "{}", value)
            }
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();
        for stmt in &self.statements {
            string.push_str(&stmt.to_string());
        }
        write!(f, "{string}")
    }
}

impl Program {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }
}
