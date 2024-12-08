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
    Call,
}

impl From<&TokenKind> for Precedence {
    fn from(value: &TokenKind) -> Self {
        match *value {
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

impl TryFrom<&str> for Operator {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "!" => Ok(Self::Bang),
            "+" => Ok(Self::Plus),
            "-" => Ok(Self::Minus),
            "*" => Ok(Self::Asterisk),
            "/" => Ok(Self::Slash),
            "<" => Ok(Self::Lt),
            ">" => Ok(Self::Gt),
            "==" => Ok(Self::Eq),
            "!=" => Ok(Self::NotEq),
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

#[derive(Debug, PartialEq)]
pub struct Expression {
    pub token: Token,
    pub kind: ExpressionKind,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: might have to impl this per kind
        write!(f, "{}", self.token.literal)
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
                write!(f, "{};", value)
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
