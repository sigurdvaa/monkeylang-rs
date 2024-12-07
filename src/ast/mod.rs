use crate::lexer::Token;
use std::fmt;

pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Asterisk,
}

#[derive(Debug, PartialEq)]
pub enum ExpressionKind {
    Boolean {
        value: bool,
    },
    Identifier {
        value: String,
    },
    Prefix {
        operator: Operator,
    },
    Infix {
        // left: Box<Expression>,
        operator: Operator,
        // right: Box<Expression>,
    },
    IntegerLiteral {
        value: usize,
    },
}

#[derive(Debug)]
pub struct Expression {
    pub token: Token,
    pub kind: ExpressionKind,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:}", self.token.literal)
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
            _ => write!(f, "{:?}", self),
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
