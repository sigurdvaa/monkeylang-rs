use crate::lexer::Token;

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

// pub struct Expression {
//     token: Token,
//     // TODO: enum?
//     operator: String,
//     // right: Self
// }

#[derive(Debug)]
pub enum Operator {
    Add,
    Minus,
    Multiply,
}

#[derive(Debug)]
pub enum Expression {
    Prefix {
        token: Token,
        operator: Operator,
        // right: Rc<Self>
    },
}

enum StatementKind {
    Let,
    If,
}

#[derive(Debug)]
pub enum Statement {
    Let {
        token: Token,
        name: Identifier,
        // value: Expression,
    },
    If {
        token: Token,
        left: Expression,
        operator: Operator,
        right: Expression,
    },
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }
}
