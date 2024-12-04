use crate::lexer::Token;

pub struct Identifier {
    token: Token,
    value: String,
}

// pub struct Expression {
//     token: Token,
//     // TODO: enum?
//     operator: String,
//     // right: Self
// }

pub enum Expression {
    Prefix {
        token: Token,
        // TODO: enum?
        operator: String,
        // right: Rc<Self>
    },
}

pub enum Statement {
    Let {
        token: Token,
        name: Identifier,
        value: Expression,
    },
    If {
        token: Token,
        left: Expression,
        right: Expression,
    },
}

pub struct Program {
    pub statements: Vec<Statement>,
}

struct LetStatement {}
