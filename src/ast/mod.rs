use crate::lexer::Token;

struct Node {}
struct Expression {}
struct Statement<'a> {
    token: Token<'a>,
}

struct Identifier<'a> {
    token: Token<'a>,
    value: String,
}

struct Program<'a> {
    statements: Vec<Statement<'a>>,
}

struct LetStatement<'a> {
    name: Identifier<'a>,
    value: Expression,
    token: Token<'a>,
}
