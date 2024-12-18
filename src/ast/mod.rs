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
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

#[derive(Debug)]
pub struct IdentifierLiteral {
    pub token: Token,
    pub value: String,
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: usize,
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: Operator,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: Operator,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(Debug)]
pub struct FunctionExpression {
    pub token: Token,
    pub parameters: Vec<IdentifierLiteral>,
    pub block: BlockStatement,
}

#[derive(Debug)]
pub enum Expression {
    Boolean(BooleanLiteral),
    Function(FunctionExpression),
    Identifier(IdentifierLiteral),
    If(IfExpression),
    Infix(InfixExpression),
    IntegerLiteral(IntegerLiteral),
    Prefix(PrefixExpression),
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Boolean(a), Self::Boolean(b)) => a.value == b.value,
            (Self::Identifier(a), Self::Identifier(b)) => a.value == b.value,
            (Self::Infix(a), Self::Infix(b)) => {
                a.left == b.left && a.operator == b.operator && a.right == b.right
            }
            (Self::IntegerLiteral(a), Self::IntegerLiteral(b)) => a.value == b.value,
            (Self::Prefix(a), Self::Prefix(b)) => a.operator == b.operator && a.right == b.right,
            _ => false,
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Boolean(expr) => write!(f, "{}", expr.value),
            Self::Identifier(expr) => write!(f, "{}", expr.value),
            Self::IntegerLiteral(expr) => write!(f, "{}", expr.value),
            Self::Prefix(expr) => write!(f, "({}{})", expr.operator, expr.right),
            Self::Infix(expr) => write!(f, "({} {} {})", expr.left, expr.operator, expr.right),
            Self::If(expr) => {
                if let Some(alternative) = &expr.alternative {
                    write!(
                        f,
                        "if {} {} else {alternative}",
                        expr.condition, expr.consequence
                    )
                } else {
                    write!(f, "if {} {}", expr.condition, expr.consequence)
                }
            }
            Self::Function(expr) => write!(
                f,
                "({}) {}",
                expr.parameters
                    .iter()
                    .map(|i| i.value.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                expr.block,
            ),
        }
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Expression,
    pub value: Expression,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Expression,
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub value: Expression,
}

#[derive(Debug)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();
        for sub in &self.statements {
            string.push_str(&sub.to_string());
        }
        write!(f, "{string}")
    }
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(stmt) => {
                write!(f, "{} {} = {};", stmt.token.literal, stmt.name, stmt.value)
            }
            Statement::Return(stmt) => {
                write!(f, "{} {};", stmt.token.literal, stmt.value)
            }
            Statement::Expression(stmt) => write!(f, "{}", stmt.value),
            Statement::Block(stmt) => write!(f, "{stmt}"),
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
