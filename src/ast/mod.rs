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
            TokenKind::Lparen => Self::Call,
            _ => Self::Lowest,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierLiteral {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: Operator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: Operator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<IdentifierLiteral>,
    pub body: BlockStatement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Boolean(BooleanLiteral),
    Call(CallExpression),
    Function(FunctionLiteral),
    Identifier(IdentifierLiteral),
    If(IfExpression),
    Infix(InfixExpression),
    IntegerLiteral(IntegerLiteral),
    Prefix(PrefixExpression),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Boolean(expr) => write!(f, "{}", expr.value),
            Self::Call(expr) => write!(
                f,
                "{}({})",
                expr.function,
                expr.arguments
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
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
                "{}({}) {}",
                expr.token.literal,
                expr.parameters
                    .iter()
                    .map(|i| i.value.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                expr.body,
            ),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub name: IdentifierLiteral,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(stmt) => {
                write!(
                    f,
                    "{} {} = {};",
                    stmt.token.literal, stmt.name.value, stmt.value
                )
            }
            Statement::Return(stmt) => {
                write!(f, "{} {};", stmt.token.literal, stmt.value)
            }
            Statement::Expression(stmt) => write!(f, "{}", stmt.value),
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
