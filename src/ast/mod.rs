pub mod modify;

use crate::token::Token;
use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IdentifierLiteral {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: usize,
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: Operator,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: Operator,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<IdentifierLiteral>,
    pub body: BlockStatement,
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: BTreeMap<Expression, Expression>,
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub enum Expression {
    Boolean(BooleanLiteral),
    Call(CallExpression),
    Function(FunctionLiteral),
    Identifier(IdentifierLiteral),
    Null(Token),
    If(IfExpression),
    Infix(InfixExpression),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
    String(StringLiteral),
    Array(ArrayLiteral),
    Index(IndexExpression),
    Hash(HashLiteral),
    Macro(FunctionLiteral),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Boolean(expr) => write!(f, "{}", expr.value),
            Self::Null(_token) => write!(f, "null"),
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
            Self::Integer(expr) => write!(f, "{}", expr.value),
            Self::Prefix(expr) => write!(f, "({}{})", expr.operator, expr.right),
            Self::Infix(expr) => write!(f, "({} {} {})", expr.left, expr.operator, expr.right),
            Self::If(expr) => {
                if let Some(alternative) = &expr.alternative {
                    write!(
                        f,
                        "if ({}) {{ {} }} else {{ {alternative} }}",
                        expr.condition, expr.consequence
                    )
                } else {
                    write!(f, "if ({}) {{ {} }}", expr.condition, expr.consequence)
                }
            }
            Self::Function(expr) | Self::Macro(expr) => write!(
                f,
                "{}({}) {{ {}}}",
                expr.token.literal,
                expr.parameters
                    .iter()
                    .map(|i| i.value.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                expr.body,
            ),
            Self::String(expr) => write!(f, "\"{}\"", expr.value),
            Self::Array(expr) => write!(
                f,
                "[{}]",
                expr.elements
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Index(expr) => {
                write!(f, "({}[{}])", expr.left, expr.index)
            }
            Self::Hash(expr) => {
                let mut buffer = String::from("{");
                for (key, value) in expr.pairs.iter() {
                    buffer.push_str(&key.to_string());
                    buffer.push(':');
                    buffer.push_str(&value.to_string());
                }
                write!(f, "{{{buffer}}}")
            }
        }
    }
}

impl Expression {
    pub fn get_token(&self) -> &Token {
        match self {
            Self::Boolean(expr) => &expr.token,
            Self::Call(expr) => &expr.token,
            Self::Function(expr) | Self::Macro(expr) => &expr.token,
            Self::Identifier(expr) => &expr.token,
            Self::Null(token) => token,
            Self::If(expr) => &expr.token,
            Self::Infix(expr) => &expr.token,
            Self::Integer(expr) => &expr.token,
            Self::Prefix(expr) => &expr.token,
            Self::String(expr) => &expr.token,
            Self::Array(expr) => &expr.token,
            Self::Index(expr) => &expr.token,
            Self::Hash(expr) => &expr.token,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct LetStatement {
    pub token: Token,
    pub name: IdentifierLiteral,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct ExpressionStatement {
    pub token: Token,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Debug, PartialEq)]
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
