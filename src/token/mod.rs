use std::fmt;

#[derive(Debug, Hash, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    Illegal,
    EndOfFile,

    // Identifiers and literals
    Identifier,
    Integer,
    String,
    Null,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,

    // Delimiters
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,
    Colon,

    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
    Macro,
    Quote,
    Unquote,
    Exit,
    Loop,
    Break,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Identifier => write!(f, "identifier"),
            Self::Lparen => write!(f, "opening parenthesis"),
            Self::Rparen => write!(f, "closing parenthesis"),
            Self::Lbrace => write!(f, "opening brace"),
            Self::Rbrace => write!(f, "closing brace"),
            Self::Lbracket => write!(f, "opening bracket"),
            Self::Rbracket => write!(f, "closing bracket"),
            _ => write!(f, "{}", format!("{:?}", self).to_lowercase()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token {
    pub kind: TokenKind,
    pub literal: String,
    pub file: Option<String>,
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Token {
    pub fn loc(&self) -> String {
        match &self.file {
            Some(file) => format!("{file}:{}:{}", self.line, self.col),
            None => format!("(input):{}:{}", self.line, self.col),
        }
    }
}
