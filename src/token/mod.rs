use std::fmt;

#[derive(Debug, Hash, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    Illegal,
    EndOfFile,

    // Identifiers and literals
    Ident,
    Int,
    String,
    Null,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,

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
    // TODO: loop
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
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
            None => format!("{}:{}", self.line, self.col),
        }
    }
}
