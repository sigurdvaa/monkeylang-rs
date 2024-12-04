use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Illegal(String),
    Eof,

    // Identifiers and literals
    Ident(String),
    Int(usize),

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

    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub file: Option<String>,
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    file: Option<String>,
    line: usize,
    curr_col: usize,
    next_col: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(file: Option<String>, input: Peekable<Chars<'a>>) -> Self {
        Self {
            input,
            file,
            line: 1,
            curr_col: 1,
            next_col: 1,
        }
    }

    fn is_letter(c: &char) -> bool {
        c.is_ascii_alphabetic() || *c == '_'
    }

    fn new_token(&mut self, kind: TokenKind) -> Token {
        let token = Token {
            kind,
            file: self.file.clone(),
            line: self.line,
            col: self.curr_col,
        };
        self.curr_col = self.next_col;
        token
    }

    fn lookup_ident(&mut self, ident: String) -> Token {
        match ident.as_str() {
            "let" => self.new_token(TokenKind::Let),
            "fn" => self.new_token(TokenKind::Function),
            "if" => self.new_token(TokenKind::If),
            "else" => self.new_token(TokenKind::Else),
            "return" => self.new_token(TokenKind::Return),
            "true" => self.new_token(TokenKind::True),
            "false" => self.new_token(TokenKind::False),
            _ => self.new_token(TokenKind::Ident(ident)),
        }
    }

    fn read_ident(&mut self, c: char) -> Token {
        let mut ident = c.to_string();
        while let Some(c) = self.input.peek() {
            if c.is_ascii_alphabetic() || *c == '_' {
                ident.push(self.input.next().expect("invalid char"));
                self.next_col += 1;
            } else {
                break;
            }
        }
        self.lookup_ident(ident)
    }

    fn read_number(&mut self, c: char) -> Token {
        let mut number = c.to_string();
        while let Some(c) = self.input.peek() {
            if c.is_ascii_digit() {
                number.push(self.input.next().expect("invalid char"));
                self.next_col += 1;
            } else {
                break;
            }
        }
        self.new_token(TokenKind::Int(number.parse().expect("Invalid number")))
    }

    pub fn next_token(&mut self) -> Token {
        self.next_col += 1;
        match self.input.next() {
            None => {
                self.next_col -= 1;
                self.new_token(TokenKind::Eof)
            }
            Some(c) => match c {
                '=' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        self.next_col += 1;
                        self.new_token(TokenKind::Eq)
                    } else {
                        self.new_token(TokenKind::Assign)
                    }
                }
                '+' => self.new_token(TokenKind::Plus),
                '-' => self.new_token(TokenKind::Minus),
                '(' => self.new_token(TokenKind::Lparen),
                ')' => self.new_token(TokenKind::Rparen),
                '{' => self.new_token(TokenKind::Lbrace),
                '}' => self.new_token(TokenKind::Rbrace),
                ';' => self.new_token(TokenKind::Semicolon),
                ',' => self.new_token(TokenKind::Comma),
                '!' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        self.next_col += 1;
                        self.new_token(TokenKind::NotEq)
                    } else {
                        self.new_token(TokenKind::Bang)
                    }
                }
                '*' => self.new_token(TokenKind::Asterisk),
                '/' => self.new_token(TokenKind::Slash),
                '<' => self.new_token(TokenKind::Lt),
                '>' => self.new_token(TokenKind::Gt),
                '\n' => {
                    self.line += 1;
                    self.curr_col = 1;
                    self.next_col = 1;
                    self.next_token()
                }
                c if c.is_whitespace() => {
                    self.curr_col = self.next_col;
                    self.next_token()
                }
                c if Self::is_letter(&c) => self.read_ident(c),
                c if c.is_ascii_digit() => self.read_number(c),
                _ => self.new_token(TokenKind::Illegal(c.to_string())),
            },
        }
    }
}

#[cfg(test)
mod tests {
    use super::*;

    #[test]
    fn test_parse_tokens() {
        let input = concat!(
            "let five = 5;\n",
            "let ten = 10;\n",
            "\n",
            "let add = fn(x, y) {\n",
            "    x + y;\n",
            "};\n",
            "\n",
            "let result = add(five, ten);\n",
            "!-/*5;\n",
            "5 < 10 > 5;\n",
            "\n",
            "if (5 < 10) {\n",
            "    return true;\n",
            "} else {\n",
            "    return false;\n",
            "}\n",
            "10 == 10;\n",
            "10 != 9;\n",
        );

        let mut lexer = Lexer::new(None, input.chars().peekable());
        #[rustfmt::skip]
        let tests = [
            Token { file: None, line: 1, col: 1, kind: TokenKind::Let },
            Token { file: None, line: 1, col: 5, kind: TokenKind::Ident("five".to_string()) },
            Token { file: None, line: 1, col: 10, kind: TokenKind::Assign },
            Token { file: None, line: 1, col: 12, kind: TokenKind::Int(5) },
            Token { file: None, line: 1, col: 13, kind: TokenKind::Semicolon },
            Token { file: None, line: 2, col: 1, kind: TokenKind::Let },
            Token { file: None, line: 2, col: 5, kind: TokenKind::Ident("ten".to_string()) },
            Token { file: None, line: 2, col: 9, kind: TokenKind::Assign },
            Token { file: None, line: 2, col: 11, kind: TokenKind::Int(10) },
            Token { file: None, line: 2, col: 13, kind: TokenKind::Semicolon },
            Token { file: None, line: 4, col: 1, kind: TokenKind::Let },
            Token { file: None, line: 4, col: 5, kind: TokenKind::Ident("add".to_string()) },
            Token { file: None, line: 4, col: 9, kind: TokenKind::Assign },
            Token { file: None, line: 4, col: 11, kind: TokenKind::Function },
            Token { file: None, line: 4, col: 13, kind: TokenKind::Lparen },
            Token { file: None, line: 4, col: 14, kind: TokenKind::Ident("x".to_string()) },
            Token { file: None, line: 4, col: 15, kind: TokenKind::Comma },
            Token { file: None, line: 4, col: 17, kind: TokenKind::Ident("y".to_string()) },
            Token { file: None, line: 4, col: 18, kind: TokenKind::Rparen },
            Token { file: None, line: 4, col: 20, kind: TokenKind::Lbrace},
            Token { file: None, line: 5, col: 5, kind: TokenKind::Ident("x".to_string()) },
            Token { file: None, line: 5, col: 7, kind: TokenKind::Plus },
            Token { file: None, line: 5, col: 9, kind: TokenKind::Ident("y".to_string()) },
            Token { file: None, line: 5, col: 10, kind: TokenKind::Semicolon },
            Token { file: None, line: 6, col: 1, kind: TokenKind::Rbrace},
            Token { file: None, line: 6, col: 2, kind: TokenKind::Semicolon },
            Token { file: None, line: 8, col: 1, kind: TokenKind::Let },
            Token { file: None, line: 8, col: 5, kind: TokenKind::Ident("result".to_string()) },
            Token { file: None, line: 8, col: 12, kind: TokenKind::Assign },
            Token { file: None, line: 8, col: 14, kind: TokenKind::Ident("add".to_string()) },
            Token { file: None, line: 8, col: 17, kind: TokenKind::Lparen },
            Token { file: None, line: 8, col: 18, kind: TokenKind::Ident("five".to_string()) },
            Token { file: None, line: 8, col: 22, kind: TokenKind::Comma },
            Token { file: None, line: 8, col: 24, kind: TokenKind::Ident("ten".to_string()) },
            Token { file: None, line: 8, col: 27, kind: TokenKind::Rparen },
            Token { file: None, line: 8, col: 28, kind: TokenKind::Semicolon },
            Token { file: None, line: 9, col: 1, kind: TokenKind::Bang },
            Token { file: None, line: 9, col: 2, kind: TokenKind::Minus },
            Token { file: None, line: 9, col: 3, kind: TokenKind::Slash },
            Token { file: None, line: 9, col: 4, kind: TokenKind::Asterisk },
            Token { file: None, line: 9, col: 5, kind: TokenKind::Int(5) },
            Token { file: None, line: 9, col: 6, kind: TokenKind::Semicolon },
            Token { file: None, line: 10, col: 1, kind: TokenKind::Int(5) },
            Token { file: None, line: 10, col: 3, kind: TokenKind::Lt},
            Token { file: None, line: 10, col: 5, kind: TokenKind::Int(10) },
            Token { file: None, line: 10, col: 8, kind: TokenKind::Gt},
            Token { file: None, line: 10, col: 10, kind: TokenKind::Int(5) },
            Token { file: None, line: 10, col: 11, kind: TokenKind::Semicolon },
            Token { file: None, line: 12, col: 1, kind: TokenKind::If },
            Token { file: None, line: 12, col: 4, kind: TokenKind::Lparen },
            Token { file: None, line: 12, col: 5, kind: TokenKind::Int(5) },
            Token { file: None, line: 12, col: 7, kind: TokenKind::Lt},
            Token { file: None, line: 12, col: 9, kind: TokenKind::Int(10) },
            Token { file: None, line: 12, col: 11, kind: TokenKind::Rparen },
            Token { file: None, line: 12, col: 13, kind: TokenKind::Lbrace },
            Token { file: None, line: 13, col: 5, kind: TokenKind::Return },
            Token { file: None, line: 13, col: 12, kind: TokenKind::True },
            Token { file: None, line: 13, col: 16, kind: TokenKind::Semicolon },
            Token { file: None, line: 14, col: 1, kind: TokenKind::Rbrace },
            Token { file: None, line: 14, col: 3, kind: TokenKind::Else },
            Token { file: None, line: 14, col: 8, kind: TokenKind::Lbrace },
            Token { file: None, line: 15, col: 5, kind: TokenKind::Return },
            Token { file: None, line: 15, col: 12, kind: TokenKind::False },
            Token { file: None, line: 15, col: 17, kind: TokenKind::Semicolon },
            Token { file: None, line: 16, col: 1, kind: TokenKind::Rbrace },
            Token { file: None, line: 17, col: 1, kind: TokenKind::Int(10) },
            Token { file: None, line: 17, col: 4, kind: TokenKind::Eq },
            Token { file: None, line: 17, col: 7, kind: TokenKind::Int(10) },
            Token { file: None, line: 17, col: 9, kind: TokenKind::Semicolon },
            Token { file: None, line: 18, col: 1, kind: TokenKind::Int(10) },
            Token { file: None, line: 18, col: 4, kind: TokenKind::NotEq },
            Token { file: None, line: 18, col: 7, kind: TokenKind::Int(9) },
            Token { file: None, line: 18, col: 8, kind: TokenKind::Semicolon },
            // end
            Token { file: None, line: 19, col: 1, kind: TokenKind::Eof },
            Token { file: None, line: 19, col: 1, kind: TokenKind::Eof },
        ];

        for token in tests {
            assert_eq!(lexer.next_token(), token);
        }
    }
}
