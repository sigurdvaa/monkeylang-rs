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
    LT,
    GT,
    EQ,
    NEQ,

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
    // TODO: file, line, col
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
        }
    }

    fn is_letter(c: &char) -> bool {
        c.is_ascii_alphabetic() || *c == '_'
    }

    #[rustfmt::skip]
    fn lookup_ident(ident: String) -> Token {
        match ident.as_str() {
            "let" => Token { kind: TokenKind::Let },
            "fn" => Token { kind: TokenKind::Function },
            "if" => Token { kind: TokenKind::If },
            "else" => Token { kind: TokenKind::Else },
            "return" => Token { kind: TokenKind::Return },
            "true" => Token { kind: TokenKind::True },
            "false" => Token { kind: TokenKind::False },
            _ => Token { kind: TokenKind::Ident(ident) },
        }
    }

    fn read_ident(&mut self, c: char) -> Token {
        let mut ident = c.to_string();
        while let Some(c) = self.input.peek() {
            if c.is_ascii_alphabetic() || *c == '_' {
                ident.push(*c);
                self.input.next();
            } else {
                break;
            }
        }
        Self::lookup_ident(ident)
    }

    fn read_number(&mut self, c: char) -> Token {
        let mut number = c.to_string();
        while let Some(c) = self.input.peek() {
            if c.is_ascii_digit() {
                number.push(*c);
                self.input.next();
            } else {
                break;
            }
        }
        Token {
            kind: TokenKind::Int(number.parse().expect("Invalid number")),
        }
    }

    pub fn next_token(&mut self) -> Token {
        match self.input.next() {
            None => Token {
                kind: TokenKind::Eof,
            },
            #[rustfmt::skip]
            Some(c) => match c {
                '=' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        Token { kind: TokenKind::EQ }
                    } else {
                        Token { kind: TokenKind::Assign }
                    }
                },
                '+' => Token { kind: TokenKind::Plus },
                '-' => Token { kind: TokenKind::Minus },
                '(' => Token { kind: TokenKind::Lparen },
                ')' => Token { kind: TokenKind::Rparen },
                '{' => Token { kind: TokenKind::Lbrace },
                '}' => Token { kind: TokenKind::Rbrace },
                ';' => Token { kind: TokenKind::Semicolon },
                ',' => Token { kind: TokenKind::Comma },
                '!' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        Token { kind: TokenKind::NEQ }
                    } else {
                        Token { kind: TokenKind::Bang }
                    }
                },
                '*' => Token { kind: TokenKind::Asterisk },
                '/' => Token { kind: TokenKind::Slash },
                '<' => Token { kind: TokenKind::LT },
                '>' => Token { kind: TokenKind::GT},
                c if c.is_whitespace() => self.next_token(),
                c if Self::is_letter(&c) => self.read_ident(c),
                c if c.is_ascii_digit() => self.read_number(c),
                _ => Token { kind: TokenKind::Illegal(c.to_string()) },
            },
        }
    }
}

#[cfg(test)]
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

        let mut lexer = Lexer::new(input);
        #[rustfmt::skip]
        let tests = [
            Token { kind: TokenKind::Let },
            Token { kind: TokenKind::Ident("five".to_string()) },
            Token { kind: TokenKind::Assign },
            Token { kind: TokenKind::Int(5) },
            Token { kind: TokenKind::Semicolon },
            Token { kind: TokenKind::Let },
            Token { kind: TokenKind::Ident("ten".to_string()) },
            Token { kind: TokenKind::Assign },
            Token { kind: TokenKind::Int(10) },
            Token { kind: TokenKind::Semicolon },
            Token { kind: TokenKind::Let },
            Token { kind: TokenKind::Ident("add".to_string()) },
            Token { kind: TokenKind::Assign },
            Token { kind: TokenKind::Function },
            Token { kind: TokenKind::Lparen },
            Token { kind: TokenKind::Ident("x".to_string()) },
            Token { kind: TokenKind::Comma },
            Token { kind: TokenKind::Ident("y".to_string()) },
            Token { kind: TokenKind::Rparen },
            Token { kind: TokenKind::Lbrace},
            Token { kind: TokenKind::Ident("x".to_string()) },
            Token { kind: TokenKind::Plus },
            Token { kind: TokenKind::Ident("y".to_string()) },
            Token { kind: TokenKind::Semicolon },
            Token { kind: TokenKind::Rbrace},
            Token { kind: TokenKind::Semicolon },
            Token { kind: TokenKind::Let },
            Token { kind: TokenKind::Ident("result".to_string()) },
            Token { kind: TokenKind::Assign },
            Token { kind: TokenKind::Ident("add".to_string()) },
            Token { kind: TokenKind::Lparen },
            Token { kind: TokenKind::Ident("five".to_string()) },
            Token { kind: TokenKind::Comma },
            Token { kind: TokenKind::Ident("ten".to_string()) },
            Token { kind: TokenKind::Rparen },
            Token { kind: TokenKind::Semicolon },
            Token { kind: TokenKind::Bang },
            Token { kind: TokenKind::Minus },
            Token { kind: TokenKind::Slash },
            Token { kind: TokenKind::Asterisk },
            Token { kind: TokenKind::Int(5) },
            Token { kind: TokenKind::Semicolon },
            Token { kind: TokenKind::Int(5) },
            Token { kind: TokenKind::LT},
            Token { kind: TokenKind::Int(10) },
            Token { kind: TokenKind::GT},
            Token { kind: TokenKind::Int(5) },
            Token { kind: TokenKind::Semicolon },
            Token { kind: TokenKind::If },
            Token { kind: TokenKind::Lparen },
            Token { kind: TokenKind::Int(5) },
            Token { kind: TokenKind::LT},
            Token { kind: TokenKind::Int(10) },
            Token { kind: TokenKind::Rparen },
            Token { kind: TokenKind::Lbrace },
            Token { kind: TokenKind::Return },
            Token { kind: TokenKind::True },
            Token { kind: TokenKind::Semicolon },
            Token { kind: TokenKind::Rbrace },
            Token { kind: TokenKind::Else },
            Token { kind: TokenKind::Lbrace },
            Token { kind: TokenKind::Return },
            Token { kind: TokenKind::False },
            Token { kind: TokenKind::Semicolon },
            Token { kind: TokenKind::Rbrace },
            Token { kind: TokenKind::Int(10) },
            Token { kind: TokenKind::EQ },
            Token { kind: TokenKind::Int(10) },
            Token { kind: TokenKind::Semicolon },
            Token { kind: TokenKind::Int(10) },
            Token { kind: TokenKind::NEQ },
            Token { kind: TokenKind::Int(9) },
            Token { kind: TokenKind::Semicolon },
            // end
            Token { kind: TokenKind::Eof },
            Token { kind: TokenKind::Eof },
        ];

        for (i, token) in tests.into_iter().enumerate() {
            assert_eq!((i, lexer.next_token()), (i, token));
        }
    }
}
