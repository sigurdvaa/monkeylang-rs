use crate::token::{Token, TokenKind};
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    file: Option<String>,
    curr_line: usize,
    next_line: usize,
    curr_col: usize,
    next_col: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(file: Option<String>, input: Peekable<Chars<'a>>) -> Self {
        Self {
            input,
            file,
            curr_line: 1,
            next_line: 1,
            curr_col: 1,
            next_col: 1,
        }
    }

    fn is_letter(c: &char) -> bool {
        c.is_ascii_alphabetic() || *c == '_'
    }

    fn new_token(&mut self, kind: TokenKind, literal: String) -> Token {
        let token = Token {
            kind,
            literal,
            file: self.file.clone(),
            line: self.curr_line,
            col: self.curr_col,
        };
        self.curr_line = self.next_line;
        self.curr_col = self.next_col;
        token
    }

    fn lookup_ident(&mut self, literal: String) -> Token {
        match literal.as_str() {
            "let" => self.new_token(TokenKind::Let, literal),
            "fn" => self.new_token(TokenKind::Function, literal),
            "if" => self.new_token(TokenKind::If, literal),
            "else" => self.new_token(TokenKind::Else, literal),
            "return" => self.new_token(TokenKind::Return, literal),
            "true" => self.new_token(TokenKind::True, literal),
            "false" => self.new_token(TokenKind::False, literal),
            _ => self.new_token(TokenKind::Ident, literal),
        }
    }

    fn read_ident(&mut self, c: char) -> Token {
        let mut literal = c.to_string();
        while let Some(c) = self.input.peek() {
            if c.is_ascii_alphabetic() || *c == '_' {
                literal.push(self.input.next().expect("invalid char"));
                self.next_col += 1;
            } else {
                break;
            }
        }
        self.lookup_ident(literal)
    }

    fn read_number(&mut self, c: char) -> Token {
        let mut literal = c.to_string();
        while let Some(c) = self.input.peek() {
            if c.is_ascii_digit() {
                literal.push(self.input.next().expect("invalid char"));
                self.next_col += 1;
            } else {
                break;
            }
        }
        self.new_token(TokenKind::Int, literal)
    }

    fn read_string(&mut self) -> Token {
        let mut buffer = vec![];
        while let Some(c) = self.input.next() {
            match c {
                '\\' => {
                    match self.input.next() {
                        Some('n') => buffer.push('\n'),
                        Some('"') => buffer.push('"'),
                        Some('\\') => buffer.push('\\'),
                        Some(c) => {
                            return self.new_token(
                                TokenKind::Illegal,
                                format!("unknown character escape: {c}"),
                            )
                        }
                        None => {
                            return self.new_token(
                                TokenKind::Illegal,
                                format!(
                                    "unterminated character literal, line {}, col {}",
                                    self.next_line, self.next_col
                                ),
                            )
                        }
                    }
                    self.next_col += 1;
                }
                '"' => {
                    break;
                }
                '\n' => {
                    self.next_line += 1;
                    buffer.push(c);
                }
                _ => buffer.push(c),
            }
            self.next_col += 1;
        }
        self.new_token(TokenKind::String, buffer.into_iter().collect())
    }

    pub fn next_token(&mut self) -> Token {
        self.next_col += 1;
        match self.input.next() {
            None => {
                self.next_col -= 1;
                self.new_token(TokenKind::EndOfFile, "".into())
            }
            Some(c) => match c {
                '=' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        self.next_col += 1;
                        self.new_token(TokenKind::Eq, "==".into())
                    } else {
                        self.new_token(TokenKind::Assign, c.into())
                    }
                }
                '+' => self.new_token(TokenKind::Plus, c.into()),
                '-' => self.new_token(TokenKind::Minus, c.into()),
                '(' => self.new_token(TokenKind::Lparen, c.into()),
                ')' => self.new_token(TokenKind::Rparen, c.into()),
                '{' => self.new_token(TokenKind::Lbrace, c.into()),
                '}' => self.new_token(TokenKind::Rbrace, c.into()),
                '[' => self.new_token(TokenKind::Lbracket, c.into()),
                ']' => self.new_token(TokenKind::Rbracket, c.into()),
                ':' => self.new_token(TokenKind::Colon, c.into()),
                ';' => self.new_token(TokenKind::Semicolon, c.into()),
                ',' => self.new_token(TokenKind::Comma, c.into()),
                '!' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        self.next_col += 1;
                        self.new_token(TokenKind::NotEq, "!=".into())
                    } else {
                        self.new_token(TokenKind::Bang, c.into())
                    }
                }
                '*' => self.new_token(TokenKind::Asterisk, c.into()),
                '/' => self.new_token(TokenKind::Slash, c.into()),
                '<' => self.new_token(TokenKind::Lt, c.into()),
                '>' => self.new_token(TokenKind::Gt, c.into()),
                '\n' => {
                    self.next_line += 1;
                    self.curr_line = self.next_line;
                    self.curr_col = 1;
                    self.next_col = 1;
                    self.next_token()
                }
                '"' => self.read_string(),
                c if c.is_whitespace() => {
                    self.curr_col = self.next_col;
                    self.next_token()
                }
                c if Self::is_letter(&c) => self.read_ident(c),
                c if c.is_ascii_digit() => self.read_number(c),
                _ => self.new_token(TokenKind::Illegal, c.into()),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_token(line: usize, col: usize, kind: TokenKind, literal: &str) -> Token {
        Token {
            file: None,
            line,
            col,
            kind,
            literal: literal.into(),
        }
    }

    #[test]
    fn test_next_tokens() {
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
            "\"foobar\";\n",
            "\"foo bar\";\n",
            "\"foo\nbar\";\n",
            "\"foo\\\"bar\";\n",
            "\"foo\\nbar\";\n",
            "\"foo\\\\bar\";\n",
            "[1, 2];\n",
            "{\"foo\": \"bar\"}\n",
        );

        let mut lexer = Lexer::new(None, input.chars().peekable());
        let tests = [
            create_token(1, 1, TokenKind::Let, "let"),
            create_token(1, 5, TokenKind::Ident, "five"),
            create_token(1, 10, TokenKind::Assign, "="),
            create_token(1, 12, TokenKind::Int, "5"),
            create_token(1, 13, TokenKind::Semicolon, ";"),
            create_token(2, 1, TokenKind::Let, "let"),
            create_token(2, 5, TokenKind::Ident, "ten"),
            create_token(2, 9, TokenKind::Assign, "="),
            create_token(2, 11, TokenKind::Int, "10"),
            create_token(2, 13, TokenKind::Semicolon, ";"),
            create_token(4, 1, TokenKind::Let, "let"),
            create_token(4, 5, TokenKind::Ident, "add"),
            create_token(4, 9, TokenKind::Assign, "="),
            create_token(4, 11, TokenKind::Function, "fn"),
            create_token(4, 13, TokenKind::Lparen, "("),
            create_token(4, 14, TokenKind::Ident, "x"),
            create_token(4, 15, TokenKind::Comma, ","),
            create_token(4, 17, TokenKind::Ident, "y"),
            create_token(4, 18, TokenKind::Rparen, ")"),
            create_token(4, 20, TokenKind::Lbrace, "{"),
            create_token(5, 5, TokenKind::Ident, "x"),
            create_token(5, 7, TokenKind::Plus, "+"),
            create_token(5, 9, TokenKind::Ident, "y"),
            create_token(5, 10, TokenKind::Semicolon, ";"),
            create_token(6, 1, TokenKind::Rbrace, "}"),
            create_token(6, 2, TokenKind::Semicolon, ";"),
            create_token(8, 1, TokenKind::Let, "let"),
            create_token(8, 5, TokenKind::Ident, "result"),
            create_token(8, 12, TokenKind::Assign, "="),
            create_token(8, 14, TokenKind::Ident, "add"),
            create_token(8, 17, TokenKind::Lparen, "("),
            create_token(8, 18, TokenKind::Ident, "five"),
            create_token(8, 22, TokenKind::Comma, ","),
            create_token(8, 24, TokenKind::Ident, "ten"),
            create_token(8, 27, TokenKind::Rparen, ")"),
            create_token(8, 28, TokenKind::Semicolon, ";"),
            create_token(9, 1, TokenKind::Bang, "!"),
            create_token(9, 2, TokenKind::Minus, "-"),
            create_token(9, 3, TokenKind::Slash, "/"),
            create_token(9, 4, TokenKind::Asterisk, "*"),
            create_token(9, 5, TokenKind::Int, "5"),
            create_token(9, 6, TokenKind::Semicolon, ";"),
            create_token(10, 1, TokenKind::Int, "5"),
            create_token(10, 3, TokenKind::Lt, "<"),
            create_token(10, 5, TokenKind::Int, "10"),
            create_token(10, 8, TokenKind::Gt, ">"),
            create_token(10, 10, TokenKind::Int, "5"),
            create_token(10, 11, TokenKind::Semicolon, ";"),
            create_token(12, 1, TokenKind::If, "if"),
            create_token(12, 4, TokenKind::Lparen, "("),
            create_token(12, 5, TokenKind::Int, "5"),
            create_token(12, 7, TokenKind::Lt, "<"),
            create_token(12, 9, TokenKind::Int, "10"),
            create_token(12, 11, TokenKind::Rparen, ")"),
            create_token(12, 13, TokenKind::Lbrace, "{"),
            create_token(13, 5, TokenKind::Return, "return"),
            create_token(13, 12, TokenKind::True, "true"),
            create_token(13, 16, TokenKind::Semicolon, ";"),
            create_token(14, 1, TokenKind::Rbrace, "}"),
            create_token(14, 3, TokenKind::Else, "else"),
            create_token(14, 8, TokenKind::Lbrace, "{"),
            create_token(15, 5, TokenKind::Return, "return"),
            create_token(15, 12, TokenKind::False, "false"),
            create_token(15, 17, TokenKind::Semicolon, ";"),
            create_token(16, 1, TokenKind::Rbrace, "}"),
            create_token(17, 1, TokenKind::Int, "10"),
            create_token(17, 4, TokenKind::Eq, "=="),
            create_token(17, 7, TokenKind::Int, "10"),
            create_token(17, 9, TokenKind::Semicolon, ";"),
            create_token(18, 1, TokenKind::Int, "10"),
            create_token(18, 4, TokenKind::NotEq, "!="),
            create_token(18, 7, TokenKind::Int, "9"),
            create_token(18, 8, TokenKind::Semicolon, ";"),
            create_token(19, 1, TokenKind::String, "foobar"),
            create_token(19, 8, TokenKind::Semicolon, ";"),
            create_token(20, 1, TokenKind::String, "foo bar"),
            create_token(20, 9, TokenKind::Semicolon, ";"),
            create_token(21, 1, TokenKind::String, "foo\nbar"),
            create_token(22, 9, TokenKind::Semicolon, ";"),
            create_token(23, 1, TokenKind::String, "foo\"bar"),
            create_token(23, 10, TokenKind::Semicolon, ";"),
            create_token(24, 1, TokenKind::String, "foo\nbar"),
            create_token(24, 10, TokenKind::Semicolon, ";"),
            create_token(25, 1, TokenKind::String, "foo\\bar"),
            create_token(25, 10, TokenKind::Semicolon, ";"),
            create_token(26, 1, TokenKind::Lbracket, "["),
            create_token(26, 2, TokenKind::Int, "1"),
            create_token(26, 3, TokenKind::Comma, ","),
            create_token(26, 5, TokenKind::Int, "2"),
            create_token(26, 6, TokenKind::Rbracket, "]"),
            create_token(26, 7, TokenKind::Semicolon, ";"),
            create_token(27, 1, TokenKind::Lbrace, "{"),
            create_token(27, 2, TokenKind::String, "foo"),
            create_token(27, 6, TokenKind::Colon, ":"),
            create_token(27, 8, TokenKind::String, "bar"),
            create_token(27, 12, TokenKind::Rbrace, "}"),
            create_token(28, 1, TokenKind::EndOfFile, ""),
            create_token(28, 1, TokenKind::EndOfFile, ""),
        ];

        for token in tests {
            assert_eq!(lexer.next_token(), token);
        }
    }
}
