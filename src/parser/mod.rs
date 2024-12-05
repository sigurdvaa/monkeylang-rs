use crate::ast::{Identifier, Program, Statement};
use crate::lexer::{Lexer, Token, TokenKind};
use std::fmt;

#[derive(Debug)]
enum ParserError {
    StatementError(String),
    ExpectError(String),
    InvalidLetStatement(String),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::StatementError(err) => write!(f, "{}", err),
            Self::ExpectError(err) => write!(f, "{}", err),
            Self::InvalidLetStatement(err) => write!(f, "{}", err),
        }
    }
}

struct Parser<'a> {
    curr_token: Token,
    next_token: Token,
    lexer: Lexer<'a>,
    errors: Vec<ParserError>,
}

impl<'a> Parser<'a> {
    fn new(mut lexer: Lexer<'a>) -> Self {
        Self {
            curr_token: lexer.next_token(),
            next_token: lexer.next_token(),
            lexer,
            errors: vec![],
        }
    }

    fn update_tokens(&mut self) {
        std::mem::swap(&mut self.curr_token, &mut self.next_token);
        self.next_token = self.lexer.next_token();
    }

    fn expect_token(&mut self, token: TokenKind) -> Result<(), ParserError> {
        if self.next_token.kind == token {
            self.update_tokens();
            Ok(())
        } else {
            Err(ParserError::ExpectError(format!(
                "Expected next token to be {token:?}, but got: {:?}",
                self.next_token
            )))
        }
    }

    fn expect_error(&mut self, token: TokenKind) {
        self.errors.push(ParserError::ExpectError(format!(
            "Expected next token to be {token:?}, but got: {:?}",
            self.next_token
        )));
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.curr_token.clone();

        self.expect_token(TokenKind::Ident)?;
        let name = Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };

        self.expect_token(TokenKind::Assign)?;

        // TODO: skipping the expression for now
        while self.curr_token.kind != TokenKind::Semicolon {
            self.update_tokens();
        }

        Ok(Statement::Let { token, name })
        // Ok(Statement::Let { token, name, value })
    }

    fn parse_program(&mut self) -> Result<Program, ParserError> {
        let mut prog = Program::new();

        while self.curr_token.kind != TokenKind::EndOfFile {
            match self.parse_statement() {
                Ok(stmt) => prog.statements.push(stmt),
                Err(err) => self.errors.push(err),
            };
            self.update_tokens();
        }

        Ok(prog)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.curr_token.kind {
            TokenKind::Let => self.parse_let_statement(),
            _ => Err(ParserError::StatementError(format!(
                "Unexpected: {:?}",
                self.curr_token
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statements() {
        #[rustfmt::skip]
        let input = concat!(
            "let x = 5;\n",
            "let y = 10;\n",
            "let 1foobar = 838383;\n"
        );
        let lexer = Lexer::new(None, input.chars().peekable());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if !parser.errors.is_empty() {
            println!("parser has {} errors", parser.errors.len());
            for err in parser.errors {
                println!("{err}");
            }
            panic!();
        }

        assert!(program.is_ok());
        let program = program.unwrap();

        assert_eq!(program.statements.len(), 3);

        for (i, _ident) in ["x", "y", "foobar"].iter().enumerate() {
            let stmt = &program.statements[i];
            match stmt {
                Statement::Let {
                    token: _,
                    name: _,
                    // value: _,
                } => (),
                _ => panic!("Not a let statement"),
            }
        }
    }
}
