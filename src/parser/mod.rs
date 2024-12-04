use crate::ast::Program;
use crate::lexer::{Lexer, Token};

#[derive(Debug)]
enum ParserError {}

struct Parser<'a> {
    curr_token: Token,
    next_token: Token,
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    fn new(mut lexer: Lexer<'a>) -> Self {
        Self {
            curr_token: lexer.next_token(),
            next_token: lexer.next_token(),
            lexer,
        }
    }

    fn next_token(&mut self) {
        std::mem::swap(&mut self.curr_token, &mut self.next_token);
        self.next_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Result<Program, ParserError> {
        Ok(Program { statements: vec![] })
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Statement;

    use super::*;

    #[test]
    fn test_let_statements() {
        #[rustfmt::skip]
        let input = concat!(
            "let x = 5;\n",
            "let y = 10;\n",
            "let foobar = 838383;\n"
        );
        let lexer = Lexer::new(None, input.chars().peekable());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert!(program.is_ok());
        let program = program.unwrap();

        assert_eq!(program.statements.len(), 3);

        for (i, ident) in ["x", "y", "foobar"].iter().enumerate() {
            let stmt = &program.statements[i];
            match stmt {
                Statement::Let {
                    token: _,
                    name: _,
                    value: _,
                } => (),
                _ => panic!("Not a let statement"),
            }
        }
    }
}
