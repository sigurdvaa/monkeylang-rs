use crate::ast::{Expression, ExpressionKind, Operator, Precedence, Program, Statement};
use crate::lexer::{Lexer, Token, TokenKind};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug)]
pub enum ParserError {
    ExpectError(String),
    ExpressionError(String),
    InvalidLetStatement(String),
    StatementError(String),
    ParseIntError(String),
    ParsePrefixError(String),
    ParseInfixError(String),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ExpectError(err) => write!(f, "{}", err),
            Self::ExpressionError(err) => write!(f, "{}", err),
            Self::InvalidLetStatement(err) => write!(f, "{}", err),
            Self::StatementError(err) => write!(f, "{}", err),
            Self::ParseIntError(err) => write!(f, "{}", err),
            Self::ParsePrefixError(err) => write!(f, "{}", err),
            Self::ParseInfixError(err) => write!(f, "{}", err),
        }
    }
}

type PrefixParseFn = fn(&mut Parser) -> Result<Expression, ParserError>;
type InfixParseFn = fn(&mut Parser, Box<Expression>) -> Result<Expression, ParserError>;

pub struct Parser<'a> {
    curr_token: Token,
    next_token: Token,
    lexer: Lexer<'a>,
    pub errors: Vec<ParserError>,
    prefix_parse_fns: HashMap<TokenKind, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenKind, InfixParseFn>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let mut parser = Self {
            curr_token: lexer.next_token(),
            next_token: lexer.next_token(),
            lexer,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser
            .prefix_parse_fns
            .insert(TokenKind::Ident, Parser::parse_fn_identifier);
        parser
            .prefix_parse_fns
            .insert(TokenKind::Int, Parser::parse_fn_integer_literal);
        parser
            .prefix_parse_fns
            .insert(TokenKind::Bang, Parser::parse_fn_prefix_expression);
        parser
            .prefix_parse_fns
            .insert(TokenKind::Minus, Parser::parse_fn_prefix_expression);

        parser
            .infix_parse_fns
            .insert(TokenKind::Plus, Parser::parse_fn_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenKind::Minus, Parser::parse_fn_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenKind::Slash, Parser::parse_fn_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenKind::Asterisk, Parser::parse_fn_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenKind::Eq, Parser::parse_fn_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenKind::NotEq, Parser::parse_fn_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenKind::Lt, Parser::parse_fn_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenKind::Gt, Parser::parse_fn_infix_expression);

        parser
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
        let name = Expression {
            token: self.curr_token.clone(),
            kind: ExpressionKind::Identifier {
                value: self.curr_token.literal.clone(),
            },
        };

        self.expect_token(TokenKind::Assign)?;
        self.update_tokens();

        let value = self.parse_expression(Precedence::Lowest)?;

        Ok(Statement::Let { token, name, value })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.curr_token.clone();

        self.update_tokens();
        let value = self.parse_expression(Precedence::Lowest)?;

        Ok(Statement::Return { token, value })
    }

    fn parse_fn_identifier(parser: &mut Parser) -> Result<Expression, ParserError> {
        let expression = Ok(Expression {
            token: parser.curr_token.clone(),
            kind: ExpressionKind::Identifier {
                value: parser.curr_token.literal.clone(),
            },
        });
        parser.update_tokens();
        expression
    }

    fn parse_fn_integer_literal(parser: &mut Parser) -> Result<Expression, ParserError> {
        let value = match parser.curr_token.literal.parse::<usize>() {
            Ok(value) => value,
            Err(err) => return Err(ParserError::ParseIntError(err.to_string())),
        };
        let expression = Ok(Expression {
            token: parser.curr_token.clone(),
            kind: ExpressionKind::IntegerLiteral { value },
        });
        parser.update_tokens();
        expression
    }

    fn parse_fn_prefix_expression(parser: &mut Parser) -> Result<Expression, ParserError> {
        let token = parser.curr_token.clone();
        let operator = Operator::try_from(token.literal.as_str())
            .map_err(|err| ParserError::ParsePrefixError(format!("{}: {}", err, token)))?;

        parser.update_tokens();
        let right = Box::new(parser.parse_expression(Precedence::Prefix)?);

        Ok(Expression {
            token,
            kind: ExpressionKind::Prefix { operator, right },
        })
    }

    fn parse_fn_infix_expression(
        parser: &mut Parser,
        left: Box<Expression>,
    ) -> Result<Expression, ParserError> {
        let token = parser.curr_token.clone();
        let operator = Operator::try_from(token.literal.as_str())
            .map_err(|err| ParserError::ParsePrefixError(format!("{}: {}", err, token)))?;

        parser.update_tokens();
        let right = Box::new(parser.parse_expression(Precedence::from(&token.kind))?);

        Ok(Expression {
            token,
            kind: ExpressionKind::Infix {
                left,
                operator,
                right,
            },
        })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError> {
        let mut expression = match self.prefix_parse_fns.get(&self.curr_token.kind) {
            Some(prefix) => prefix(self)?,
            None => {
                return Err(ParserError::ExpressionError(format!(
                    "no prefix parse function found for {}",
                    self.curr_token
                )))
            }
        };

        while self.curr_token.kind != TokenKind::Semicolon
            && precedence < Precedence::from(&self.curr_token.kind)
        {
            expression = match self.infix_parse_fns.get(&self.curr_token.kind) {
                Some(infix) => infix(self, Box::new(expression))?,
                None => {
                    return Ok(expression);
                }
            };
        }

        Ok(expression)
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.curr_token.clone();
        let value = self.parse_expression(Precedence::Lowest)?;

        if self.next_token.kind == TokenKind::Semicolon {
            self.update_tokens();
        }

        Ok(Statement::Expression { token, value })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.curr_token.kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    // TODO: consider not returning Result, as errors are stored in parser
    pub fn parse_program(&mut self) -> Result<Program, ParserError> {
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
}

// TODO: consider moving tests to separate file
#[cfg(test)]
mod tests {
    use super::*;

    fn check_for_errors(parser: &Parser) {
        if !parser.errors.is_empty() {
            println!("parser has {} errors", parser.errors.len());
            for err in &parser.errors {
                println!("{err}");
            }
            panic!("Parsing errors found");
        }
    }

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

        check_for_errors(&parser);
        assert!(program.is_ok());
        let program = program.unwrap();
        assert_eq!(program.statements.len(), 3);

        for (i, (ident, literal)) in [("x", 5), ("y", 10), ("foobar", 838383)].iter().enumerate() {
            let stmt = &program.statements[i];
            match stmt {
                Statement::Let { token, name, value } => {
                    assert_eq!(token.kind, TokenKind::Let);
                    assert_eq!(token.literal, "let".to_string());
                    assert_eq!(name.token.kind, TokenKind::Ident);
                    assert_eq!(name.token.literal, ident.to_string());
                    assert_eq!(
                        value.kind,
                        ExpressionKind::IntegerLiteral { value: *literal }
                    );
                }
                _ => panic!("Not a valid let statement"),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        #[rustfmt::skip]
        let input = concat!(
            "return 5;\n",
            "return 10;\n",
            "return 993322;\n"
        );
        let lexer = Lexer::new(None, input.chars().peekable());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_for_errors(&parser);
        assert!(program.is_ok());
        let program = program.unwrap();
        assert_eq!(program.statements.len(), 3);

        for (i, literal) in [5, 10, 993322].iter().enumerate() {
            match &program.statements[i] {
                Statement::Return { token, value } => {
                    assert_eq!(token.kind, TokenKind::Return);
                    assert_eq!(token.literal, "return".to_string());
                    assert_eq!(
                        value.kind,
                        ExpressionKind::IntegerLiteral { value: *literal }
                    );
                }
                _ => panic!(
                    "not a valid return statement, got {}",
                    &program.statements[i]
                ),
            }
        }
    }

    #[test]
    fn test_program_to_string() {
        let program = Program {
            statements: vec![Statement::Let {
                token: Token {
                    file: None,
                    col: 0,
                    line: 0,
                    kind: TokenKind::Let,
                    literal: "let".into(),
                },
                name: Expression {
                    token: Token {
                        file: None,
                        col: 0,
                        line: 0,
                        kind: TokenKind::Ident,
                        literal: "myVar".into(),
                    },
                    kind: ExpressionKind::Identifier {
                        value: "myVar".into(),
                    },
                },
                value: Expression {
                    token: Token {
                        file: None,
                        col: 0,
                        line: 0,
                        kind: TokenKind::Ident,
                        literal: "anotherVar".into(),
                    },
                    kind: ExpressionKind::Identifier {
                        value: "anotherVar".into(),
                    },
                },
            }],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let lexer = Lexer::new(None, input.chars().peekable());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_for_errors(&parser);
        assert!(program.is_ok());
        let program = program.unwrap();
        assert_eq!(program.statements.len(), 1);

        for stmt in program.statements {
            match stmt {
                Statement::Expression { token, value } => {
                    assert_eq!(token.kind, TokenKind::Ident);
                    assert_eq!(token.literal, "foobar".to_string());
                    assert_eq!(
                        value.kind,
                        ExpressionKind::Identifier {
                            value: "foobar".to_string()
                        }
                    );
                    assert_eq!(token.literal, "foobar".to_string());
                }
                _ => panic!("not a valid expression statement, got {}", stmt),
            }
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let lexer = Lexer::new(None, input.chars().peekable());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_for_errors(&parser);
        assert!(program.is_ok());
        let program = program.unwrap();
        assert_eq!(program.statements.len(), 1);

        for stmt in program.statements {
            match stmt {
                Statement::Expression { token, value } => {
                    assert_eq!(token.kind, TokenKind::Int);
                    assert_eq!(token.literal, "5".to_string());
                    assert_eq!(value.kind, ExpressionKind::IntegerLiteral { value: 5 });
                    assert_eq!(value.token.literal, "5".to_string());
                }
                _ => panic!("not a valid expression statement, got {}", stmt),
            }
        }
    }

    #[test]
    fn test_parsing_prefix_expression() {
        let prefix_test = [("!5;", Operator::Bang, 5), ("-15;", Operator::Minus, 15)];
        for (input_test, operator_test, value_test) in prefix_test {
            let lexer = Lexer::new(None, input_test.chars().peekable());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_for_errors(&parser);
            assert!(program.is_ok());
            let program = program.unwrap();
            assert_eq!(program.statements.len(), 1);

            for stmt in program.statements {
                let expr = match &stmt {
                    Statement::Expression { token: _, value } => value,
                    _ => panic!("not an expression statement, got {}", stmt),
                };

                let (operator, right) = match expr {
                    Expression {
                        token: _,
                        kind: ExpressionKind::Prefix { operator, right },
                    } => (operator, right),
                    _ => panic!("not a prefix expression, got {}", stmt),
                };

                assert_eq!(operator_test, *operator);

                let rhs_value = match **right {
                    Expression {
                        token: _,
                        kind: ExpressionKind::IntegerLiteral { value },
                    } => value,
                    _ => panic!("rhs is not integer litersl, got {}", right),
                };

                assert_eq!(value_test, rhs_value);
            }
        }
    }
    #[test]
    fn test_parsing_infix_expression() {
        let infix_test = [
            ("1 + 2;", 1, Operator::Plus, 2),
            ("1 - 2;", 1, Operator::Minus, 2),
            ("1 * 2;", 1, Operator::Asterisk, 2),
            ("1 / 2;", 1, Operator::Slash, 2),
            ("1 > 2;", 1, Operator::Gt, 2),
            ("1 < 2;", 1, Operator::Lt, 2),
            ("1 == 2;", 1, Operator::Eq, 2),
            ("1 != 2;", 1, Operator::NotEq, 2),
        ];
        for (input_test, left_test, operator_test, right_test) in infix_test {
            let lexer = Lexer::new(None, input_test.chars().peekable());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_for_errors(&parser);
            assert!(program.is_ok());
            let program = program.unwrap();
            assert_eq!(program.statements.len(), 1);

            for stmt in program.statements {
                let expr = match &stmt {
                    Statement::Expression { token: _, value } => value,
                    _ => panic!("not an expression statement, got {}", stmt),
                };

                let (left, operator, right) = match expr {
                    Expression {
                        token: _,
                        kind:
                            ExpressionKind::Infix {
                                left,
                                operator,
                                right,
                            },
                    } => (left, operator, right),
                    _ => panic!("not a infix expression, got {}", stmt),
                };

                let left_value = match **left {
                    Expression {
                        token: _,
                        kind: ExpressionKind::IntegerLiteral { value },
                    } => value,
                    _ => panic!("left is not integer literal, got {}", left),
                };
                assert_eq!(left_test, left_value);

                assert_eq!(operator_test, *operator);

                let right_value = match **right {
                    Expression {
                        token: _,
                        kind: ExpressionKind::IntegerLiteral { value },
                    } => value,
                    _ => panic!("right is not integer literal, got {}", right),
                };
                assert_eq!(right_test, right_value);
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let precedence_test = [
            ("-a * b", "((-a) * b)", 1),
            ("!-a", "(!(-a))", 1),
            ("a + b + c", "((a + b) + c)", 1),
            ("a + b - c", "((a + b) - c)", 1),
            ("a * b * c", "((a * b) * c)", 1),
            ("a * b / c", "((a * b) / c)", 1),
            ("a + b / c", "(a + (b / c))", 1),
            (
                "a + b * c + d / e - f",
                "(((a + (b * c)) + (d / e)) - f)",
                1,
            ),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)", 2),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))", 1),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))", 1),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
                1,
            ),
        ];

        for (input_test, value_test, statements_test) in precedence_test {
            let lexer = Lexer::new(None, input_test.chars().peekable());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_for_errors(&parser);
            let program = program.expect("no program parsed");
            assert_eq!(program.statements.len(), statements_test);
            assert_eq!(program.to_string(), value_test.to_string());
        }
    }
}
