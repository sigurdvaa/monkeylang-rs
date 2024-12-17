mod tests;

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
            .prefix_parse_fns
            .insert(TokenKind::True, Parser::parse_fn_boolean_literal);
        parser
            .prefix_parse_fns
            .insert(TokenKind::False, Parser::parse_fn_boolean_literal);

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

    fn parse_fn_boolean_literal(parser: &mut Parser) -> Result<Expression, ParserError> {
        let value = parser.curr_token.kind == TokenKind::True;
        let expression = Ok(Expression {
            token: parser.curr_token.clone(),
            kind: ExpressionKind::Boolean { value },
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
