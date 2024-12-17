mod tests;

use crate::ast::{Expression, ExpressionKind, Operator, Precedence, Program, Statement};
use crate::lexer::{Lexer, Token, TokenKind};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug)]
pub enum ParserError {
    Expect(String),
    Expression(String),
    // TODO: remove or use
    // InvalidLetStatement(String),
    // StatementError(String),
    ParseInt(String),
    ParsePrefix(String),
    ParseInfix(String),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Expect(err) => write!(f, "{}", err),
            Self::Expression(err) => write!(f, "{}", err),
            // TODO: remove or use
            // Self::InvalidLetStatement(err) => write!(f, "{}", err),
            // Self::StatementError(err) => write!(f, "{}", err),
            Self::ParseInt(err) => write!(f, "{}", err),
            Self::ParsePrefix(err) => write!(f, "{}", err),
            Self::ParseInfix(err) => write!(f, "{}", err),
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
            .prefix_parse_fns
            .insert(TokenKind::Lparen, Parser::parse_fn_grouped_expression);
        parser
            .prefix_parse_fns
            .insert(TokenKind::If, Parser::parse_fn_if_expression);

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
            Err(ParserError::Expect(format!(
                "expected next token to be {token:?}, but got: {:?}",
                self.next_token
            )))
        }
    }

    // TODO: remove
    // fn expect_error(&mut self, token: TokenKind) {
    //     self.errors.push(ParserError::ExpectError(format!(
    //         "expected next token to be {token:?}, but got: {:?}",
    //         self.next_token
    //     )));
    // }

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

        if self.next_token.kind == TokenKind::Semicolon {
            self.update_tokens();
        }

        Ok(Statement::Let { token, name, value })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.curr_token.clone();

        self.update_tokens();
        let value = self.parse_expression(Precedence::Lowest)?;

        if self.next_token.kind == TokenKind::Semicolon {
            self.update_tokens();
        }

        Ok(Statement::Return { token, value })
    }

    fn parse_fn_identifier(parser: &mut Parser) -> Result<Expression, ParserError> {
        Ok(Expression {
            token: parser.curr_token.clone(),
            kind: ExpressionKind::Identifier {
                value: parser.curr_token.literal.clone(),
            },
        })
    }

    fn parse_fn_integer_literal(parser: &mut Parser) -> Result<Expression, ParserError> {
        let value = match parser.curr_token.literal.parse::<usize>() {
            Ok(value) => value,
            Err(err) => return Err(ParserError::ParseInt(err.to_string())),
        };
        Ok(Expression {
            token: parser.curr_token.clone(),
            kind: ExpressionKind::IntegerLiteral { value },
        })
    }

    fn parse_fn_boolean_literal(parser: &mut Parser) -> Result<Expression, ParserError> {
        let value = parser.curr_token.kind == TokenKind::True;
        Ok(Expression {
            token: parser.curr_token.clone(),
            kind: ExpressionKind::Boolean { value },
        })
    }

    fn parse_fn_if_expression(parser: &mut Parser) -> Result<Expression, ParserError> {
        let token = parser.curr_token.clone();

        parser.expect_token(TokenKind::Lparen)?;
        parser.update_tokens();

        let condition = Box::new(parser.parse_expression(Precedence::Lowest)?);

        parser.expect_token(TokenKind::Rparen)?;
        parser.expect_token(TokenKind::Lbrace)?;

        let consequence = Box::new(parser.parse_block_statement()?);

        let mut alternative = None;
        if parser.next_token.kind == TokenKind::Else {
            parser.update_tokens();
            parser.expect_token(TokenKind::Lbrace)?;
            alternative = Some(Box::new(parser.parse_block_statement()?));
        }

        Ok(Expression {
            token,
            kind: ExpressionKind::If {
                condition,
                consequence,
                alternative,
            },
        })
    }

    fn parse_fn_prefix_expression(parser: &mut Parser) -> Result<Expression, ParserError> {
        let token = parser.curr_token.clone();
        let operator = Operator::try_from(token.literal.as_str())
            .map_err(|err| ParserError::ParsePrefix(format!("{}: {}", err, token)))?;

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
        parser.update_tokens();
        let token = parser.curr_token.clone();
        let operator = Operator::try_from(token.literal.as_str())
            .map_err(|err| ParserError::ParseInfix(format!("{err}: {token}")))?;

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

    fn parse_fn_grouped_expression(parser: &mut Parser) -> Result<Expression, ParserError> {
        parser.update_tokens();
        let expression = parser.parse_expression(Precedence::Lowest)?;
        parser.expect_token(TokenKind::Rparen)?;
        Ok(expression)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError> {
        let mut expression = match self.prefix_parse_fns.get(&self.curr_token.kind) {
            Some(prefix) => prefix(self)?,
            None => {
                return Err(ParserError::Expression(format!(
                    "no prefix parse function found for {}",
                    self.curr_token
                )));
            }
        };

        while self.next_token.kind != TokenKind::Semicolon
            && precedence < Precedence::from(&self.next_token.kind)
        {
            expression = match self.infix_parse_fns.get(&self.next_token.kind) {
                Some(infix) => infix(self, Box::new(expression))?,
                None => {
                    return Ok(expression);
                }
            };
        }
        Ok(expression)
    }

    fn parse_block_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.curr_token.clone();
        self.update_tokens();

        let mut statements = vec![];
        while self.curr_token.kind != TokenKind::Rbrace
            && self.curr_token.kind != TokenKind::EndOfFile
        {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => self.errors.push(err),
            }
            self.update_tokens();
        }

        Ok(Statement::Block { token, statements })
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
