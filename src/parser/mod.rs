#[cfg(test)]
pub mod tests;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fmt;
use std::num::ParseIntError;

#[derive(PartialEq, PartialOrd, Debug)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

impl From<&TokenKind> for Precedence {
    fn from(value: &TokenKind) -> Self {
        match value {
            TokenKind::Equal => Self::Equals,
            TokenKind::NotEqual => Self::Equals,
            TokenKind::LessThan => Self::LessGreater,
            TokenKind::GreaterThan => Self::LessGreater,
            TokenKind::Plus => Self::Sum,
            TokenKind::Minus => Self::Sum,
            TokenKind::Slash => Self::Product,
            TokenKind::Asterisk => Self::Product,
            TokenKind::Lparen => Self::Call,
            TokenKind::Lbracket => Self::Index,
            _ => Self::Lowest,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParserContext {
    Function,
    Loop,
}

#[derive(Debug)]
pub enum ParserError {
    ExpectToken(TokenKind, Token),
    ParseInt(Token, ParseIntError),
    UnknownOperator(Token),
    InvalidPrefix(Token),
    QuoteWrongNumArgs(Token, usize),
    UnquoteWrongNumArgs(Token, usize),
    InvalidContext(Token, ParserContext, Option<ParserContext>),
    BreakOutsideLoop(Token),
    ReturnOutsideFunction(Token),
}

impl std::error::Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ExpectToken(want, got) => {
                write!(f, "{}: expected {want}, got \"{}\"", got.loc(), got.literal)
            }
            Self::ParseInt(token, err) => {
                write!(f, "{}: invalid integer, {err}", token.loc())
            }
            Self::UnknownOperator(token) => {
                write!(f, "{}: invalid operator \"{}\"", token.loc(), token.literal)
            }
            Self::InvalidPrefix(token) => {
                write!(
                    f,
                    "{}: invalid syntax, unexpected \"{}\"",
                    token.loc(),
                    token.literal
                )
            }
            Self::QuoteWrongNumArgs(token, len) => {
                write!(f, "{}: \"quote\" expect 1 argument, got {len}", token.loc(),)
            }
            Self::UnquoteWrongNumArgs(token, len) => write!(
                f,
                "{}: \"unquote\" expect 1 argument, got {len}",
                token.loc(),
            ),
            Self::InvalidContext(token, want, got) => {
                write!(f, "{}: expected context {want:?}, got {got:?}", token.loc())
            }
            Self::BreakOutsideLoop(token) => {
                write!(f, "{}: \"break\" outside loop", token.loc())
            }
            Self::ReturnOutsideFunction(token) => {
                write!(f, "{}: \"return\" outside function", token.loc())
            }
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
    context: Vec<ParserContext>,
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
            context: vec![],
        };

        parser
            .prefix_parse_fns
            .insert(TokenKind::Identifier, Parser::parse_fn_identifier_literal);
        parser
            .prefix_parse_fns
            .insert(TokenKind::Integer, Parser::parse_fn_integer_literal);
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
            .insert(TokenKind::Null, Parser::parse_fn_null_literal);
        parser
            .prefix_parse_fns
            .insert(TokenKind::Lparen, Parser::parse_fn_grouped_expression);
        parser
            .prefix_parse_fns
            .insert(TokenKind::If, Parser::parse_fn_if_expression);
        parser
            .prefix_parse_fns
            .insert(TokenKind::Function, Parser::parse_fn_function_literal);
        parser
            .prefix_parse_fns
            .insert(TokenKind::String, Parser::parse_fn_string_literal);
        parser
            .prefix_parse_fns
            .insert(TokenKind::Lbracket, Parser::parse_fn_array_literal);
        parser
            .prefix_parse_fns
            .insert(TokenKind::Lbrace, Parser::parse_fn_hash_literal);
        parser
            .prefix_parse_fns
            .insert(TokenKind::Macro, Parser::parse_fn_macro_literal);
        parser
            .prefix_parse_fns
            .insert(TokenKind::Quote, Parser::parse_fn_quote_expression);
        parser
            .prefix_parse_fns
            .insert(TokenKind::Unquote, Parser::parse_fn_unquote_expression);
        parser
            .prefix_parse_fns
            .insert(TokenKind::Loop, Parser::parse_fn_loop_expression);

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
            .insert(TokenKind::Equal, Parser::parse_fn_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenKind::NotEqual, Parser::parse_fn_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenKind::LessThan, Parser::parse_fn_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenKind::GreaterThan, Parser::parse_fn_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenKind::Lparen, Parser::parse_fn_call_expression);
        parser
            .infix_parse_fns
            .insert(TokenKind::Lbracket, Parser::parse_fn_index_expression);

        parser
    }

    fn next_tokens(&mut self) {
        std::mem::swap(&mut self.curr_token, &mut self.next_token);
        self.next_token = self.lexer.next_token();
    }

    fn expect_token(&mut self, token: TokenKind) -> Result<(), ParserError> {
        if self.next_token.kind == token {
            self.next_tokens();
            Ok(())
        } else {
            Err(ParserError::ExpectToken(token, self.next_token.clone()))
        }
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, ParserError> {
        let token = self.curr_token.clone();

        self.expect_token(TokenKind::Identifier)?;
        let name = IdentifierLiteral {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };

        self.expect_token(TokenKind::Assign)?;
        self.next_tokens();

        let mut value = self.parse_expression(Precedence::Lowest)?;
        if let Expression::Function(expr) = &mut value {
            expr.name = Some(name.value.clone());
        }

        self.expect_token(TokenKind::Semicolon)?;

        Ok(LetStatement { token, name, value })
    }

    fn parse_return_statement(&mut self) -> Result<ExpressionStatement, ParserError> {
        let token = self.curr_token.clone();
        if !self.context.contains(&ParserContext::Function) {
            return Err(ParserError::ReturnOutsideFunction(token));
        }

        let value = match self.next_token.kind {
            TokenKind::Semicolon => Expression::Null(token.clone()),
            _ => {
                self.next_tokens();
                self.parse_expression(Precedence::Lowest)?
            }
        };

        self.expect_token(TokenKind::Semicolon)?;

        Ok(ExpressionStatement { token, value })
    }

    fn parse_exit_statement(&mut self) -> Result<ExpressionStatement, ParserError> {
        let token = self.curr_token.clone();

        let value = match self.next_token.kind {
            TokenKind::Semicolon => Expression::Integer(IntegerLiteral {
                token: token.clone(),
                value: 0,
            }),
            _ => {
                self.next_tokens();
                self.parse_expression(Precedence::Lowest)?
            }
        };

        self.expect_token(TokenKind::Semicolon)?;

        Ok(ExpressionStatement { token, value })
    }

    fn parse_break_statement(&mut self) -> Result<ExpressionStatement, ParserError> {
        let token = self.curr_token.clone();
        if !matches!(self.context.last(), Some(ParserContext::Loop)) {
            return Err(ParserError::BreakOutsideLoop(token));
        }

        let value = match self.next_token.kind {
            TokenKind::Semicolon => Expression::Null(token.clone()),
            _ => {
                self.next_tokens();
                self.parse_expression(Precedence::Lowest)?
            }
        };

        self.expect_token(TokenKind::Semicolon)?;

        Ok(ExpressionStatement { token, value })
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<IdentifierLiteral>, ParserError> {
        let mut params = vec![];

        if self.next_token.kind == TokenKind::Rparen {
            self.next_tokens();
            return Ok(params);
        }

        self.next_tokens();
        params.push(IdentifierLiteral {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        });

        while self.next_token.kind == TokenKind::Comma {
            self.next_tokens();
            self.next_tokens();
            params.push(IdentifierLiteral {
                token: self.curr_token.clone(),
                value: self.curr_token.literal.clone(),
            });
        }

        self.expect_token(TokenKind::Rparen)?;

        Ok(params)
    }

    fn parse_expression_list(&mut self, end: TokenKind) -> Result<Vec<Expression>, ParserError> {
        let mut list = vec![];

        if self.next_token.kind == end {
            self.next_tokens();
            return Ok(list);
        }

        self.next_tokens();
        list.push(self.parse_expression(Precedence::Lowest)?);

        while self.next_token.kind == TokenKind::Comma {
            self.next_tokens();
            self.next_tokens();
            list.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_token(end)?;

        Ok(list)
    }

    fn parse_fn_identifier_literal(parser: &mut Parser) -> Result<Expression, ParserError> {
        Ok(Expression::Identifier(IdentifierLiteral {
            token: parser.curr_token.clone(),
            value: parser.curr_token.literal.clone(),
        }))
    }

    fn parse_fn_integer_literal(parser: &mut Parser) -> Result<Expression, ParserError> {
        let value = match parser.curr_token.literal.parse::<usize>() {
            Ok(value) => value,
            Err(err) => return Err(ParserError::ParseInt(parser.curr_token.clone(), err)),
        };
        Ok(Expression::Integer(IntegerLiteral {
            token: parser.curr_token.clone(),
            value,
        }))
    }

    fn parse_fn_boolean_literal(parser: &mut Parser) -> Result<Expression, ParserError> {
        let value = parser.curr_token.kind == TokenKind::True;
        Ok(Expression::Boolean(BooleanLiteral {
            token: parser.curr_token.clone(),
            value,
        }))
    }

    fn parse_fn_null_literal(parser: &mut Parser) -> Result<Expression, ParserError> {
        Ok(Expression::Null(parser.curr_token.clone()))
    }

    fn parse_fn_function_literal(parser: &mut Parser) -> Result<Expression, ParserError> {
        let token = parser.curr_token.clone();

        parser.expect_token(TokenKind::Lparen)?;
        let parameters = parser.parse_function_parameters()?;

        parser.expect_token(TokenKind::Lbrace)?;
        parser.context.push(ParserContext::Function);
        let body = parser.parse_block_statement()?;
        match parser.context.pop() {
            Some(ParserContext::Function) => (),
            unexpected => {
                return Err(ParserError::InvalidContext(
                    token,
                    ParserContext::Loop,
                    unexpected,
                ))
            }
        }

        Ok(Expression::Function(FunctionLiteral {
            token,
            parameters,
            body,
            name: None,
        }))
    }

    fn parse_fn_macro_literal(parser: &mut Parser) -> Result<Expression, ParserError> {
        let token = parser.curr_token.clone();

        parser.expect_token(TokenKind::Lparen)?;
        let parameters = parser.parse_function_parameters()?;

        parser.expect_token(TokenKind::Lbrace)?;
        let body = parser.parse_block_statement()?;

        Ok(Expression::Macro(FunctionLiteral {
            token,
            parameters,
            body,
            name: None,
        }))
    }

    fn parse_fn_array_literal(parser: &mut Parser) -> Result<Expression, ParserError> {
        let token = parser.curr_token.clone();
        let elements = parser.parse_expression_list(TokenKind::Rbracket)?;
        Ok(Expression::Array(ArrayLiteral { token, elements }))
    }

    fn parse_fn_hash_literal(parser: &mut Parser) -> Result<Expression, ParserError> {
        let token = parser.curr_token.clone();
        let mut pairs = BTreeMap::new();

        while parser.next_token.kind != TokenKind::Rbrace {
            parser.next_tokens();

            let key = parser.parse_expression(Precedence::Lowest)?;

            parser.expect_token(TokenKind::Colon)?;
            parser.next_tokens();

            let value = parser.parse_expression(Precedence::Lowest)?;

            pairs.insert(key, value);

            if parser.next_token.kind != TokenKind::Rbrace {
                parser.expect_token(TokenKind::Comma)?;
            }
        }

        parser.expect_token(TokenKind::Rbrace)?;

        Ok(Expression::Hash(HashLiteral { token, pairs }))
    }

    fn parse_fn_string_literal(parser: &mut Parser) -> Result<Expression, ParserError> {
        Ok(Expression::String(StringLiteral {
            token: parser.curr_token.clone(),
            value: parser.curr_token.literal.clone(),
        }))
    }

    fn parse_fn_if_expression(parser: &mut Parser) -> Result<Expression, ParserError> {
        let token = parser.curr_token.clone();

        parser.expect_token(TokenKind::Lparen)?;
        parser.next_tokens();

        let condition = Box::new(parser.parse_expression(Precedence::Lowest)?);

        parser.expect_token(TokenKind::Rparen)?;
        parser.expect_token(TokenKind::Lbrace)?;

        let consequence = parser.parse_block_statement()?;

        let mut alternative = None;
        if parser.next_token.kind == TokenKind::Else {
            parser.next_tokens();
            parser.expect_token(TokenKind::Lbrace)?;
            alternative = Some(parser.parse_block_statement()?);
        }

        Ok(Expression::If(IfExpression {
            token,
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_fn_loop_expression(parser: &mut Parser) -> Result<Expression, ParserError> {
        let token = parser.curr_token.clone();

        parser.expect_token(TokenKind::Lbrace)?;
        parser.context.push(ParserContext::Loop);
        let body = parser.parse_block_statement()?;
        match parser.context.pop() {
            Some(ParserContext::Loop) => (),
            unexpected => {
                return Err(ParserError::InvalidContext(
                    token,
                    ParserContext::Loop,
                    unexpected,
                ))
            }
        }

        Ok(Expression::Loop(LoopExpression { token, body }))
    }

    fn parse_fn_prefix_expression(parser: &mut Parser) -> Result<Expression, ParserError> {
        let token = parser.curr_token.clone();
        let operator = Operator::try_from(token.literal.as_str())
            .map_err(|_| ParserError::UnknownOperator(token.clone()))?;

        parser.next_tokens();
        let right = Box::new(parser.parse_expression(Precedence::Prefix)?);

        Ok(Expression::Prefix(PrefixExpression {
            token,
            operator,
            right,
        }))
    }

    fn parse_fn_infix_expression(
        parser: &mut Parser,
        left: Box<Expression>,
    ) -> Result<Expression, ParserError> {
        parser.next_tokens();
        let token = parser.curr_token.clone();
        let operator = Operator::try_from(token.literal.as_str())
            .map_err(|_| ParserError::UnknownOperator(token.clone()))?;

        parser.next_tokens();
        let right = Box::new(parser.parse_expression(Precedence::from(&token.kind))?);

        Ok(Expression::Infix(InfixExpression {
            token,
            left,
            operator,
            right,
        }))
    }

    fn parse_fn_grouped_expression(parser: &mut Parser) -> Result<Expression, ParserError> {
        parser.next_tokens();
        let expression = parser.parse_expression(Precedence::Lowest)?;
        parser.expect_token(TokenKind::Rparen)?;
        Ok(expression)
    }

    fn parse_fn_call_expression(
        parser: &mut Parser,
        function: Box<Expression>,
    ) -> Result<Expression, ParserError> {
        let token = parser.curr_token.clone();
        parser.next_tokens();
        let arguments = parser.parse_expression_list(TokenKind::Rparen)?;
        Ok(Expression::Call(CallExpression {
            token,
            function,
            arguments,
        }))
    }

    fn parse_fn_quote_expression(parser: &mut Parser) -> Result<Expression, ParserError> {
        let token = parser.curr_token.clone();
        parser.next_tokens();
        let arguments = parser.parse_expression_list(TokenKind::Rparen)?;
        if arguments.len() != 1 {
            return Err(ParserError::QuoteWrongNumArgs(token, arguments.len()));
        }
        Ok(Expression::Quote(CallExpression {
            token: token.clone(),
            function: Box::new(Expression::Null(token)),
            arguments,
        }))
    }

    fn parse_fn_unquote_expression(parser: &mut Parser) -> Result<Expression, ParserError> {
        let token = parser.curr_token.clone();
        parser.next_tokens();
        let arguments = parser.parse_expression_list(TokenKind::Rparen)?;
        if arguments.len() != 1 {
            return Err(ParserError::UnquoteWrongNumArgs(token, arguments.len()));
        }
        Ok(Expression::Unquote(CallExpression {
            token: token.clone(),
            function: Box::new(Expression::Null(token)),
            arguments,
        }))
    }

    fn parse_fn_index_expression(
        parser: &mut Parser,
        left: Box<Expression>,
    ) -> Result<Expression, ParserError> {
        let token = parser.curr_token.clone();
        parser.next_tokens();

        parser.next_tokens();
        let index = Box::new(parser.parse_expression(Precedence::Lowest)?);

        parser.expect_token(TokenKind::Rbracket)?;

        Ok(Expression::Index(IndexExpression { token, left, index }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError> {
        let mut expression = match self.prefix_parse_fns.get(&self.curr_token.kind) {
            Some(prefix) => prefix(self)?,
            None => {
                return Err(ParserError::InvalidPrefix(self.curr_token.clone()));
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

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParserError> {
        let token = self.curr_token.clone();
        self.next_tokens();

        let mut statements = vec![];
        while self.curr_token.kind != TokenKind::Rbrace
            && self.curr_token.kind != TokenKind::EndOfFile
        {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => self.errors.push(err),
            }
            self.next_tokens();
        }

        Ok(BlockStatement { token, statements })
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, ParserError> {
        let token = self.curr_token.clone();
        let value = self.parse_expression(Precedence::Lowest)?;

        if self.next_token.kind == TokenKind::Semicolon {
            self.next_tokens();
        }

        Ok(ExpressionStatement { token, value })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        Ok(match self.curr_token.kind {
            TokenKind::Let => Statement::Let(self.parse_let_statement()?),
            TokenKind::Return => Statement::Return(self.parse_return_statement()?),
            TokenKind::Exit => Statement::Exit(self.parse_exit_statement()?),
            TokenKind::Break => Statement::Break(self.parse_break_statement()?),
            _ => Statement::Expression(self.parse_expression_statement()?),
        })
    }

    pub fn parse_program(&mut self) -> Program {
        let mut prog = Program::new();

        while self.curr_token.kind != TokenKind::EndOfFile {
            match self.parse_statement() {
                Ok(stmt) => prog.statements.push(stmt),
                Err(err) => {
                    self.errors.push(err);
                    while self.curr_token.kind != TokenKind::Semicolon
                        && self.curr_token.kind != TokenKind::EndOfFile
                    {
                        self.next_tokens();
                    }
                }
            };
            self.next_tokens();
        }

        prog
    }
}
