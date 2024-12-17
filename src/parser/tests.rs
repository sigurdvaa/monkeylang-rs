#[cfg(test)]
use super::*;

#[cfg(test)]
#[derive(Debug)]
enum Literal {
    Bool(bool),
    Ident(&'static str),
    Int(usize),
}

#[cfg(test)]
fn parse_program(input: &str, statements: usize) -> Program {
    let lexer = Lexer::new(None, input.chars().peekable());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if !parser.errors.is_empty() {
        for err in &parser.errors {
            println!("{err}");
        }
        panic!("parser errors found: {}", parser.errors.len());
    }

    assert!(program.is_ok());
    let program = program.unwrap();
    assert_eq!(program.statements.len(), statements);
    program
}

#[cfg(test)]
fn assert_identifier_literal(expression: &Expression, assert_value: &str) {
    assert_eq!(expression.token.kind, TokenKind::Ident);
    assert_eq!(expression.token.literal, assert_value);
    match &expression.kind {
        ExpressionKind::Identifier { value } => assert_eq!(value, assert_value),
        _ => panic!("invalid identifier, got {expression:?}"),
    }
}

#[cfg(test)]
fn assert_integer_literal(expression: &Expression, assert_value: &usize) {
    assert_eq!(expression.token.kind, TokenKind::Int);
    assert_eq!(expression.token.literal, assert_value.to_string());
    match &expression.kind {
        ExpressionKind::IntegerLiteral { value } => assert_eq!(value, assert_value),
        _ => panic!("invalid integer literal, got {expression:?}"),
    }
}

#[cfg(test)]
fn assert_boolean_literal(expression: &Expression, assert_value: &bool) {
    if *assert_value {
        assert_eq!(expression.token.kind, TokenKind::True);
    } else {
        assert_eq!(expression.token.kind, TokenKind::False);
    }
    assert_eq!(expression.token.literal, assert_value.to_string());
    match &expression.kind {
        ExpressionKind::Boolean { value } => assert_eq!(value, assert_value),
        _ => panic!("invalid boolean literal, got {expression:?}"),
    }
}

#[cfg(test)]
fn assert_literal(expression: &Expression, assert_value: &Literal) {
    match (&expression.kind, &assert_value) {
        (ExpressionKind::Boolean { .. }, Literal::Bool(value)) => {
            assert_boolean_literal(expression, value)
        }
        (ExpressionKind::Identifier { .. }, Literal::Ident(value)) => {
            assert_identifier_literal(expression, value)
        }
        (ExpressionKind::IntegerLiteral { .. }, Literal::Int(value)) => {
            assert_integer_literal(expression, value)
        }
        _ => {
            panic!("Mismatched expression and literal value, got {expression} and {assert_value:?}")
        }
    }
}

#[cfg(test)]
fn assert_infix_expression(
    expression: &Expression,
    left_test: &Literal,
    operator_test: &Operator,
    right_test: &Literal,
) {
    let (left, operator, right) = match expression {
        Expression {
            token: _,
            kind:
                ExpressionKind::Infix {
                    left,
                    operator,
                    right,
                },
        } => (left, operator, right),
        _ => panic!("not a infix expression, got: {expression}"),
    };

    assert_literal(left, left_test);
    assert_eq!(operator_test, operator);
    assert_literal(right, right_test);
}

#[cfg(test)]
fn assert_prefix_expression(
    expression: &Expression,
    operator_test: &Operator,
    right_test: &Literal,
) {
    let (operator, right) = match expression {
        Expression {
            token: _,
            kind: ExpressionKind::Prefix { operator, right },
        } => (operator, right),
        _ => panic!("not a prefix expression, got: {expression}"),
    };
    assert_eq!(operator_test, operator);
    assert_literal(right, right_test);
}

#[test]
fn test_let_statements() {
    #[rustfmt::skip]
    let input = concat!(
        "let x = 5;\n",
        "let y = 10;\n",
        "let foobar = 838383;\n"
    );
    let program = parse_program(input, 3);

    for (i, (ident, literal)) in [("x", 5), ("y", 10), ("foobar", 838383)].iter().enumerate() {
        let stmt = &program.statements[i];
        match stmt {
            Statement::Let { token, name, value } => {
                assert_eq!(token.kind, TokenKind::Let);
                assert_eq!(token.literal, "let");
                assert_identifier_literal(name, ident);
                assert_integer_literal(value, literal);
            }
            _ => panic!("Not a valid let statement, got: {stmt}"),
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
    let program = parse_program(input, 3);

    for (i, literal) in [5, 10, 993322].iter().enumerate() {
        match &program.statements[i] {
            Statement::Return { token, value } => {
                assert_eq!(token.kind, TokenKind::Return);
                assert_eq!(token.literal, "return");
                assert_integer_literal(value, literal);
            }
            _ => panic!(
                "not a valid return statement, got: {}",
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
    let (test_input, test_value) = ("foobar;", "foobar");
    let program = parse_program(test_input, 1);

    for stmt in &program.statements {
        match stmt {
            Statement::Expression { token, value } => {
                assert_eq!(token.kind, TokenKind::Ident);
                assert_eq!(token.literal, test_value);
                assert_literal(value, &Literal::Ident(test_value));
            }
            _ => panic!("not a valid expression statement, got: {stmt}"),
        }
    }
}

#[test]
fn test_integer_literal_expression() {
    let (test_input, test_value) = ("5;", 5);
    let program = parse_program(test_input, 1);

    for stmt in &program.statements {
        match stmt {
            Statement::Expression { token, value } => {
                assert_eq!(token.kind, TokenKind::Int);
                assert_eq!(token.literal, test_value.to_string());
                assert_literal(value, &Literal::Int(test_value));
            }
            _ => panic!("not a valid expression statement, got: {stmt}"),
        }
    }
}

#[test]
fn test_parsing_prefix_expression() {
    let prefix_test = [
        ("!5;", Operator::Bang, Literal::Int(5)),
        ("-15;", Operator::Minus, Literal::Int(15)),
        ("!true", Operator::Bang, Literal::Bool(true)),
        ("!false", Operator::Bang, Literal::Bool(false)),
    ];
    for (input_test, operator_test, right_test) in &prefix_test {
        let program = parse_program(input_test, 1);

        for stmt in &program.statements {
            let expr = match &stmt {
                Statement::Expression { token: _, value } => value,
                _ => panic!("not an expression statement, got: {stmt}"),
            };

            assert_prefix_expression(expr, operator_test, right_test);
        }
    }
}

#[test]
fn test_parsing_infix_expression() {
    let infix_test = [
        ("1 + 2;", Literal::Int(1), Operator::Plus, Literal::Int(2)),
        ("1 - 2;", Literal::Int(1), Operator::Minus, Literal::Int(2)),
        (
            "1 * 2;",
            Literal::Int(1),
            Operator::Asterisk,
            Literal::Int(2),
        ),
        ("1 / 2;", Literal::Int(1), Operator::Slash, Literal::Int(2)),
        ("1 > 2;", Literal::Int(1), Operator::Gt, Literal::Int(2)),
        ("1 < 2;", Literal::Int(1), Operator::Lt, Literal::Int(2)),
        ("1 == 2;", Literal::Int(1), Operator::Eq, Literal::Int(2)),
        ("1 != 2;", Literal::Int(1), Operator::NotEq, Literal::Int(2)),
        (
            "true == true",
            Literal::Bool(true),
            Operator::Eq,
            Literal::Bool(true),
        ),
        (
            "true != false",
            Literal::Bool(true),
            Operator::NotEq,
            Literal::Bool(false),
        ),
        (
            "false == false",
            Literal::Bool(false),
            Operator::Eq,
            Literal::Bool(false),
        ),
    ];
    for (input_test, left_test, operator_test, right_test) in &infix_test {
        let program = parse_program(input_test, 1);

        for stmt in &program.statements {
            let expr = match &stmt {
                Statement::Expression { token: _, value } => value,
                _ => panic!("not an expression statement, got: {stmt}"),
            };

            assert_infix_expression(expr, left_test, operator_test, right_test);
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
        ("true", "true", 1),
        ("false", "false", 1),
        ("3 > 5 == false", "((3 > 5) == false)", 1),
        ("3 < 5 == true", "((3 < 5) == true)", 1),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)", 1),
        ("(5 + 5) * 2", "((5 + 5) * 2)", 1),
        ("2 / (5 + 5)", "(2 / (5 + 5))", 1),
        ("-(5 + 5)", "(-(5 + 5))", 1),
        ("!(true == true)", "(!(true == true))", 1),
    ];

    for (input_test, value_test, statements_test) in precedence_test {
        let program = parse_program(input_test, statements_test);
        assert_eq!(program.to_string(), value_test.to_string());
    }
}

#[test]
fn test_boolean_expression() {
    let boolean_test = [
        ("true;", TokenKind::True, "true", Literal::Bool(true)),
        ("false;", TokenKind::False, "false", Literal::Bool(false)),
    ];

    for (test_input, test_token, test_literal, test_value) in &boolean_test {
        let program = parse_program(test_input, 1);
        for stmt in &program.statements {
            match stmt {
                Statement::Expression { token, value } => {
                    assert_eq!(token.kind, *test_token);
                    assert_eq!(token.literal, *test_literal);
                    assert_literal(value, test_value);
                }
                _ => panic!("not a valid expression statement, got: {stmt}"),
            }
        }
    }
}
