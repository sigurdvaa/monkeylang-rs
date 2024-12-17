#[cfg(test)]
use super::*;

#[cfg(test)]
fn get_program(input: &str, statements: usize) -> Program {
    let lexer = Lexer::new(None, input.chars().peekable());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if !parser.errors.is_empty() {
        println!("parser has {} errors", parser.errors.len());
        for err in &parser.errors {
            println!("{err}");
        }
        panic!("Parsing errors found");
    }

    assert!(program.is_ok());
    let program = program.unwrap();
    assert_eq!(program.statements.len(), statements);
    program
}

#[cfg(test)]
fn assert_identifier(expression: &Expression, assert_value: &str) {
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

#[test]
fn test_let_statements() {
    #[rustfmt::skip]
    let input = concat!(
        "let x = 5;\n",
        "let y = 10;\n",
        "let foobar = 838383;\n"
    );
    let program = get_program(input, 3);

    for (i, (ident, literal)) in [("x", 5), ("y", 10), ("foobar", 838383)].iter().enumerate() {
        let stmt = &program.statements[i];
        match stmt {
            Statement::Let { token, name, value } => {
                assert_eq!(token.kind, TokenKind::Let);
                assert_eq!(token.literal, "let".to_string());
                assert_identifier(name, ident);
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
    let program = get_program(input, 3);

    for (i, literal) in [5, 10, 993322].iter().enumerate() {
        match &program.statements[i] {
            Statement::Return { token, value } => {
                assert_eq!(token.kind, TokenKind::Return);
                assert_eq!(token.literal, "return".to_string());
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
    let input = "foobar;";
    let program = get_program(input, 1);

    for stmt in &program.statements {
        match stmt {
            Statement::Expression { token, value } => {
                assert_eq!(token.kind, TokenKind::Ident);
                assert_eq!(token.literal, "foobar".to_string());
                assert_identifier(value, "foobar");
            }
            _ => panic!("not a valid expression statement, got: {stmt}"),
        }
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;";
    let program = get_program(input, 1);

    for stmt in &program.statements {
        match stmt {
            Statement::Expression { token, value } => {
                assert_eq!(token.kind, TokenKind::Int);
                assert_eq!(token.literal, "5".to_string());
                assert_integer_literal(value, &5);
            }
            _ => panic!("not a valid expression statement, got: {stmt}"),
        }
    }
}

#[test]
fn test_parsing_prefix_expression() {
    let prefix_test = [("!5;", Operator::Bang, 5), ("-15;", Operator::Minus, 15)];
    for (input_test, operator_test, right_test) in prefix_test {
        let program = get_program(input_test, 1);

        for stmt in &program.statements {
            let expr = match &stmt {
                Statement::Expression { token: _, value } => value,
                _ => panic!("not an expression statement, got: {stmt}"),
            };

            let (operator, right) = match expr {
                Expression {
                    token: _,
                    kind: ExpressionKind::Prefix { operator, right },
                } => (operator, right),
                _ => panic!("not a prefix expression, got: {stmt}"),
            };
            assert_eq!(operator_test, *operator);

            assert_integer_literal(right.as_ref(), &right_test);
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
        let program = get_program(input_test, 1);

        for stmt in &program.statements {
            let expr = match &stmt {
                Statement::Expression { token: _, value } => value,
                _ => panic!("not an expression statement, got: {stmt}"),
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
                _ => panic!("not a infix expression, got: {stmt}"),
            };

            assert_integer_literal(left.as_ref(), &left_test);
            assert_eq!(operator_test, *operator);
            assert_integer_literal(right.as_ref(), &right_test);
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
        let program = get_program(input_test, statements_test);
        assert_eq!(program.to_string(), value_test.to_string());
    }
}
