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

    assert_eq!(program.statements.len(), statements);
    program
}

#[cfg(test)]
fn assert_identifier_literal(expr: &IdentifierLiteral, assert_value: &str) {
    assert_eq!(expr.token.kind, TokenKind::Ident);
    assert_eq!(expr.token.literal, assert_value);
    assert_eq!(expr.value, assert_value);
}

#[cfg(test)]
fn assert_integer_literal(expr: &IntegerLiteral, assert_value: &usize) {
    assert_eq!(expr.token.kind, TokenKind::Int);
    assert_eq!(expr.token.literal, assert_value.to_string());
    assert_eq!(&expr.value, assert_value);
}

#[cfg(test)]
fn assert_boolean_literal(expr: &BooleanLiteral, assert_value: &bool) {
    if *assert_value {
        assert_eq!(expr.token.kind, TokenKind::True);
    } else {
        assert_eq!(expr.token.kind, TokenKind::False);
    }
    assert_eq!(expr.token.literal, assert_value.to_string());
    assert_eq!(&expr.value, assert_value);
}

#[cfg(test)]
fn assert_literal(expression: &Expression, assert_value: &Literal) {
    match (&expression, &assert_value) {
        (Expression::Boolean(expr), Literal::Bool(value)) => assert_boolean_literal(expr, value),
        (Expression::Identifier(expr), Literal::Ident(value)) => {
            assert_identifier_literal(expr, value)
        }
        (Expression::IntegerLiteral(expr), Literal::Int(value)) => {
            assert_integer_literal(expr, value)
        }
        _ => {
            panic!("mismatched expression and literal value, got {expression} and {assert_value:?}")
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
    let expr = match expression {
        Expression::Infix(expr) => expr,
        _ => panic!("not an infix expression, got: {expression}"),
    };

    assert_literal(&expr.left, left_test);
    assert_eq!(&expr.operator, operator_test);
    assert_literal(&expr.right, right_test);
}

#[cfg(test)]
fn assert_prefix_expression(
    expression: &Expression,
    operator_test: &Operator,
    right_test: &Literal,
) {
    let expr = match expression {
        Expression::Prefix(expr) => expr,
        _ => panic!("not a prefix expression, got: {expression}"),
    };
    assert_eq!(&expr.operator, operator_test);
    assert_literal(&expr.right, right_test);
}

#[test]
fn test_let_statements() {
    let tests = [
        ("let x = 5;", Literal::Ident("x"), Literal::Int(5)),
        ("let y = true;", Literal::Ident("y"), Literal::Bool(true)),
        (
            "let foobar = y;",
            Literal::Ident("foobar"),
            Literal::Ident("y"),
        ),
    ];

    for (test_input, test_ident, test_value) in &tests {
        let program = parse_program(test_input, 1);
        match &program.statements[0] {
            Statement::Let(stmt) => {
                assert_eq!(stmt.token.kind, TokenKind::Let);
                assert_eq!(stmt.token.literal, "let");
                assert_literal(&stmt.name, test_ident);
                assert_literal(&stmt.value, test_value);
            }
            _ => panic!("Not a valid let statement, got: {}", program.statements[0]),
        }
    }
}

#[test]
fn test_return_statements() {
    let tests = [
        ("return 5;", Literal::Int(5)),
        ("return false;", Literal::Bool(false)),
        ("return foobar;", Literal::Ident("foobar")),
    ];

    for (test_input, test_value) in &tests {
        let program = parse_program(test_input, 1);
        match &program.statements[0] {
            Statement::Return(stmt) => {
                assert_eq!(stmt.token.kind, TokenKind::Return);
                assert_eq!(stmt.token.literal, "return");
                assert_literal(&stmt.value, test_value);
            }
            _ => panic!(
                "not a valid return statement, got: {}",
                &program.statements[0]
            ),
        }
    }
}

#[test]
fn test_program_to_string() {
    let program = Program {
        statements: vec![Statement::Let(LetStatement {
            token: Token {
                file: None,
                col: 0,
                line: 0,
                kind: TokenKind::Let,
                literal: "let".into(),
            },
            name: Expression::Identifier(IdentifierLiteral {
                token: Token {
                    file: None,
                    col: 0,
                    line: 0,
                    kind: TokenKind::Ident,
                    literal: "myVar".into(),
                },
                value: "myVar".into(),
            }),
            value: Expression::Identifier(IdentifierLiteral {
                token: Token {
                    file: None,
                    col: 0,
                    line: 0,
                    kind: TokenKind::Ident,
                    literal: "anotherVar".into(),
                },
                value: "anotherVar".into(),
            }),
        })],
    };

    assert_eq!(program.to_string(), "let myVar = anotherVar;");
}

#[test]
fn test_identifier_expression() {
    let (test_input, test_value) = ("foobar;", "foobar");
    let program = parse_program(test_input, 1);

    for stmt in &program.statements {
        match stmt {
            Statement::Expression(stmt) => {
                assert_eq!(stmt.token.kind, TokenKind::Ident);
                assert_eq!(stmt.token.literal, test_value);
                assert_literal(&stmt.value, &Literal::Ident(test_value));
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
            Statement::Expression(stmt) => {
                assert_eq!(stmt.token.kind, TokenKind::Int);
                assert_eq!(stmt.token.literal, test_value.to_string());
                assert_literal(&stmt.value, &Literal::Int(test_value));
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
                Statement::Expression(stmt) => stmt,
                _ => panic!("not an expression statement, got: {stmt}"),
            };

            assert_prefix_expression(&expr.value, operator_test, right_test);
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
            let stmt_expr = match &stmt {
                Statement::Expression(stmt) => stmt,
                _ => panic!("not an expression statement, got: {stmt}"),
            };

            assert_infix_expression(&stmt_expr.value, left_test, operator_test, right_test);
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
        ("a + add(b * c) + d", "((a + add((b * c))) + d)", 1),
        (
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            1,
        ),
        (
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))",
            1,
        ),
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
                Statement::Expression(stmt) => {
                    assert_eq!(stmt.token.kind, *test_token);
                    assert_eq!(stmt.token.literal, *test_literal);
                    assert_literal(&stmt.value, test_value);
                }
                _ => panic!("not a valid expression statement, got: {stmt}"),
            }
        }
    }
}

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";
    let program = parse_program(input, 1);

    for stmt in &program.statements {
        let expr = match stmt {
            Statement::Expression(stmt) => {
                assert_eq!(stmt.token.kind, TokenKind::If);
                assert_eq!(stmt.token.literal, "if");
                &stmt.value
            }
            _ => panic!("not a valid expression statement, got: {stmt}"),
        };

        let expr = match &expr {
            Expression::If(expr) => expr,
            _ => panic!("not a valid if expression, got: {expr}"),
        };

        assert_infix_expression(
            &expr.condition,
            &Literal::Ident("x"),
            &Operator::Lt,
            &Literal::Ident("y"),
        );

        assert_eq!(expr.consequence.statements.len(), 1);
        match &expr.consequence.statements[0] {
            Statement::Expression(stmt) => {
                assert_literal(&stmt.value, &Literal::Ident("x"));
            }
            _ => panic!("not valid expression statement"),
        }

        assert!(expr.alternative.is_none());
    }
}

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y }";
    let program = parse_program(input, 1);

    for stmt in &program.statements {
        let expr = match stmt {
            Statement::Expression(stmt) => {
                assert_eq!(stmt.token.kind, TokenKind::If);
                assert_eq!(stmt.token.literal, "if");
                &stmt.value
            }
            _ => panic!("not a valid expression statement, got: {stmt}"),
        };

        let expr = match &expr {
            Expression::If(expr) => expr,
            _ => panic!("not a valid if expression, got: {expr}"),
        };

        assert_infix_expression(
            &expr.condition,
            &Literal::Ident("x"),
            &Operator::Lt,
            &Literal::Ident("y"),
        );

        assert_eq!(expr.consequence.statements.len(), 1);
        match &expr.consequence.statements[0] {
            Statement::Expression(stmt) => {
                assert_literal(&stmt.value, &Literal::Ident("x"));
            }
            _ => panic!("not valid expression statement"),
        }

        match &expr.alternative {
            Some(alternative) => {
                assert_eq!(alternative.statements.len(), 1);
                match &alternative.statements[0] {
                    Statement::Expression(stmt) => {
                        assert_literal(&stmt.value, &Literal::Ident("y"));
                    }
                    _ => panic!(
                        "not a valid expression statement, got {}",
                        alternative.statements[0]
                    ),
                }
            }
            None => panic!("if alternative block missing"),
        }
    }
}

#[test]
fn test_function_literal_parsing() {
    let input = "fn(x, y) { x + y; }";
    let program = parse_program(input, 1);

    for stmt in &program.statements {
        let stmt_expr = match stmt {
            Statement::Expression(stmt) => {
                assert_eq!(stmt.token.kind, TokenKind::Function);
                assert_eq!(stmt.token.literal, "fn");
                &stmt.value
            }
            _ => panic!("not a valid expression statement, got: {stmt}"),
        };

        let expr = match &stmt_expr {
            Expression::Function(expr) => expr,
            _ => panic!("not a valid function literal, got: {stmt_expr}"),
        };

        assert_eq!(expr.parameters.len(), 2);

        assert_identifier_literal(&expr.parameters[0], "x");
        assert_identifier_literal(&expr.parameters[1], "y");

        assert_eq!(expr.body.statements.len(), 1);

        match &expr.body.statements[0] {
            Statement::Expression(stmt) => {
                assert_infix_expression(
                    &stmt.value,
                    &Literal::Ident("x"),
                    &Operator::Plus,
                    &Literal::Ident("y"),
                );
            }
            _ => panic!(
                "not a valid expression statement, got {}",
                expr.body.statements[0]
            ),
        }
    }
}

#[test]
fn test_function_parameters_parsing() {
    let test = [
        ("fn() {};", vec![]),
        ("fn(x) {};", vec!["x"]),
        ("fn(x, y, z) {};", vec!["x", "y", "z"]),
    ];

    for (test_input, test_params) in test {
        let program = parse_program(test_input, 1);
        for stmt in &program.statements {
            let stmt_expr = match stmt {
                Statement::Expression(stmt) => {
                    assert_eq!(stmt.token.kind, TokenKind::Function);
                    assert_eq!(stmt.token.literal, "fn");
                    &stmt.value
                }
                _ => panic!("not a valid expression statement, got: {stmt}"),
            };

            let expr = match &stmt_expr {
                Expression::Function(expr) => expr,
                _ => panic!("not a valid function literal, got: {stmt_expr}"),
            };

            assert_eq!(expr.parameters.len(), test_params.len());
            for (i, param) in expr.parameters.iter().enumerate() {
                assert_identifier_literal(param, test_params[i]);
            }
        }
    }
}

#[test]
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";
    let program = parse_program(input, 1);

    for stmt in &program.statements {
        let stmt_expr = match stmt {
            Statement::Expression(stmt) => {
                assert_eq!(stmt.token.kind, TokenKind::Ident);
                assert_eq!(stmt.token.literal, "add");
                &stmt.value
            }
            _ => panic!("not a valid expression statement, got: {stmt}"),
        };

        let expr = match &stmt_expr {
            Expression::Call(expr) => expr,
            _ => panic!("not a valid call expression, got: {stmt_expr}"),
        };

        assert_literal(expr.function.as_ref(), &Literal::Ident("add"));

        assert_eq!(expr.arguments.len(), 3);

        assert_literal(&expr.arguments[0], &Literal::Int(1));
        assert_infix_expression(
            &expr.arguments[1],
            &Literal::Int(2),
            &Operator::Asterisk,
            &Literal::Int(3),
        );
        assert_infix_expression(
            &expr.arguments[2],
            &Literal::Int(4),
            &Operator::Plus,
            &Literal::Int(5),
        );
    }
}
