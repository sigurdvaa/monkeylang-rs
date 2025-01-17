use super::*;

#[derive(Debug)]
enum Literal {
    Bool(bool),
    Ident(&'static str),
    Int(usize),
    String(String),
}

pub fn parse_program(input: &str, statements: usize) -> Program {
    let lexer = Lexer::new(None, input.chars().peekable());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if !parser.errors.is_empty() {
        for err in &parser.errors {
            println!("{err}");
        }
        panic!("parser errors found: {}", parser.errors.len());
    }

    assert_eq!(
        program.statements.len(),
        statements,
        "unexpected statements"
    );
    program
}

fn assert_identifier_literal(expr: &IdentifierLiteral, assert_value: &str) {
    assert_eq!(expr.token.kind, TokenKind::Identifier);
    assert_eq!(expr.token.literal, assert_value);
    assert_eq!(expr.value, assert_value);
}

fn assert_integer_literal(expr: &IntegerLiteral, assert_value: &usize) {
    assert_eq!(expr.token.kind, TokenKind::Integer);
    assert_eq!(expr.token.literal, assert_value.to_string());
    assert_eq!(&expr.value, assert_value);
}

fn assert_string_literal(expr: &StringLiteral, assert_value: &str) {
    assert_eq!(expr.token.kind, TokenKind::String);
    assert_eq!(expr.token.literal, assert_value);
    assert_eq!(&expr.value, assert_value);
}

fn assert_boolean_literal(expr: &BooleanLiteral, assert_value: &bool) {
    if *assert_value {
        assert_eq!(expr.token.kind, TokenKind::True);
    } else {
        assert_eq!(expr.token.kind, TokenKind::False);
    }
    assert_eq!(expr.token.literal, assert_value.to_string());
    assert_eq!(&expr.value, assert_value);
}

fn assert_literal(expression: &Expression, assert_value: &Literal) {
    match (&expression, &assert_value) {
        (Expression::Boolean(expr), Literal::Bool(value)) => assert_boolean_literal(expr, value),
        (Expression::Identifier(expr), Literal::Ident(value)) => {
            assert_identifier_literal(expr, value)
        }
        (Expression::Integer(expr), Literal::Int(value)) => assert_integer_literal(expr, value),
        (Expression::String(expr), Literal::String(value)) => assert_string_literal(expr, value),
        _ => {
            panic!(
                "mismatched expression and literal value, got {expression:?} and {assert_value:?}"
            )
        }
    }
}

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
        ("let x = 5;", "x", Literal::Int(5)),
        ("let y = true;", "y", Literal::Bool(true)),
        ("let foobar = y;", "foobar", Literal::Ident("y")),
    ];

    for (test_input, test_ident, test_value) in &tests {
        let program = parse_program(test_input, 1);
        match &program.statements[0] {
            Statement::Let(stmt) => {
                assert_eq!(stmt.token.kind, TokenKind::Let);
                assert_eq!(stmt.token.literal, "let");
                assert_identifier_literal(&stmt.name, test_ident);
                assert_literal(&stmt.value, test_value);
            }
            _ => panic!("Not a valid let statement, got: {}", program.statements[0]),
        }
    }
}

#[test]
fn test_return_statements() {
    let tests = [
        ("fn() { return 5; }", Literal::Int(5)),
        ("fn() { return false; }", Literal::Bool(false)),
        ("fn() { return foobar; }", Literal::Ident("foobar")),
    ];

    for (test_input, test_value) in &tests {
        let program = parse_program(test_input, 1);
        let outer_stmt = match &program.statements[0] {
            Statement::Expression(expr_stmt) => expr_stmt,
            _ => panic!(
                "not a valid expression statement, got: {}",
                &program.statements[0]
            ),
        };

        let func = match &outer_stmt.value {
            Expression::Function(func) => func,
            _ => panic!(
                "not a valid function expression, got: {:?}",
                outer_stmt.value
            ),
        };

        match &func.body.statements[0] {
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
fn test_return_statements_without_value() {
    let program = parse_program("fn() { return; }", 1);
    let outer_stmt = match &program.statements[0] {
        Statement::Expression(expr_stmt) => expr_stmt,
        _ => panic!(
            "not a valid expression statement, got: {}",
            &program.statements[0]
        ),
    };

    let func = match &outer_stmt.value {
        Expression::Function(func) => func,
        _ => panic!("not a valid function expression, got: {}", outer_stmt.value),
    };

    match &func.body.statements[0] {
        Statement::Return(stmt) => {
            assert_eq!(stmt.token.kind, TokenKind::Return);
            assert_eq!(stmt.token.literal, "return");
            match stmt.value {
                Expression::Null(_) => (),
                _ => panic!("not a valid null expression, got {:?}", stmt.value),
            }
        }
        _ => panic!(
            "not a valid return statement, got: {}",
            &program.statements[0]
        ),
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
            name: IdentifierLiteral {
                token: Token {
                    file: None,
                    col: 0,
                    line: 0,
                    kind: TokenKind::Identifier,
                    literal: "myVar".into(),
                },
                value: "myVar".into(),
            },
            value: Expression::Identifier(IdentifierLiteral {
                token: Token {
                    file: None,
                    col: 0,
                    line: 0,
                    kind: TokenKind::Identifier,
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
                assert_eq!(stmt.token.kind, TokenKind::Identifier);
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
                assert_eq!(stmt.token.kind, TokenKind::Integer);
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
        (
            "a * [1, 2, 3, 4][b * c] * d",
            "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            1,
        ),
        (
            "add(a * b[2], b[1], 2 * [1, 2][1])",
            "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
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
                assert_eq!(stmt.token.kind, TokenKind::Identifier);
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

#[test]
fn test_string_literal_expression() {
    let (test_input, test_value) = ("\"hello world\";", "hello world");
    let program = parse_program(test_input, 1);
    for stmt in &program.statements {
        match stmt {
            Statement::Expression(stmt) => {
                assert_eq!(stmt.token.kind, TokenKind::String);
                assert_eq!(stmt.token.literal, test_value);
                assert_literal(&stmt.value, &Literal::String(test_value.into()));
            }
            _ => panic!("not a valid expression statement, got: {stmt}"),
        }
    }
}

#[test]
fn test_parsing_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";
    let program = parse_program(input, 1);

    let stmt_expr = match &program.statements[0] {
        Statement::Expression(stmt) => {
            assert_eq!(stmt.token.kind, TokenKind::Lbracket);
            assert_eq!(stmt.token.literal, "[");
            &stmt.value
        }
        _ => panic!(
            "not a valid expression statement, got: {}",
            &program.statements[0]
        ),
    };

    let expr = match stmt_expr {
        Expression::Array(expr) => expr,
        _ => panic!("not a valid array expression, got: {stmt_expr}"),
    };

    assert_eq!(expr.elements.len(), 3);
    assert_literal(&expr.elements[0], &Literal::Int(1));
    assert_infix_expression(
        &expr.elements[1],
        &Literal::Int(2),
        &Operator::Asterisk,
        &Literal::Int(2),
    );
    assert_infix_expression(
        &expr.elements[2],
        &Literal::Int(3),
        &Operator::Plus,
        &Literal::Int(3),
    );
}

#[test]
fn test_parsing_index_expressions() {
    let input = "myArray[1 + 1]";
    let program = parse_program(input, 1);

    let stmt_expr = match &program.statements[0] {
        Statement::Expression(stmt) => {
            assert_eq!(stmt.token.kind, TokenKind::Identifier);
            assert_eq!(stmt.token.literal, "myArray");
            &stmt.value
        }
        _ => panic!(
            "not a valid expression statement, got: {}",
            &program.statements[0]
        ),
    };

    let expr = match stmt_expr {
        Expression::Index(expr) => expr,
        _ => panic!("not a valid index expression, got: {stmt_expr}"),
    };

    assert_literal(&expr.left, &Literal::Ident("myArray"));
    assert_infix_expression(
        &expr.index,
        &Literal::Int(1),
        &Operator::Plus,
        &Literal::Int(1),
    );
}

#[test]
fn test_parsing_hash_literals_string_keys() {
    let input = r#"{"one": 1, "two": 2, "three": 3}"#;
    let tests = HashMap::from([("one", 1), ("two", 2), ("three", 3)]);
    let program = parse_program(input, 1);

    let stmt_expr = match &program.statements[0] {
        Statement::Expression(stmt) => {
            assert_eq!(stmt.token.kind, TokenKind::Lbrace);
            assert_eq!(stmt.token.literal, "{");
            &stmt.value
        }
        _ => panic!(
            "not a valid expression statement, got: {}",
            &program.statements[0]
        ),
    };

    let expr = match stmt_expr {
        Expression::Hash(expr) => expr,
        _ => panic!("not a valid hash expression, got: {stmt_expr}"),
    };

    assert_eq!(expr.pairs.len(), 3);
    for (expr_key, expr_value) in &expr.pairs {
        if let Expression::String(key) = expr_key {
            let test_value = tests[key.value.as_str()];
            assert_literal(expr_value, &Literal::Int(test_value));
        } else {
            panic!("not a string expression, got: {expr_key}");
        }
    }
}

#[test]
fn test_parsing_empty_hash_literal() {
    let input = "{}";
    let program = parse_program(input, 1);

    let stmt_expr = match &program.statements[0] {
        Statement::Expression(stmt) => {
            assert_eq!(stmt.token.kind, TokenKind::Lbrace);
            assert_eq!(stmt.token.literal, "{");
            &stmt.value
        }
        _ => panic!(
            "not a valid expression statement, got: {}",
            &program.statements[0]
        ),
    };

    let expr = match stmt_expr {
        Expression::Hash(expr) => expr,
        _ => panic!("not a valid hash expression, got: {stmt_expr}"),
    };

    assert_eq!(expr.pairs.len(), 0);
}

#[test]
fn test_parsing_hash_literals_with_expressions() {
    let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;
    let tests = HashMap::from([
        ("one", (&Literal::Int(0), &Operator::Plus, &Literal::Int(1))),
        (
            "two",
            (&Literal::Int(10), &Operator::Minus, &Literal::Int(8)),
        ),
        (
            "three",
            (&Literal::Int(15), &Operator::Slash, &Literal::Int(5)),
        ),
    ]);
    let program = parse_program(input, 1);

    let stmt_expr = match &program.statements[0] {
        Statement::Expression(stmt) => {
            assert_eq!(stmt.token.kind, TokenKind::Lbrace);
            assert_eq!(stmt.token.literal, "{");
            &stmt.value
        }
        _ => panic!(
            "not a expression statement, got: {}",
            &program.statements[0]
        ),
    };

    let expr = match stmt_expr {
        Expression::Hash(expr) => expr,
        _ => panic!("not a hash expression, got: {stmt_expr}"),
    };

    assert_eq!(expr.pairs.len(), 3);
    for (expr_key, expr_value) in &expr.pairs {
        if let Expression::String(key) = expr_key {
            let (left_test, operator_test, right_test) = tests[key.value.as_str()];
            assert_infix_expression(expr_value, left_test, operator_test, right_test);
        } else {
            panic!("not a string expression, got: {expr_key}");
        }
    }
}

#[test]
fn test_macro_literal_parsing() {
    let input = "macro(x, y) { x + y; }";
    let program = parse_program(input, 1);

    let stmt_expr = match &program.statements[0] {
        Statement::Expression(stmt) => {
            assert_eq!(stmt.token.kind, TokenKind::Macro);
            assert_eq!(stmt.token.literal, "macro");
            &stmt.value
        }
        _ => panic!(
            "not an expression statement, got: {}",
            &program.statements[0]
        ),
    };

    let expr = match stmt_expr {
        Expression::Macro(expr) => expr,
        _ => panic!("not a macro expression, got: {stmt_expr}"),
    };

    assert_eq!(expr.parameters.len(), 2);
    assert_identifier_literal(&expr.parameters[0], "x");
    assert_identifier_literal(&expr.parameters[1], "y");

    assert_eq!(expr.body.statements.len(), 1);
    let body_stmt = match &expr.body.statements[0] {
        Statement::Expression(stmt) => stmt,
        _ => panic!(
            "not an expression statement, got: {}",
            expr.body.statements[0]
        ),
    };

    assert_infix_expression(
        &body_stmt.value,
        &Literal::Ident("x"),
        &Operator::Plus,
        &Literal::Ident("y"),
    );
}

#[test]
fn test_function_literal_with_name() {
    let input = "let myFunction = fn() { };";
    let program = parse_program(input, 1);

    let stmt = match &program.statements[0] {
        Statement::Let(stmt) => {
            assert_eq!(stmt.token.kind, TokenKind::Let);
            assert_eq!(stmt.token.literal, "let");
            stmt
        }
        _ => panic!("not a let statement, got: {}", &program.statements[0]),
    };

    let expr = match &stmt.value {
        Expression::Function(expr) => expr,
        _ => panic!("not a function expression, got: {}", stmt.value),
    };

    assert_eq!(expr.name, Some("myFunction".into()));
}

#[test]
fn test_loop_expression_with_value() {
    let input = "loop { break x; }";
    let program = parse_program(input, 1);

    for stmt in &program.statements {
        let expr = match stmt {
            Statement::Expression(stmt) => {
                assert_eq!(stmt.token.kind, TokenKind::Loop);
                assert_eq!(stmt.token.literal, "loop");
                &stmt.value
            }
            _ => panic!("not a valid expression statement, got: {stmt}"),
        };

        let expr = match &expr {
            Expression::Loop(expr) => expr,
            _ => panic!("not a valid loop expression, got: {expr}"),
        };

        assert_eq!(expr.body.statements.len(), 1);
        match &expr.body.statements[0] {
            Statement::Break(stmt) => {
                assert_literal(&stmt.value, &Literal::Ident("x"));
            }
            _ => panic!("not a valid expression statement"),
        }
    }
}

#[test]
fn test_loop_expression_without_value() {
    let input = "loop { break; }";
    let program = parse_program(input, 1);

    for stmt in &program.statements {
        let expr = match stmt {
            Statement::Expression(stmt) => {
                assert_eq!(stmt.token.kind, TokenKind::Loop);
                assert_eq!(stmt.token.literal, "loop");
                &stmt.value
            }
            _ => panic!("not a valid expression statement, got: {stmt}"),
        };

        let expr = match &expr {
            Expression::Loop(expr) => expr,
            _ => panic!("not a valid loop expression, got: {expr}"),
        };

        assert_eq!(expr.body.statements.len(), 1);
        match &expr.body.statements[0] {
            Statement::Break(stmt) => match stmt.value {
                Expression::Null(_) => (),
                _ => panic!("not a valid null expression, got {:?}", stmt.value),
            },
            _ => panic!("not a valid break statement"),
        }
    }
}
