#[cfg(test)]
use super::*;

#[cfg(test)]
fn test_eval(input: &str) -> Object {
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    let env = Environment::new();
    let lexer = Lexer::new(None, input.chars().peekable());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    eval_program(&program, env)
}

#[test]
fn test_eval_integer_expression() {
    let tests = [
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];
    for (test_input, test_value) in &tests {
        assert_eq!(test_eval(test_input), Object::Integer(*test_value));
    }
}

#[test]
fn test_eval_boolean_expression() {
    let tests = [
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
    ];
    for (test_input, test_value) in &tests {
        assert_eq!(test_eval(test_input), Object::Boolean(*test_value));
    }
}

#[test]
fn test_bang_operator() {
    let tests = [
        ("!false", true),
        ("!true", false),
        ("!5", false),
        ("!!false", false),
        ("!!true", true),
        ("!!5", true),
    ];
    for (test_input, test_value) in &tests {
        assert_eq!(test_eval(test_input), Object::Boolean(*test_value));
    }
}

#[test]
fn test_if_else_expression() {
    let tests = [
        ("if (true) { 10 }", Object::Integer(10)),
        ("if (false) { 10 }", Object::Null),
        ("if (1) { 10 }", Object::Integer(10)),
        ("if (1 < 2) { 10 }", Object::Integer(10)),
        ("if (1 > 2) { 10 }", Object::Null),
        ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
        ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
    ];
    for (test_input, test_value) in &tests {
        assert_eq!(test_eval(test_input), *test_value);
    }
}

#[test]
fn test_return_statements() {
    let tests = [
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        ("if (10 > 1) { return 10; } return 1; }", 10),
    ];
    for (test_input, test_value) in &tests {
        assert_eq!(test_eval(test_input), Object::Integer(*test_value));
    }
}

#[test]
fn test_error_handling() {
    let tests = [
        ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
        ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
        ("-true", "unknown operator: -BOOLEAN"),
        ("true + false;", "unknown boolean operator: +"),
        ("5; true + false; 5", "unknown boolean operator: +"),
        (
            "if (10 > 1) { true + false; }",
            "unknown boolean operator: +",
        ),
        (
            "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
            "unknown boolean operator: +",
        ),
        ("foobar", "identifier not found: foobar"),
    ];
    for (test_input, test_value) in &tests {
        assert_eq!(test_eval(test_input), Object::Error(test_value.to_string()));
    }
}

#[test]
fn test_let_statements() {
    let tests = [
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    ];
    for (test_input, test_value) in &tests {
        assert_eq!(test_eval(test_input), Object::Integer(*test_value));
    }
}

#[test]
fn test_function_object() {
    let input = "fn(x) { x + 2; };";
    let eval = test_eval(input);
    match eval {
        Object::Function(func) => {
            assert_eq!(func.parameters.len(), 1);
            assert_eq!(func.parameters[0].value, "x");
            assert_eq!(func.body.to_string(), "(x + 2)");
        }
        _ => panic!("object is not Function, got {eval:?}"),
    }
}

#[test]
fn test_function_application() {
    let tests = [
        ("let identity = fn(x) { x; }; identity(5);", 5),
        ("let identity = fn(x) { return x; }; identity(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        ("fn(x) { x; }(5)", 5),
    ];
    for (test_input, test_value) in &tests {
        assert_eq!(test_eval(test_input), Object::Integer(*test_value));
    }
}
