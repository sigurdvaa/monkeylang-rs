use super::*;
use crate::parser::tests::parse_program;

pub fn test_eval(input: &str, stmts: usize) -> Rc<Object> {
    let program = parse_program(input, stmts);
    let mut eval = Eval::new();
    eval.eval_program(&program)
}

#[test]
fn test_eval_integer_expression() {
    let tests = [
        ("5", 1, 5),
        ("10", 1, 10),
        ("-5", 1, -5),
        ("-10", 1, -10),
        ("5 + 5 + 5 + 5 - 10", 1, 10),
        ("2 * 2 * 2 * 2 * 2", 1, 32),
        ("-50 + 100 + -50", 1, 0),
        ("5 * 2 + 10", 1, 20),
        ("5 + 2 * 10", 1, 25),
        ("20 + 2 * -10", 1, 0),
        ("50 / 2 * 2 + 10", 1, 60),
        ("2 * (5 + 10)", 1, 30),
        ("3 * 3 * 3 + 10", 1, 37),
        ("3 * (3 * 3) + 10", 1, 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 1, 50),
    ];
    for (test_input, test_stmts, test_value) in tests {
        assert_eq!(
            *test_eval(test_input, test_stmts),
            Object::Integer(test_value)
        );
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
        ("\"foo\" != \"bar\"", true),
        ("\"foo\" == \"bar\"", false),
        ("\"foo\" == \"foo\"", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
        ("(\"foo\" == \"bar\") == false", true),
        ("(\"foo\" != \"bar\") == true", true),
        ("(\"foo\" == \"foo\") == true", true),
        ("null == null", true),
        ("null != null", false),
        ("1 != null", true),
        ("1 == null", false),
        ("\"foo\" != null", true),
        ("\"foo\" == null", false),
    ];
    for (test_input, test_value) in &tests {
        assert_eq!(*test_eval(test_input, 1), Object::Boolean(*test_value));
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
        assert_eq!(*test_eval(test_input, 1), Object::Boolean(*test_value));
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
    for (test_input, test_value) in tests {
        assert_eq!(*test_eval(test_input, 1), test_value);
    }
}

#[test]
fn test_return_statements() {
    let tests = [
        ("fn() { return 10; }()", 10),
        ("fn() { return 10; 9; }()", 10),
        ("fn() { return 2 * 5; 9; }()", 10),
        ("fn() { 9; return 2 * 5; 9; }()", 10),
        ("fn() { if (10 > 1) { return 10; } return 1; }()", 10),
    ];
    for (test_input, test_value) in tests {
        assert_eq!(
            test_eval(test_input, 1),
            Rc::new(Object::Integer(test_value))
        );
    }
}

#[test]
fn test_error_handling() {
    let tests = [
        ("5 + true;", 1, "type mismatch: INTEGER + BOOLEAN"),
        ("5 + true; 5;", 2, "type mismatch: INTEGER + BOOLEAN"),
        ("-true", 1, "unknown operator: -BOOLEAN"),
        ("true + false;", 1, "unknown boolean operator: +"),
        ("5; true + false; 5", 3, "unknown boolean operator: +"),
        (
            "if (10 > 1) { true + false; }",
            1,
            "unknown boolean operator: +",
        ),
        (
            "fn() { if (10 > 1) { if (10 > 1) { return true + false; } return 1; } }()",
            1,
            "unknown boolean operator: +",
        ),
        ("foobar", 1, "identifier not found: foobar"),
        ("\"Hello\" - \"World\"", 1, "unknown string operator: -"),
        (
            r#"{"name": "Monkey"}[fn(x) { x }];"#,
            1,
            "type not supported as hash key: FUNCTION",
        ),
    ];
    for (test_input, test_stmts, test_value) in tests {
        assert_eq!(
            *test_eval(test_input, test_stmts),
            Object::Error(test_value.to_string())
        );
    }
}

#[test]
fn test_let_statements() {
    let tests = [
        ("let a = 5; a;", 2, 5),
        ("let a = 5 * 5; a;", 2, 25),
        ("let a = 5; let b = a; b;", 3, 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 4, 15),
    ];
    for (test_input, test_stmts, test_value) in tests {
        assert_eq!(
            *test_eval(test_input, test_stmts),
            Object::Integer(test_value)
        );
    }
}

#[test]
fn test_function_object() {
    let input = "fn(x) { x + 2; };";
    let eval = test_eval(input, 1);
    match &*eval {
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
        ("let identity = fn(x) { x; }; identity(5);", 2, 5),
        ("let identity = fn(x) { return x; }; identity(5);", 2, 5),
        ("let double = fn(x) { x * 2; }; double(5);", 2, 10),
        ("let add = fn(x, y) { x + y; }; add(5, 5);", 2, 10),
        (
            "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
            2,
            20,
        ),
        ("fn(x) { x; }(5)", 1, 5),
    ];
    for (test_input, test_stmts, test_value) in tests {
        assert_eq!(
            *test_eval(test_input, test_stmts),
            Object::Integer(test_value)
        );
    }
}

#[test]
fn test_closures() {
    let input = concat!(
        "let newAdder = fn(x) {\n",
        "  fn(y) { x + y };\n",
        "};\n",
        "\n",
        "let addTwo = newAdder(2);\n",
        "addTwo(2);"
    );
    assert_eq!(*test_eval(input, 3), Object::Integer(4));
}

#[test]
fn test_string_literal() {
    let (test_input, test_value) = ("\"Hello World!\";", "Hello World!");
    assert_eq!(*test_eval(test_input, 1), Object::String(test_value.into()));
}

#[test]
fn test_string_concatenation() {
    let (test_input, test_value) = ("\"Hello\" + \" \" + \"World!\";", "Hello World!");
    assert_eq!(*test_eval(test_input, 1), Object::String(test_value.into()));
}

#[test]
fn test_builtin_functions() {
    let tests = [
        ("len(\"\")", 1, Object::Integer(0)),
        ("len(\"four\")", 1, Object::Integer(4)),
        ("len(\"hello world\")", 1, Object::Integer(11)),
        (
            "len(1)",
            1,
            Object::Error("argument to \"len\" not supported, got INTEGER".into()),
        ),
        (
            "len(\"one\", \"two\")",
            1,
            Object::Error("wrong number of arguments to \"len\". got=2, want=1".into()),
        ),
        (
            "let list = [1, 2, 3]; insert(list, 1, 9)",
            2,
            Object::Array(vec![
                Rc::new(Object::Integer(1)),
                Rc::new(Object::Integer(9)),
                Rc::new(Object::Integer(2)),
                Rc::new(Object::Integer(3)),
            ]),
        ),
        (
            "let hashmap = {1: \"one\"}; insert(hashmap, 2, \"two\")[2]",
            2,
            Object::String("two".into()),
        ),
        (
            "let list = [1, 2, 3]; map(list, string)",
            2,
            Object::Array(vec![
                Rc::new(Object::String("1".into())),
                Rc::new(Object::String("2".into())),
                Rc::new(Object::String("3".into())),
            ]),
        ),
        (
            "let double = fn(x) { x * 2 }; let list = [1, 2, 3]; map(list, double)",
            3,
            Object::Array(vec![
                Rc::new(Object::Integer(2)),
                Rc::new(Object::Integer(4)),
                Rc::new(Object::Integer(6)),
            ]),
        ),
    ];
    for (test_input, test_stmts, test_value) in tests {
        assert_eq!(*test_eval(test_input, test_stmts), test_value);
    }
}

#[test]
fn test_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";
    assert_eq!(
        test_eval(input, 1),
        Rc::new(Object::Array(vec![
            Rc::new(Object::Integer(1)),
            Rc::new(Object::Integer(4)),
            Rc::new(Object::Integer(6)),
        ]))
    );
}

#[test]
fn test_array_index_expressions() {
    let tests = [
        ("[1, 2, 3][0]", 1, Object::Integer(1)),
        ("[1, 2, 3][1]", 1, Object::Integer(2)),
        ("[1, 2, 3][2]", 1, Object::Integer(3)),
        ("let i = 0; [1][i];", 2, Object::Integer(1)),
        ("[1, 2, 3][1 + 1];", 1, Object::Integer(3)),
        (
            "let myArray = [1, 2, 3]; myArray[2];",
            2,
            Object::Integer(3),
        ),
        (
            "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            2,
            Object::Integer(6),
        ),
        (
            "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
            3,
            Object::Integer(2),
        ),
        ("[1, 2, 3][3]", 1, Object::Null),
        ("[1, 2, 3][-1]", 1, Object::Null),
    ];
    for (test_input, test_stmts, test_value) in tests {
        assert_eq!(*test_eval(test_input, test_stmts), test_value);
    }
}

#[test]
fn test_hash_literals() {
    let input = concat!(
        "let two = \"two\";\n",
        "{\n",
        "  \"one\": 10 - 9,\n",
        "  two: 1 + 1,\n",
        "  \"thr\" + \"ee\": 6 / 2,\n",
        "  4: 4,\n",
        "  true: 5,\n",
        "  false: 6\n",
        "}\n",
    );
    let mut objutil = ObjectUtil::new();
    let one = Rc::new(Object::String("one".into()));
    let two = Rc::new(Object::String("two".into()));
    let three = Rc::new(Object::String("three".into()));
    let four = Rc::new(Object::Integer(4));
    let tests = HashMap::from([
        (
            objutil.hash_key(one.clone()).unwrap(),
            (one, Object::Integer(1)),
        ),
        (
            objutil.hash_key(two.clone()).unwrap(),
            (two, Object::Integer(2)),
        ),
        (
            objutil.hash_key(three.clone()).unwrap(),
            (three, Object::Integer(3)),
        ),
        (
            objutil.hash_key(four.clone()).unwrap(),
            (four, Object::Integer(4)),
        ),
        (
            objutil.hash_key(objutil.obj_true.clone()).unwrap(),
            (objutil.obj_true.clone(), Object::Integer(5)),
        ),
        (
            objutil.hash_key(objutil.obj_false.clone()).unwrap(),
            (objutil.obj_false.clone(), Object::Integer(6)),
        ),
    ]);

    let eval = test_eval(input, 2);
    match eval.as_ref() {
        Object::Hash(hash) => {
            assert_eq!(hash.len(), 6);
            for (hash, (key, value)) in hash {
                let (test_key, test_value) = &tests[hash];
                let _ = objutil.hash_key(test_key.clone());
                assert_eq!(key, test_key);
                assert_eq!(**value, *test_value);
            }
        }
        _ => panic!("object is not Hash, got {eval:?}"),
    }
}

#[test]
fn test_hash_index_expressions() {
    let tests = [
        (r#"{"foo": 5}["foo"]"#, 1, Object::Integer(5)),
        (r#"{"foo": 5}["bar"]"#, 1, Object::Null),
        (r#"let key = "foo"; {"foo": 5}[key]"#, 2, Object::Integer(5)),
        (r#"{}["foo"]"#, 1, Object::Null),
        (r#"{5: 5}[5]"#, 1, Object::Integer(5)),
        (r#"{true: 5}[true]"#, 1, Object::Integer(5)),
        (r#"{false: 5}[false]"#, 1, Object::Integer(5)),
    ];

    for (test_input, test_stmts, test_value) in tests {
        assert_eq!(*test_eval(test_input, test_stmts), test_value);
    }
}

#[test]
fn test_loop_expressions() {
    let tests = [
        (
            "let a = 0; loop { if (a > 5) { break 6; }; let a = a + 1; }",
            2,
            Object::Integer(6),
        ),
        (
            "fn(x) { let a = x; loop { if (a > 5) { break 6; }; let a = a + 1; return 2; } }(0)",
            1,
            Object::Integer(2),
        ),
        (
            "let a = 0; loop { if (a > 5) { break \"foo\"; }; let a = a + 1; }",
            2,
            Object::String("foo".into()),
        ),
        (
            "let a = 0; let b = loop { if (a > 5) { break \"bar\"; }; let a = a + 1; }; b;",
            3,
            Object::String("bar".into()),
        ),
        (
            "let a = 0; loop { if (a > 5) { break; }; let a = a + 1; }",
            2,
            Object::Null,
        ),
    ];

    for (test_input, test_stmts, test_value) in tests {
        assert_eq!(*test_eval(test_input, test_stmts), test_value);
    }
}
