use super::*;
use crate::compiler::Compiler;
use crate::parser::tests::parse_program;

fn run_vm_test(input: &str, statements: usize, value: Object) {
    let prog = parse_program(input, statements);
    let mut compiler = Compiler::new();
    let _ = compiler
        .compile_program(&prog)
        .map_err(|e| panic!("compiler error: {e}"));
    let mut vm = Vm::new(Some(compiler.bytecode()));
    let result = vm.run().unwrap_or_else(|e| panic!("vm error:\n {e}"));
    assert_eq!(
        *result, value,
        "unexpected result, got {result:?}, want {value:?}"
    );
}

#[test]
fn test_integer_arithmetic() {
    let tests = [
        ("1", 1),
        ("2", 2),
        ("1 + 2", 3),
        ("1 - 2", -1),
        ("1 * 2", 2),
        ("4 / 2", 2),
        ("50 / 2 * 2 + 10 - 5", 55),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("5 * (2 + 10)", 60),
        ("-5", -5),
        ("-10", -10),
        ("-50 + 100 + -50", 0),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];
    for (test_input, test_value) in tests {
        run_vm_test(test_input, 1, Object::new_integer(test_value));
    }
}

#[test]
fn test_boolean_expressions() {
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
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
        ("!(if (false) { 5; })", true),
    ];
    for (test_input, test_value) in tests {
        run_vm_test(test_input, 1, Object::new_boolean(test_value));
    }
}

#[test]
fn test_conditionals() {
    let tests = [
        ("if (true) { 10 }", Object::new_integer(10)),
        ("if (true) { 10 } else { 20 }", Object::new_integer(10)),
        ("if (false) { 10 } else { 20 } ", Object::new_integer(20)),
        ("if (1) { 10 }", Object::new_integer(10)),
        ("if (1 < 2) { 10 }", Object::new_integer(10)),
        ("if (1 < 2) { 10 } else { 20 }", Object::new_integer(10)),
        ("if (1 > 2) { 10 } else { 20 }", Object::new_integer(20)),
        ("if (1 > 2) { 10 }", Object::Null),
        ("if (false) { 10 }", Object::Null),
        (
            "if ((if (false) { 10 })) { 10 } else { 20 }",
            Object::new_integer(20),
        ),
    ];
    for (test_input, test_value) in tests {
        run_vm_test(test_input, 1, test_value);
    }
}

#[test]
fn test_global_let_statements() {
    let tests = [
        ("let one = 1; one", 2, 1),
        ("let one = 1; let two = 2; one + two", 3, 3),
        ("let one = 1; let two = one + one; one + two", 3, 3),
    ];
    for (test_input, test_stmts, test_value) in tests {
        run_vm_test(test_input, test_stmts, Object::new_integer(test_value));
    }
}

#[test]
fn test_string_expressions() {
    let tests = [
        (r#""monkey""#, "monkey"),
        (r#""mon" + "key""#, "monkey"),
        (r#""mon" + "key" + "banana""#, "monkeybanana"),
    ];
    for (test_input, test_value) in tests {
        run_vm_test(test_input, 1, Object::new_string(test_value.into()));
    }
}

#[test]
fn test_array_literals() {
    let tests = [
        ("[]", vec![]),
        ("[1, 2, 3]", vec![1, 2, 3]),
        ("[1 + 2, 3 * 4, 5 + 6]", vec![3, 12, 11]),
    ];
    for (test_input, test_value) in tests {
        let mut test_array = vec![];
        for v in test_value {
            test_array.push(Rc::new(Object::new_integer(v)));
        }
        run_vm_test(test_input, 1, Object::Array(test_array));
    }
}

#[test]
fn test_hash_literals() {
    let tests = [
        ("{}", vec![]),
        ("{1: 2, 2:3}", vec![(1, 2), (2, 3)]),
        ("{1 + 1: 2 * 2, 3 + 3: 4 * 4}", vec![(2, 4), (6, 16)]),
    ];
    for (test_input, test_value) in tests {
        let mut test_hash = HashMap::new();
        for (k, v) in test_value {
            let key = Object::new_integer(k);
            let value = Object::new_integer(v);
            let hash = key.hash_key().expect("couldn't generate hash key");
            let pair = (Rc::new(key), Rc::new(value));
            test_hash.insert(hash, pair);
        }
        run_vm_test(test_input, 1, Object::Hash(test_hash));
    }
}

#[test]
fn test_index_expressions() {
    let tests = [
        ("[1, 2, 3][1]", Object::new_integer(2)),
        ("[1, 2, 3][0 + 2]", Object::new_integer(3)),
        ("[[1, 1, 1]][0][0]", Object::new_integer(1)),
        ("[][0]", Object::Null),
        ("[1, 2, 3][99]", Object::Null),
        ("[1][-1]", Object::Null),
        ("{1: 1, 2: 2}[1]", Object::new_integer(1)),
        ("{1: 1, 2: 2}[2]", Object::new_integer(2)),
        ("{1: 1}[0]", Object::Null),
        ("{}[0]", Object::Null),
    ];
    for (test_input, test_value) in tests {
        run_vm_test(test_input, 1, test_value);
    }
}

#[test]
fn test_calling_functions_without_arguments() {
    let tests = [
        ("let fivePlusTen = fn() { 5 + 10; }; fivePlusTen();", 2, 15),
        (
            "let one = fn() { 1; }; let two = fn() { 2; }; one() + two()",
            3,
            3,
        ),
        (
            "let a = fn() { 1 }; let b = fn() { a() + 1 }; let c = fn() { b() + 1 }; c();",
            4,
            3,
        ),
    ];
    for (test_input, test_stmts, test_value) in tests {
        run_vm_test(test_input, test_stmts, Object::new_integer(test_value));
    }
}

#[test]
fn test_functions_with_return_statement() {
    let tests = [
        (
            "let earlyExit = fn() { return 99; 100; }; earlyExit();",
            2,
            99,
        ),
        (
            "let earlyExit = fn() { return 99; return 100; }; earlyExit();",
            2,
            99,
        ),
    ];
    for (test_input, test_stmts, test_value) in tests {
        run_vm_test(test_input, test_stmts, Object::new_integer(test_value));
    }
}

#[test]
fn test_functions_without_return_value() {
    let tests = [
        (
            " let noReturn = fn() { }; noReturn();",
            2,
        ),
        (
            "let noReturn = fn() { }; let noReturnTwo = fn() { noReturn(); }; noReturn(); noReturnTwo();",
            4,
        ),
    ];
    for (test_input, test_stmts) in tests {
        run_vm_test(test_input, test_stmts, Object::Null);
    }
}

#[test]
fn test_first_class_functions() {
    let tests = [
        (
            concat!(
                "let returnsOne = fn() { 1; };\n",
                "let returnsOneReturner = fn() { returnsOne; };\n",
                "returnsOneReturner()();"
            ),
            3,
            1,
        ),
        (
            concat!(
                "let returnsOneReturner = fn() {\n",
                "    let returnsOne = fn() { 1; };\n",
                "    returnsOne;\n",
                "};\n",
                "returnsOneReturner()();"
            ),
            2,
            1,
        ),
    ];
    for (test_input, test_stmts, test_value) in tests {
        run_vm_test(test_input, test_stmts, Object::new_integer(test_value));
    }
}

#[test]
fn test_calling_functions_with_bindings() {
    let tests = [
        ("let one = fn() { let one = 1; one }; one();", 2, 1),
        (
            "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; oneAndTwo();",
            2,
            3,
        ),
        (
            concat!(
                "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };\n",
                "let threeAndFour = fn() { let three = 3; let four = 4; three + four; };\n",
                "oneAndTwo() + threeAndFour();"
            ),
            3,
            10,
        ),
        (
            concat!(
                "let firstFoobar = fn() { let foobar = 50; foobar; };\n",
                "let secondFoobar = fn() { let foobar = 100; foobar; };\n",
                "firstFoobar() + secondFoobar();",
            ),
            3,
            150,
        ),
        (
            concat!(
                "let globalSeed = 50;\n",
                "let minusOne = fn() {\n",
                "    let num = 1;\n",
                "    globalSeed - num;\n",
                "}\n",
                "let minusTwo = fn() {\n",
                "    let num = 2;\n",
                "    globalSeed - num;\n",
                "}\n",
                "minusOne() + minusTwo();",
            ),
            4,
            97,
        ),
    ];
    for (test_input, test_stmts, test_value) in tests {
        run_vm_test(test_input, test_stmts, Object::new_integer(test_value));
    }
}

#[test]
fn test_calling_functions_with_arguments_and_bindings() {
    let tests = [
        ("let identity = fn(a) { a; }; identity(4);", 2, 4),
        ("let sum = fn(a, b) { a + b; }; sum(1, 2);", 2, 3),
        ("let sum = fn(a, b) { let c = a + b; c; }; sum(1, 2);", 2, 3),
        (
            "let sum = fn(a, b) { let c = a + b; c; }; sum(1, 2) + sum(3, 4);",
            2,
            10,
        ),
        (
            concat!(
                "let sum = fn(a, b) { let c = a + b; c; };\n",
                "let outer = fn() { sum(1, 2) + sum(3, 4); };\n",
                "outer();"
            ),
            3,
            10,
        ),
        (
            concat!(
                "let globalNum = 10;\n",
                "let sum = fn(a, b) {\n",
                "    let c = a + b;\n",
                "    c + globalNum;\n",
                "};\n",
                "let outer = fn() {\n",
                "    sum(1, 2) + sum(3, 4) + globalNum;\n",
                "};\n",
                "outer() + globalNum;"
            ),
            4,
            50,
        ),
    ];
    for (test_input, test_stmts, test_value) in tests {
        run_vm_test(test_input, test_stmts, Object::new_integer(test_value));
    }
}

#[test]
fn test_calling_functions_with_wrong_arguments() {
    let tests = [
        ("fn() { 1; }(1);", 1, 0, 1),
        ("fn(a) { a; }();", 1, 1, 0),
        ("fn(a, b) { a + b; }(1);", 1, 2, 1),
    ];
    for (test_input, test_stmts, test_want, test_got) in tests {
        let prog = parse_program(test_input, test_stmts);
        let mut compiler = Compiler::new();
        let _ = compiler
            .compile_program(&prog)
            .map_err(|e| panic!("compiler error: {e:?}"));
        let mut vm = Vm::new(Some(compiler.bytecode()));
        let result = vm.run();
        assert_eq!(
            result,
            Err(VmError::WrongNumberArguments(test_want, test_got))
        );
    }
}

#[test]
fn test_builtin_functions() {
    let tests = [
        ("len(\"\")", 1, Object::new_integer(0)),
        ("len(\"four\")", 1, Object::new_integer(4)),
        ("len(\"hello world\")", 1, Object::new_integer(11)),
        ("len([1, 2, 3])", 1, Object::new_integer(3)),
        ("len([])", 1, Object::new_integer(0)),
        (r#"puts("")"#, 1, Object::None),
        ("first([1, 2, 3])", 1, Object::new_integer(1)),
        ("first([])", 1, Object::Null),
        ("last([1, 2, 3])", 1, Object::new_integer(3)),
        ("last([])", 1, Object::Null),
        (
            "rest([1, 2, 3])",
            1,
            Object::Array(vec![
                Rc::new(Object::new_integer(2)),
                Rc::new(Object::new_integer(3)),
            ]),
        ),
        ("rest([])", 1, Object::Null),
        (
            "push([], 1)",
            1,
            Object::Array(vec![Rc::new(Object::new_integer(1))]),
        ),
        (
            "let list = [1,2,3]; insert(list, 1, 9)",
            2,
            Object::Array(vec![
                Rc::new(Object::new_integer(1)),
                Rc::new(Object::new_integer(9)),
                Rc::new(Object::new_integer(2)),
                Rc::new(Object::new_integer(3)),
            ]),
        ),
        (
            "let list = [1,2,3]; map(list, string)",
            2,
            Object::Array(vec![
                Rc::new(Object::new_string("1".into())),
                Rc::new(Object::new_string("2".into())),
                Rc::new(Object::new_string("3".into())),
            ]),
        ),
    ];
    for (test_input, test_stmts, test_value) in tests {
        run_vm_test(test_input, test_stmts, test_value);
    }
}

#[test]
#[should_panic(expected = "vm error")]
fn test_builtin_functions_error() {
    let tests = [
        (
            "len(1)",
            1,
            Object::Error("argument to \"len\" not supported, got INTEGER".into()),
        ),
        (
            r#"len("one", "two")"#,
            1,
            Object::Error("wrong number of arguments to \"len\". got=2, want=1".into()),
        ),
        (
            "first(1)",
            1,
            Object::Error("argument to \"first\" not supported, got INTEGER".into()),
        ),
        (
            "last(1)",
            1,
            Object::Error("argument to \"last\" not supported, got INTEGER".into()),
        ),
        (
            "push(1, 1)",
            1,
            Object::Error("arguments to \"push\" not supported, got INTEGER, INTEGER".into()),
        ),
    ];
    for (test_input, test_stmts, test_value) in tests {
        run_vm_test(test_input, test_stmts, test_value);
    }
}

#[test]
fn test_closures() {
    let tests = [
        (
            concat!(
                "let newClosure = fn(a) {\n",
                "    fn() { a; };\n",
                "};\n",
                "let closure = newClosure(99);\n",
                "closure();\n",
            ),
            3,
            99,
        ),
        (
            concat!(
                "let newAdder = fn(a, b) {\n",
                "    fn(c) { a + b + c };\n",
                "};\n",
                "let adder = newAdder(1, 2);\n",
                "adder(8);",
            ),
            3,
            11,
        ),
        (
            concat!(
                "let newAdder = fn(a, b) {\n",
                "    let c = a + b;\n",
                "    fn(d) { c + d };\n",
                "};\n",
                "let adder = newAdder(1, 2);\n",
                "adder(8);",
            ),
            3,
            11,
        ),
        (
            concat!(
                "let newAdderOuter = fn(a, b) {\n",
                "    let c = a + b;\n",
                "    fn(d) {\n",
                "        let e = d + c;\n",
                "        fn(f) { e + f; };\n",
                "    };\n",
                "};\n",
                "let newAdderInner = newAdderOuter(1, 2)\n",
                "let adder = newAdderInner(3);\n",
                "adder(8);\n",
            ),
            4,
            14,
        ),
        (
            concat!(
                "let a = 1;\n",
                "let newAdderOuter = fn(b) {\n",
                "    fn(c) {\n",
                "        fn(d) { a + b + c + d };\n",
                "    };\n",
                "};\n",
                "let newAdderInner = newAdderOuter(2)\n",
                "let adder = newAdderInner(3);\n",
                "adder(8);\n",
            ),
            5,
            14,
        ),
        (
            concat!(
                "let newClosure = fn(a, b) {\n",
                "    let one = fn() { a; };\n",
                "    let two = fn() { b; };\n",
                "    fn() { one() + two(); };\n",
                "};\n",
                "let closure = newClosure(9, 90);\n",
                "closure();\n",
            ),
            3,
            99,
        ),
    ];
    for (test_input, test_stmts, test_value) in tests {
        run_vm_test(test_input, test_stmts, Object::new_integer(test_value));
    }
}

#[test]
fn test_recursive_functions() {
    let tests = [
        (
            concat!(
                "let countDown = fn(x) {\n",
                "    if (x == 0) {\n",
                "        return 0;\n",
                "    } else {\n",
                "        countDown(x - 1);\n",
                "    }\n",
                "};\n",
                "countDown(1);\n",
            ),
            2,
            0,
        ),
        (
            concat!(
                "let countDown = fn(x) {\n",
                "    if (x == 0) {\n",
                "        return 0;\n",
                "    } else {\n",
                "        countDown(x - 1);\n",
                "    }\n",
                "};\n",
                "let wrapper = fn() {\n",
                "    countDown(1);\n",
                "};\n",
                "wrapper();\n",
            ),
            3,
            0,
        ),
        (
            concat!(
                "let wrapper = fn() {\n",
                "    let countDown = fn(x) {\n",
                "        if (x == 0) {\n",
                "            return 0;\n",
                "        } else {\n",
                "            countDown(x - 1);\n",
                "        }\n",
                "    };\n",
                "    countDown(1);\n",
                "};\n",
                "wrapper();\n",
            ),
            2,
            0,
        ),
    ];
    for (test_input, test_stmts, test_value) in tests {
        run_vm_test(test_input, test_stmts, Object::new_integer(test_value));
    }
}

#[test]
fn test_recursive_fibonacci() {
    let input = concat!(
        "let fibonacci = fn(x) {\n",
        "    if (x == 0) {\n",
        "        return 0;\n",
        "    } else {\n",
        "        if (x == 1) {\n",
        "            return 1;\n",
        "        } else {\n",
        "            fibonacci(x - 1) + fibonacci(x - 2);\n",
        "        }\n",
        "    }\n",
        "};\n",
        "fibonacci(15);\n",
    );
    run_vm_test(input, 2, Object::new_integer(610));
}

#[test]
fn test_loop_expression() {
    let tests = [
        ("loop { break; }", 1, Object::Null),
        (
            "let a = 0; loop { if (a > 5) { break 6; }; let a = a + 1; }",
            2,
            Object::new_integer(6),
        ),
        // (
        //     "let a = 0; fn() { loop { if (a > 5) { break 6; }; let a = a + 1; return 2; } }()",
        //     2,
        //     Object::new_integer(2),
        // ),
        (
            "let a = 0; loop { if (a > 5) { break \"foo\"; }; let a = a + 1; }",
            2,
            Object::new_string("foo".into()),
        ),
        (
            "let a = 0; let b = loop { if (a > 5) { break \"bar\"; }; let a = a + 1; }; b;",
            3,
            Object::new_string("bar".into()),
        ),
        (
            "let a = 0; loop { if (a > 5) { break; }; let a = a + 1; }",
            2,
            Object::Null,
        ),
        (
            "let a = 0; loop { if (a > 5) { break a; }; let b = 0; loop { if (b > 10) { break b; } let b = b + 1; }; let a = a + 1; }",
            2,
            Object::new_integer(6),
        ),
    ];

    for (test_input, test_stmts, test_value) in tests {
        run_vm_test(test_input, test_stmts, test_value);
    }
}
