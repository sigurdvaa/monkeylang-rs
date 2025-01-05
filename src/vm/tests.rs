use super::*;
use crate::compiler::Compiler;
use crate::parser::tests::parse_program;

fn run_vm_test(input: &str, statements: usize, value: Object) {
    let prog = parse_program(input, statements);
    let mut compiler = Compiler::new();
    let _ = compiler
        .compile_program(&prog)
        .map_err(|e| panic!("compiler error: {e:?}"));
    let mut vm = Vm::new(Some(compiler.bytecode()));
    let result = vm.run().unwrap_or_else(|e| panic!("vm error:\n {e}"));
    assert_eq!(
        result, value,
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
        ("if (1 > 2) { 10 }", Object::new_null()),
        ("if (false) { 10 }", Object::new_null()),
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
        ("[][0]", Object::new_null()),
        ("[1, 2, 3][99]", Object::new_null()),
        ("[1][-1]", Object::new_null()),
        ("{1: 1, 2: 2}[1]", Object::new_integer(1)),
        ("{1: 1, 2: 2}[2]", Object::new_integer(2)),
        ("{1: 1}[0]", Object::new_null()),
        ("{}[0]", Object::new_null()),
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
        run_vm_test(test_input, test_stmts, Object::new_null());
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
