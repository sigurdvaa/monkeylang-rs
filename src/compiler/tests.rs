use super::*;
use crate::code::{make_ins, tests::ReadInstructions, Opcode};
use crate::parser::tests::parse_program;

fn make_compiled_func(
    ins: Vec<Vec<Instruction>>,
    num_locals: usize,
    num_parameters: usize,
) -> Object {
    Object::CompiledFunction(Rc::new(CompiledFunctionObj {
        instructions: ins.into_iter().flatten().collect(),
        num_locals,
        num_parameters,
    }))
}

struct TestCase {
    input: &'static str,
    statements: usize,
    constants: Vec<Rc<Object>>,
    instructions: Vec<Instruction>,
}

impl TestCase {
    fn new(
        input: &'static str,
        statements: usize,
        constants: Vec<Object>,
        instructions: Vec<Vec<Instruction>>,
    ) -> Self {
        Self {
            input,
            statements,
            constants: constants.into_iter().map(Rc::new).collect(),
            instructions: instructions.into_iter().flatten().collect(),
        }
    }
    fn new_integer(
        input: &'static str,
        statements: usize,
        constants: Vec<usize>,
        instructions: Vec<Vec<Instruction>>,
    ) -> Self {
        Self {
            input,
            statements,
            constants: constants
                .into_iter()
                .map(|i| Rc::new(Object::new_integer(i as isize)))
                .collect(),
            instructions: instructions.into_iter().flatten().collect(),
        }
    }

    fn new_string(
        input: &'static str,
        statements: usize,
        constants: Vec<&'static str>,
        instructions: Vec<Vec<Instruction>>,
    ) -> Self {
        Self {
            input,
            statements,
            constants: constants
                .into_iter()
                .map(|i| Rc::new(Object::new_string(i.into())))
                .collect(),
            instructions: instructions.into_iter().flatten().collect(),
        }
    }
}

fn run_compiler_tests(tests: &[TestCase]) {
    for test in tests {
        let program = parse_program(test.input, test.statements);
        let mut compiler = Compiler::new();

        let _ = compiler
            .compile_program(&program)
            .map_err(|e| panic!("compile error:\n {e}"));

        let bytecode = compiler.bytecode();
        assert_eq!(bytecode.constants, test.constants);
        assert_eq!(
            bytecode.instructions.to_string(),
            test.instructions.to_string()
        );
    }
}

#[test]
fn test_integer_arithmetic() {
    let tests = [
        TestCase::new_integer(
            "1 + 2",
            1,
            vec![1, 2],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Add, &[]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new_integer(
            "1; 2",
            2,
            vec![1, 2],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Pop, &[]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new_integer(
            "1 - 2",
            1,
            vec![1, 2],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Sub, &[]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new_integer(
            "1 * 2",
            1,
            vec![1, 2],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Mul, &[]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new_integer(
            "2 / 1",
            1,
            vec![2, 1],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Div, &[]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new_integer(
            "-1",
            1,
            vec![1],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Minus, &[]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
    ];
    run_compiler_tests(&tests);
}

#[test]
fn test_boolean_expressions() {
    let tests = [
        TestCase::new_integer(
            "true",
            1,
            vec![],
            vec![make_ins(Opcode::True, &[]), make_ins(Opcode::Pop, &[])],
        ),
        TestCase::new_integer(
            "false",
            1,
            vec![],
            vec![make_ins(Opcode::False, &[]), make_ins(Opcode::Pop, &[])],
        ),
        TestCase::new_integer(
            "1 > 2",
            1,
            vec![1, 2],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Gt, &[]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new_integer(
            "1 < 2",
            1,
            vec![1, 2],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Lt, &[]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new_integer(
            "1 == 2",
            1,
            vec![1, 2],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Eq, &[]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new_integer(
            "1 != 2",
            1,
            vec![1, 2],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::NotEq, &[]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new_integer(
            "true == false",
            1,
            vec![],
            vec![
                make_ins(Opcode::True, &[]),
                make_ins(Opcode::False, &[]),
                make_ins(Opcode::Eq, &[]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new_integer(
            "true != false",
            1,
            vec![],
            vec![
                make_ins(Opcode::True, &[]),
                make_ins(Opcode::False, &[]),
                make_ins(Opcode::NotEq, &[]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new_integer(
            "!true",
            1,
            vec![],
            vec![
                make_ins(Opcode::True, &[]),
                make_ins(Opcode::Bang, &[]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
    ];
    run_compiler_tests(&tests);
}

#[test]
fn test_conditionals() {
    let tests = [
        TestCase::new_integer(
            "if (true) { 10 }; 3333;",
            2,
            vec![10, 3333],
            vec![
                make_ins(Opcode::True, &[]),
                make_ins(Opcode::JumpNotTrue, &[10]),
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Jump, &[11]),
                make_ins(Opcode::Null, &[]),
                make_ins(Opcode::Pop, &[]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new_integer(
            "if (true) { 10 } else { 20 }; 3333;",
            2,
            vec![10, 20, 3333],
            vec![
                make_ins(Opcode::True, &[]),
                make_ins(Opcode::JumpNotTrue, &[10]),
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Jump, &[13]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Pop, &[]),
                make_ins(Opcode::Constant, &[2]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
    ];
    run_compiler_tests(&tests);
}

#[test]
fn test_global_let_statements() {
    let tests = [
        TestCase::new_integer(
            "let one = 1; let two = 2;",
            2,
            vec![1, 2],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::SetGlobal, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::SetGlobal, &[1]),
            ],
        ),
        TestCase::new_integer(
            "let one = 1; one",
            2,
            vec![1],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::SetGlobal, &[0]),
                make_ins(Opcode::GetGlobal, &[0]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new_integer(
            "let one = 1; let two = one; two",
            3,
            vec![1],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::SetGlobal, &[0]),
                make_ins(Opcode::GetGlobal, &[0]),
                make_ins(Opcode::SetGlobal, &[1]),
                make_ins(Opcode::GetGlobal, &[1]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
    ];
    run_compiler_tests(&tests);
}

#[test]
fn test_string_expressions() {
    let tests = [
        TestCase::new_string(
            r#""monkey""#,
            1,
            vec!["monkey"],
            vec![make_ins(Opcode::Constant, &[0]), make_ins(Opcode::Pop, &[])],
        ),
        TestCase::new_string(
            r#""mon" + "key""#,
            1,
            vec!["mon", "key"],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Add, &[]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
    ];
    run_compiler_tests(&tests);
}

#[test]
fn test_array_literals() {
    let tests = [
        TestCase::new_integer(
            "[]",
            1,
            vec![],
            vec![make_ins(Opcode::Array, &[0]), make_ins(Opcode::Pop, &[])],
        ),
        TestCase::new_integer(
            "[1, 2, 3]",
            1,
            vec![1, 2, 3],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Constant, &[2]),
                make_ins(Opcode::Array, &[3]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new_integer(
            "[1 + 2, 3 - 4, 5 * 6]",
            1,
            vec![1, 2, 3, 4, 5, 6],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Add, &[]),
                make_ins(Opcode::Constant, &[2]),
                make_ins(Opcode::Constant, &[3]),
                make_ins(Opcode::Sub, &[]),
                make_ins(Opcode::Constant, &[4]),
                make_ins(Opcode::Constant, &[5]),
                make_ins(Opcode::Mul, &[]),
                make_ins(Opcode::Array, &[3]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
    ];
    run_compiler_tests(&tests);
}

#[test]
fn test_hash_literal() {
    let tests = [
        TestCase::new_integer(
            "{}",
            1,
            vec![],
            vec![make_ins(Opcode::Hash, &[0]), make_ins(Opcode::Pop, &[])],
        ),
        TestCase::new_integer(
            "{1: 2, 3: 4, 5: 6}",
            1,
            vec![1, 2, 3, 4, 5, 6],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Constant, &[2]),
                make_ins(Opcode::Constant, &[3]),
                make_ins(Opcode::Constant, &[4]),
                make_ins(Opcode::Constant, &[5]),
                make_ins(Opcode::Hash, &[6]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new_integer(
            "{1: 2 + 3, 4: 5 * 6}",
            1,
            vec![1, 2, 3, 4, 5, 6],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Constant, &[2]),
                make_ins(Opcode::Add, &[]),
                make_ins(Opcode::Constant, &[3]),
                make_ins(Opcode::Constant, &[4]),
                make_ins(Opcode::Constant, &[5]),
                make_ins(Opcode::Mul, &[]),
                make_ins(Opcode::Hash, &[4]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
    ];
    run_compiler_tests(&tests);
}

#[test]
fn test_index_expressions() {
    let tests = [
        TestCase::new_integer(
            "[1, 2, 3][1 + 1]",
            1,
            vec![1, 2, 3, 1, 1],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Constant, &[2]),
                make_ins(Opcode::Array, &[3]),
                make_ins(Opcode::Constant, &[3]),
                make_ins(Opcode::Constant, &[4]),
                make_ins(Opcode::Add, &[]),
                make_ins(Opcode::Index, &[]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new_integer(
            "{1: 2}[2 - 1]",
            1,
            vec![1, 2, 2, 1],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Hash, &[2]),
                make_ins(Opcode::Constant, &[2]),
                make_ins(Opcode::Constant, &[3]),
                make_ins(Opcode::Sub, &[]),
                make_ins(Opcode::Index, &[]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
    ];
    run_compiler_tests(&tests);
}

#[test]
fn test_functions() {
    let tests = [
        TestCase::new(
            "fn() { return 5 + 10 }",
            1,
            vec![
                Object::new_integer(5),
                Object::new_integer(10),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::Constant, &[0]),
                        make_ins(Opcode::Constant, &[1]),
                        make_ins(Opcode::Add, &[]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    0,
                    0,
                ),
            ],
            vec![
                make_ins(Opcode::Closure, &[2, 0]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new(
            "fn() { 5 + 10 }",
            1,
            vec![
                Object::new_integer(5),
                Object::new_integer(10),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::Constant, &[0]),
                        make_ins(Opcode::Constant, &[1]),
                        make_ins(Opcode::Add, &[]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    0,
                    0,
                ),
            ],
            vec![
                make_ins(Opcode::Closure, &[2, 0]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new(
            "fn() { 1; 2 }",
            1,
            vec![
                Object::new_integer(1),
                Object::new_integer(2),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::Constant, &[0]),
                        make_ins(Opcode::Pop, &[]),
                        make_ins(Opcode::Constant, &[1]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    0,
                    0,
                ),
            ],
            vec![
                make_ins(Opcode::Closure, &[2, 0]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
    ];
    run_compiler_tests(&tests);
}

#[test]
fn test_functions_without_return_value() {
    let tests = [TestCase::new(
        "fn() { }",
        1,
        vec![make_compiled_func(
            vec![make_ins(Opcode::Return, &[])],
            0,
            0,
        )],
        vec![
            make_ins(Opcode::Closure, &[0, 0]),
            make_ins(Opcode::Pop, &[]),
        ],
    )];
    run_compiler_tests(&tests);
}

#[test]
fn test_compiler_scopes() {
    let mut compiler = Compiler::new();
    let global = compiler.symbols.clone();
    assert_eq!(compiler.scope_idx, 0);

    compiler.emit(Opcode::Mul, &[]);

    compiler.enter_scope();
    assert_eq!(compiler.scope_idx, 1);

    compiler.emit(Opcode::Sub, &[]);
    assert_eq!(compiler.curr_scope_ins_len(), 1);
    assert_eq!(
        compiler.scopes[compiler.scope_idx].first_prev_ins,
        Some(EmittedIns {
            op: Opcode::Sub,
            pos: 0
        })
    );

    assert_ne!(
        global, compiler.symbols,
        "compiler did not enclose symbolTable"
    );

    compiler.leave_scope();
    assert_eq!(compiler.scope_idx, 0);

    assert_eq!(
        global, compiler.symbols,
        "compiler did not restore global symbol table"
    );
    assert_eq!(
        compiler.symbols.outer, None,
        "compiler modified global symbol table incorrectly"
    );

    compiler.emit(Opcode::Add, &[]);
    assert_eq!(compiler.curr_scope_ins_len(), 2);
    assert_eq!(
        compiler.scopes[compiler.scope_idx].first_prev_ins,
        Some(EmittedIns {
            op: Opcode::Add,
            pos: 1
        })
    );
    assert_eq!(
        compiler.scopes[compiler.scope_idx].second_prev_ins,
        Some(EmittedIns {
            op: Opcode::Mul,
            pos: 0
        })
    );
}

#[test]
fn test_function_calls() {
    let tests = [
        TestCase::new(
            "fn() { 24 }();",
            1,
            vec![
                Object::new_integer(24),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::Constant, &[0]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    0,
                    0,
                ),
            ],
            vec![
                make_ins(Opcode::Closure, &[1, 0]),
                make_ins(Opcode::Call, &[0]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new(
            "let noArg = fn() { 24 }; noArg();",
            2,
            vec![
                Object::new_integer(24),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::Constant, &[0]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    0,
                    0,
                ),
            ],
            vec![
                make_ins(Opcode::Closure, &[1, 0]),
                make_ins(Opcode::SetGlobal, &[0]),
                make_ins(Opcode::GetGlobal, &[0]),
                make_ins(Opcode::Call, &[0]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new(
            "let oneArg = fn(a) { a }; oneArg(24);",
            2,
            vec![
                make_compiled_func(
                    vec![
                        make_ins(Opcode::GetLocal, &[0]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    1,
                    1,
                ),
                Object::new_integer(24),
            ],
            vec![
                make_ins(Opcode::Closure, &[0, 0]),
                make_ins(Opcode::SetGlobal, &[0]),
                make_ins(Opcode::GetGlobal, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Call, &[1]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new(
            "let manyArg = fn(a, b, c) { a; b; c }; manyArg(24, 25, 26);",
            2,
            vec![
                make_compiled_func(
                    vec![
                        make_ins(Opcode::GetLocal, &[0]),
                        make_ins(Opcode::Pop, &[]),
                        make_ins(Opcode::GetLocal, &[1]),
                        make_ins(Opcode::Pop, &[]),
                        make_ins(Opcode::GetLocal, &[2]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    3,
                    3,
                ),
                Object::new_integer(24),
                Object::new_integer(25),
                Object::new_integer(26),
            ],
            vec![
                make_ins(Opcode::Closure, &[0, 0]),
                make_ins(Opcode::SetGlobal, &[0]),
                make_ins(Opcode::GetGlobal, &[0]),
                make_ins(Opcode::Constant, &[1]),
                make_ins(Opcode::Constant, &[2]),
                make_ins(Opcode::Constant, &[3]),
                make_ins(Opcode::Call, &[3]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
    ];
    run_compiler_tests(&tests);
}

#[test]
fn test_let_statement_scopes() {
    let tests = [
        TestCase::new(
            "let num = 55; fn() { num }",
            2,
            vec![
                Object::new_integer(55),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::GetGlobal, &[0]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    0,
                    0,
                ),
            ],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::SetGlobal, &[0]),
                make_ins(Opcode::Closure, &[1, 0]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new(
            "fn() { let num = 55; num }",
            1,
            vec![
                Object::new_integer(55),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::Constant, &[0]),
                        make_ins(Opcode::SetLocal, &[0]),
                        make_ins(Opcode::GetLocal, &[0]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    1,
                    0,
                ),
            ],
            vec![
                make_ins(Opcode::Closure, &[1, 0]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new(
            "fn() { let a = 55; let b = 77; a + b }",
            1,
            vec![
                Object::new_integer(55),
                Object::new_integer(77),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::Constant, &[0]),
                        make_ins(Opcode::SetLocal, &[0]),
                        make_ins(Opcode::Constant, &[1]),
                        make_ins(Opcode::SetLocal, &[1]),
                        make_ins(Opcode::GetLocal, &[0]),
                        make_ins(Opcode::GetLocal, &[1]),
                        make_ins(Opcode::Add, &[]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    2,
                    0,
                ),
            ],
            vec![
                make_ins(Opcode::Closure, &[2, 0]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
    ];
    run_compiler_tests(&tests);
}

#[test]
fn test_builtins() {
    let tests = [
        TestCase::new(
            "len([]); push([], 1);",
            2,
            vec![Object::new_integer(1)],
            vec![
                make_ins(Opcode::GetBuiltin, &[0]),
                make_ins(Opcode::Array, &[0]),
                make_ins(Opcode::Call, &[1]),
                make_ins(Opcode::Pop, &[]),
                make_ins(Opcode::GetBuiltin, &[4]),
                make_ins(Opcode::Array, &[0]),
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Call, &[2]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new(
            "fn() { len([]) }",
            1,
            vec![make_compiled_func(
                vec![
                    make_ins(Opcode::GetBuiltin, &[0]),
                    make_ins(Opcode::Array, &[0]),
                    make_ins(Opcode::Call, &[1]),
                    make_ins(Opcode::ReturnValue, &[]),
                ],
                0,
                0,
            )],
            vec![
                make_ins(Opcode::Closure, &[0, 0]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
    ];
    run_compiler_tests(&tests);
}

#[test]
fn test_closures() {
    let tests = [
        TestCase::new(
            "fn(a) { fn(b) { a + b } }",
            1,
            vec![
                make_compiled_func(
                    vec![
                        make_ins(Opcode::GetFree, &[0]),
                        make_ins(Opcode::GetLocal, &[0]),
                        make_ins(Opcode::Add, &[]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    1,
                    1,
                ),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::GetLocal, &[0]),
                        make_ins(Opcode::Closure, &[0, 1]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    1,
                    1,
                ),
            ],
            vec![
                make_ins(Opcode::Closure, &[1, 0]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new(
            "fn(a) { fn(b) { fn(c) { a + b + c } } };",
            1,
            vec![
                make_compiled_func(
                    vec![
                        make_ins(Opcode::GetFree, &[0]),
                        make_ins(Opcode::GetFree, &[1]),
                        make_ins(Opcode::Add, &[]),
                        make_ins(Opcode::GetLocal, &[0]),
                        make_ins(Opcode::Add, &[]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    1,
                    1,
                ),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::GetFree, &[0]),
                        make_ins(Opcode::GetLocal, &[0]),
                        make_ins(Opcode::Closure, &[0, 2]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    1,
                    1,
                ),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::GetLocal, &[0]),
                        make_ins(Opcode::Closure, &[1, 1]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    1,
                    1,
                ),
            ],
            vec![
                make_ins(Opcode::Closure, &[2, 0]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new(
            concat!(
                "let global = 55;\n",
                "fn() {\n",
                "    let a = 66;\n",
                "    fn() {\n",
                "        let b = 77;\n",
                "        fn() {\n",
                "            let c = 88;\n",
                "            global + a + b + c;\n",
                "        }\n",
                "    }\n",
                "}\n",
            ),
            2,
            vec![
                Object::new_integer(55),
                Object::new_integer(66),
                Object::new_integer(77),
                Object::new_integer(88),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::Constant, &[3]),
                        make_ins(Opcode::SetLocal, &[0]),
                        make_ins(Opcode::GetGlobal, &[0]),
                        make_ins(Opcode::GetFree, &[0]),
                        make_ins(Opcode::Add, &[]),
                        make_ins(Opcode::GetFree, &[1]),
                        make_ins(Opcode::Add, &[]),
                        make_ins(Opcode::GetLocal, &[0]),
                        make_ins(Opcode::Add, &[]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    1,
                    0,
                ),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::Constant, &[2]),
                        make_ins(Opcode::SetLocal, &[0]),
                        make_ins(Opcode::GetFree, &[0]),
                        make_ins(Opcode::GetLocal, &[0]),
                        make_ins(Opcode::Closure, &[4, 2]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    1,
                    0,
                ),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::Constant, &[1]),
                        make_ins(Opcode::SetLocal, &[0]),
                        make_ins(Opcode::GetLocal, &[0]),
                        make_ins(Opcode::Closure, &[5, 1]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    1,
                    0,
                ),
            ],
            vec![
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::SetGlobal, &[0]),
                make_ins(Opcode::Closure, &[6, 0]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
    ];
    run_compiler_tests(&tests);
}

#[test]
fn test_recursive_functions() {
    let tests = [
        TestCase::new(
            "let countDown = fn(x) { countDown(x - 1); }; countDown(1);",
            2,
            vec![
                Object::new_integer(1),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::CurrentClosure, &[]),
                        make_ins(Opcode::GetLocal, &[0]),
                        make_ins(Opcode::Constant, &[0]),
                        make_ins(Opcode::Sub, &[]),
                        make_ins(Opcode::Call, &[1]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    1,
                    1,
                ),
                Object::new_integer(1),
            ],
            vec![
                make_ins(Opcode::Closure, &[1, 0]),
                make_ins(Opcode::SetGlobal, &[0]),
                make_ins(Opcode::GetGlobal, &[0]),
                make_ins(Opcode::Constant, &[2]),
                make_ins(Opcode::Call, &[1]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
        TestCase::new(
            concat!(
                "let wrapper = fn() {\n",
                "    let countDown = fn(x) { countDown(x - 1); };\n",
                "    countDown(1);\n",
                "};\n",
                "wrapper();\n",
            ),
            2,
            vec![
                Object::new_integer(1),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::CurrentClosure, &[]),
                        make_ins(Opcode::GetLocal, &[0]),
                        make_ins(Opcode::Constant, &[0]),
                        make_ins(Opcode::Sub, &[]),
                        make_ins(Opcode::Call, &[1]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    1,
                    1,
                ),
                Object::new_integer(1),
                make_compiled_func(
                    vec![
                        make_ins(Opcode::Closure, &[1, 0]),
                        make_ins(Opcode::SetLocal, &[0]),
                        make_ins(Opcode::GetLocal, &[0]),
                        make_ins(Opcode::Constant, &[2]),
                        make_ins(Opcode::Call, &[1]),
                        make_ins(Opcode::ReturnValue, &[]),
                    ],
                    1,
                    0,
                ),
            ],
            vec![
                make_ins(Opcode::Closure, &[3, 0]),
                make_ins(Opcode::SetGlobal, &[0]),
                make_ins(Opcode::GetGlobal, &[0]),
                make_ins(Opcode::Call, &[0]),
                make_ins(Opcode::Pop, &[]),
            ],
        ),
    ];
    run_compiler_tests(&tests);
}
