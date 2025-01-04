mod symbol;

use std::fmt::Display;

use crate::ast::{Expression, Operator, Program, Statement};
use crate::code::{make_ins, Instruction, Opcode};
use crate::object::Object;
pub use symbol::SymbolTable;

#[derive(Debug)]
pub enum CompilerError {
    UnknownPrefixOperator(Operator),
    UnknownInfixOperator(Operator),
    UndefinedVariable(String),
}

impl std::error::Error for CompilerError {}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownPrefixOperator(op) => write!(f, "unknown prefix operator: {op:?}"),
            Self::UnknownInfixOperator(op) => write!(f, "unknown infix operator: {op:?}"),
            Self::UndefinedVariable(name) => write!(f, "undefined variable: {name}"),
        }
    }
}

pub struct Bytecode {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Object>,
}

struct EmittedIns {
    op: Opcode,
    pos: usize,
}

pub struct Compiler {
    instructions: Vec<Instruction>,
    constants: Vec<Object>,
    first_prev_ins: Option<EmittedIns>,
    second_prev_ins: Option<EmittedIns>,
    symbols: SymbolTable,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
            constants: vec![],
            first_prev_ins: None,
            second_prev_ins: None,
            symbols: SymbolTable::new(),
        }
    }

    pub fn soft_reset(&mut self) {
        self.instructions.clear();
        self.first_prev_ins = None;
        self.second_prev_ins = None;
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn add_instruction(&mut self, ins: Vec<Instruction>) -> usize {
        let pos = self.instructions.len();
        self.instructions.extend(ins);
        pos
    }

    fn emit(&mut self, op: Opcode, operands: &[usize]) -> usize {
        let ins = make_ins(op.clone(), operands);
        let pos = self.add_instruction(ins);

        std::mem::swap(&mut self.first_prev_ins, &mut self.second_prev_ins);
        self.first_prev_ins = Some(EmittedIns { op, pos });

        pos
    }

    fn remove_last_pop(&mut self) {
        if let Some(EmittedIns {
            op: Opcode::Pop,
            pos,
        }) = self.first_prev_ins
        {
            self.instructions.remove(pos);
            std::mem::swap(&mut self.first_prev_ins, &mut self.second_prev_ins);
        }
    }

    fn replace_ins(&mut self, pos: usize, new_ins: Vec<Instruction>) {
        for (i, ins) in new_ins.into_iter().enumerate() {
            self.instructions[pos + i] = ins;
        }
    }

    fn change_operand(&mut self, pos: usize, operand: usize) {
        let op = Opcode::try_from(self.instructions[pos]).unwrap_or_else(|_| {
            panic!(
                "can't replace operand, unknown opcode at position {pos}: {}",
                self.instructions[pos]
            )
        });
        let new_ins = make_ins(op, &[operand]);
        self.replace_ins(pos, new_ins);
    }

    fn compile_expression(&mut self, expr: &Expression) -> Result<(), CompilerError> {
        let _ = match expr {
            Expression::Boolean(expr) => match expr.value {
                true => self.emit(Opcode::True, &[]),
                false => self.emit(Opcode::False, &[]),
            },
            Expression::Integer(expr) => {
                let obj = Object::new_integer(expr.value as isize);
                let operands = &[self.add_constant(obj)];
                self.emit(Opcode::Constant, operands)
            }
            Expression::String(expr) => {
                let obj = Object::new_string(expr.value.clone());
                let operands = &[self.add_constant(obj)];
                self.emit(Opcode::Constant, operands)
            }
            Expression::Array(expr) => {
                for element in &expr.elements {
                    self.compile_expression(element)?;
                }
                self.emit(Opcode::Array, &[expr.elements.len()])
            }
            Expression::Hash(expr) => {
                for (key, value) in &expr.pairs {
                    self.compile_expression(key)?;
                    self.compile_expression(value)?;
                }
                self.emit(Opcode::Hash, &[expr.pairs.len() * 2])
            }
            Expression::Null(expr) => todo!("{expr:?}"),
            Expression::Infix(expr) => {
                self.compile_expression(&expr.left)?;
                self.compile_expression(&expr.right)?;
                match expr.operator {
                    Operator::Plus => self.emit(Opcode::Add, &[]),
                    Operator::Minus => self.emit(Opcode::Sub, &[]),
                    Operator::Asterisk => self.emit(Opcode::Mul, &[]),
                    Operator::Slash => self.emit(Opcode::Div, &[]),
                    Operator::Gt => self.emit(Opcode::Gt, &[]),
                    Operator::Lt => self.emit(Opcode::Lt, &[]),
                    Operator::Eq => self.emit(Opcode::Eq, &[]),
                    Operator::NotEq => self.emit(Opcode::NotEq, &[]),
                    _ => return Err(CompilerError::UnknownInfixOperator(expr.operator.clone())),
                }
            }
            Expression::Prefix(expr) => {
                self.compile_expression(&expr.right)?;
                match expr.operator {
                    Operator::Bang => self.emit(Opcode::Bang, &[]),
                    Operator::Minus => self.emit(Opcode::Minus, &[]),
                    _ => return Err(CompilerError::UnknownPrefixOperator(expr.operator.clone())),
                }
            }
            Expression::If(expr) => {
                self.compile_expression(&expr.condition)?;
                let jumpnottrue_pos = self.emit(Opcode::JumpNotTrue, &[0]); // tmp bogus value

                self.compile_statements(&expr.consequence.statements)?;
                self.remove_last_pop();

                let jump_pos = self.emit(Opcode::Jump, &[0]); // tmp bogus value
                let after_consequence_pos = self.instructions.len();
                self.change_operand(jumpnottrue_pos, after_consequence_pos);

                if let Some(alt) = &expr.alternative {
                    self.compile_statements(&alt.statements)?;
                    self.remove_last_pop();
                } else {
                    let _ = self.emit(Opcode::Null, &[]);
                }

                let after_alternative_pos = self.instructions.len();
                self.change_operand(jump_pos, after_alternative_pos);

                after_alternative_pos
            }
            Expression::Call(expr) => todo!("{expr:?}"),
            Expression::Function(expr) => todo!("{expr:?}"),
            Expression::Identifier(expr) => match self.symbols.resolve(&expr.value) {
                Some(sym) => {
                    let index = sym.index;
                    self.emit(Opcode::GetGlobal, &[index])
                }
                None => return Err(CompilerError::UndefinedVariable(expr.value.clone())),
            },
            Expression::Index(expr) => todo!("{expr:?}"),
            Expression::Macro(expr) => todo!("{expr:?}"),
        };
        Ok(())
    }

    fn compile_statement(&mut self, stmt: &Statement) -> Result<(), CompilerError> {
        match stmt {
            Statement::Let(expr) => {
                self.compile_expression(&expr.value)?;
                let sym = self.symbols.define(expr.name.value.clone());
                let index = sym.index;
                self.emit(Opcode::SetGlobal, &[index]);
            }
            Statement::Return(_expr) => todo!(),
            Statement::Expression(expr) => {
                self.compile_expression(&expr.value)?;
                self.emit(Opcode::Pop, &[]);
            }
        }
        Ok(())
    }

    fn compile_statements(&mut self, stmts: &[Statement]) -> Result<(), CompilerError> {
        for stmt in stmts {
            self.compile_statement(stmt)?;
        }
        Ok(())
    }

    pub fn compile_program(&mut self, prog: &Program) -> Result<(), CompilerError> {
        self.compile_statements(&prog.statements)?;
        Ok(())
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::code::{make_ins, Instructions, Opcode};
    use crate::parser::tests::parse_program;

    struct TestCase {
        input: &'static str,
        statements: usize,
        constants: Vec<Object>,
        instructions: Vec<Instruction>,
    }

    impl TestCase {
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
                    .map(|i| Object::new_integer(i as isize))
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
                    .map(|i| Object::new_string(i.into()))
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
                .map_err(|e| panic!("compile error: {e:?}"));

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
                vec![make_ins(Opcode::True, &[0]), make_ins(Opcode::Pop, &[])],
            ),
            TestCase::new_integer(
                "false",
                1,
                vec![],
                vec![make_ins(Opcode::False, &[0]), make_ins(Opcode::Pop, &[])],
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
}
