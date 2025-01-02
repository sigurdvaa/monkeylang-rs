use crate::ast::{Expression, Operator, Program, Statement};
use crate::code::{make_ins, Instruction, Opcode};
use crate::object::Object;

#[derive(Debug)]
pub enum CompilerError {
    UnknownOperator(String),
}

pub struct Bytecode {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Object>,
}

#[derive(Default)]
pub struct Compiler {
    instructions: Vec<Instruction>,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
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
        let ins = make_ins(op, operands);
        self.add_instruction(ins)
    }

    fn compile_expression(&mut self, expr: &Expression) -> Result<(), CompilerError> {
        match expr {
            Expression::Integer(expr) => {
                let obj = Object::new_integer(expr.value as isize);
                let operands = &[self.add_constant(obj)];
                self.emit(Opcode::Constant, operands);
            }
            Expression::Infix(expr) => {
                self.compile_expression(&expr.left)?;
                self.compile_expression(&expr.right)?;
                match expr.operator {
                    Operator::Plus => {
                        let _ = self.emit(Opcode::Add, &[]);
                    }
                    Operator::Minus => {
                        let _ = self.emit(Opcode::Sub, &[]);
                    }
                    Operator::Asterisk => {
                        let _ = self.emit(Opcode::Mul, &[]);
                    }
                    Operator::Slash => {
                        let _ = self.emit(Opcode::Div, &[]);
                    }
                    _ => {
                        return Err(CompilerError::UnknownOperator(format!(
                            "unknown infix operator: {}",
                            expr.operator
                        )))
                    }
                }
            }
            _ => todo!("{expr:?}"),
        }
        Ok(())
    }

    fn compile_statement(&mut self, stmt: &Statement) -> Result<(), CompilerError> {
        match stmt {
            // Statement::Let(expr) => self.compile_expression(&expr.value)?,
            // Statement::Return(expr) => self.compile_expression(&expr.value)?,
            Statement::Expression(expr) => {
                self.compile_expression(&expr.value)?;
                self.emit(Opcode::Pop, &[]);
            }
            _ => todo!(),
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
        fn new(
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
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = [
            TestCase::new(
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
            TestCase::new(
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
            TestCase::new(
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
            TestCase::new(
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
            TestCase::new(
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
        ];

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
}
