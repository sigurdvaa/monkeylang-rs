use crate::ast::{Expression, Program, Statement};
use crate::code::{make_ins, Instruction, Opcode};
use crate::object::Object;

struct Bytecode {
    instructions: Vec<Instruction>,
    constants: Vec<Object>,
}

#[derive(Default)]
struct Compiler {
    instructions: Vec<Instruction>,
    constants: Vec<Object>,
}

impl Compiler {
    fn new() -> Self {
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
            }
            _ => todo!("{expr:?}"),
        }
        Ok(())
    }

    fn compile_statement(&mut self, stmt: &Statement) -> Result<(), CompilerError> {
        match stmt {
            // Statement::Let(expr) => self.compile_expression(&expr.value)?,
            // Statement::Return(expr) => self.compile_expression(&expr.value)?,
            Statement::Expression(expr) => self.compile_expression(&expr.value)?,
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

    fn compile_program(&mut self, prog: &Program) -> Result<(), CompilerError> {
        self.compile_statements(&prog.statements)?;
        Ok(())
    }

    fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }
}

enum CompilerError {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::code::{make_ins, Opcode};
    use crate::parser::tests::parse_program;

    #[test]
    fn test_integer_arithmetic() {
        let tests = [(
            "1 + 2",
            vec![Object::new_integer(1), Object::new_integer(2)],
            [
                make_ins(Opcode::Constant, &[0]),
                make_ins(Opcode::Constant, &[1]),
            ]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>(),
        )];

        for (test_input, test_constants, test_instructions) in tests {
            let program = parse_program(test_input, 1);
            let mut compiler = Compiler::new();

            // TODO: verify no errors?
            let _ = compiler.compile_program(&program);

            let bytecode = compiler.bytecode();
            assert_eq!(bytecode.constants, test_constants);
            assert_eq!(bytecode.instructions, test_instructions);
            // TODO: do we need better testing of const - Object?
        }
    }
}
