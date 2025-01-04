mod symbol;
#[cfg(test)]
mod tests;

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

#[derive(Default)]
struct CompilationScope {
    instructions: Vec<Instruction>,
    first_prev_ins: Option<EmittedIns>,
    second_prev_ins: Option<EmittedIns>,
}

impl CompilationScope {
    fn new() -> Self {
        Self::default()
    }
}

#[derive(Debug, PartialEq)]
struct EmittedIns {
    op: Opcode,
    pos: usize,
}

pub struct Compiler {
    constants: Vec<Object>,
    symbols: SymbolTable,
    scopes: Vec<CompilationScope>,
    // TODO: remove idx, use vec len/last?
    scope_idx: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            constants: vec![],
            symbols: SymbolTable::new(),
            scopes: vec![CompilationScope::new()],
            scope_idx: 0,
        }
    }

    pub fn soft_reset(&mut self) {
        self.scopes.clear();
        self.scopes.push(CompilationScope::new());
        self.scope_idx = 0;
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn add_instruction(&mut self, ins: Vec<Instruction>) -> usize {
        let curr_ins = &mut self.scopes[self.scope_idx].instructions;
        let pos = curr_ins.len();
        curr_ins.extend(ins);
        pos
    }

    fn curr_scope_ins_len(&self) -> usize {
        self.scopes[self.scope_idx].instructions.len()
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope::new();
        self.scopes.push(scope);
        self.scope_idx += 1;
    }
    fn leave_scope(&mut self) -> Vec<Instruction> {
        let ins = self.scopes.pop().expect("can't leave last scope");
        self.scope_idx -= 1;
        ins.instructions
    }

    fn set_prev_ins(&mut self, op: Opcode, pos: usize) {
        let curr_scope = &mut self.scopes[self.scope_idx];
        std::mem::swap(
            &mut curr_scope.first_prev_ins,
            &mut curr_scope.second_prev_ins,
        );
        curr_scope.first_prev_ins = Some(EmittedIns { op, pos });
    }

    fn emit(&mut self, op: Opcode, operands: &[usize]) -> usize {
        let ins = make_ins(op.clone(), operands);
        let pos = self.add_instruction(ins);
        self.set_prev_ins(op, pos);
        pos
    }

    fn remove_last_pop(&mut self) {
        let curr_scope = &mut self.scopes[self.scope_idx];
        if let Some(EmittedIns {
            op: Opcode::Pop,
            pos,
        }) = curr_scope.first_prev_ins
        {
            curr_scope.instructions.remove(pos);
            std::mem::swap(
                &mut curr_scope.first_prev_ins,
                &mut curr_scope.second_prev_ins,
            );
        }
    }

    fn replace_last_pop_with_return(&mut self) {
        let curr_scope = &mut self.scopes[self.scope_idx];
        if let Some(EmittedIns {
            op: Opcode::Pop,
            pos,
        }) = curr_scope.first_prev_ins
        {
            if let Some(emitted) = &mut curr_scope.first_prev_ins {
                emitted.op = Opcode::ReturnValue;
            }
            let new_ins = make_ins(Opcode::ReturnValue, &[]);
            self.replace_ins(pos, new_ins);
        }
    }

    fn emit_return_if_missing(&mut self) {
        let curr_scope = &mut self.scopes[self.scope_idx];
        if !matches!(
            curr_scope.first_prev_ins,
            Some(EmittedIns {
                op: Opcode::ReturnValue,
                pos: _,
            })
        ) {
            self.emit(Opcode::Return, &[]);
        }
    }

    fn replace_ins(&mut self, pos: usize, new_ins: Vec<Instruction>) {
        let curr_scope = &mut self.scopes[self.scope_idx];
        for (i, ins) in new_ins.into_iter().enumerate() {
            curr_scope.instructions[pos + i] = ins;
        }
    }

    fn change_operand(&mut self, pos: usize, operand: usize) {
        let curr_scope = &mut self.scopes[self.scope_idx];
        let op = Opcode::try_from(curr_scope.instructions[pos]).unwrap_or_else(|_| {
            panic!(
                "can't replace operand, unknown opcode at position {pos}: {}",
                curr_scope.instructions[pos]
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
            Expression::Function(expr) => {
                self.enter_scope();

                self.compile_statements(&expr.body.statements)?;
                self.replace_last_pop_with_return();
                self.emit_return_if_missing();

                let ins = self.leave_scope();

                let obj = Object::CompiledFunction(ins);
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
                let after_consequence_pos = self.curr_scope_ins_len();
                self.change_operand(jumpnottrue_pos, after_consequence_pos);

                if let Some(alt) = &expr.alternative {
                    self.compile_statements(&alt.statements)?;
                    self.remove_last_pop();
                } else {
                    let _ = self.emit(Opcode::Null, &[]);
                }

                let after_alternative_pos = self.curr_scope_ins_len();
                self.change_operand(jump_pos, after_alternative_pos);

                after_alternative_pos
            }
            Expression::Call(expr) => {
                self.compile_expression(expr.function.as_ref())?;
                self.emit(Opcode::Call, &[])
            }
            Expression::Identifier(expr) => match self.symbols.resolve(&expr.value) {
                Some(sym) => {
                    let index = sym.index;
                    self.emit(Opcode::GetGlobal, &[index])
                }
                None => return Err(CompilerError::UndefinedVariable(expr.value.clone())),
            },
            Expression::Index(expr) => {
                self.compile_expression(&expr.left)?;
                self.compile_expression(&expr.index)?;
                self.emit(Opcode::Index, &[])
            }
            Expression::Macro(_expr) => {
                unreachable!("macros must be evaluated before generating bytecode")
            }
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
            Statement::Return(expr) => {
                self.compile_expression(&expr.value)?;
                self.emit(Opcode::ReturnValue, &[]);
            }
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
            instructions: self.scopes[self.scope_idx].instructions.clone(),
            constants: self.constants.clone(),
        }
    }
}
