mod symbol;
#[cfg(test)]
mod tests;

use crate::ast::{Expression, Operator, Program, Statement};
use crate::code::{make_ins, Instruction, Opcode};
use crate::object::{builtins, CompiledFunctionObj, Object};
use std::fmt::Display;
use std::rc::Rc;
use symbol::Symbol;
pub use symbol::{SymbolScope, SymbolTable, Symbols};

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
    pub constants: Vec<Rc<Object>>,
}

#[derive(Default)]
struct CompilationScope {
    instructions: Vec<Instruction>,
    first_prev_ins: Option<EmittedIns>,
    second_prev_ins: Option<EmittedIns>,
    loops: Vec<Vec<usize>>,
    loops_idx: usize,
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
    constants: Vec<Rc<Object>>,
    symbols: Symbols,
    scopes: Vec<CompilationScope>,
    scope_idx: usize,
}

impl Compiler {
    pub fn new() -> Self {
        let symbols = SymbolTable::new();
        for (i, (name, _builtin)) in builtins::get_all().iter().enumerate() {
            symbols.define_builtin(i, name.to_string());
        }
        Self {
            constants: vec![],
            symbols,
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
        // TODO: store idx in a map?
        if let Some(idx) = self.constants.iter().position(|v| v.as_ref() == &obj) {
            return idx;
        }
        self.constants.push(Rc::new(obj));
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
        self.symbols = SymbolTable::new_enclosed(self.symbols.clone());
    }

    fn leave_scope(&mut self) -> Vec<Instruction> {
        self.symbols = self
            .symbols
            .outer
            .clone()
            .expect("no outer scope to restore");
        let ins = self.scopes.pop().expect("popped last scope");
        self.scope_idx -= 1;
        ins.instructions
    }

    fn enter_loop(&mut self) {
        self.scopes[self.scope_idx].loops.push(vec![]);
        self.scopes[self.scope_idx].loops_idx += 1;
    }

    fn leave_loop(&mut self) -> Vec<usize> {
        self.scopes[self.scope_idx].loops_idx -= 1;
        self.scopes[self.scope_idx]
            .loops
            .pop()
            .expect("popped last loop")
    }

    fn push_break(&mut self, pos: usize) {
        let curr = &mut self.scopes[self.scope_idx];
        curr.loops[curr.loops_idx - 1].push(pos);
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

    fn load_symbol(&mut self, sym: &Symbol) -> usize {
        match sym.scope {
            SymbolScope::Global => self.emit(Opcode::GetGlobal, &[sym.index]),
            SymbolScope::Free => self.emit(Opcode::GetFree, &[sym.index]),
            SymbolScope::Local => self.emit(Opcode::GetLocal, &[sym.index]),
            SymbolScope::Builtin => self.emit(Opcode::GetBuiltin, &[sym.index]),
            SymbolScope::Function => self.emit(Opcode::CurrentClosure, &[]),
        }
    }

    fn compile_expression(&mut self, expr: &Expression) -> Result<(), CompilerError> {
        _ = match expr {
            Expression::Boolean(expr) => match expr.value {
                true => self.emit(Opcode::True, &[]),
                false => self.emit(Opcode::False, &[]),
            },
            Expression::Integer(expr) => {
                let obj = Object::Integer(expr.value as isize);
                let operands = &[self.add_constant(obj)];
                self.emit(Opcode::Constant, operands)
            }
            Expression::String(expr) => {
                let obj = Object::String(expr.value.clone());
                let operands = &[self.add_constant(obj)];
                self.emit(Opcode::Constant, operands)
            }
            Expression::Function(expr) => {
                self.enter_scope();

                if let Some(name) = &expr.name {
                    self.symbols.define_function_name(name.into());
                }

                for param in &expr.parameters {
                    self.symbols.define(param.value.clone());
                }

                self.compile_statements(&expr.body.statements)?;
                self.replace_last_pop_with_return();
                self.emit_return_if_missing();

                let prev_scope = self.symbols.clone();
                let num_locals = self.symbols.num_definitions.get();
                let instructions = self.leave_scope();
                for sym in prev_scope.free.borrow().iter() {
                    self.load_symbol(sym);
                }

                let func = Rc::new(CompiledFunctionObj {
                    instructions,
                    num_locals,
                    num_parameters: expr.parameters.len(),
                });
                let obj = Object::CompiledFunction(func);
                let operands = &[self.add_constant(obj), prev_scope.free.borrow().len()];
                self.emit(Opcode::Closure, operands)
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
            Expression::Null(_expr) => self.emit(Opcode::Null, &[]),
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
                for arg in &expr.arguments {
                    self.compile_expression(arg)?;
                }
                self.emit(Opcode::Call, &[expr.arguments.len()])
            }
            Expression::Identifier(expr) => match &self.symbols.resolve(&expr.value) {
                Some(sym) => self.load_symbol(sym),
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
            Expression::Quote(_expr) => {
                unreachable!("\"quote\" must be evaluated before generating bytecode")
            }
            Expression::Unquote(_expr) => {
                unreachable!(
                    "Unquote must be evaluated inside \"quote\" before generating bytecode"
                )
            }
            Expression::Loop(expr) => {
                self.enter_loop();

                let loop_start_pos = self.curr_scope_ins_len();
                self.compile_statements(&expr.body.statements)?;
                self.emit(Opcode::Jump, &[loop_start_pos]);
                let loop_end_pos = self.curr_scope_ins_len();

                for break_pos in self.leave_loop() {
                    self.change_operand(break_pos, loop_end_pos);
                }
                loop_end_pos
            }
        };
        Ok(())
    }

    fn compile_statement(&mut self, stmt: &Statement) -> Result<(), CompilerError> {
        match stmt {
            Statement::Let(stmt) => {
                // defining sym before compiling expression causes invalid stack access when shadowing a global
                // as local in a function in a let stmt and using same symbol in the expression on the right side
                // let sym = self.symbols.define(stmt.name.value.clone());
                self.compile_expression(&stmt.value)?;
                let sym = self.symbols.define(stmt.name.value.clone());
                match sym.scope {
                    SymbolScope::Global => self.emit(Opcode::SetGlobal, &[sym.index]),
                    SymbolScope::Local => self.emit(Opcode::SetLocal, &[sym.index]),
                    _ => unreachable!(),
                };
            }
            Statement::Return(stmt) => {
                self.compile_expression(&stmt.value)?;
                self.emit(Opcode::ReturnValue, &[]);
            }
            Statement::Expression(stmt) => {
                self.compile_expression(&stmt.value)?;
                self.emit(Opcode::Pop, &[]);
            }
            Statement::Exit(stmt) => {
                self.compile_expression(&stmt.value)?;
                self.emit(Opcode::Exit, &[]);
            }
            Statement::Break(stmt) => {
                self.compile_expression(&stmt.value)?;
                let pos = self.emit(Opcode::Break, &[0]); // tmp bogus value, replaced by loop expr
                self.push_break(pos);
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
