use crate::compiler::Compiler;
use crate::evaluator::Eval;
use crate::lexer::Lexer;
use crate::object::{Engine, Object};
use crate::parser::{Parser, ParserError};
use crate::terminal::Terminal;
use crate::vm::Vm;
use std::fmt::Display;
use std::iter::Peekable;
use std::rc::Rc;
use std::str::Chars;

const MONKEY_FACE: &str = concat!(
    "            __,__\n",
    "   .--.  .-\"     \"-.  .--.\n",
    "  / .. \\/  .-. .-.  \\/ .. \\\n",
    " | |  '|  /   Y   \\  |'  | |\n",
    " | \\   \\  \\ 0 | 0 /  /   / |\n",
    "  \\ '- ,\\.-\"\"\"\"\"\"\"-./, -' /\n",
    "   ''-' /_   ^ ^   _\\ '-''\n",
    "       |  \\._   _./  |\n",
    "       \\   \\ '~' /   /\n",
    "        '._ '-=-' _.'\n",
    "           '-----'\n",
);

#[derive(Debug)]
pub enum EngineKind {
    Eval,
    Vm,
}

impl Display for EngineKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eval => write!(f, "eval"),
            Self::Vm => write!(f, "vm"),
        }
    }
}

fn print_parser_errors(errors: &[ParserError]) {
    println!("{MONKEY_FACE}Woops! We ran into some monkey business here!\n parser errors:");
    for err in errors {
        println!("  {err}");
    }
}

fn repl_eval(
    file: Option<String>,
    input: Peekable<Chars<'_>>,
    eval: &mut Eval,
    macro_eval: &mut Eval,
) -> Rc<Object> {
    let lexer = Lexer::new(file, input);
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program();

    if !parser.errors.is_empty() {
        print_parser_errors(&parser.errors);
        return eval.get_obj_none();
    }

    macro_eval.define_macros(&mut program);
    macro_eval.expand_macros(&mut program);

    eval.eval_program(&program)
}

pub fn run_repl_eval(file: Option<String>, input: Peekable<Chars<'_>>) -> Rc<Object> {
    let mut eval = Eval::new();
    let mut macro_eval = Eval::new();
    repl_eval(file, input, &mut eval, &mut macro_eval)
}

pub fn start_repl_eval() {
    let mut term = Terminal::new();
    let mut eval = Eval::new();
    let mut macro_eval = Eval::new();
    loop {
        if let Some(input) = term.get_input() {
            let eval = repl_eval(None, input.chars().peekable(), &mut eval, &mut macro_eval);
            match *eval {
                Object::None => (),
                _ => term.writeln(eval.inspect().as_bytes()),
            }
        }
    }
}

fn repl_vm(
    file: Option<String>,
    input: Peekable<Chars<'_>>,
    compiler: &mut Compiler,
    vm: &mut Vm,
) -> Option<Rc<Object>> {
    let lexer = Lexer::new(file, input);
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program();

    if !parser.errors.is_empty() {
        print_parser_errors(&parser.errors);
        return None;
    }

    let mut macro_eval = Eval::new();
    macro_eval.define_macros(&mut program);
    macro_eval.expand_macros(&mut program);

    compiler.soft_reset();
    if let Err(e) = compiler.compile_program(&program) {
        println!("Whoops! Compilation failed:\n {e}");
        return None;
    }

    vm.soft_reset(compiler.bytecode());
    match vm.run() {
        Err(e) => {
            println!("Whoops! Executing bytecode failed:\n {e}");
            None
        }
        Ok(v) => Some(v),
    }
}

pub fn run_repl_vm(file: Option<String>, input: Peekable<Chars<'_>>) -> Option<Rc<Object>> {
    let mut compiler = Compiler::new();
    let mut vm = Vm::new(None);
    repl_vm(file, input, &mut compiler, &mut vm)
}

pub fn start_repl_vm() {
    let mut term = Terminal::new();
    let mut compiler = Compiler::new();
    let mut vm = Vm::new(None);
    while let Some(input) = term.get_input() {
        let result = repl_vm(None, input.chars().peekable(), &mut compiler, &mut vm);
        if let Some(result) = result {
            match *result {
                Object::None => (),
                _ => term.writeln(result.inspect().as_bytes()),
            }
        }
    }
}
