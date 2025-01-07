use crate::compiler::Compiler;
use crate::evaluator::{
    eval_program,
    r#macro::{define_macros, expand_macros},
};
use crate::lexer::Lexer;
use crate::object::environment::{Env, Environment};
use crate::object::Object;
use crate::parser::{Parser, ParserError};
use crate::vm::Vm;
use std::fmt::Display;
use std::io::{stdin, Write};
use std::iter::Peekable;
use std::rc::Rc;
use std::str::Chars;

const PROMPT: &str = ">> ";
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
pub enum Engine {
    Eval,
    Vm,
}

impl Display for Engine {
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
        println!("\t{err}");
    }
}

fn repl_eval(input: Peekable<Chars<'_>>, env: Env, macro_env: Env) -> Rc<Object> {
    let lexer = Lexer::new(None, input);
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program();

    if !parser.errors.is_empty() {
        print_parser_errors(&parser.errors);
        return Rc::new(Object::None);
    }

    define_macros(&mut program, macro_env.clone());
    expand_macros(&mut program, macro_env);

    eval_program(&program, env.clone())
}

pub fn run_repl_eval(input: Peekable<Chars<'_>>) -> Rc<Object> {
    let env = Environment::new();
    let macro_env = Environment::new();
    repl_eval(input, env, macro_env)
}

// TODO: add engine as arg
pub fn _start_repl_eval() {
    let input = stdin();
    let env = Environment::new();
    let macro_env = Environment::new();
    loop {
        // TODO: add history? will require tty raw mode
        let mut buf = String::new();
        print!("{PROMPT}");
        let _ = std::io::stdout().flush();

        input.read_line(&mut buf).expect("reading input failed");
        let eval = repl_eval(buf.chars().peekable(), env.clone(), macro_env.clone());

        match *eval {
            Object::None => (),
            _ => println!("{}", eval.inspect()),
        }
    }
}

// TODO: fix same return type for eval and vm
fn repl_vm(input: Peekable<Chars<'_>>, compiler: &mut Compiler, vm: &mut Vm) -> Option<Object> {
    let lexer = Lexer::new(None, input);
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program();

    if !parser.errors.is_empty() {
        print_parser_errors(&parser.errors);
        return Some(Object::None);
    }

    let macro_env = Environment::new();
    define_macros(&mut program, macro_env.clone());
    expand_macros(&mut program, macro_env);

    compiler.soft_reset();
    if let Err(e) = compiler.compile_program(&program) {
        println!("Whoops! Compilation failed:\n {e:?}");
        return None;
    }

    vm.soft_reset(compiler.bytecode());
    match vm.run() {
        Err(e) => {
            println!("Whoops! Executing bytecode failed:\n {e:?}");
            None
        }
        Ok(v) => Some(v),
    }
}

pub fn run_repl_vm(input: Peekable<Chars<'_>>) -> Option<Object> {
    let mut compiler = Compiler::new();
    let mut vm = Vm::new(None);
    repl_vm(input, &mut compiler, &mut vm)
}

pub fn start_repl_vm() {
    let input = stdin();
    let mut compiler = Compiler::new();
    let mut vm = Vm::new(None);
    loop {
        // TODO: add history? will require tty raw mode
        let mut buf = String::new();
        print!("{PROMPT}");
        let _ = std::io::stdout().flush();
        input.read_line(&mut buf).expect("reading input failed");
        let result = repl_vm(buf.chars().peekable(), &mut compiler, &mut vm);
        match result {
            Some(Object::None) | None => (),
            Some(result) => {
                println!("{}", result.inspect());
            }
        }
    }
}
