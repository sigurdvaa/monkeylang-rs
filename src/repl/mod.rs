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
use std::io::{stdin, stdout, Stdout, Write};
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

fn print_parser_errors(output: &mut Stdout, errors: &[ParserError]) {
    writeln!(
        output,
        "{MONKEY_FACE}Woops! We ran into some monkey business here!\n parser errors:"
    )
    .expect("writing to stdout failed");
    for err in errors {
        writeln!(output, "\t{err}").expect("writing to stdout failed")
    }
}

fn repl_eval(
    input: Peekable<Chars<'_>>,
    output: &mut Stdout,
    env: Env,
    macro_env: Env,
) -> Rc<Object> {
    let lexer = Lexer::new(None, input);
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program();

    if !parser.errors.is_empty() {
        print_parser_errors(output, &parser.errors);
        return Rc::new(Object::None);
    }

    define_macros(&mut program, macro_env.clone());
    expand_macros(&mut program, macro_env);

    eval_program(&program, env.clone())
}

pub fn run_repl_eval(input: Peekable<Chars<'_>>) {
    let env = Environment::new();
    let macro_env = Environment::new();
    let mut output = stdout();
    let _ = repl_eval(input, &mut output, env, macro_env);
}

pub fn start_repl_eval() {
    let input = stdin();
    let mut output = stdout();
    let env = Environment::new();
    let macro_env = Environment::new();
    loop {
        // TODO: add history? will require tty raw mode
        let mut buf = String::new();

        output
            .write_all(PROMPT.as_bytes())
            .expect("failed to write prompt");
        output.flush().expect("failed to flush prompt");

        input.read_line(&mut buf).expect("reading input failed");
        let eval = repl_eval(
            buf.chars().peekable(),
            &mut output,
            env.clone(),
            macro_env.clone(),
        );

        match *eval {
            Object::None => (),
            _ => writeln!(output, "{}", eval.inspect()).expect("writing to stdout failed"),
        }
    }
}

fn repl_vm(input: Peekable<Chars<'_>>, output: &mut Stdout) -> Option<Object> {
    let lexer = Lexer::new(None, input);
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program();

    if !parser.errors.is_empty() {
        print_parser_errors(output, &parser.errors);
        return Some(Object::None);
    }

    let macro_env = Environment::new();
    define_macros(&mut program, macro_env.clone());
    expand_macros(&mut program, macro_env);

    let mut compiler = Compiler::new();
    if let Err(e) = compiler.compile_program(&program) {
        println!("Whoops! Compilation failed:\n {e:?}");
        return None;
    }

    let mut vm = Vm::new(compiler.bytecode());
    if let Err(e) = vm.run() {
        println!("Whoops! Executing bytecode failed:\n {e:?}");
        return None;
    }
    vm.stack_top().cloned()
}

pub fn start_repl_vm() {
    let input = stdin();
    let mut output = stdout();
    loop {
        // TODO: add history? will require tty raw mode
        let mut buf = String::new();

        output
            .write_all(PROMPT.as_bytes())
            .expect("failed to write prompt");
        output.flush().expect("failed to flush prompt");

        input.read_line(&mut buf).expect("reading input failed");
        let result = repl_vm(buf.chars().peekable(), &mut output);

        match result {
            Some(Object::None) | None => (),
            Some(result) => {
                writeln!(output, "{}", result.inspect()).expect("writing to stdout failed")
            }
        }
    }
}
