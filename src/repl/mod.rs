use crate::evaluator::eval_program;
use crate::lexer::Lexer;
use crate::object::environment::{Env, Environment};
use crate::object::Object;
use crate::parser::{Parser, ParserError};
use std::io::{stdin, stdout, Stdout, Write};
use std::iter::Peekable;
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

fn repl(input: Peekable<Chars<'_>>, output: &mut Stdout, env: Env) {
    let lexer = Lexer::new(None, input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if !parser.errors.is_empty() {
        print_parser_errors(output, &parser.errors);
        return;
    }

    let eval = eval_program(&program, env.clone());
    match *eval {
        Object::None => (),
        _ => writeln!(output, "{}", eval.inspect()).expect("writing to stdout failed"),
    }
}

pub fn run_repl(input: Peekable<Chars<'_>>) {
    let env = Environment::new();
    let mut output = stdout();
    repl(input, &mut output, env);
}

pub fn start_repl() {
    let input = stdin();
    let mut output = stdout();
    let env = Environment::new();
    loop {
        // TODO: add history? will require tty raw mode
        let mut buf = String::new();

        output
            .write_all(PROMPT.as_bytes())
            .expect("failed to write prompt");
        output.flush().expect("failed to flush prompt");

        input.read_line(&mut buf).expect("reading input failed");
        repl(buf.chars().peekable(), &mut output, env.clone())
    }
}
