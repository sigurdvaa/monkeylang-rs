use crate::evaluator::eval_program;
use crate::lexer::Lexer;
use crate::object::environment::Environment;
use crate::parser::{Parser, ParserError};
use std::io::{BufRead, Write};

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

fn print_parser_errors<O>(output: &mut O, errors: &[ParserError])
where
    O: Write,
{
    writeln!(
        output,
        "{MONKEY_FACE}Woops! We ran into some monkey business here!\n parser errors:"
    )
    .expect("writing to output buffer failed");
    for err in errors {
        writeln!(output, "\t{err}").expect("writing to output buffer failed")
    }
}

pub fn start_repl<I, O>(mut input: I, mut output: O)
where
    I: BufRead,
    O: Write,
{
    let env = Environment::new();
    loop {
        let mut buf = String::new();
        output
            .write_all(PROMPT.as_bytes())
            .expect("failed to write prompt to output buffer");
        output
            .flush()
            .expect("failed to flush prompt to output buffer");
        input.read_line(&mut buf).expect("invalid input");

        let lexer = Lexer::new(None, buf.chars().peekable());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if !parser.errors.is_empty() {
            print_parser_errors(&mut output, &parser.errors);
            continue;
        }

        let eval = eval_program(&program, env.clone());
        writeln!(&mut output, "{}", eval.inspect()).expect("writing to output buffer failed")
    }
}
