use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{BufRead, Write};

const PROMPT: &str = ">> ";

pub fn start_repl<I, O>(mut input: I, mut output: O)
where
    I: BufRead,
    O: Write,
{
    loop {
        let mut buf = String::new();
        output
            .write_all(PROMPT.as_bytes())
            .expect("failed to write prompt");
        // output.flush().expect("failed to flush prompt");
        input.read_line(&mut buf).expect("invalid input");
        let lexer = Lexer::new(None, buf.chars().peekable());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        for err in parser.errors {
            writeln!(&mut output, "error: {err}").expect("writing to output buffer failed")
        }

        match program {
            Ok(program) => {
                writeln!(&mut output, "{program}").expect("writing to output buffer failed")
            }
            Err(err) => writeln!(&mut output, "Parsing program failed: {err}")
                .expect("writing to output buffer failed"),
        };
    }
}
