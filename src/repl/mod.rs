use crate::lexer::{Lexer, TokenKind};
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
        output.flush().expect("failed to flush prompt");
        input.read_line(&mut buf).expect("invalid input");
        let mut lexer = Lexer::new(None, buf.chars().peekable());
        loop {
            let token = lexer.next_token();
            if token.kind == TokenKind::Eof {
                break;
            }
            writeln!(&mut output, "{}", token).expect("writing to output buffer failed");
        }
    }
}
