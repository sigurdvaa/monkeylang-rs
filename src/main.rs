mod ast;
mod lexer;
mod parser;
mod repl;
use std::io::{stdin, stdout};

fn main() {
    println!("Monkeylang");
    repl::start_repl(stdin().lock(), stdout());
}
