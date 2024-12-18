mod ast;
mod lexer;
mod parser;
mod repl;
use std::io::{stdin, stdout};

const VERSION: &str = "0.1.0";

fn main() {
    println!("Monkeylang {VERSION}.\npress ctrl+c to exit.");
    repl::start_repl(stdin().lock(), stdout());
}
