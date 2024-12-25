mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

const VERSION: &str = "0.1.0";

fn main() {
    println!("Monkeylang {VERSION}.\npress ctrl+c to exit.");
    repl::start_repl();
}
