mod ast;
mod code;
mod compiler;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;
mod vm;

const VERSION: &str = "0.1.0";

fn usage(args: &[String]) {
    eprintln!("Usage: {} [file]", args[0]);
    std::process::exit(1);
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.len() {
        1 => {
            println!("Monkeylang {VERSION}.\npress ctrl+c to exit.");
            // TODO: add args for choosing eval or vm
            repl::start_repl_vm();
        }
        2 => {
            let file_path = args[1].as_str();
            // TODO: replace with iter of chars in file?
            let input = std::fs::read_to_string(file_path).unwrap_or_else(|err| {
                println!("Error reading file '{file_path}': {err}");
                std::process::exit(1);
            });
            repl::run_repl_eval(input.chars().peekable())
        }
        _ => usage(&args),
    }
}
