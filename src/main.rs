mod ast;
mod benchmark;
mod code;
mod compiler;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;
mod vm;

use repl::EngineKind;

const VERSION: &str = "0.2.5";

fn usage(args: &[String]) {
    eprintln!(
        concat!(
            "usage: {} [options]\n",
            "options:\n",
            "  (none)\t\tstart repl\n",
            "  <file>\t\texecute file\n",
            "  run <file>\t\texecute file\n",
            "  benchmark <eval|vm>\trun benchmark with specified engine\n",
        ),
        args[0]
    );
    std::process::exit(1);
}

fn run_with_file_input(args: &[String]) {
    let file_path = args[1].as_str();
    let input = std::fs::read_to_string(file_path).unwrap_or_else(|err| {
        println!("Error reading file '{file_path}': {err}");
        std::process::exit(1);
    });
    // repl::run_repl_eval(input.chars().peekable());
    repl::run_repl_vm(input.chars().peekable());
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.len() {
        1 => {
            println!("Monkeylang {VERSION}.\npress ctrl-c to exit.");
            repl::start_repl_vm();
        }
        2 => run_with_file_input(&args),
        3 => match args[1].as_str() {
            "benchmark" => match args[2].as_str() {
                "eval" => benchmark::run(EngineKind::Eval),
                "vm" => benchmark::run(EngineKind::Vm),
                _ => usage(&args),
            },
            "run" => run_with_file_input(&args),
            _ => usage(&args),
        },
        _ => usage(&args),
    }
}
