mod ast;
mod benchmark;
mod code;
mod compiler;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod terminal;
mod token;
mod vm;

use repl::{run_repl_vm, start_repl_eval, start_repl_vm, EngineKind};

const VERSION: &str = "0.3.1";

fn usage(args: &[String]) {
    eprintln!(
        concat!(
            "usage: {} [options]\n",
            "options:\n",
            "  (none)\t\tstart repl\n",
            "  <file>\t\texecute file\n",
            "  run <file>\t\texecute file\n",
            "  repr <eval|vm>\trun repl with specified engine\n",
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
    run_repl_vm(Some(file_path.to_string()), input.chars().peekable());
}

fn main() {
    let welcome = format!("Monkeylang {VERSION}.\npress ctrl-c to exit.");
    let args: Vec<String> = std::env::args().collect();
    match args.len() {
        1 => {
            println!("{welcome}");
            start_repl_vm();
        }
        2 => run_with_file_input(&args),
        3 => match args[1].as_str() {
            "benchmark" => match args[2].as_str() {
                "eval" => benchmark::run(EngineKind::Eval),
                "vm" => benchmark::run(EngineKind::Vm),
                _ => usage(&args),
            },
            "repl" => match args[2].as_str() {
                "eval" => {
                    println!("{welcome}");
                    start_repl_eval()
                }
                "vm" => {
                    println!("{welcome}");
                    start_repl_vm()
                }
                _ => usage(&args),
            },
            "run" => run_with_file_input(&args),
            _ => usage(&args),
        },
        _ => usage(&args),
    }
}
