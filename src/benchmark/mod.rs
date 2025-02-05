use crate::repl::{run_repl_eval, run_repl_vm, EngineKind};
use std::time::Instant;

const TEST: &str = concat!(
    "let fibonacci = fn(x) {\n",
    "  if (x == 0) {\n",
    "    0\n",
    "  } else {\n",
    "    if (x == 1) {\n",
    "      return 1;\n",
    "    } else {\n",
    "      fibonacci(x - 1) + fibonacci(x - 2);\n",
    "    }\n",
    "  }\n",
    "};\n",
    "fibonacci(35);\n",
);

pub fn run(engine: EngineKind) {
    let start = Instant::now();
    let result = match engine {
        EngineKind::Eval => run_repl_eval(None, TEST.chars().peekable()).inspect(),
        EngineKind::Vm => run_repl_vm(None, TEST.chars().peekable())
            .unwrap()
            .inspect(),
    };
    let duration = start.elapsed();
    println!(
        "engine={engine}, result={result}, duration={}s",
        duration.as_secs_f64()
    );
}
