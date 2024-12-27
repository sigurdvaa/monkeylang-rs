use crate::ast::{
    modify::{modify_program, ModifierFunc},
    Expression, Program, Statement,
};
use crate::evaluator::{eval_block_statement, Env};
use crate::object::{FunctionObj, Object};
use std::rc::Rc;

use super::{eval_program, eval_statement};

pub fn define_macros(prog: &mut Program, env: Env) {
    let mut defs = vec![];

    for (i, stmt) in prog.statements.iter().enumerate() {
        let stmt = match stmt {
            Statement::Let(stmt) => stmt,
            _ => continue,
        };

        let expr = match &stmt.value {
            Expression::Macro(expr) => expr,
            _ => continue,
        };

        let obj = Object::Macro(FunctionObj {
            parameters: expr.parameters.clone(),
            body: expr.body.clone(),
            env: env.clone(),
        });

        env.set(stmt.name.value.to_owned(), Rc::new(obj));
        defs.push(i);
    }

    for i in defs.into_iter().rev() {
        prog.statements.remove(i);
    }
}

pub fn expand_macros(prog: &mut Program, env: Env) {
    let expand: ModifierFunc = |expr: &mut Expression, env: &Env| {
        let call = match expr {
            Expression::Call(expr) => expr,
            _ => return,
        };

        let ident = match call.function.as_ref() {
            Expression::Identifier(expr) => expr,
            _ => return,
        };

        let obj = match env.get(&ident.value) {
            Some(obj) => obj,
            _ => return,
        };

        let mac = match obj.as_ref() {
            Object::Macro(obj) => obj,
            _ => return,
        };

        let args = quote_args(mac);
        let eval_env = extend_macro_env(mac, args);

        let eval = eval_block_statement(&mac.body, eval_env);
    };
    modify_program(prog, expand, &env);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::evaluator::Environment;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    // TODO: import from parser tests? create testutil module?
    fn parse_program(input: &str, statements: usize) -> Program {
        let lexer = Lexer::new(None, input.chars().peekable());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if !parser.errors.is_empty() {
            for err in &parser.errors {
                println!("{err}");
            }
            panic!("parser errors found: {}", parser.errors.len());
        }

        assert_eq!(
            program.statements.len(),
            statements,
            "unexpected statements"
        );
        program
    }

    #[test]
    fn test_define_macros() {
        let input = concat!(
            "let number = 1;\n",
            "let function = fn(x, y) { x + y };\n",
            "let mymacro = macro(x, y) { x + y; };\n",
        );
        let mut program = parse_program(input, 3);

        let env = Environment::new();
        define_macros(&mut program, env.clone());

        assert_eq!(program.statements.len(), 2);
        assert_eq!(env.get("number"), None);
        assert_eq!(env.get("function"), None);

        let mac = env.get("mymacro").expect("macro not defined in env");
        let mac = match mac.as_ref() {
            Object::Macro(obj) => obj,
            _ => panic!("not a Macro object, got {mac:?}"),
        };

        assert_eq!(mac.parameters.len(), 2);
        assert_eq!(mac.parameters[0].value, "x");
        assert_eq!(mac.parameters[1].value, "y");
        assert_eq!(mac.body.to_string(), "(x + y)");
    }

    #[test]
    fn test_expand_macros() {
        let tests = [
            (
                "let infixExpression = macro() { quote(1 + 2); }; infixExpression();",
                "(1 + 2)",
            ),
            (
                "let reverse = macro(a, b) { quote(unquote(b) - unquote(a)); }; reverse(2 + 2, 10 - 5);",
                "(10 - 5) - (2 + 2)",
            ),
        ];

        for (test_input, test_value) in tests {
            let mut program = parse_program(test_input, 2);
            let env = Environment::new();
            define_macros(&mut program, env);

            todo!();
        }
    }
}
