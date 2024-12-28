use crate::ast::CallExpression;
use crate::evaluator::{eval_block_statement, Env};
use crate::object::{FunctionObj, Object};
use crate::{
    ast::{
        modify::{modify_program, ModifierFunc},
        Expression, Program, Statement,
    },
    object::environment::Environment,
};
use std::rc::Rc;

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

fn quote_args(call: &CallExpression) -> Vec<Rc<Object>> {
    let mut args = vec![];
    for arg in &call.arguments {
        args.push(Rc::new(Object::Quote(arg.clone())));
    }
    args
}

fn extend_macro_env(macr: &FunctionObj, args: Vec<Rc<Object>>) -> Env {
    let extended = Environment::new_enclosed(&macr.env);
    for (i, param) in macr.parameters.iter().enumerate() {
        extended.set(param.value.to_owned(), args[i].to_owned());
    }
    extended
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

        let macr = match obj.as_ref() {
            Object::Macro(obj) => obj,
            _ => return,
        };

        let args = quote_args(call);
        let eval_env = extend_macro_env(macr, args);

        let eval = eval_block_statement(&macr.body, eval_env);
        let new_expr = match eval.as_ref() {
            Object::Quote(expr) => expr,
            _ => panic!("returning AST expressions only supported form macros"),
        };
        *expr = new_expr.to_owned();
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
            // (
            //     "let infixExpression = macro() { quote(1 + 2); }; infixExpression();",
            //     "(1 + 2)",
            // ),
            // (
            //     "let reverse = macro(a, b) { quote(unquote(b) - unquote(a)); }; reverse(2 + 2, 10 - 5);",
            //     "((10 - 5) - (2 + 2))",
            // ),
            (
                r#"let unless = macro(condition, consequence, alternative) {
                    quote(if (!(unquote(condition))) {
                        unquote(consequence);
                    } else {
                        unquote(alternative);
                    });
                };

                unless(10 > 5, puts("not greater"), puts("greater"));"#,
                r#"if (!(10 > 5)) { puts("not greater") } else { puts("greater") }"#,
            ),
        ];

        for (test_input, test_value) in tests {
            let mut prog = parse_program(test_input, 2);
            let env = Environment::new();
            define_macros(&mut prog, env.clone());
            expand_macros(&mut prog, env);
            assert_eq!(prog.to_string(), test_value);
        }
    }
}
