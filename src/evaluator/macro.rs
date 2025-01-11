use super::{Env, Environment, Eval};
use crate::ast::{
    modify::{modify_expression, modify_program, ModifierFunc},
    BooleanLiteral, CallExpression, Expression, IntegerLiteral, Program, Statement, StringLiteral,
};
use crate::object::{FunctionObj, Object};
use std::rc::Rc;

fn convert_object_to_expression(obj: Rc<Object>, expr: &Expression) -> Expression {
    match obj.as_ref() {
        Object::Boolean(obj) => Expression::Boolean(BooleanLiteral {
            token: expr.get_token().clone(),
            value: obj.value,
        }),
        Object::Integer(obj) => Expression::Integer(IntegerLiteral {
            token: expr.get_token().clone(),
            value: obj.value as usize,
        }),
        Object::String(obj) => Expression::String(StringLiteral {
            token: expr.get_token().clone(),
            value: obj.value.to_owned(),
        }),
        Object::Null => Expression::Null(expr.get_token().clone()),
        Object::Quote(expr) => expr.clone(),
        _ => panic!("can't convert object to expression: {obj:?}"),
    }
}

fn eval_unquote_calls(expr: &mut Expression, env: &Env) {
    let unquote_calls = |expr: &mut Expression, env: &Env| {
        let eval = Eval::new();
        let call = match expr {
            Expression::Call(expr) if expr.token.literal == "unquote" => expr,
            _ => return,
        };
        if call.arguments.len() != 1 {
            return;
        }
        *expr = convert_object_to_expression(
            eval.eval_expression(&call.arguments[0], env.clone()),
            expr,
        );
    };

    modify_expression(expr, unquote_calls, env);
}

pub fn quote(mut expr: Expression, env: &Env) -> Rc<Object> {
    eval_unquote_calls(&mut expr, env);
    Rc::new(Object::Quote(expr))
}
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
        let eval = Eval::new();
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

        let eval = eval.eval_block_statement(&macr.body, eval_env);
        let new_expr = match eval.as_ref() {
            Object::Quote(expr) => expr,
            _ => panic!("returning AST expressions only supported from macros"),
        };
        *expr = new_expr.to_owned();
    };
    modify_program(prog, expand, &env);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::evaluator::tests::test_eval;
    use crate::parser::tests::parse_program;

    #[test]
    fn test_quote() {
        let tests = [
            ("quote(5)", "5"),
            ("quote(5 + 8)", "(5 + 8)"),
            ("quote(foobar)", "foobar"),
            ("quote(foobar + barfoo)", "(foobar + barfoo)"),
        ];

        for (test_input, test_value) in tests {
            let eval = test_eval(test_input);
            match eval.as_ref() {
                Object::Quote(expr) => {
                    assert_eq!(expr.to_string(), test_value);
                }
                _ => panic!("object is not Quote, got {eval:?}"),
            }
        }
    }

    #[test]
    fn test_quote_unquote() {
        let tests = [
            ("quote(unquote(4))", "4"),
            ("quote(unquote(4 + 4))", "8"),
            ("quote(8 + unquote(4 + 4))", "(8 + 8)"),
            ("quote(unquote(4 + 4) + 8)", "(8 + 8)"),
            ("let foobar = 8; quote(foobar)", "foobar"),
            ("let foobar = 8; quote(unquote(foobar))", "8"),
            ("quote(unquote(true))", "true"),
            ("quote(unquote(true == false))", "false"),
            ("quote(unquote(quote(4 + 4)))", "(4 + 4)"),
            (
                "let quotedInfixExpression = quote(4 + 4);
            quote(unquote(4 + 4) + unquote(quotedInfixExpression))",
                "(8 + (4 + 4))",
            ),
        ];

        for (test_input, test_value) in tests {
            let eval = test_eval(test_input);
            match eval.as_ref() {
                Object::Quote(expr) => {
                    assert_eq!(expr.to_string(), test_value);
                }
                _ => panic!("object is not Quote, got {eval:?}"),
            }
        }
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
                "((10 - 5) - (2 + 2))",
            ),
            (
                r#"let unless = macro(condition, consequence, alternative) {
                    quote(if (!(unquote(condition))) {
                        unquote(consequence);
                    } else {
                        unquote(alternative);
                    });
                };

                unless(10 > 5, puts("not greater"), puts("greater"));"#,
                r#"if ((!(10 > 5))) { puts("not greater") } else { puts("greater") }"#,
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
