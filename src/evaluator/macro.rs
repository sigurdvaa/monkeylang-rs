use super::{Environment, Eval};
use crate::ast::{
    modify::{modify_expression, modify_program, ModifierFunc},
    BooleanLiteral, Expression, IntegerLiteral, Program, Statement, StringLiteral,
};
use crate::object::{FunctionObj, Object};
use std::rc::Rc;

impl Eval {
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

    fn eval_unquote_calls(&mut self, expr: &mut Expression) {
        let unquote_calls = |expr: &mut Expression, eval: &mut Eval| {
            let call = match expr {
                Expression::Call(expr) if expr.token.literal == "unquote" => expr,
                _ => return,
            };
            if call.arguments.len() != 1 {
                return;
            }
            *expr =
                Eval::convert_object_to_expression(eval.eval_expression(&call.arguments[0]), expr);
        };

        modify_expression(expr, unquote_calls, self);
    }

    pub fn quote(&mut self, mut expr: Expression) -> Rc<Object> {
        self.eval_unquote_calls(&mut expr);
        Rc::new(Object::Quote(expr))
    }

    pub fn define_macros(&mut self, prog: &mut Program) {
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
                env: self.envs[self.ep].clone(),
            });

            self.envs[self.ep].set(stmt.name.value.to_owned(), Rc::new(obj));
            defs.push(i);
        }
        for i in defs.into_iter().rev() {
            prog.statements.remove(i);
        }
    }

    pub fn expand_macros(&mut self, prog: &mut Program) {
        let expand: ModifierFunc = |expr: &mut Expression, eval: &mut Eval| {
            let call = match expr {
                Expression::Call(expr) => expr,
                _ => return,
            };
            let ident = match call.function.as_ref() {
                Expression::Identifier(expr) => expr,
                _ => return,
            };
            let obj = match eval.envs[eval.ep].get(&ident.value) {
                Some(obj) => obj,
                _ => return,
            };
            let macr = match obj.as_ref() {
                Object::Macro(obj) => obj,
                _ => return,
            };
            let args: Vec<_> = call
                .arguments
                .iter()
                .map(|a| Rc::new(Object::Quote(a.clone())))
                .collect();

            let macro_env = Environment::new_enclosed(macr.env.clone());
            for (i, param) in macr.parameters.iter().enumerate() {
                macro_env.set(param.value.to_owned(), args[i].to_owned());
            }
            eval.envs.push(macro_env);
            eval.ep += 1;
            let result = eval.eval_block_statement(&macr.body);
            eval.envs.pop();
            eval.ep -= 1;

            let new_expr = match result.as_ref() {
                Object::Quote(expr) => expr,
                _ => panic!("returning AST expressions only supported from macros"),
            };
            *expr = new_expr.to_owned();
        };
        modify_program(prog, expand, self);
    }
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

        let mut eval = Eval::new();
        eval.define_macros(&mut program);

        assert_eq!(program.statements.len(), 2);
        assert_eq!(eval.envs[eval.ep].get("number"), None);
        assert_eq!(eval.envs[eval.ep].get("function"), None);

        let mac = eval.envs[eval.ep]
            .get("mymacro")
            .expect("macro not defined in env");
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
            let mut eval = Eval::new();
            eval.define_macros(&mut prog);
            eval.expand_macros(&mut prog);
            assert_eq!(prog.to_string(), test_value);
        }
    }
}
