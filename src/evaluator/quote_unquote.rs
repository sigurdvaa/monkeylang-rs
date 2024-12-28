use super::eval_expression;
use crate::ast::{modify::modify_expression, *};
use crate::object::{environment::Env, Object};
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
        let call = match expr {
            Expression::Call(expr) if expr.token.literal == "unquote" => expr,
            _ => return,
        };
        if call.arguments.len() != 1 {
            return;
        }
        *expr =
            convert_object_to_expression(eval_expression(&call.arguments[0], env.clone()), expr);
    };

    modify_expression(expr, unquote_calls, env);
}

pub fn quote(mut expr: Expression, env: &Env) -> Rc<Object> {
    eval_unquote_calls(&mut expr, env);
    Rc::new(Object::Quote(expr))
}

#[cfg(test)]
use super::tests::test_eval;

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
