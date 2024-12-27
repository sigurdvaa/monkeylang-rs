use super::eval_expression;
use crate::ast::{modify::modify_expression, *};
use crate::object::{environment::Env, Object};
use std::rc::Rc;

fn convert_object_to_expression(obj: Rc<Object>, expr: &Expression) -> Expression {
    println!("converting obj");
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
        _ => todo!("convert {obj:?}"),
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
