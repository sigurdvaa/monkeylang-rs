use super::{eval_block_statement, eval_expression, Env, Environment};
use crate::ast::{
    modify::{modify_expression, modify_program, ModifierFunc},
    *,
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
            _ => panic!("returning AST expressions only supported from macros"),
        };
        *expr = new_expr.to_owned();
    };
    modify_program(prog, expand, &env);
}
