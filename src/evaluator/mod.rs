mod builtins;
mod tests;

use crate::ast::{
    BlockStatement, Expression, IdentifierLiteral, IfExpression, Operator, Program, Statement,
};
use crate::object::{
    environment::{Env, Environment},
    FunctionObject, Integer, Object,
};

fn extend_function_env(function: &FunctionObject, args: Vec<Object>) -> Env {
    let env = Environment::new_enclosed(&function.env);
    for (i, param) in function.parameters.iter().enumerate() {
        env.borrow_mut()
            .set(param.value.to_owned(), args[i].to_owned());
    }
    env
}

fn apply_function(function: Object, args: Vec<Object>) -> Object {
    match function {
        Object::Function(func) => {
            if func.parameters.len() != args.len() {
                return Object::Error(format!(
                    "unmatched number of arguments in function call, expected {}, got {}",
                    func.parameters.len(),
                    args.len(),
                ));
            }

            let extended_env = extend_function_env(&func, args);
            let eval = eval_block_statement(&func.body, extended_env);
            match eval {
                Object::Return(value) => *value,
                value => value,
            }
        }
        Object::Builtin(func) => func(&args),
        _ => Object::Error(format!("not a function: {}", function.kind())),
    }
}

fn eval_identifier(identifier: &IdentifierLiteral, env: Env) -> Object {
    if let Some(value) = env.borrow().get(&identifier.value) {
        return value.clone();
    }

    if let Some(value) = builtins::get(&identifier.value) {
        return value;
    }

    Object::Error(format!("identifier not found: {}", identifier.value))
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(value) => Object::Integer(-value),
        _ => Object::Error(format!("unknown operator: -{}", right.kind())),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Null => Object::Boolean(true),
        Object::Integer(value) => Object::Boolean(value < 1),
        Object::Boolean(value) => Object::Boolean(!value),
        _ => Object::Error(format!("unknown operator: !{}", right.kind())),
    }
}

fn eval_integer_infix_expression(operator: &Operator, a: Integer, b: Integer) -> Object {
    match operator {
        Operator::Plus => Object::Integer(a + b),
        Operator::Minus => Object::Integer(a - b),
        Operator::Asterisk => Object::Integer(a * b),
        Operator::Slash => Object::Integer(a / b),
        Operator::Gt => Object::Boolean(a > b),
        Operator::Lt => Object::Boolean(a < b),
        Operator::Eq => Object::Boolean(a == b),
        Operator::NotEq => Object::Boolean(a != b),
        _ => Object::Error(format!("unknown integer operator: {operator}",)),
    }
}

fn eval_boolean_infix_expression(operator: &Operator, a: bool, b: bool) -> Object {
    match operator {
        Operator::Eq => Object::Boolean(a == b),
        Operator::NotEq => Object::Boolean(a != b),
        _ => Object::Error(format!("unknown boolean operator: {operator}",)),
    }
}

fn eval_string_infix_expression(operator: &Operator, a: &str, b: &str) -> Object {
    match operator {
        Operator::Plus => Object::String(String::from_iter([a, b])),
        Operator::Eq => Object::Boolean(a == b),
        Operator::NotEq => Object::Boolean(a != b),
        _ => Object::Error(format!("unknown string operator: {operator}",)),
    }
}

fn eval_infix_expression(operator: &Operator, left: Object, right: Object) -> Object {
    match (&left, &right) {
        (Object::Integer(a), Object::Integer(b)) => eval_integer_infix_expression(operator, *a, *b),
        (Object::Boolean(a), Object::Boolean(b)) => eval_boolean_infix_expression(operator, *a, *b),
        (Object::String(a), Object::String(b)) => eval_string_infix_expression(operator, a, b),
        (a, b) if a.kind() != b.kind() => Object::Error(format!(
            "type mismatch: {} {operator} {}",
            left.kind(),
            right.kind(),
        )),
        _ => Object::Error(format!(
            "unknown operator: {} {operator} {}",
            left.kind(),
            right.kind(),
        )),
    }
}

fn eval_prefix_expression(operator: &Operator, right: Object) -> Object {
    match operator {
        Operator::Bang => eval_bang_operator_expression(right),
        Operator::Minus => eval_minus_prefix_operator_expression(right),
        _ => Object::Error(format!("unknown operator: {operator}{}", right.kind(),)),
    }
}

fn is_truthy(object: Object) -> bool {
    match object {
        Object::Integer(_) => true,
        Object::Boolean(value) => value,
        _ => false,
    }
}

fn eval_if_expression(expression: &IfExpression, env: Env) -> Object {
    let condition = eval_expression(&expression.condition, env.clone());
    if let Object::Error(_) = condition {
        return condition;
    }

    match is_truthy(condition) {
        true => eval_block_statement(&expression.consequence, env),
        false => match &expression.alternative {
            Some(alt) => eval_block_statement(alt, env),
            None => Object::Null,
        },
    }
}

fn eval_expressions(expressions: &[Expression], env: Env) -> Vec<Object> {
    let mut result = vec![];

    for expr in expressions {
        let eval = eval_expression(expr, env.clone());
        match eval {
            Object::Error(_) => return vec![eval],
            _ => result.push(eval),
        }
    }

    result
}

fn eval_expression(expression: &Expression, env: Env) -> Object {
    match expression {
        Expression::Boolean(expr) => Object::Boolean(expr.value),
        Expression::Call(expr) => {
            let func = eval_expression(&expr.function, env.clone());
            if let Object::Error(_) = func {
                return func;
            }

            let args = eval_expressions(&expr.arguments, env);
            if args.len() == 1 {
                if let Object::Error(_) = args[0] {
                    return args[0].to_owned();
                }
            }

            apply_function(func, args)
        }
        Expression::Function(expr) => Object::Function(FunctionObject {
            parameters: expr.parameters.clone(),
            body: expr.body.clone(),
            env: env.clone(),
        }),
        Expression::Identifier(expr) => eval_identifier(expr, env),
        Expression::If(expr) => eval_if_expression(expr, env),
        Expression::Infix(expr) => {
            let left = eval_expression(&expr.left, env.clone());
            if let Object::Error(_) = left {
                return left;
            }

            let right = eval_expression(&expr.right, env);
            if let Object::Error(_) = right {
                return right;
            }

            eval_infix_expression(&expr.operator, left, right)
        }
        Expression::Integer(expr) => {
            Object::Integer(expr.value.try_into().expect("integer too large"))
        }
        Expression::Prefix(expr) => {
            let right = eval_expression(&expr.right, env);
            match right {
                Object::Error(_) => right,
                _ => eval_prefix_expression(&expr.operator, right),
            }
        }
        Expression::String(expr) => Object::String(expr.value.to_owned()),
        Expression::Array(expr) => todo!(),
        Expression::Index(expr) => todo!(),
    }
}

fn eval_statement(statement: &Statement, env: Env) -> Object {
    match statement {
        Statement::Let(expr) => {
            let value = eval_expression(&expr.value, env.clone());
            if let Object::Error(_) = value {
                return value;
            }
            env.borrow_mut().set(expr.name.value.clone(), value.clone());
            value
        }
        Statement::Return(expr) => {
            let eval = eval_expression(&expr.value, env);
            match eval {
                Object::Error(_) => eval,
                _ => Object::Return(Box::new(eval)),
            }
        }
        Statement::Expression(expr) => eval_expression(&expr.value, env),
    }
}

fn eval_block_statement(block: &BlockStatement, env: Env) -> Object {
    let mut result = Object::Null;
    for stmt in &block.statements {
        result = eval_statement(stmt, env.clone());
        if let Object::Return(_) | Object::Error(_) = result {
            return result;
        }
    }
    result
}

pub fn eval_program(program: &Program, env: Env) -> Object {
    let mut result = Object::Null;
    for stmt in &program.statements {
        result = eval_statement(stmt, env.clone());
        match result {
            Object::Return(value) => return *value,
            Object::Error(_) => return result,
            _ => (),
        }
    }
    result
}
