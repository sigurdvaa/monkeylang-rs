mod tests;

use crate::ast::{BlockStatement, Expression, IfExpression, Operator, Program, Statement};
use crate::object::{Integer, Object};

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

fn eval_infix_expression(operator: &Operator, left: Object, right: Object) -> Object {
    match (&left, &right) {
        (Object::Integer(a), Object::Integer(b)) => eval_integer_infix_expression(operator, *a, *b),
        (Object::Boolean(a), Object::Boolean(b)) => eval_boolean_infix_expression(operator, *a, *b),
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

fn eval_if_expression(expression: &IfExpression) -> Object {
    let condition = eval_expression(&expression.condition);
    if let Object::Error(_) = condition {
        return condition;
    }

    match is_truthy(condition) {
        true => eval_block_statement(&expression.consequence),
        false => match &expression.alternative {
            Some(alt) => eval_block_statement(alt),
            None => Object::Null,
        },
    }
}

fn eval_expression(expression: &Expression) -> Object {
    match expression {
        Expression::Boolean(expr) => Object::Boolean(expr.value),
        Expression::Call(expr) => todo!(),
        Expression::Function(expr) => todo!(),
        Expression::Identifier(expr) => todo!(),
        Expression::If(expr) => eval_if_expression(expr),
        Expression::Infix(expr) => {
            let left = eval_expression(&expr.left);
            if let Object::Error(_) = left {
                return left;
            }

            let right = eval_expression(&expr.right);
            if let Object::Error(_) = right {
                return right;
            }

            eval_infix_expression(&expr.operator, left, right)
        }
        Expression::IntegerLiteral(expr) => {
            Object::Integer(expr.value.try_into().expect("integer too large"))
        }
        Expression::Prefix(expr) => {
            let right = eval_expression(&expr.right);
            match right {
                Object::Error(_) => right,
                _ => eval_prefix_expression(&expr.operator, right),
            }
        }
    }
}

fn eval_statement(statement: &Statement) -> Object {
    match statement {
        Statement::Let(expr) => eval_expression(&expr.value),
        Statement::Return(expr) => {
            let eval = eval_expression(&expr.value);
            match eval {
                Object::Error(_) => eval,
                _ => Object::Return(Box::new(eval)),
            }
        }
        Statement::Expression(expr) => eval_expression(&expr.value),
        Statement::Block(expr) => eval_block_statement(expr),
    }
}

fn eval_block_statement(block: &BlockStatement) -> Object {
    let mut result = Object::Null;
    for stmt in &block.statements {
        result = eval_statement(stmt);
        if let Object::Return(_) | Object::Error(_) = result {
            return result;
        }
    }
    result
}

pub fn eval_program(program: &Program) -> Object {
    let mut result = Object::Null;
    for stmt in &program.statements {
        result = eval_statement(stmt);
        match result {
            Object::Return(value) => return *value,
            Object::Error(_) => return result,
            _ => (),
        }
    }
    result
}
