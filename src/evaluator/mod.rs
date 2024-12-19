use crate::ast::{Expression, Operator, Program, Statement};
use crate::object::Object;

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(value) => Object::Integer(-value),
        _ => todo!(),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Null => Object::Boolean(true),
        Object::Integer(value) => Object::Boolean(value < 1),
        Object::Boolean(value) => Object::Boolean(!value),
    }
}

fn eval_prefix_expression(operator: &Operator, right: Object) -> Object {
    match operator {
        Operator::Bang => eval_bang_operator_expression(right),
        Operator::Minus => eval_minus_prefix_operator_expression(right),
        _ => todo!(),
    }
}

fn eval_expression(expression: &Expression) -> Object {
    match expression {
        Expression::Boolean(expr) => Object::Boolean(expr.value),
        Expression::Call(expr) => todo!(),
        Expression::Function(expr) => todo!(),
        Expression::Identifier(expr) => todo!(),
        Expression::If(expr) => todo!(),
        Expression::Infix(expr) => todo!(),
        Expression::IntegerLiteral(expr) => {
            Object::Integer(expr.value.try_into().expect("integer too large"))
        }
        Expression::Prefix(expr) => {
            let right = eval_expression(&expr.right);
            eval_prefix_expression(&expr.operator, right)
        }
    }
}

fn eval_statement(statement: &Statement) -> Object {
    match statement {
        Statement::Let(expr) => eval_expression(&expr.value),
        Statement::Return(expr) => eval_expression(&expr.value),
        Statement::Expression(expr) => eval_expression(&expr.value),
        Statement::Block(expr) => eval_statements(&expr.statements),
    }
}

fn eval_statements(statements: &[Statement]) -> Object {
    eval_statement(&statements[0])
}

pub fn eval_program(program: &Program) -> Object {
    eval_statements(&program.statements)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::*;

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(None, input.chars().peekable());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        eval_program(&program)
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = [
            ("5", Object::Integer(5)),
            ("10", Object::Integer(10)),
            ("-5", Object::Integer(-5)),
            ("-10", Object::Integer(-10)),
        ];
        for (test_input, test_value) in &tests {
            assert_eq!(test_eval(test_input), *test_value);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = [
            ("true", Object::Boolean(true)),
            ("false", Object::Boolean(false)),
        ];
        for (test_input, test_value) in &tests {
            assert_eq!(test_eval(test_input), *test_value);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = [
            ("!false", Object::Boolean(true)),
            ("!true", Object::Boolean(false)),
            ("!5", Object::Boolean(false)),
            ("!!false", Object::Boolean(false)),
            ("!!true", Object::Boolean(true)),
            ("!!5", Object::Boolean(true)),
        ];
        for (test_input, test_value) in &tests {
            assert_eq!(test_eval(test_input), *test_value);
        }
    }

    // #[test]
    // fn test_eval_integer_expression() {
    //     let tests = [("5", Object::Integer(5)), ("-5", Object::Integer(-5))];
    // }
}
