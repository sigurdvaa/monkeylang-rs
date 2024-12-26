use crate::ast::{Expression, Program, Statement};

pub fn modify_expression(expr: &mut Expression, func: &dyn Fn(&mut Expression)) {
    match expr {
        Expression::If(expr) => {
            func(&mut expr.condition);
            modify_statements(&mut expr.consequence.statements, &func);
            if let Some(alt) = &mut expr.alternative {
                modify_statements(&mut alt.statements, func);
            }
        }
        Expression::Integer(_) => func(expr),
        _ => todo!(),
    }
}

pub fn modify_statement(stmt: &mut Statement, func: &dyn Fn(&mut Expression)) {
    match stmt {
        Statement::Let(stmt) => modify_expression(&mut stmt.value, func),
        Statement::Return(stmt) => modify_expression(&mut stmt.value, func),
        Statement::Expression(stmt) => modify_expression(&mut stmt.value, func),
    }
}

pub fn modify_statements(stmts: &mut [Statement], func: &dyn Fn(&mut Expression)) {
    for stmt in stmts {
        modify_statement(stmt, &func);
    }
}

pub fn modify_program(prog: &mut Program, func: &dyn Fn(&mut Expression)) {
    modify_statements(&mut prog.statements, func);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ExpressionStatement, IntegerLiteral, Program, Statement};
    use crate::token::{Token, TokenKind};

    fn create_token(kind: TokenKind) -> Token {
        Token {
            kind,
            literal: "test".into(),
            file: None,
            line: 0,
            col: 0,
        }
    }

    fn create_expression(token: Token, value: usize) -> Expression {
        Expression::Integer(IntegerLiteral { token, value })
    }

    fn create_statement(value: usize) -> Statement {
        let token = create_token(TokenKind::Int);
        let value = create_expression(token.clone(), value);
        Statement::Expression(ExpressionStatement { token, value })
    }

    fn create_program(value: usize) -> Program {
        Program {
            statements: vec![create_statement(value)],
        }
    }

    #[test]
    fn test_modify() {
        let turn_one_into_two: &dyn Fn(&mut Expression) = &|expr: &mut Expression| {
            let int = match expr {
                Expression::Integer(int) => int,
                _ => return,
            };

            if int.value != 1 {
                return;
            }

            int.value = 2;
        };

        let tests = [(create_program(1), create_program(2))];
        for (mut test_input, test_value) in tests {
            modify_program(&mut test_input, turn_one_into_two);
            assert_eq!(test_input, test_value);
        }
    }
}
