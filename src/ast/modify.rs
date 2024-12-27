use crate::ast::{Expression, Program, Statement};
use crate::object::environment::Env;
use std::collections::BTreeMap;

pub type ModifierFunc = fn(&mut Expression, env: &Env);

pub fn modify_expression(expr: &mut Expression, func: ModifierFunc, env: &Env) {
    match expr {
        Expression::If(expr) => {
            func(&mut expr.condition, env);
            modify_statements(&mut expr.consequence.statements, func, env);
            if let Some(alt) = &mut expr.alternative {
                modify_statements(&mut alt.statements, func, env);
            }
        }
        Expression::Infix(sub) => {
            func(&mut sub.left, env);
            func(&mut sub.right, env);
        }
        Expression::Prefix(sub) => {
            func(&mut sub.right, env);
        }
        Expression::Call(expr) => {
            modify_expression(&mut expr.function, func, env);
            modify_expressions(&mut expr.arguments, func, env);
        }
        Expression::Function(sub) => {
            modify_statements(&mut sub.body.statements, func, env);
        }
        Expression::Array(expr) => {
            modify_expressions(&mut expr.elements, func, env);
        }
        Expression::Index(expr) => {
            func(&mut expr.left, env);
            func(&mut expr.index, env);
        }
        Expression::Hash(expr) => {
            let mut new_pairs = BTreeMap::new();
            for (key, value) in expr.pairs.iter_mut() {
                let mut key = key.clone();
                func(&mut key, env);
                func(value, env);
                new_pairs.insert(key, value.to_owned());
            }
            expr.pairs = new_pairs;
        }
        _ => (),
    }
    func(expr, env);
    // TODO: return Expression?
}

pub fn modify_expressions(exprs: &mut [Expression], func: ModifierFunc, env: &Env) {
    for expr in exprs {
        modify_expression(expr, func, env);
    }
}

pub fn modify_statement(stmt: &mut Statement, func: ModifierFunc, env: &Env) {
    match stmt {
        Statement::Let(stmt) => modify_expression(&mut stmt.value, func, env),
        Statement::Return(stmt) => modify_expression(&mut stmt.value, func, env),
        Statement::Expression(stmt) => modify_expression(&mut stmt.value, func, env),
    }
}

pub fn modify_statements(stmts: &mut [Statement], func: ModifierFunc, env: &Env) {
    for stmt in stmts {
        modify_statement(stmt, func, env);
    }
}

pub fn modify_program(prog: &mut Program, func: ModifierFunc, env: &Env) {
    modify_statements(&mut prog.statements, func, env);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    use crate::object::environment::Environment;
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

    fn create_int_expression(token: Token, value: usize) -> Expression {
        Expression::Integer(IntegerLiteral { token, value })
    }

    fn create_block_statement(token: Token, value: usize) -> BlockStatement {
        let value = create_int_expression(token.clone(), value);
        let statements = vec![Statement::Expression(ExpressionStatement {
            token: token.clone(),
            value,
        })];
        BlockStatement { token, statements }
    }

    fn create_int_program(value: usize) -> Program {
        let token = create_token(TokenKind::Int);
        let value = create_int_expression(token.clone(), value);
        let statements = vec![Statement::Expression(ExpressionStatement { token, value })];
        Program { statements }
    }

    fn create_infix_program(left: usize, right: usize) -> Program {
        let token = create_token(TokenKind::Int);
        let left = Box::new(create_int_expression(token.clone(), left));
        let operator = Operator::Plus;
        let right = Box::new(create_int_expression(token.clone(), right));
        let value = Expression::Infix(InfixExpression {
            token: token.clone(),
            left,
            operator,
            right,
        });
        let statements = vec![Statement::Expression(ExpressionStatement { token, value })];
        Program { statements }
    }

    fn create_prefix_program(right: usize) -> Program {
        let token = create_token(TokenKind::Int);
        let operator = Operator::Minus;
        let right = Box::new(create_int_expression(token.clone(), right));
        let value = Expression::Prefix(PrefixExpression {
            token: token.clone(),
            operator,
            right,
        });
        let statements = vec![Statement::Expression(ExpressionStatement { token, value })];
        Program { statements }
    }

    fn create_index_program(left: usize, index: usize) -> Program {
        let token = create_token(TokenKind::Int);
        let left = Box::new(create_int_expression(token.clone(), left));
        let index = Box::new(create_int_expression(token.clone(), index));
        let value = Expression::Index(IndexExpression {
            token: token.clone(),
            left,
            index,
        });
        let statements = vec![Statement::Expression(ExpressionStatement { token, value })];
        Program { statements }
    }

    fn create_if_program(cond: usize, cons: usize, alt: usize) -> Program {
        let token = create_token(TokenKind::Int);
        let condition = Box::new(create_int_expression(token.clone(), cond));
        let consequence = create_block_statement(token.clone(), cons);
        let alternative = Some(create_block_statement(token.clone(), alt));
        let value = Expression::If(IfExpression {
            token: token.clone(),
            condition,
            consequence,
            alternative,
        });
        let statements = vec![Statement::Expression(ExpressionStatement { token, value })];
        Program { statements }
    }

    fn create_return_program(value: usize) -> Program {
        let token = create_token(TokenKind::Int);
        let value = create_int_expression(token.clone(), value);
        let statements = vec![Statement::Return(ReturnStatement { token, value })];
        Program { statements }
    }

    fn create_let_program(value: usize) -> Program {
        let token = create_token(TokenKind::Int);
        let name = IdentifierLiteral {
            token: token.clone(),
            value: "test".into(),
        };
        let value = create_int_expression(token.clone(), value);
        let statements = vec![Statement::Let(LetStatement { token, name, value })];
        Program { statements }
    }

    fn create_fn_program(value: usize) -> Program {
        let token = create_token(TokenKind::Int);
        let parameters = vec![IdentifierLiteral {
            token: token.clone(),
            value: "test".into(),
        }];
        let body = create_block_statement(token.clone(), value);
        let value = Expression::Function(FunctionLiteral {
            token: token.clone(),
            parameters,
            body,
        });
        let statements = vec![Statement::Expression(ExpressionStatement { token, value })];
        Program { statements }
    }

    fn create_array_program(value: usize) -> Program {
        let token = create_token(TokenKind::Int);
        let elements = vec![create_int_expression(token.clone(), value)];
        let value = Expression::Array(ArrayLiteral {
            token: token.clone(),
            elements,
        });
        let statements = vec![Statement::Expression(ExpressionStatement { token, value })];
        Program { statements }
    }

    fn create_hash_program(value: usize) -> Program {
        let token = create_token(TokenKind::Int);
        let pairs = BTreeMap::from_iter([(
            create_int_expression(token.clone(), value),
            create_int_expression(token.clone(), value),
        )]);
        let value = Expression::Hash(HashLiteral {
            token: token.clone(),
            pairs,
        });
        let statements = vec![Statement::Expression(ExpressionStatement { token, value })];
        Program { statements }
    }

    #[test]
    fn test_modify() {
        let env = Environment::new();
        let turn_one_into_two: ModifierFunc = |expr: &mut Expression, _env: &Env| {
            let int = match expr {
                Expression::Integer(int) => int,
                _ => return,
            };

            if int.value != 1 {
                return;
            }

            int.value = 2;
        };

        let tests = [
            (create_int_program(1), create_int_program(2)),
            (create_infix_program(1, 2), create_infix_program(2, 2)),
            (create_infix_program(2, 1), create_infix_program(2, 2)),
            (create_prefix_program(1), create_prefix_program(2)),
            (create_index_program(1, 1), create_index_program(2, 2)),
            (create_if_program(1, 1, 1), create_if_program(2, 2, 2)),
            (create_return_program(1), create_return_program(2)),
            (create_let_program(1), create_let_program(2)),
            (create_fn_program(1), create_fn_program(2)),
            (create_array_program(1), create_array_program(2)),
            (create_hash_program(1), create_hash_program(2)),
        ];

        for (mut test_input, test_value) in tests {
            modify_program(&mut test_input, turn_one_into_two, &env);
            assert_eq!(test_input, test_value);
        }
    }
}