use crate::ast::{Expression, Program, Statement};
use crate::evaluator::Eval;
use std::collections::BTreeMap;

pub type ModifierFunc = fn(&mut Expression, &mut Eval);

pub fn modify_expression(expr: &mut Expression, func: ModifierFunc, eval: &mut Eval) {
    match expr {
        Expression::If(expr) => {
            modify_expression(&mut expr.condition, func, eval);
            modify_statements(&mut expr.consequence.statements, func, eval);
            if let Some(alt) = &mut expr.alternative {
                modify_statements(&mut alt.statements, func, eval);
            }
        }
        Expression::Infix(expr) => {
            modify_expression(&mut expr.left, func, eval);
            modify_expression(&mut expr.right, func, eval);
        }
        Expression::Prefix(expr) => {
            modify_expression(&mut expr.right, func, eval);
        }
        Expression::Call(expr) | Expression::Quote(expr) | Expression::Unquote(expr) => {
            modify_expression(&mut expr.function, func, eval);
            modify_expressions(&mut expr.arguments, func, eval);
        }
        Expression::Function(expr) => {
            modify_statements(&mut expr.body.statements, func, eval);
        }
        Expression::Loop(expr) => {
            modify_statements(&mut expr.body.statements, func, eval);
        }
        Expression::Array(expr) => {
            modify_expressions(&mut expr.elements, func, eval);
        }
        Expression::Index(expr) => {
            modify_expression(&mut expr.left, func, eval);
            modify_expression(&mut expr.index, func, eval);
        }
        Expression::Hash(expr) => {
            let mut new_pairs = BTreeMap::new();
            for (key, value) in expr.pairs.iter_mut() {
                let mut key = key.clone();
                modify_expression(&mut key, func, eval);
                modify_expression(value, func, eval);
                new_pairs.insert(key, value.to_owned());
            }
            expr.pairs = new_pairs;
        }
        Expression::Boolean(_)
        | Expression::Integer(_)
        | Expression::String(_)
        | Expression::Identifier(_)
        | Expression::Macro(_)
        | Expression::Null(_) => (),
    }
    func(expr, eval);
}

pub fn modify_expressions(exprs: &mut [Expression], func: ModifierFunc, eval: &mut Eval) {
    for expr in exprs {
        modify_expression(expr, func, eval);
    }
}

pub fn modify_statement(stmt: &mut Statement, func: ModifierFunc, eval: &mut Eval) {
    match stmt {
        Statement::Let(stmt) => modify_expression(&mut stmt.value, func, eval),
        Statement::Return(stmt) => modify_expression(&mut stmt.value, func, eval),
        Statement::Expression(stmt) => modify_expression(&mut stmt.value, func, eval),
        Statement::Exit(stmt) => modify_expression(&mut stmt.value, func, eval),
        Statement::Break(stmt) => modify_expression(&mut stmt.value, func, eval),
    }
}

pub fn modify_statements(stmts: &mut [Statement], func: ModifierFunc, eval: &mut Eval) {
    for stmt in stmts {
        modify_statement(stmt, func, eval);
    }
}

pub fn modify_program(prog: &mut Program, func: ModifierFunc, eval: &mut Eval) {
    modify_statements(&mut prog.statements, func, eval);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
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
        let statements = vec![Statement::Return(ExpressionStatement { token, value })];
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
            name: None,
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
        let mut eval = Eval::new();
        let turn_one_into_two: ModifierFunc = |expr: &mut Expression, _eval: &mut Eval| {
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
            modify_program(&mut test_input, turn_one_into_two, &mut eval);
            assert_eq!(test_input, test_value);
        }
    }
}
