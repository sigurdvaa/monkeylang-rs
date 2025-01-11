mod builtins;
mod environment;
pub mod r#macro;
#[cfg(test)]
pub mod tests;

use crate::ast::{
    BlockStatement, Expression, HashLiteral, IdentifierLiteral, IfExpression, Operator, Program,
    Statement,
};
use crate::object::{
    Array, BooleanObj, FunctionObj, HashKeyData, HashObj, Integer, IntegerObj, Object, StringObj,
};
pub use environment::{Env, Environment};
use r#macro::quote;
use std::collections::HashMap;
use std::rc::Rc;

// TODO: allocate only one Rc<Object> per bool, null, and none. Add to env? or introduce a struct
// for eval?
pub struct Eval {
    // TODO move env into struct
    // env: Env,
    obj_true: Rc<Object>,
    obj_false: Rc<Object>,
    obj_null: Rc<Object>,
    obj_none: Rc<Object>,
}

impl Eval {
    pub fn new() -> Self {
        Self {
            obj_true: Rc::new(Object::new_boolean(true)),
            obj_false: Rc::new(Object::new_boolean(false)),
            obj_null: Rc::new(Object::Null),
            obj_none: Rc::new(Object::None),
        }
    }

    fn get_obj_bool(&self, value: bool) -> Rc<Object> {
        match value {
            true => self.obj_true.clone(),
            false => self.obj_false.clone(),
        }
    }

    fn extend_function_env(&self, function: &FunctionObj, args: &[Rc<Object>]) -> Env {
        let env = Environment::new_enclosed(&function.env);
        for (i, param) in function.parameters.iter().enumerate() {
            env.set(param.value.to_owned(), args[i].clone());
        }
        env
    }

    fn apply_function(&self, function: &Rc<Object>, args: &[Rc<Object>]) -> Rc<Object> {
        match &**function {
            Object::Function(func) => {
                if func.parameters.len() != args.len() {
                    return Rc::new(Object::Error(format!(
                        "unmatched number of arguments in function call, expected {}, got {}",
                        func.parameters.len(),
                        args.len(),
                    )));
                }

                let extended_env = self.extend_function_env(func, args);
                let eval = self.eval_block_statement(&func.body, extended_env);
                match &*eval {
                    Object::Return(value) => value.clone(),
                    _ => eval,
                }
            }
            Object::Builtin(func) => func(args),
            _ => Rc::new(Object::Error(format!(
                "not a function: {}",
                function.kind()
            ))),
        }
    }

    fn eval_identifier(&self, identifier: &IdentifierLiteral, env: Env) -> Rc<Object> {
        if let Some(value) = env.get(&identifier.value) {
            return value;
        }

        Rc::new(Object::Error(format!(
            "identifier not found: {}",
            identifier.value
        )))
    }

    fn eval_array_index_expression(&self, left: &Array, index: &Integer) -> Rc<Object> {
        left.get(index.value as usize)
            .unwrap_or(&Rc::new(Object::Null))
            .clone()
    }

    fn eval_hash_index_expression(&self, left: &HashObj, index: &HashKeyData) -> Rc<Object> {
        match left.get(index) {
            Some((_key, value)) => value.clone(),
            None => Rc::new(Object::Null),
        }
    }

    fn eval_index_expression(&self, left: Rc<Object>, index: Rc<Object>) -> Rc<Object> {
        match (&*left, &*index) {
            (Object::Array(left), Object::Integer(index)) => {
                self.eval_array_index_expression(left, index)
            }
            (Object::Hash(left), _) => {
                let hash_key = match index.hash_key() {
                    Ok(hash_key) => hash_key,
                    Err(err) => return Rc::new(Object::Error(err.to_string())),
                };
                self.eval_hash_index_expression(left, &hash_key)
            }
            _ => Rc::new(Object::Error(format!(
                "index operator not supported for: {}",
                left.kind()
            ))),
        }
    }

    fn eval_minus_prefix_operator_expression(&self, right: Rc<Object>) -> Rc<Object> {
        Rc::new(match right.as_ref() {
            Object::Integer(obj) => Object::new_integer(-obj.value),
            _ => Object::Error(format!("unknown operator: -{}", right.kind())),
        })
    }

    fn eval_bang_operator_expression(&self, right: Rc<Object>) -> Rc<Object> {
        match right.as_ref() {
            Object::Null => self.get_obj_bool(true),
            Object::Integer(obj) => self.get_obj_bool(obj.value < 1),
            Object::Boolean(obj) => self.get_obj_bool(!obj.value),
            _ => {
                return Rc::new(Object::Error(format!(
                    "unknown operator: !{}",
                    right.kind()
                )))
            }
        }
    }

    fn eval_integer_infix_expression(
        &self,
        operator: &Operator,
        a: &IntegerObj,
        b: &IntegerObj,
    ) -> Rc<Object> {
        match operator {
            Operator::Plus => Rc::new(Object::new_integer(a.value + b.value)),
            Operator::Minus => Rc::new(Object::new_integer(a.value - b.value)),
            Operator::Asterisk => Rc::new(Object::new_integer(a.value * b.value)),
            Operator::Slash => Rc::new(Object::new_integer(a.value / b.value)),
            Operator::Gt => self.get_obj_bool(a > b),
            Operator::Lt => self.get_obj_bool(a < b),
            Operator::Eq => self.get_obj_bool(a == b),
            Operator::NotEq => self.get_obj_bool(a != b),
            _ => Rc::new(Object::Error(format!(
                "unknown integer operator: {operator}",
            ))),
        }
    }

    fn eval_boolean_infix_expression(
        &self,
        operator: &Operator,
        a: &BooleanObj,
        b: &BooleanObj,
    ) -> Rc<Object> {
        match operator {
            Operator::Eq => self.get_obj_bool(a.value == b.value),
            Operator::NotEq => self.get_obj_bool(a.value != b.value),
            _ => Rc::new(Object::Error(format!(
                "unknown boolean operator: {operator}",
            ))),
        }
    }

    fn eval_string_infix_expression(
        &self,
        operator: &Operator,
        a: &StringObj,
        b: &StringObj,
    ) -> Rc<Object> {
        match operator {
            Operator::Plus => Rc::new(Object::new_string(String::from_iter([
                a.value.as_str(),
                b.value.as_str(),
            ]))),
            Operator::Eq => self.get_obj_bool(a.value == b.value),
            Operator::NotEq => self.get_obj_bool(a.value != b.value),
            _ => Rc::new(Object::Error(format!(
                "unknown string operator: {operator}",
            ))),
        }
    }

    fn eval_null_infix_expression(&self, operator: &Operator, other: &Object) -> Rc<Object> {
        let truth = matches!(other, Object::Null);
        match operator {
            Operator::Eq => self.get_obj_bool(truth),
            Operator::NotEq => self.get_obj_bool(!truth),
            _ => Rc::new(Object::Error(format!("unknown null operator: {operator}",))),
        }
    }

    fn eval_hash_literal(&self, expr: &HashLiteral, env: Env) -> Rc<Object> {
        let mut pairs = HashMap::new();

        for (key_expr, value_expr) in &expr.pairs {
            let key = self.eval_expression(key_expr, env.clone());
            if let Object::Error(_) = *key {
                return key;
            }

            let hash_key = match key.hash_key() {
                Ok(hash_key) => hash_key,
                Err(err) => return Rc::new(Object::Error(err.to_string())),
            };

            let value = self.eval_expression(value_expr, env.clone());
            if let Object::Error(_) = *value {
                return value;
            }

            pairs.insert(hash_key, (key, value));
        }

        Rc::new(Object::Hash(pairs))
    }

    fn eval_infix_expression(
        &self,
        operator: &Operator,
        left: Rc<Object>,
        right: Rc<Object>,
    ) -> Rc<Object> {
        match (&*left, &*right) {
            (Object::Integer(a), Object::Integer(b)) => {
                self.eval_integer_infix_expression(operator, a, b)
            }
            (Object::Boolean(a), Object::Boolean(b)) => {
                self.eval_boolean_infix_expression(operator, a, b)
            }
            (Object::String(a), Object::String(b)) => {
                self.eval_string_infix_expression(operator, a, b)
            }
            (Object::Null, other) | (other, Object::Null) => {
                self.eval_null_infix_expression(operator, other)
            }
            (a, b) if a.kind() != b.kind() => Rc::new(Object::Error(format!(
                "type mismatch: {} {operator} {}",
                left.kind(),
                right.kind(),
            ))),
            _ => Rc::new(Object::Error(format!(
                "unknown operator: {} {operator} {}",
                left.kind(),
                right.kind(),
            ))),
        }
    }

    fn eval_prefix_expression(&self, operator: &Operator, right: Rc<Object>) -> Rc<Object> {
        match operator {
            Operator::Bang => self.eval_bang_operator_expression(right),
            Operator::Minus => self.eval_minus_prefix_operator_expression(right),
            _ => Rc::new(Object::Error(format!(
                "unknown operator: {operator}{}",
                right.kind(),
            ))),
        }
    }

    fn eval_if_expression(&self, expression: &IfExpression, env: Env) -> Rc<Object> {
        let condition = self.eval_expression(&expression.condition, env.clone());
        if let Object::Error(_) = *condition {
            return condition;
        }

        match condition.is_truthy() {
            true => self.eval_block_statement(&expression.consequence, env),
            false => match &expression.alternative {
                Some(alt) => self.eval_block_statement(alt, env),
                None => Rc::new(Object::Null),
            },
        }
    }

    fn eval_expressions(&self, expressions: &[Expression], env: Env) -> Vec<Rc<Object>> {
        let mut result = vec![];
        for expr in expressions {
            let eval = self.eval_expression(expr, env.clone());
            match *eval {
                Object::Error(_) => return vec![eval],
                _ => result.push(eval),
            }
        }
        result
    }

    fn eval_expression(&self, expression: &Expression, env: Env) -> Rc<Object> {
        match expression {
            Expression::Boolean(expr) => self.get_obj_bool(expr.value),
            Expression::Null(_token) => self.obj_null.clone(),
            Expression::Call(expr) => {
                // TODO: replace with tokenkind? will have to add TokenKind::Quote
                if expr.token.literal == "quote" {
                    return quote(expr.arguments[0].clone(), &env);
                }

                let func = self.eval_expression(&expr.function, env.clone());
                if let Object::Error(_) = *func {
                    return func;
                }

                let args = self.eval_expressions(&expr.arguments, env);
                if args.len() == 1 {
                    if let Object::Error(_) = *args[0] {
                        return args[0].clone();
                    }
                }

                self.apply_function(&func, &args)
            }
            Expression::Function(expr) => Rc::new(Object::Function(FunctionObj {
                parameters: expr.parameters.clone(),
                body: expr.body.clone(),
                env: env.clone(),
            })),
            Expression::Macro(expr) => {
                panic!("found Macro expression during evaluation: {:?}", expr)
            }
            Expression::Identifier(expr) => self.eval_identifier(expr, env),
            Expression::If(expr) => self.eval_if_expression(expr, env),
            Expression::Infix(expr) => {
                let left = self.eval_expression(&expr.left, env.clone());
                if let Object::Error(_) = *left {
                    return left;
                }

                let right = self.eval_expression(&expr.right, env);
                if let Object::Error(_) = *right {
                    return right;
                }

                self.eval_infix_expression(&expr.operator, left, right)
            }
            Expression::Integer(expr) => Rc::new(Object::new_integer(
                expr.value.try_into().expect("integer too large"),
            )),
            Expression::Prefix(expr) => {
                let right = self.eval_expression(&expr.right, env);
                match *right {
                    Object::Error(_) => right,
                    _ => self.eval_prefix_expression(&expr.operator, right),
                }
            }
            Expression::String(expr) => Rc::new(Object::new_string(expr.value.to_owned())),
            Expression::Array(expr) => {
                let array = self.eval_expressions(&expr.elements, env);
                if array.len() == 1 {
                    if let Object::Error(_) = *array[0] {
                        return array[0].clone();
                    }
                }
                Rc::new(Object::Array(array))
            }
            Expression::Index(expr) => {
                let left = self.eval_expression(&expr.left, env.clone());
                if let Object::Error(_) = *left {
                    return left;
                }

                let index = self.eval_expression(&expr.index, env);
                if let Object::Error(_) = *index {
                    return index;
                }

                self.eval_index_expression(left, index)
            }
            Expression::Hash(expr) => self.eval_hash_literal(expr, env),
        }
    }

    fn eval_statement(&self, statement: &Statement, env: Env) -> Rc<Object> {
        match statement {
            Statement::Let(expr) => {
                let value = self.eval_expression(&expr.value, env.clone());
                if let Object::Error(_) = *value {
                    return value;
                }
                env.set(expr.name.value.clone(), value);
                Rc::new(Object::None)
            }
            Statement::Return(expr) => {
                let eval = self.eval_expression(&expr.value, env);
                match *eval {
                    Object::Error(_) => eval,
                    _ => Rc::new(Object::Return(eval)),
                }
            }
            Statement::Expression(expr) => self.eval_expression(&expr.value, env),
        }
    }

    fn eval_block_statement(&self, block: &BlockStatement, env: Env) -> Rc<Object> {
        let mut result = self.obj_null.clone();
        for stmt in &block.statements {
            result = self.eval_statement(stmt, env.clone());
            if let Object::Return(_) | Object::Error(_) = *result {
                return result;
            }
        }
        result
    }

    pub fn eval_program(&self, program: &Program, env: Env) -> Rc<Object> {
        let mut result = self.obj_none.clone();
        for stmt in &program.statements {
            result = self.eval_statement(stmt, env.clone());
            match result.as_ref() {
                Object::Return(value) => return value.clone(),
                Object::Error(_) => return result,
                _ => (),
            }
        }
        result
    }
}
