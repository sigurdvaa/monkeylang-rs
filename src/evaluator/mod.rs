mod environment;
pub mod r#macro;
#[cfg(test)]
pub mod tests;

use crate::ast::{BlockStatement, Expression, HashLiteral, Operator, Program, Statement};
use crate::object::{builtins, Engine, FunctionObj, Object, ObjectUtil};
pub use environment::{Env, Environment};
use std::collections::{BTreeMap, HashMap, VecDeque};
use std::rc::Rc;

const RCPOOL_SIZE: usize = 512;

pub struct Eval {
    envs: Vec<Env>,
    ep: usize,
    constant_int: BTreeMap<usize, Rc<Object>>,
    constant_str: BTreeMap<String, Rc<Object>>,
    objutil: ObjectUtil,
    rcobjpool: VecDeque<Rc<Object>>,
    rcenvpool: VecDeque<Rc<Environment>>,
}

impl Engine for Eval {
    #[inline(always)]
    fn call_func(&mut self, func: Rc<Object>, args: &[Rc<Object>]) -> Rc<Object> {
        let obj = match func.as_ref() {
            Object::Function(func) => {
                if func.parameters.len() != args.len() {
                    return self.get_rcobj(Object::Error(format!(
                        "unmatched number of arguments in function call, expected {}, got {}",
                        func.parameters.len(),
                        args.len(),
                    )));
                }

                let extended_env = Environment::new_enclosed(self, func.env.clone());
                for (i, param) in func.parameters.iter().enumerate() {
                    extended_env.set(param.value.to_owned(), args[i].clone());
                }

                self.envs.push(extended_env);
                self.ep += 1;

                let eval = self.eval_block_statement(&func.body);

                if let Some(env) = self.envs.pop() {
                    self.return_rcenv(env);
                }
                self.ep -= 1;

                if let Object::Return(value) = eval.as_ref() {
                    let obj = value.clone();
                    self.return_rcobj(eval);
                    obj
                } else {
                    eval
                }
            }
            Object::Builtin(func) => func(args, self),
            _ => self.get_rcobj(Object::Error(format!("not a function: {}", func.kind()))),
        };
        self.return_rcobj(func);
        obj
    }

    fn get_objutil(&mut self) -> &mut ObjectUtil {
        &mut self.objutil
    }

    fn get_obj_null(&self) -> Rc<Object> {
        self.objutil.obj_null.clone()
    }

    fn get_obj_none(&self) -> Rc<Object> {
        self.objutil.obj_none.clone()
    }

    #[inline(always)]
    fn get_rcobj(&mut self, obj: Object) -> Rc<Object> {
        match self.rcobjpool.pop_front() {
            None => Rc::new(obj),
            Some(mut rc) => {
                *Rc::get_mut(&mut rc).expect("rc in use") = obj;
                rc
            }
        }
    }

    #[inline(always)]
    fn return_rcobj(&mut self, rc: Rc<Object>) {
        if Rc::strong_count(&rc) == 1 && self.rcobjpool.len() < RCPOOL_SIZE {
            self.rcobjpool.push_back(rc);
        }
    }
}

impl Eval {
    pub fn new() -> Self {
        let mut new = Self {
            envs: vec![],
            ep: 0,
            constant_int: BTreeMap::new(),
            constant_str: BTreeMap::new(),
            objutil: ObjectUtil::new(),
            rcobjpool: VecDeque::with_capacity(RCPOOL_SIZE),
            rcenvpool: VecDeque::with_capacity(RCPOOL_SIZE),
        };
        let env = Environment::new(&mut new);
        new.envs.push(env);
        new
    }

    #[inline(always)]
    fn get_rcenv(&mut self, env: Environment) -> Rc<Environment> {
        match self.rcenvpool.pop_front() {
            None => Rc::new(env),
            Some(mut rc) => {
                *Rc::get_mut(&mut rc).expect("rc in use") = env;
                rc
            }
        }
    }

    #[inline(always)]
    fn return_rcenv(&mut self, rc: Rc<Environment>) {
        if Rc::strong_count(&rc) == 1 && self.rcenvpool.len() < RCPOOL_SIZE {
            self.rcenvpool.push_back(rc);
        }
    }

    fn get_obj_bool(&self, value: bool) -> Rc<Object> {
        match value {
            true => self.objutil.obj_true.clone(),
            false => self.objutil.obj_false.clone(),
        }
    }

    fn eval_index_expression(&mut self, left: Rc<Object>, index: Rc<Object>) -> Rc<Object> {
        let obj = match (left.as_ref(), index.as_ref()) {
            (Object::Array(left), Object::Integer(index)) => match left.get(*index as usize) {
                Some(value) => value.clone(),
                None => self.objutil.obj_null.clone(),
            },
            (Object::Hash(left), _) => {
                let hash_key = match self.objutil.hash_key(&index) {
                    Ok(hash_key) => hash_key,
                    Err(err) => return self.get_rcobj(Object::Error(err.to_string())),
                };
                match left.get(&hash_key) {
                    Some((_key, value)) => value.clone(),
                    None => self.objutil.obj_null.clone(),
                }
            }
            _ => self.get_rcobj(Object::Error(format!(
                "index operator not supported for: {}",
                left.kind()
            ))),
        };
        self.return_rcobj(index);
        self.return_rcobj(left);
        obj
    }

    fn eval_hash_literal(&mut self, expr: &HashLiteral) -> Rc<Object> {
        let mut pairs = HashMap::with_capacity(expr.pairs.len());

        for (key_expr, value_expr) in &expr.pairs {
            let key = self.eval_expression(key_expr);
            if let Object::Error(_) = *key {
                return key;
            }

            let hash_key = match self.objutil.hash_key(&key) {
                Ok(hash_key) => hash_key,
                Err(err) => return self.get_rcobj(Object::Error(err.to_string())),
            };

            let value = self.eval_expression(value_expr);
            if let Object::Error(_) = *value {
                return value;
            }

            pairs.insert(hash_key, (key, value));
        }

        self.get_rcobj(Object::Hash(pairs))
    }

    #[inline(always)]
    fn eval_infix_expression(
        &mut self,
        operator: &Operator,
        left: Rc<Object>,
        right: Rc<Object>,
    ) -> Rc<Object> {
        let obj = match (left.as_ref(), right.as_ref()) {
            (Object::Integer(a), Object::Integer(b)) => match operator {
                Operator::Plus => self.get_rcobj(Object::Integer(a + b)),
                Operator::Minus => self.get_rcobj(Object::Integer(a - b)),
                Operator::Asterisk => self.get_rcobj(Object::Integer(a * b)),
                Operator::Slash => self.get_rcobj(Object::Integer(a / b)),
                Operator::Gt => self.get_obj_bool(a > b),
                Operator::Lt => self.get_obj_bool(a < b),
                Operator::Eq => self.get_obj_bool(a == b),
                Operator::NotEq => self.get_obj_bool(a != b),
                _ => self.get_rcobj(Object::Error(format!(
                    "unknown integer operator: {operator}",
                ))),
            },
            (Object::Boolean(a), Object::Boolean(b)) => match operator {
                Operator::Eq => self.get_obj_bool(a == b),
                Operator::NotEq => self.get_obj_bool(a != b),
                _ => self.get_rcobj(Object::Error(format!(
                    "unknown boolean operator: {operator}",
                ))),
            },
            (Object::String(a), Object::String(b)) => match operator {
                Operator::Plus => {
                    self.get_rcobj(Object::String(String::from_iter([a.as_str(), b.as_str()])))
                }
                Operator::Eq => self.get_obj_bool(a == b),
                Operator::NotEq => self.get_obj_bool(a != b),
                _ => self.get_rcobj(Object::Error(format!(
                    "unknown string operator: {operator}",
                ))),
            },
            (Object::Null, other) | (other, Object::Null) => {
                let truth = matches!(other, Object::Null);
                match operator {
                    Operator::Eq => self.get_obj_bool(truth),
                    Operator::NotEq => self.get_obj_bool(!truth),
                    _ => {
                        self.get_rcobj(Object::Error(format!("unknown null operator: {operator}",)))
                    }
                }
            }
            (a, b) if a.kind() != b.kind() => self.get_rcobj(Object::Error(format!(
                "type mismatch: {} {operator} {}",
                left.kind(),
                right.kind(),
            ))),
            _ => self.get_rcobj(Object::Error(format!(
                "unknown operator: {} {operator} {}",
                left.kind(),
                right.kind(),
            ))),
        };
        self.return_rcobj(left);
        self.return_rcobj(right);
        obj
    }

    fn eval_prefix_expression(&mut self, operator: &Operator, right: Rc<Object>) -> Rc<Object> {
        let obj = match operator {
            Operator::Bang => match right.as_ref() {
                Object::Null => self.get_obj_bool(true),
                Object::Integer(value) => self.get_obj_bool(*value < 1),
                Object::Boolean(value) => self.get_obj_bool(!value),
                _ => self.get_rcobj(Object::Error(format!(
                    "unknown operator: !{}",
                    right.kind()
                ))),
            },
            Operator::Minus => self.get_rcobj(match right.as_ref() {
                Object::Integer(value) => Object::Integer(-value),
                _ => Object::Error(format!("unknown operator: -{}", right.kind())),
            }),
            _ => self.get_rcobj(Object::Error(format!(
                "unknown operator: {operator}{}",
                right.kind(),
            ))),
        };
        self.return_rcobj(right);
        obj
    }

    fn eval_expressions(&mut self, expressions: &[Expression]) -> Vec<Rc<Object>> {
        let mut result = vec![];
        for expr in expressions {
            let eval = self.eval_expression(expr);
            match *eval {
                Object::Error(_) => return vec![eval],
                _ => result.push(eval),
            }
        }
        result
    }

    fn eval_expression(&mut self, expression: &Expression) -> Rc<Object> {
        match expression {
            Expression::Boolean(expr) => self.get_obj_bool(expr.value),
            Expression::Null(_token) => self.objutil.obj_null.clone(),
            Expression::Call(expr) => {
                let func = self.eval_expression(&expr.function);
                if let Object::Error(_) = *func {
                    return func;
                }
                let args = self.eval_expressions(&expr.arguments);
                if args.len() == 1 {
                    if let Object::Error(_) = *args[0] {
                        return args[0].clone();
                    }
                }
                let obj = self.call_func(func, &args);
                for arg in args {
                    self.return_rcobj(arg);
                }
                obj
            }
            Expression::Quote(expr) => self.quote(expr.arguments[0].clone()),
            Expression::Unquote(expr) => {
                panic!(
                    "\"unquote\" can't be used outside of \"quote\": {}",
                    expr.token
                )
            }
            Expression::Function(expr) => self.get_rcobj(Object::Function(Box::new(FunctionObj {
                parameters: expr.parameters.clone(),
                body: expr.body.clone(),
                env: self.envs[self.ep].clone(),
            }))),
            Expression::Macro(expr) => {
                panic!("found Macro expression during evaluation: {:?}", expr)
            }
            Expression::Identifier(expr) => match self.envs[self.ep].get(&expr.value) {
                Some(value) => value.clone(),
                None => self.get_rcobj(Object::Error(format!(
                    "identifier not found: {}",
                    expr.value
                ))),
            },
            Expression::If(expr) => {
                let condition = self.eval_expression(&expr.condition);
                if let Object::Error(_) = *condition {
                    return condition;
                }
                let obj = match condition.is_truthy() {
                    true => self.eval_block_statement(&expr.consequence),
                    false => match &expr.alternative {
                        Some(alt) => self.eval_block_statement(alt),
                        None => self.objutil.obj_null.clone(),
                    },
                };
                self.return_rcobj(condition);
                obj
            }
            Expression::Infix(expr) => {
                let left = self.eval_expression(&expr.left);
                if let Object::Error(_) = *left {
                    return left;
                }
                let right = self.eval_expression(&expr.right);
                if let Object::Error(_) = *right {
                    return right;
                }
                self.eval_infix_expression(&expr.operator, left, right)
            }
            Expression::Prefix(expr) => {
                let right = self.eval_expression(&expr.right);
                match *right {
                    Object::Error(_) => right,
                    _ => self.eval_prefix_expression(&expr.operator, right),
                }
            }
            Expression::Integer(expr) => {
                if let Some(obj) = self.constant_int.get(&expr.value) {
                    return obj.clone();
                }
                let obj = self.get_rcobj(match expr.value.try_into() {
                    Ok(value) => Object::Integer(value),
                    Err(err) => Object::Error(format!("invalid interger, {err}")),
                });
                self.constant_int.insert(expr.value, obj.clone());
                obj
            }
            Expression::String(expr) => {
                if let Some(obj) = self.constant_str.get(&expr.value) {
                    return obj.clone();
                }
                let obj = self.get_rcobj(Object::String(expr.value.to_owned()));
                self.constant_str.insert(expr.value.clone(), obj.clone());
                obj
            }
            Expression::Array(expr) => {
                let array = self.eval_expressions(&expr.elements);
                if array.len() == 1 {
                    if let Object::Error(_) = *array[0] {
                        return array[0].clone();
                    }
                }
                self.get_rcobj(Object::Array(array))
            }
            Expression::Index(expr) => {
                let left = self.eval_expression(&expr.left);
                if let Object::Error(_) = *left {
                    return left;
                }
                let index = self.eval_expression(&expr.index);
                if let Object::Error(_) = *index {
                    return index;
                }
                self.eval_index_expression(left, index)
            }
            Expression::Hash(expr) => self.eval_hash_literal(expr),
            Expression::Loop(expr) => loop {
                let result = self.eval_loop_block_statement(&expr.body);
                match result.as_ref() {
                    Object::Return(_) => return result,
                    Object::Break(value) => return value.clone(),
                    _ => (),
                }
            },
        }
    }

    #[inline(always)]
    fn eval_statement(&mut self, statement: &Statement) -> Rc<Object> {
        match statement {
            Statement::Let(stmt) => {
                let value = self.eval_expression(&stmt.value);
                if let Object::Error(_) = *value {
                    return value;
                }
                self.envs[self.ep].set(stmt.name.value.clone(), value);
                self.objutil.obj_none.clone()
            }
            Statement::Return(stmt) => {
                let eval = self.eval_expression(&stmt.value);
                match *eval {
                    Object::Error(_) => eval,
                    _ => self.get_rcobj(Object::Return(eval)),
                }
            }
            Statement::Expression(stmt) => self.eval_expression(&stmt.value),
            Statement::Exit(stmt) => {
                let obj = self.eval_expression(&stmt.value);
                match obj.as_ref() {
                    Object::Error(_) => obj,
                    Object::Integer(value) if i32::try_from(*value).is_ok() => {
                        std::process::exit(*value as i32)
                    }
                    _ => self.get_rcobj(Object::Error(format!(
                        "invalid exit code: {obj} ({})",
                        obj.kind()
                    ))),
                }
            }
            Statement::Break(stmt) => {
                let eval = self.eval_expression(&stmt.value);
                match eval.as_ref() {
                    Object::Error(_) => eval,
                    _ => self.get_rcobj(Object::Break(eval)),
                }
            }
        }
    }

    #[inline(always)]
    fn eval_block_statement(&mut self, block: &BlockStatement) -> Rc<Object> {
        let mut result = self.objutil.obj_null.clone();
        for stmt in &block.statements {
            result = self.eval_statement(stmt);
            if let Object::Return(_) | Object::Error(_) = *result {
                return result;
            }
        }
        result
    }

    #[inline(always)]
    fn eval_loop_block_statement(&mut self, block: &BlockStatement) -> Rc<Object> {
        let mut result = self.objutil.obj_null.clone();
        for stmt in &block.statements {
            result = self.eval_statement(stmt);
            if let Object::Break(_) | Object::Return(_) | Object::Error(_) = *result {
                return result;
            }
        }
        result
    }

    pub fn eval_program(&mut self, program: &Program) -> Rc<Object> {
        let mut result = self.objutil.obj_none.clone();
        for stmt in &program.statements {
            result = self.eval_statement(stmt);
            match result.as_ref() {
                Object::Return(value) => return value.clone(),
                Object::Error(_) => return result,
                _ => (),
            }
        }
        result
    }
}
