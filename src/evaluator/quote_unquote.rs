use crate::ast::Expression;
use crate::object::Object;
use std::rc::Rc;

pub fn quote(expr: Expression) -> Rc<Object> {
    Rc::new(Object::Quote(expr))
}
