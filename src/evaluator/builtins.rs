use super::apply_function;
use crate::object::{self, Object};
use std::rc::Rc;

pub fn get(name: &str) -> Option<Rc<Object>> {
    match name {
        "map" => Some(Rc::new(Object::Builtin(map))),
        _ => object::builtins::get(name),
    }
}

fn map(args: &[Rc<Object>]) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments, got={}, want=2",
            args.len()
        )));
    }

    Rc::new(match (&*args[0], &*args[1]) {
        (Object::Array(value), Object::Function(_)) => {
            let new = value
                .iter()
                .map(|i| apply_function(&args[1], &[i.clone()]))
                .collect();
            Object::Array(new)
        }
        _ => Object::Error(format!(
            "argument to \"map\" not supported, got {} and {}",
            args[0].kind(),
            args[1].kind()
        )),
    })
}
