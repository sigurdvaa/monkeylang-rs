use crate::object::{self, Engine, Object};
use std::rc::Rc;

// TODO: merge with object/builtins? possible with the trait

pub fn get_all() -> Vec<(&'static str, Rc<Object>)> {
    let mut all = object::builtins::get_all();
    all.push(("map", Rc::new(Object::Builtin(map))));
    all
}

fn map(args: &[Rc<Object>], engine: &mut dyn Engine) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments to \"map\", got={}, want=2",
            args.len()
        )));
    }

    match (&*args[0], &*args[1]) {
        (Object::Array(values), Object::Function(_) | Object::Closure(_) | Object::Builtin(_)) => {
            let new = values
                .iter()
                .map(|i| engine.call_func(args[1].clone(), &[i.clone()]))
                .collect();
            Rc::new(Object::Array(new))
        }
        _ => Rc::new(Object::Error(format!(
            "argument to \"map\" not supported, got {} and {}",
            args[0].kind(),
            args[1].kind()
        ))),
    }
}
