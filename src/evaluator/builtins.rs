use super::apply_function;
use crate::object::Object;
use std::rc::Rc;

pub fn get(name: &str) -> Option<Rc<Object>> {
    match name {
        "len" => Some(Rc::new(Object::Builtin(len))),
        "first" => Some(Rc::new(Object::Builtin(first))),
        "last" => Some(Rc::new(Object::Builtin(last))),
        "rest" => Some(Rc::new(Object::Builtin(rest))),
        "push" => Some(Rc::new(Object::Builtin(push))),
        "map" => Some(Rc::new(Object::Builtin(map))),
        _ => None,
    }
}

fn len(args: &[Rc<Object>]) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    Rc::new(match &*args[0] {
        Object::String(obj) => Object::new_integer(obj.value.len() as isize),
        Object::Array(value) => Object::new_integer(value.len() as isize),
        _ => Object::Error(format!(
            "argument to \"len\" not supported, got {}",
            args[0].kind()
        )),
    })
}

fn first(args: &[Rc<Object>]) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    match &*args[0] {
        Object::Array(value) => value.first().unwrap_or(&Rc::new(Object::Null)).clone(),
        _ => Rc::new(Object::Error(format!(
            "argument to \"first\" not supported, got {}",
            args[0].kind()
        ))),
    }
}

fn last(args: &[Rc<Object>]) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    match &*args[0] {
        Object::Array(value) => value.last().unwrap_or(&Rc::new(Object::Null)).clone(),
        _ => Rc::new(Object::Error(format!(
            "argument to \"last\" not supported, got {}",
            args[0].kind()
        ))),
    }
}

fn rest(args: &[Rc<Object>]) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    Rc::new(match &*args[0] {
        Object::Array(value) => {
            if value.is_empty() {
                Object::Null
            } else {
                Object::Array(value.iter().skip(1).cloned().collect())
            }
        }
        _ => Object::Error(format!(
            "argument to \"rest\" not supported, got {}",
            args[0].kind()
        )),
    })
}

fn push(args: &[Rc<Object>]) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=2",
            args.len()
        )));
    }

    Rc::new(match &*args[0] {
        Object::Array(value) => {
            let mut new = value.clone();
            new.push(args[1].clone());
            Object::Array(new)
        }
        _ => Object::Error(format!(
            "argument to \"push\" not supported, got {} and {}",
            args[0].kind(),
            args[1].kind()
        )),
    })
}

fn map(args: &[Rc<Object>]) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments. got={}, want=2",
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
