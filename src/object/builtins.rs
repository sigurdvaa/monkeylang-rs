use super::{Engine, Object};
use std::io::{stdout, Write};
use std::rc::Rc;

pub fn get_all() -> Vec<(&'static str, Rc<Object>)> {
    vec![
        ("len", Rc::new(const { Object::Builtin(len) })),
        ("first", Rc::new(const { Object::Builtin(first) })),
        ("last", Rc::new(const { Object::Builtin(last) })),
        ("rest", Rc::new(const { Object::Builtin(rest) })),
        ("push", Rc::new(const { Object::Builtin(push) })),
        ("puts", Rc::new(const { Object::Builtin(puts) })),
        ("string", Rc::new(const { Object::Builtin(string) })),
        ("insert", Rc::new(const { Object::Builtin(insert) })),
    ]
}

fn len(args: &[Rc<Object>], _engine: &mut dyn Engine) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments to \"len\". got={}, want=1",
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

fn first(args: &[Rc<Object>], engine: &mut dyn Engine) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments to \"first\". got={}, want=1",
            args.len()
        )));
    }

    match &*args[0] {
        Object::Array(value) => match value.first() {
            Some(value) => value.clone(),
            None => engine.get_null(),
        },
        _ => Rc::new(Object::Error(format!(
            "argument to \"first\" not supported, got {}",
            args[0].kind()
        ))),
    }
}

fn last(args: &[Rc<Object>], engine: &mut dyn Engine) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments to \"last\". got={}, want=1",
            args.len()
        )));
    }

    match &*args[0] {
        Object::Array(value) => match value.last() {
            Some(value) => value.clone(),
            None => engine.get_null(),
        },
        _ => Rc::new(Object::Error(format!(
            "argument to \"last\" not supported, got {}",
            args[0].kind()
        ))),
    }
}

fn rest(args: &[Rc<Object>], engine: &mut dyn Engine) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments to \"rest\". got={}, want=1",
            args.len()
        )));
    }

    match &*args[0] {
        Object::Array(value) => {
            if value.is_empty() {
                engine.get_null()
            } else {
                Rc::new(Object::Array(value.iter().skip(1).cloned().collect()))
            }
        }
        _ => Rc::new(Object::Error(format!(
            "argument to \"rest\" not supported, got {}",
            args[0].kind()
        ))),
    }
}

fn push(args: &[Rc<Object>], _engine: &mut dyn Engine) -> Rc<Object> {
    if args.len() != 2 && args.len() != 3 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments to \"push\", got={}, want=2",
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
            "arguments to \"push\" not supported, got {}",
            args.iter().map(|v| v.kind()).collect::<Vec<_>>().join(", ")
        )),
    })
}

fn insert(args: &[Rc<Object>], _engine: &mut dyn Engine) -> Rc<Object> {
    if args.len() != 3 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments \"insert\", got={}, want=3",
            args.len()
        )));
    }

    Rc::new(match &*args[0] {
        Object::Array(value) => {
            let mut new = value.clone();
            match &*args[1] {
                Object::Integer(i) => new.insert(i.value as usize, args[2].clone()),
                _ => {
                    return Rc::new(Object::Error(format!(
                        "2nd argument to \"insert\" not supported, got {}, want=INTEGER",
                        args[1].kind()
                    )))
                }
            }
            Object::Array(new)
        }
        _ => Object::Error(format!(
            "arguments to \"insert\" not supported, got {}",
            args.iter().map(|v| v.kind()).collect::<Vec<_>>().join(", ")
        )),
    })
}

fn puts(args: &[Rc<Object>], engine: &mut dyn Engine) -> Rc<Object> {
    let mut out = stdout().lock();
    for arg in args {
        _ = match arg.as_ref() {
            Object::String(_) => write!(out, "{arg}"),
            _ => write!(out, "{}", arg.inspect()),
        };
        _ = out.flush();
    }
    engine.get_none()
}

fn string(args: &[Rc<Object>], _engine: &mut dyn Engine) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments to \"string\", got={}, want=1",
            args.len()
        )));
    }

    Rc::new(match &*args[0] {
        Object::String(obj) => Object::new_string(obj.value.clone()),
        Object::Integer(obj) => Object::new_string(obj.value.to_string()),
        Object::Array(value) => Object::new_string(
            value
                .iter()
                .map(|i| i.to_string())
                .collect::<Vec<_>>()
                .join(", "),
        ),
        _ => Object::Error(format!(
            "argument to \"string\" not supported, got {}",
            args[0].kind()
        )),
    })
}
