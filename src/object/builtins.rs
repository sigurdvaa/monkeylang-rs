use super::Object;
use std::{io::Write, rc::Rc};

pub fn get(name: &str) -> Option<Rc<Object>> {
    match name {
        "len" => Some(Rc::new(Object::Builtin(len))),
        "first" => Some(Rc::new(Object::Builtin(first))),
        "last" => Some(Rc::new(Object::Builtin(last))),
        "rest" => Some(Rc::new(Object::Builtin(rest))),
        "push" => Some(Rc::new(Object::Builtin(push))),
        "puts" => Some(Rc::new(Object::Builtin(puts))),
        "string" => Some(Rc::new(Object::Builtin(string))),
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
    if args.len() != 2 && args.len() != 3 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments, got={}, want=2 or 3",
            args.len()
        )));
    }

    Rc::new(match &*args[0] {
        Object::Array(value) => {
            let mut new = value.clone();
            if args.len() == 3 {
                match &*args[2] {
                    Object::Integer(i) => new.insert(i.value as usize, args[1].clone()),
                    _ => {
                        return Rc::new(Object::Error(format!(
                            "3rd argument to \"push\" not supported, got {}, want=INTEGER",
                            args[2].kind()
                        )))
                    }
                }
            } else {
                new.push(args[1].clone());
            }
            Object::Array(new)
        }
        _ => Object::Error(format!(
            "argument to \"push\" not supported, got {}",
            args.iter().map(|v| v.kind()).collect::<Vec<_>>().join(", ")
        )),
    })
}

// TODO: rework puts? print first arg, second arg to control flush
fn puts(args: &[Rc<Object>]) -> Rc<Object> {
    let mut output = std::io::stdout().lock();
    for arg in args {
        match arg.as_ref() {
            Object::String(_) => {
                write!(output, "{arg}").expect("builtin \"puts\" failed writing to stdout");
                let _ = output.flush();
            }
            _ => write!(output, "{}", arg.inspect())
                .expect("builtin \"puts\" failed writing to stdout"),
        }
    }
    Rc::new(Object::None)
}

fn string(args: &[Rc<Object>]) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments, got={}, want=1",
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
