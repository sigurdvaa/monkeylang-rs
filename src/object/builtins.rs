use super::Object;
use std::rc::Rc;

pub fn get(name: &str) -> Option<Rc<Object>> {
    match name {
        "len" => Some(Rc::new(const { Object::Builtin(len) })),
        "first" => Some(Rc::new(const { Object::Builtin(first) })),
        "last" => Some(Rc::new(const { Object::Builtin(last) })),
        "rest" => Some(Rc::new(const { Object::Builtin(rest) })),
        "push" => Some(Rc::new(const { Object::Builtin(push) })),
        "puts" => Some(Rc::new(const { Object::Builtin(puts) })),
        "string" => Some(Rc::new(const { Object::Builtin(string) })),
        _ => None,
    }
}

pub fn get_all() -> &'static [(&'static str, Object)] {
    &[
        ("len", const { Object::Builtin(len) }),
        ("first", const { Object::Builtin(first) }),
        ("last", const { Object::Builtin(last) }),
        ("rest", const { Object::Builtin(rest) }),
        ("push", const { Object::Builtin(push) }),
        ("puts", const { Object::Builtin(puts) }),
        ("string", const { Object::Builtin(string) }),
    ]
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
                // if 3 args, insert value in array with 3rd arg as index
                // TODO: replace with insert builtin?
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
            "arguments to \"push\" not supported, got {}",
            args.iter().map(|v| v.kind()).collect::<Vec<_>>().join(", ")
        )),
    })
}

fn puts(args: &[Rc<Object>]) -> Rc<Object> {
    for arg in args {
        match arg.as_ref() {
            Object::String(_) => {
                print!("{arg}");
            }
            _ => print!("{}", arg.inspect()),
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
