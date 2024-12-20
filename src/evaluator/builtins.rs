use crate::object::Object;

pub fn get(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::Builtin(len)),
        _ => None,
    }
}

fn len(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::String(value) => Object::Integer(value.len() as isize),
        _ => Object::Error(format!(
            "argument to \"len\" not supported, got {}",
            args[0].kind()
        )),
    }
}
