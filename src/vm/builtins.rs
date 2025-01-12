use crate::code::Opcode;
use crate::object::{self, Engine, Object};
use std::rc::Rc;

pub fn get_all() -> Vec<(&'static str, Rc<Object>)> {
    let mut all = object::builtins::get_all();
    // all.push(("map", Rc::new(Object::Builtin(map))));
    all
}

const MAP_INS: &[u8] = &[Opcode::Constant as u8, 0];

fn map(args: &[Rc<Object>], engine: &mut dyn Engine) -> Rc<Object> {
    if args.len() != 2 {
        return Rc::new(Object::Error(format!(
            "wrong number of arguments to \"map\", got={}, want=2",
            args.len()
        )));
    }

    match (&*args[0], &*args[1]) {
        (Object::Array(_value), Object::Function(_)) => {
            // add frame with ins to collect num args into array
            //   const with &[Instructions]
            //   per args, add another frame
            //     fn call_closure( &mut self, frame: Frame, closure: Rc<ClosureObj>, num_args: usize,) -> Result<Frame, VmError>
            //     frame.sp += 1?
            //   fn build_array(&mut self, start_idx: usize, end_idx: usize) -> Result<Object, VmError>
            //
            engine.get_obj_none() // tmp bogus value, end of const frame should replace this
        }
        (Object::Array(value), Object::Builtin(_)) => {
            let new = value
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
