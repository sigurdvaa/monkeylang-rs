use super::{builtins, Object};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type Env = Rc<Environment>;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: RefCell<HashMap<String, Rc<Object>>>,
    outer: Option<Env>,
}

impl Environment {
    pub fn new() -> Env {
        let mut store = HashMap::new();
        for (name, builtin) in builtins::get_all() {
            store.insert(name.to_string(), builtin);
        }
        Rc::new(Self {
            store: RefCell::new(store),
            outer: None,
        })
    }

    pub fn new_enclosed(env: Env) -> Env {
        Rc::new(Self {
            store: RefCell::new(HashMap::new()),
            outer: Some(env),
        })
    }

    pub fn get(&self, name: &str) -> Option<Rc<Object>> {
        match self.store.borrow().get(name) {
            Some(v) => Some(v.clone()),
            None => match &self.outer {
                Some(outer) => outer.get(name),
                None => None,
            },
        }
    }

    pub fn set(&self, name: String, value: Rc<Object>) {
        self.store.borrow_mut().insert(name, value);
    }
}
