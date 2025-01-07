use super::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type Env = Rc<Environment>;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    // TODO: use &str instead of String as key?
    store: RefCell<HashMap<String, Rc<Object>>>,
    outer: Option<Env>,
}

impl Environment {
    pub fn new() -> Env {
        Rc::new(Self {
            store: RefCell::new(HashMap::new()),
            outer: None,
        })
    }

    pub fn new_enclosed(env: &Env) -> Env {
        Rc::new(Self {
            store: RefCell::new(HashMap::new()),
            outer: Some(env.clone()),
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
