use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type Symbols = Rc<SymbolTable>;

#[derive(Debug, PartialEq)]
pub enum SymbolScope {
    Global,
    Local,
}

impl SymbolScope {
    pub fn _value(&self) -> &'static str {
        match self {
            Self::Global => "GLOBAL",
            Self::Local => "LOCAL",
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

#[derive(Debug, PartialEq)]
pub struct SymbolTable {
    store: RefCell<HashMap<String, Rc<Symbol>>>,
    num_definitions: usize, // TODO: remove/replace?
    pub outer: Option<Rc<SymbolTable>>,
}

impl SymbolTable {
    pub fn new() -> Symbols {
        Rc::new(Self {
            store: RefCell::new(HashMap::new()),
            num_definitions: 0,
            outer: None,
        })
    }

    pub fn new_enclosed(outer: Symbols) -> Symbols {
        Rc::new(Self {
            store: RefCell::new(HashMap::new()),
            num_definitions: 0,
            outer: Some(outer),
        })
    }

    pub fn len(&self) -> usize {
        self.store.borrow().len()
    }

    pub fn define(&self, name: String) -> Rc<Symbol> {
        let scope = match self.outer {
            Some(_) => SymbolScope::Local,
            None => SymbolScope::Global,
        };
        let sym = Rc::new(Symbol {
            name: name.clone(),
            index: self.len(),
            scope,
        });
        self.store.borrow_mut().insert(name, sym.clone());
        sym
    }

    pub fn resolve(&self, name: &str) -> Option<Rc<Symbol>> {
        match self.store.borrow().get(name) {
            Some(v) => Some(v.clone()),
            None => match &self.outer {
                Some(outer) => outer.resolve(name),
                None => None,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_symbol_global(name: &str, index: usize) -> Rc<Symbol> {
        Rc::new(Symbol {
            name: name.into(),
            scope: SymbolScope::Global,
            index,
        })
    }

    fn make_symbol_local(name: &str, index: usize) -> Rc<Symbol> {
        Rc::new(Symbol {
            name: name.into(),
            scope: SymbolScope::Local,
            index,
        })
    }

    #[test]
    fn test_define() {
        let test: HashMap<&'static str, Rc<Symbol>> = HashMap::from_iter([
            ("a", make_symbol_global("a", 0)),
            ("b", make_symbol_global("b", 1)),
            ("c", make_symbol_local("c", 0)),
            ("d", make_symbol_local("d", 1)),
            ("e", make_symbol_local("e", 0)),
            ("f", make_symbol_local("f", 1)),
        ]);

        let global = SymbolTable::new();
        let a = global.define("a".into());
        let b = global.define("b".into());
        assert_eq!(a, test["a"]);
        assert_eq!(b, test["b"]);

        let first_local = SymbolTable::new_enclosed(global.clone());
        let c = first_local.define("c".into());
        let d = first_local.define("d".into());
        assert_eq!(c, test["c"]);
        assert_eq!(d, test["d"]);

        let second_local = SymbolTable::new_enclosed(first_local.clone());
        let e = second_local.define("e".into());
        let f = second_local.define("f".into());
        assert_eq!(e, test["e"]);
        assert_eq!(f, test["f"]);
    }

    #[test]
    fn test_resolve_global() {
        let tests = [make_symbol_global("a", 0), make_symbol_global("b", 1)];

        let global = SymbolTable::new();
        global.define("a".into());
        global.define("b".into());

        for test in tests {
            let sym = global.resolve(&test.name);
            assert_eq!(sym, Some(test));
        }
    }

    #[test]
    fn test_resolve_local() {
        let tests = [
            make_symbol_global("a", 0),
            make_symbol_global("b", 1),
            make_symbol_local("c", 0),
            make_symbol_local("d", 1),
        ];

        let global = SymbolTable::new();
        global.define("a".into());
        global.define("b".into());

        let local = SymbolTable::new_enclosed(global);
        local.define("c".into());
        local.define("d".into());

        for test in tests {
            let sym = local.resolve(&test.name);
            assert_eq!(sym, Some(test));
        }
    }

    #[test]
    fn test_resolve_nested_local() {
        let global = SymbolTable::new();
        global.define("a".into());
        global.define("b".into());

        let first_local = SymbolTable::new_enclosed(global);
        first_local.define("c".into());
        first_local.define("d".into());

        let second_local = SymbolTable::new_enclosed(first_local.clone());
        second_local.define("e".into());
        second_local.define("f".into());

        let tests = [
            (&first_local, make_symbol_global("a", 0)),
            (&first_local, make_symbol_global("b", 1)),
            (&first_local, make_symbol_local("c", 0)),
            (&first_local, make_symbol_local("d", 1)),
            (&second_local, make_symbol_global("a", 0)),
            (&second_local, make_symbol_global("b", 1)),
            (&second_local, make_symbol_local("e", 0)),
            (&second_local, make_symbol_local("f", 1)),
        ];

        for (scope, test) in tests {
            let sym = scope.resolve(&test.name);
            assert_eq!(sym, Some(test));
        }
    }
}
