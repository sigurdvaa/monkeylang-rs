use std::cell::{Cell, RefCell};
use std::collections::{hash_map::Entry, HashMap};
use std::rc::Rc;

pub type Symbols = Rc<SymbolTable>;

#[derive(Debug, PartialEq)]
pub enum SymbolScope {
    Global,
    Local,
    Builtin,
    Free,
    Function,
}

impl SymbolScope {
    pub fn _value(&self) -> &'static str {
        match self {
            Self::Global => "GLOBAL",
            Self::Local => "LOCAL",
            Self::Builtin => "BUILTIN",
            Self::Free => "FREE",
            Self::Function => "FUNCTION",
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
    pub num_definitions: Cell<usize>,
    pub outer: Option<Rc<SymbolTable>>,
    pub free: RefCell<Vec<Rc<Symbol>>>,
}

impl SymbolTable {
    pub fn new() -> Symbols {
        Rc::new(Self {
            store: RefCell::new(HashMap::new()),
            num_definitions: Cell::new(0),
            outer: None,
            free: RefCell::new(vec![]),
        })
    }

    pub fn new_enclosed(outer: Symbols) -> Symbols {
        Rc::new(Self {
            store: RefCell::new(HashMap::new()),
            num_definitions: Cell::new(0),
            outer: Some(outer),
            free: RefCell::new(vec![]),
        })
    }

    // TODO: causes issues with shadowing
    pub fn define(&self, name: String) -> Rc<Symbol> {
        let scope = match self.outer {
            Some(_) => SymbolScope::Local,
            None => SymbolScope::Global,
        };
        match self.store.borrow_mut().entry(name.clone()) {
            Entry::Occupied(sym) => sym.get().clone(),
            Entry::Vacant(slot) => {
                let index = self.num_definitions.get();
                self.num_definitions.set(index + 1);
                let sym = Rc::new(Symbol {
                    name: name.clone(),
                    index,
                    scope,
                });
                slot.insert(sym.clone());
                sym
            }
        }
    }

    pub fn define_free(&self, original: Rc<Symbol>) -> Rc<Symbol> {
        let sym = Rc::new(Symbol {
            name: original.name.clone(),
            index: self.free.borrow().len(),
            scope: SymbolScope::Free,
        });
        self.free.borrow_mut().push(original);
        sym
    }

    pub fn define_builtin(&self, index: usize, name: String) -> Rc<Symbol> {
        let sym = Rc::new(Symbol {
            name: name.clone(),
            index,
            scope: SymbolScope::Builtin,
        });
        self.store.borrow_mut().insert(name, sym.clone());
        sym
    }

    pub fn define_function_name(&self, name: String) -> Rc<Symbol> {
        let sym = Rc::new(Symbol {
            name: name.clone(),
            index: 0,
            scope: SymbolScope::Function,
        });
        self.store.borrow_mut().insert(name, sym.clone());
        sym
    }

    pub fn resolve(&self, name: &str) -> Option<Rc<Symbol>> {
        match self.store.borrow().get(name) {
            Some(v) => Some(v.clone()),
            None => match &self.outer {
                Some(outer) => outer.resolve(name).map(|sym| match sym.scope {
                    SymbolScope::Global | SymbolScope::Builtin => sym,
                    _ => self.define_free(sym),
                }),
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

    fn make_symbol_free(name: &str, index: usize) -> Rc<Symbol> {
        Rc::new(Symbol {
            name: name.into(),
            scope: SymbolScope::Free,
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

    fn make_symbol_builtin(name: &str, index: usize) -> Rc<Symbol> {
        Rc::new(Symbol {
            name: name.into(),
            scope: SymbolScope::Builtin,
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

    #[test]
    fn test_define_resolve_builtins() {
        let global = SymbolTable::new();
        let first_local = SymbolTable::new_enclosed(global.clone());
        let second_local = SymbolTable::new_enclosed(first_local.clone());

        let tests = [
            make_symbol_builtin("a", 0),
            make_symbol_builtin("c", 1),
            make_symbol_builtin("e", 2),
            make_symbol_builtin("f", 3),
        ];

        for (i, sym) in tests.iter().enumerate() {
            global.define_builtin(i, sym.name.clone());
        }

        let scopes = [global, first_local, second_local];
        for scope in scopes {
            for test in &tests {
                let sym = scope.resolve(&test.name);
                assert_eq!(sym, Some(test.clone()));
            }
        }
    }

    #[test]
    fn test_resolve_free() {
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
            (&second_local, make_symbol_free("c", 0)),
            (&second_local, make_symbol_free("d", 1)),
            (&second_local, make_symbol_local("e", 0)),
            (&second_local, make_symbol_local("f", 1)),
        ];

        for (scope, test) in tests {
            let sym = scope.resolve(&test.name);
            assert_eq!(sym, Some(test));
        }
    }

    #[test]
    fn test_resolve_unresolvable_free() {
        let global = SymbolTable::new();
        global.define("a".into());

        let first_local = SymbolTable::new_enclosed(global);
        first_local.define("c".into());

        let second_local = SymbolTable::new_enclosed(first_local.clone());
        second_local.define("e".into());
        second_local.define("f".into());

        let tests_some = [
            make_symbol_global("a", 0),
            make_symbol_free("c", 0),
            make_symbol_local("e", 0),
            make_symbol_local("f", 1),
        ];

        let tests_none = ["b", "d"];

        for some in tests_some {
            let sym = second_local.resolve(&some.name);
            assert_eq!(sym, Some(some));
        }
        for none in tests_none {
            let sym = second_local.resolve(none);
            assert_eq!(sym, None);
        }
    }

    #[test]
    fn test_define_and_resolve_function_name() {
        let global = SymbolTable::new();
        global.define_function_name("a".into());

        let test = Symbol {
            name: "a".into(),
            scope: SymbolScope::Function,
            index: 0,
        };

        assert_eq!(global.resolve("a"), Some(Rc::new(test)));
    }

    #[test]
    fn test_shadowing_function_name() {
        let global = SymbolTable::new();
        global.define_function_name("a".into());
        global.define("a".into());

        let test = Symbol {
            name: "a".into(),
            scope: SymbolScope::Global,
            index: 0,
        };

        assert_eq!(global.resolve("a"), Some(Rc::new(test)));
    }
}
