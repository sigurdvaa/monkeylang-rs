use std::collections::HashMap;
// type SymbolScope = &'static str;
//
// const GLOBAL_SCOPE: SymbolScope = "GLOBAL";

#[derive(Debug, PartialEq)]
pub enum SymbolScope {
    Global,
}

impl SymbolScope {
    pub fn value(&self) -> &'static str {
        match self {
            Self::Global => "GLOBAL",
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
    store: HashMap<String, Symbol>,
    num_definitions: usize, // TODO: remove/replace?
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.store.len()
    }

    pub fn define(&mut self, name: String) -> &Symbol {
        let sym = Symbol {
            name: name.clone(),
            index: self.len(),
            scope: SymbolScope::Global,
        };
        self.store.entry(name).or_insert(sym)
    }

    pub fn resolve(&mut self, name: &str) -> Option<&Symbol> {
        self.store.get(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define_global() {
        let test: HashMap<String, Symbol> = HashMap::from_iter([
            (
                "a".into(),
                Symbol {
                    name: "a".into(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
            ),
            (
                "b".into(),
                Symbol {
                    name: "b".into(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
            ),
        ]);

        let mut global = SymbolTable::new();
        global.define("a".into());
        global.define("b".into());
        assert_eq!(
            global.store, test,
            "global store doesn't contain expected values"
        );
    }

    #[test]
    fn test_resolve_global() {
        let tests = [
            Symbol {
                name: "a".into(),
                scope: SymbolScope::Global,
                index: 0,
            },
            Symbol {
                name: "b".into(),
                scope: SymbolScope::Global,
                index: 1,
            },
        ];

        let mut global = SymbolTable::new();
        global.define("a".into());
        global.define("b".into());

        for test in &tests {
            let sym = global.resolve(&test.name);
            assert_eq!(sym, Some(test));
        }
    }
}
