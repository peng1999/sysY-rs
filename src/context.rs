use std::collections::HashMap;

use string_interner::StringInterner;

use crate::sym_table::{SymTable, Symbol};

#[derive(Debug)]
pub struct Context {
    local_sym: LookupTable,
    pub interner: StringInterner,
    pub sym_table: SymTable,
}

/// Interned string occurs in source code
pub type IString = string_interner::DefaultSymbol;

#[derive(Debug)]
struct LookupTable {
    lookup: Vec<HashMap<IString, Symbol>>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            local_sym: LookupTable { lookup: vec![] },
            interner: StringInterner::new(),
            sym_table: SymTable::new(),
        }
    }

    pub fn sym_begin_scope(&mut self) {
        self.local_sym.lookup.push(HashMap::new());
    }

    pub fn sym_end_scope(&mut self) {
        self.local_sym.lookup.pop();
    }

    pub fn sym_insert(&mut self, sym: IString) -> Result<Symbol, ()> {
        let lookup = self
            .local_sym
            .lookup
            .last_mut()
            .expect("lookup table should have at least one entry");

        let id = self.sym_table.gen_var_symbol();
        lookup.try_insert(sym, id).map_err(|_| ()).cloned()
    }

    pub fn sym_lookup(&self, sym: IString) -> Option<Symbol> {
        self.local_sym
            .lookup
            .iter()
            .rev()
            .find_map(|map| map.get(&sym))
            .copied()
    }

    pub fn sym_lookup_or_panic(&self, sym: IString) -> Symbol {
        self.sym_lookup(sym).unwrap_or_else(|| {
            let name = self.interner.resolve(sym).unwrap();
            panic!("undefined name: {}", name);
        })
    }
}
