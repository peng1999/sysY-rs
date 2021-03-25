use std::collections::HashMap;

use string_interner::StringInterner;

#[derive(Debug)]
pub struct Context {
    var: SymTable,
    pub interner: StringInterner,
}

pub type Symbol = string_interner::DefaultSymbol;
pub struct Ident(usize);

#[derive(Debug)]
struct SymTable {
    syms: Vec<Symbol>,
    lookup: Vec<HashMap<Symbol, usize>>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            var: SymTable {
                syms: vec![],
                lookup: vec![HashMap::new()],
            },
            interner: StringInterner::new(),
        }
    }

    pub fn sym_begin_scope(&mut self) {
        self.var.lookup.push(HashMap::new());
    }

    pub fn sym_end_scope(&mut self) {
        self.var.lookup.pop();
    }

    pub fn sym_insert(&mut self, sym: Symbol) -> Result<Ident, ()> {
        let lookup = self
            .var
            .lookup
            .last_mut()
            .expect("lookup table should have at least one entry");
        if let Some(_) = lookup.get(&sym) {
            Err(())
        } else {
            let len = self.var.syms.len();
            lookup.insert(sym.clone(), len);
            self.var.syms.push(sym);
            Ok(Ident(len))
        }
    }

    pub fn sym_lookup(&self, sym: Symbol) -> Option<Ident> {
        self.var
            .lookup
            .iter()
            .rev()
            .find_map(|map| map.get(&sym))
            .map(|idx| Ident(*idx))
    }
}
