use std::collections::HashMap;

use string_interner::StringInterner;

#[derive(Debug)]
pub struct Context {
    local_sym: SymTable,
    pub interner: StringInterner,
    pub id: IdGen,
}

/// Interned string occurs in source code
pub type Symbol = string_interner::DefaultSymbol;
/// The unique identifier of a register
#[derive(Debug, Copy, Clone)]
pub struct Ident(usize);

#[derive(Debug)]
struct SymTable {
    lookup: Vec<HashMap<Symbol, Ident>>,
}

#[derive(Debug)]
pub struct IdGen {
    next_id: usize,
}

impl Context {
    pub fn new() -> Context {
        Context {
            local_sym: SymTable { lookup: vec![] },
            interner: StringInterner::new(),
            id: IdGen { next_id: 0 },
        }
    }

    pub fn sym_begin_scope(&mut self) {
        self.local_sym.lookup.push(HashMap::new());
    }

    pub fn sym_end_scope(&mut self) {
        self.local_sym.lookup.pop();
    }

    pub fn sym_insert(&mut self, sym: Symbol) -> Result<Ident, ()> {
        let lookup = self
            .local_sym
            .lookup
            .last_mut()
            .expect("lookup table should have at least one entry");

        let id = self.id.get_next_id();
        lookup.try_insert(sym, id).map_err(|_| ()).map(|id| *id)
    }

    pub fn sym_lookup(&self, sym: Symbol) -> Option<Ident> {
        self.local_sym
            .lookup
            .iter()
            .rev()
            .find_map(|map| map.get(&sym))
            .map(|idx| *idx)
    }

    pub fn sym_lookup_or_panic(&self, sym: Symbol) -> Ident {
        self.sym_lookup(sym).unwrap_or_else(|| {
            let name = self.interner.resolve(sym).unwrap();
            panic!("undefined name: {}", name);
        })
    }
}

impl IdGen {
    pub fn get_next_id(&mut self) -> Ident {
        let id = self.next_id;
        self.next_id += 1;
        Ident(id)
    }
}
