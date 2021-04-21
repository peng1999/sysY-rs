use std::collections::{HashMap, HashSet};

use string_interner::StringInterner;

#[derive(Debug)]
pub struct Context {
    local_sym: LookupTable,
    pub interner: StringInterner,
    pub id: IdGen,
    pub sym_table: SymTable,
}

/// Interned string occurs in source code
pub type IString = string_interner::DefaultSymbol;
/// The unique identifier of a register
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct Symbol(pub usize);

#[derive(Debug)]
struct LookupTable {
    lookup: Vec<HashMap<IString, Symbol>>,
}

#[derive(Debug)]
pub struct SymTable {
    non_const_set: HashSet<Symbol>,
}

#[derive(Debug)]
pub struct IdGen {
    next_id: usize,
}

impl Context {
    pub fn new() -> Context {
        Context {
            local_sym: LookupTable { lookup: vec![] },
            interner: StringInterner::new(),
            id: IdGen { next_id: 0 },
            sym_table: SymTable {
                non_const_set: HashSet::new(),
            },
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

        let id = self.id.get_next_id();
        let ident_table = &mut self.sym_table;
        lookup.try_insert(sym, id).map_err(|_| ()).map(|id| {
            ident_table.non_const_set.insert(*id);
            *id
        })
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

impl IdGen {
    pub fn get_next_id(&mut self) -> Symbol {
        let id = self.next_id;
        self.next_id += 1;
        Symbol(id)
    }
}

impl SymTable {
    pub fn is_const(&self, ident: Symbol) -> bool {
        !self.non_const_set.contains(&ident)
    }
}
