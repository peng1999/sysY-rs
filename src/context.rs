use std::collections::HashMap;

use string_interner::StringInterner;

use crate::{
    ast::Ty,
    sym_table::{SymTable, Symbol},
};

#[derive(Debug)]
pub struct Context {
    var_lookup: Vec<HashMap<IString, Symbol>>,
    ty_lookup: HashMap<IString, Ty>,
    pub interner: StringInterner,
    pub sym_table: SymTable,
}

/// Interned string occurs in source code
pub type IString = string_interner::DefaultSymbol;

impl Context {
    pub fn new() -> Context {
        let mut ctx = Context {
            var_lookup: vec![],
            ty_lookup: HashMap::new(),
            interner: StringInterner::new(),
            sym_table: SymTable::new(),
        };
        ctx.ty_lookup = [("int", Ty::Int), ("bool", Ty::Bool)]
            .iter()
            .map(|(name, ty)| (ctx.interner.get_or_intern(name), *ty))
            .collect();
        ctx
    }

    pub fn sym_begin_scope(&mut self) {
        self.var_lookup.push(HashMap::new());
    }

    pub fn sym_end_scope(&mut self) {
        self.var_lookup.pop();
    }

    pub fn sym_insert(&mut self, sym: IString) -> Result<Symbol, ()> {
        let lookup = self
            .var_lookup
            .last_mut()
            .expect("lookup table should have at least one entry");

        let id = self.sym_table.gen_var_symbol();
        lookup.try_insert(sym, id).map_err(|_| ()).cloned()
    }

    pub fn sym_lookup(&self, sym: IString) -> Option<Symbol> {
        self.var_lookup
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

    pub fn get_ty(&mut self, ty: IString) -> Ty {
        self.ty_lookup.get(&ty).cloned().unwrap_or_else(|| {
            let name = self.interner.resolve(ty).unwrap();
            panic!("undefined type: {}", name);
        })
    }
}
