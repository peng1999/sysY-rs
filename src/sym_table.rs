use std::collections::HashSet;

/// The unique identifier of a register
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct Symbol(pub usize);

#[derive(Debug)]
pub struct SymTable {
    next_id: usize,
    non_const_set: HashSet<Symbol>,
}

impl SymTable {
    pub fn new() -> SymTable {
        SymTable {
            next_id: 0,
            non_const_set: HashSet::new(),
        }
    }

    fn next_symbol(&mut self) -> Symbol {
        let id = self.next_id;
        self.next_id += 1;
        Symbol(id)
    }

    pub fn gen_var_symbol(&mut self) -> Symbol {
        let sym = self.next_symbol();
        self.non_const_set.insert(sym);
        sym
    }

    pub fn gen_const_symbol(&mut self) -> Symbol {
        self.next_symbol()
    }

    pub fn is_const(&self, ident: Symbol) -> bool {
        !self.non_const_set.contains(&ident)
    }
}
