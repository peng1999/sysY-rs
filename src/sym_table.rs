use std::collections::{HashMap, HashSet};

use crate::{ast::Ty, context::IString};

/// The unique identifier of a register
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct Symbol(pub usize);

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct FuncSymbol(IString);

#[derive(Debug)]
pub struct SymTable {
    next_id: usize,
    non_const_set: HashSet<Symbol>,
    ty_table: HashMap<Symbol, Ty>,
}

#[derive(Debug)]
pub struct FuncTable {
    fn_decl: HashMap<FuncSymbol, (Vec<Ty>, Ty)>,
}

impl SymTable {
    pub fn new() -> SymTable {
        SymTable {
            next_id: 0,
            non_const_set: HashSet::new(),
            ty_table: HashMap::new(),
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

    /// 断言符号的类型
    pub fn ty_assert(&mut self, sym: Symbol, ty: Ty) {
        self.ty_table
            .entry(sym)
            .and_modify(|prev_ty| {
                // 如果之前已经断言过，则要求相等
                if *prev_ty != ty {
                    panic!("error: type {:?} and {:?} incompatible.", ty, prev_ty);
                }
            })
            .or_insert(ty); // 否则储存本次断言
    }

    pub fn ty_of(&self, sym: Symbol) -> Option<Ty> {
        self.ty_table.get(&sym).cloned()
    }
}
