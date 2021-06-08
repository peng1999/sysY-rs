use std::collections::{HashMap, HashSet};

use crate::ty::Ty;

/// The unique identifier of a register
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct Symbol(pub usize);

#[derive(Debug)]
pub struct SymTable {
    next_id: usize,
    non_const_set: HashSet<Symbol>,
    ty_table: HashMap<Symbol, Ty>,
    sym_name: HashMap<Symbol, String>,
}

#[derive(Debug)]
pub struct FnTable {
    fn_decl: HashMap<Symbol, (Vec<Ty>, Ty)>,
}

impl SymTable {
    pub fn new() -> SymTable {
        SymTable {
            next_id: 0,
            non_const_set: HashSet::new(),
            ty_table: HashMap::new(),
            sym_name: HashMap::new(),
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
    pub fn ty_assert(&mut self, sym: Symbol, ty: impl Into<Ty>) {
        let ty = ty.into();
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

    /// 给符号添加类型和名字（用于链接）
    pub fn ty_assert_with_name(&mut self, sym: Symbol, ty: Ty, name: String) {
        self.ty_assert(sym, ty);
        self.sym_name.insert(sym, name);
    }

    pub fn ty_of(&self, sym: Symbol) -> Option<Ty> {
        self.ty_table.get(&sym).cloned()
    }

    pub fn name_of(&self, sym: Symbol) -> Option<&str> {
        self.sym_name.get(&sym).map(String::as_ref)
    }
}

#[test]
fn sym_table_ty_assert_with_name() {
    let mut sym_table = SymTable::new();
    let sym = sym_table.gen_const_symbol();
    let ty = Ty::Fn(vec![], Box::new(Ty::Void));

    sym_table.ty_assert_with_name(sym, ty.clone(), "abc".to_string());
    sym_table.ty_assert_with_name(sym, ty.clone(), "abc".to_string());

    assert_eq!(sym_table.ty_of(sym), Some(ty.clone()));
    assert_eq!(sym_table.name_of(sym), Some("abc"));
}

#[test]
#[should_panic]
fn sym_table_ty_assert_with_name_imcompatible() {
    let mut sym_table = SymTable::new();
    let sym = sym_table.gen_const_symbol();
    let t1 = Ty::Fn(vec![], Box::new(Ty::Void));
    let t2 = Ty::Fn(vec![], Box::new(TyBasic::Int.into()));

    sym_table.ty_assert_with_name(sym, t1, "abc".to_string());
    sym_table.ty_assert_with_name(sym, t2, "abc".to_string());
}
