use std::{collections::HashMap, process::exit};

use string_interner::StringInterner;

use crate::{
    ast::Ty,
    error::LineColLookup,
    ir::Label,
    sym_table::{SymTable, Symbol},
};

#[derive(Debug)]
pub struct Context<'source> {
    /// 提供从 IString 到标识符的查找表
    var_lookup: Vec<HashMap<IString, Symbol>>,
    /// 提供从 IString 到类型名的查找表
    ty_lookup: HashMap<IString, Ty>,
    /// 用于生成 Label
    next_label_id: i32,
    /// 用于 String Intern
    pub interner: StringInterner,
    /// 符号表
    pub sym_table: SymTable,
    /// 源代码
    pub source: &'source str,
    /// 查找行号和列号
    pub line_col_lookup: LineColLookup<'source>,
}

/// Interned string occurs in source code
pub type IString = string_interner::DefaultSymbol;

impl Context<'_> {
    pub fn new(source: &str) -> Context {
        let mut interner = StringInterner::new();
        let ty_lookup = [("int", Ty::Int), ("bool", Ty::Bool)]
            .iter()
            .map(|(name, ty)| (interner.get_or_intern(name), *ty))
            .collect();

        Context {
            var_lookup: vec![],
            ty_lookup,
            next_label_id: 0,
            interner,
            sym_table: SymTable::new(),
            source,
            line_col_lookup: LineColLookup::new(source),
        }
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

    pub fn sym_lookup_or_panic(&self, sym: IString, span: (usize, usize)) -> Symbol {
        self.sym_lookup(sym).unwrap_or_else(|| {
            self.line_col_lookup.lookup_and_eprint(span.0);
            let name = self.interner.resolve(sym).unwrap();
            eprintln!("error: undefined name: {}", name);
            exit(1);
        })
    }

    pub fn get_ty(&mut self, ty: IString) -> Ty {
        self.ty_lookup.get(&ty).cloned().unwrap_or_else(|| {
            let name = self.interner.resolve(ty).unwrap();
            panic!("undefined type: {}", name);
        })
    }

    pub fn next_label(&mut self) -> Label {
        let id = self.next_label_id;
        self.next_label_id += 1;
        Label(id)
    }
}
