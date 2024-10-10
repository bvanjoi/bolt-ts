use rustc_hash::FxHashMap;
use thin_vec::ThinVec;

use crate::ast::NodeID;
use crate::atoms::AtomId;

#[derive(Debug)]
pub struct Symbol {
    pub name: AtomId,
    pub kind: SymbolKind,
}

impl Symbol {
    pub fn new(name: AtomId, kind: SymbolKind) -> Self {
        Self { name, kind }
    }
}

#[derive(Debug)]
pub enum SymbolKind {
    BlockedScopeVar,
    Function(ThinVec<NodeID>),
}

rts_span::new_index!(SymbolID);

pub struct Symbols(FxHashMap<SymbolID, Symbol>);

impl Symbols {
    pub fn new() -> Self {
        Self(FxHashMap::default())
    }

    pub fn insert(&mut self, id: SymbolID, symbol: Symbol) {
        let prev = self.0.insert(id, symbol);
        assert!(prev.is_none())
    }

    pub fn get(&self, id: SymbolID) -> &Symbol {
        self.0.get(&id).unwrap()
    }

    pub fn get_mut(&mut self, id: SymbolID) -> &mut Symbol {
        self.0.get_mut(&id).unwrap()
    }
}
