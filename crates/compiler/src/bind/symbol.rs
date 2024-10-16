use rustc_hash::FxHashMap;
use thin_vec::ThinVec;

use crate::ast::NodeID;
use crate::atoms::AtomId;
use crate::keyword;

#[derive(Debug)]
pub struct Symbol {
    pub name: AtomId,
    pub kind: SymbolKind,
}

impl Symbol {
    pub fn new(name: AtomId, kind: SymbolKind) -> Self {
        Self { name, kind }
    }

    pub const ERR: SymbolID = SymbolID::root();
}

#[derive(Debug)]
pub enum SymbolKind {
    Err,
    /// `var` or parameter
    FunctionScopedVar,
    /// `let` or `const`
    BlockScopedVar,
    Function(ThinVec<NodeID>),
}

rts_span::new_index!(SymbolID);

pub struct Symbols(FxHashMap<SymbolID, Symbol>);

impl Symbols {
    pub fn new(id: SymbolID) -> Self {
        assert_eq!(id, Symbol::ERR);
        let mut this = Self(FxHashMap::default());
        this.insert(id, Symbol::new(keyword::IDENT_EMPTY, SymbolKind::Err));
        this
    }

    pub fn insert(&mut self, id: SymbolID, symbol: Symbol) {
        let prev = self.0.insert(id, symbol);
        assert!(prev.is_none(), "prev symbol: {prev:#?}")
    }

    pub fn get(&self, id: SymbolID) -> &Symbol {
        self.0.get(&id).unwrap()
    }

    pub fn get_mut(&mut self, id: SymbolID) -> &mut Symbol {
        self.0.get_mut(&id).unwrap()
    }
}
