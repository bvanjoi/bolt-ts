use rustc_hash::FxHashMap;
use thin_vec::ThinVec;

use crate::ast::NodeID;
use crate::atoms::AtomId;
use crate::keyword;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum SymbolName {
    Normal(AtomId),
    Ele(AtomId),
    Array,
    /// object literal
    Object,
    /// function expression
    Fn,
}

impl SymbolName {
    pub fn expect_atom(&self) -> AtomId {
        match self {
            SymbolName::Normal(atom) => *atom,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct Symbol {
    pub name: SymbolName,
    pub kind: SymbolKind,
}

impl Symbol {
    pub fn new(name: SymbolName, kind: SymbolKind) -> Self {
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
    Function {
        decls: ThinVec<NodeID>,
    },
    Class {
        decl: NodeID,
    },
    Property {
        decl: NodeID,
    },
    Method {
        decl: NodeID,
    },
    Object(ObjectSymbol),
    FnExpr {
        decl: NodeID,
    },
}

#[derive(Debug)]
pub struct ObjectSymbol {
    pub decl: NodeID,
    pub members: FxHashMap<SymbolName, SymbolID>,
}

impl SymbolKind {
    pub fn is_variable(&self) -> bool {
        matches!(self, Self::FunctionScopedVar | Self::BlockScopedVar)
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            SymbolKind::Err => "err",
            SymbolKind::FunctionScopedVar => todo!(),
            SymbolKind::BlockScopedVar => todo!(),
            SymbolKind::Function { .. } | SymbolKind::FnExpr { .. } => "function",
            SymbolKind::Class { .. } => "class",
            SymbolKind::Property { .. } => todo!(),
            SymbolKind::Object { .. } => todo!(),
            SymbolKind::Method { .. } => todo!(),
        }
    }

    pub fn expect_object(&self) -> &ObjectSymbol {
        match self {
            SymbolKind::Object(symbol) => symbol,
            _ => unreachable!("{:#?}", self),
        }
    }

    pub fn expect_prop(&self) -> NodeID {
        match self {
            SymbolKind::Property { decl } => *decl,
            _ => unreachable!("{:#?}", self),
        }
    }
}

rts_span::new_index!(SymbolID);

pub struct Symbols(FxHashMap<SymbolID, Symbol>);

impl Symbols {
    pub fn new(id: SymbolID) -> Self {
        assert_eq!(id, Symbol::ERR);
        let mut this = Self(FxHashMap::default());
        this.insert(
            id,
            Symbol::new(SymbolName::Normal(keyword::IDENT_EMPTY), SymbolKind::Err),
        );
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
