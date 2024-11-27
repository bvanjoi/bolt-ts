use bolt_ts_span::ModuleID;
use rustc_hash::FxHashMap;
use thin_vec::ThinVec;

use crate::ast::NodeID;
use crate::atoms::AtomId;
use crate::keyword;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum SymbolName {
    Container,
    Normal(AtomId),
    Ele(AtomId),
    ClassExpr,
    Array,
    /// object literal
    Object,
    /// function expression
    Fn,
    Constructor,
    Interface,
    Index,
}

impl SymbolName {
    pub fn expect_atom(&self) -> AtomId {
        match self {
            SymbolName::Normal(atom) => *atom,
            SymbolName::Ele(atom) => *atom,
            _ => unreachable!("{:#?}", self),
        }
    }
}

#[derive(Debug)]
pub struct Symbol {
    pub name: SymbolName,
    pub kind: SymbolKind,
}

impl Symbol {
    pub const ERR: SymbolID = SymbolID::root();
    pub fn new(name: SymbolName, kind: SymbolKind) -> Self {
        Self { name, kind }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolFnKind {
    Fn,
    Ctor,
    Method,
}

#[derive(Debug)]
pub enum SymbolKind {
    Err,
    BlockContainer {
        locals: FxHashMap<SymbolName, SymbolID>,
    },
    /// `var` or parameter
    FunctionScopedVar,
    /// `let` or `const`
    BlockScopedVar,
    Function {
        kind: SymbolFnKind,
        decls: ThinVec<NodeID>,
    },
    Class {
        decl: NodeID,
        members: FxHashMap<SymbolName, SymbolID>,
    },
    Property {
        decl: NodeID,
    },
    Object(ObjectSymbol),
    FnExpr {
        decl: NodeID,
    },
    Interface {
        decl: NodeID,
        members: FxHashMap<SymbolName, SymbolID>,
    },
    Index {
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

    pub fn is_class(&self) -> bool {
        matches!(self, Self::Class { .. })
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
            SymbolKind::BlockContainer { .. } => todo!(),
            SymbolKind::Interface { .. } => todo!(),
            SymbolKind::Index { decl } => todo!(),
        }
    }

    pub fn as_prop(&self) -> NodeID {
        match self {
            SymbolKind::Property { decl } => *decl,
            _ => unreachable!("{:#?}", self),
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

bolt_ts_span::new_index!(SymbolID);

pub struct Symbols(FxHashMap<SymbolID, Symbol>);

impl Symbols {
    pub fn new() -> Self {
        let mut this = Self(FxHashMap::default());
        this.insert(
            Symbol::ERR,
            Symbol::new(SymbolName::Normal(keyword::IDENT_EMPTY), SymbolKind::Err),
        );
        this
    }

    pub fn insert(&mut self, id: SymbolID, symbol: Symbol) {
        let prev = self.0.insert(id, symbol);
        assert!(prev.is_none(), "prev symbol: {prev:#?}")
    }

    pub fn get(&self, id: SymbolID) -> &Symbol {
        &self.0[&id]
    }

    pub fn get_mut(&mut self, id: SymbolID) -> &mut Symbol {
        self.0.get_mut(&id).unwrap()
    }
}

impl<'a> IntoIterator for &'a Symbols {
    type Item = (&'a SymbolID, &'a Symbol);
    type IntoIter = std::collections::hash_map::Iter<'a, SymbolID, Symbol>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

#[derive(Default)]
pub struct GlobalSymbols(FxHashMap<SymbolName, (ModuleID, SymbolID)>);

impl GlobalSymbols {
    pub fn insert(&mut self, name: SymbolName, module_id: ModuleID, symbol_id: SymbolID) {
        let prev = self.0.insert(name, (module_id, symbol_id));
        assert!(prev.is_none(), "prev symbol: {prev:#?}")
    }

    pub fn get(&self, name: SymbolName) -> Option<(ModuleID, SymbolID)> {
        self.0.get(&name).copied()
    }
}
