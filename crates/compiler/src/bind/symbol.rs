use bolt_ts_span::ModuleID;
use rustc_hash::FxHashMap;
use thin_vec::ThinVec;

use crate::ast::NodeID;
use crate::atoms::AtomId;
use crate::check::F64Represent;
use crate::keyword;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum SymbolName {
    Container,
    Normal(AtomId),
    Ele(AtomId),
    EleNum(F64Represent),
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
        self.as_atom().unwrap()
    }

    pub fn as_atom(&self) -> Option<AtomId> {
        match self {
            SymbolName::Normal(atom) => Some(*atom),
            SymbolName::Ele(atom) => Some(*atom),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Symbol {
    pub name: SymbolName,
    pub kind: SymbolKind,
}

impl Symbol {
    pub const ERR: SymbolID = SymbolID::root(ModuleID::root());
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
    Class(ClassSymbol),
    Property {
        decl: NodeID,
    },
    ElementProperty,
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
    TyAlias(TyAliasSymbol),
    TyParam(TyParamSymbol),
}

macro_rules! as_symbol_kind {
    ($kind: ident, $ty:ty, $as_kind: ident, $expect_kind: ident, $is_kind: ident) => {
        impl SymbolKind {
            #[inline(always)]
            pub fn $as_kind(&self) -> Option<$ty> {
                match self {
                    SymbolKind::$kind(ty) => Some(ty),
                    _ => None,
                }
            }
            #[inline(always)]
            pub fn $is_kind(&self) -> bool {
                self.$as_kind().is_some()
            }
            #[inline(always)]
            pub fn $expect_kind(&self) -> $ty {
                self.$as_kind().unwrap()
            }
        }
    };
}

as_symbol_kind!(Class, &ClassSymbol, as_class, expect_class, is_class);
as_symbol_kind!(
    TyAlias,
    &TyAliasSymbol,
    as_ty_alias,
    expect_ty_alias,
    is_ty_alias
);
as_symbol_kind!(
    TyParam,
    &TyParamSymbol,
    as_ty_param,
    expect_ty_param,
    is_ty_param
);

#[derive(Debug)]
pub struct TyParamSymbol {
    pub decl: NodeID,
}
#[derive(Debug)]
pub struct TyAliasSymbol {
    pub decl: NodeID,
}

#[derive(Debug)]
pub struct ClassSymbol {
    pub decl: NodeID,
    pub members: FxHashMap<SymbolName, SymbolID>,
}

#[derive(Debug)]
pub struct ObjectSymbol {
    pub decl: NodeID,
    pub members: FxHashMap<SymbolName, SymbolID>,
}

impl SymbolKind {
    #[inline(always)]
    pub fn is_variable(&self) -> bool {
        use SymbolKind::*;
        matches!(self, FunctionScopedVar | BlockScopedVar)
    }

    pub fn is_value(&self) -> bool {
        use SymbolKind::*;
        self.is_variable()
            || self.is_class()
            || matches!(self, Property { .. } | Object(_) | Function { .. })
    }

    #[inline(always)]
    pub fn is_interface(&self) -> bool {
        matches!(self, Self::Interface { .. })
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
            SymbolKind::Index { .. } => todo!(),
            SymbolKind::TyAlias { .. } => todo!(),
            SymbolKind::TyParam { .. } => todo!(),
            SymbolKind::ElementProperty => todo!(),
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

    pub fn is_type(&self) -> bool {
        self.is_class() || self.is_interface() || self.is_ty_param() || self.is_ty_alias()
    }
}

bolt_ts_span::new_index_with_module!(SymbolID);

impl SymbolID {
    pub(super) fn mock(index: u32) -> Self {
        SymbolID {
            module: ModuleID::MOCK,
            index,
        }
    }
}

pub struct Symbols {
    module_id: ModuleID,
    data: Vec<Symbol>,
}

impl Symbols {
    pub const ERR: SymbolID = SymbolID::root(ModuleID::root());

    pub fn new(module_id: ModuleID) -> Self {
        let mut this = Self {
            module_id,
            data: Vec::with_capacity(512),
        };
        this.insert(
            Symbol::ERR,
            Symbol::new(SymbolName::Normal(keyword::IDENT_EMPTY), SymbolKind::Err),
        );
        this
    }

    pub fn insert(&mut self, id: SymbolID, symbol: Symbol) {
        assert!(id.index_as_usize() == self.data.len());
        self.data.push(symbol);
    }

    pub fn get(&self, id: SymbolID) -> &Symbol {
        &self.data[id.index_as_usize()]
    }

    pub fn get_mut(&mut self, id: SymbolID) -> &mut Symbol {
        self.data.get_mut(id.index_as_usize()).unwrap()
    }

    pub fn len(&self) -> u32 {
        self.data.len() as u32
    }

    pub fn iter(&self) -> impl Iterator<Item = (SymbolID, &Symbol)> {
        self.data.iter().enumerate().map(|(index, symbol)| {
            let id = SymbolID {
                module: self.module_id,
                index: index as u32,
            };
            (id, symbol)
        })
    }
}

#[derive(Default)]
pub struct GlobalSymbols(FxHashMap<SymbolName, SymbolID>);

impl GlobalSymbols {
    pub fn insert(&mut self, name: SymbolName, symbol_id: SymbolID) {
        let prev = self.0.insert(name, symbol_id);
        assert!(prev.is_none(), "prev symbol: {prev:#?}")
    }

    pub fn get(&self, name: SymbolName) -> Option<SymbolID> {
        self.0.get(&name).copied()
    }
}
