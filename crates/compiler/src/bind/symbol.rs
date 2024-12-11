use bolt_ts_span::ModuleID;
use rustc_hash::FxHashMap;

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

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct SymbolFlags: u32 {
        const FUNCTION_SCOPED_VARIABLE = 1 << 0;
        const BLOCK_SCOPED_VARIABLE = 1 << 1;
        const PROPERTY = 1 << 2;
        const ENUM_MEMBER = 1 << 3;
        const FUNCTION = 1 << 4;
        const CLASS = 1 << 5;
        const INTERFACE = 1 << 6;
        const CONST_ENUM = 1 << 7;
        const REGULAR_ENUM = 1 << 8;
        const VALUE_MODULE = 1 << 9;
        const NAMESPACE_MODULE = 1 << 10;
        const TYPE_LITERAL = 1 << 11;
        const OBJECT_LITERAL = 1 << 12;
        const METHOD = 1 << 13;
        const CONSTRUCTOR = 1 << 14;
        const GET_ACCESSOR = 1 << 15;
        const SET_ACCESSOR = 1 << 16;
        const SIGNATURE = 1 << 17;
        const TYPE_PARAMETER = 1 << 18;
        const TYPE_ALIAS = 1 << 19;
        const EXPORT_VALUE = 1 << 20;
        const ALIAS = 1 << 21;
        const PROTOTYPE = 1 << 22;
        const EXPORT_STAR = 1 << 23;
        const OPTIONAL = 1 << 24;
        const TRANSIENT = 1 << 25;
        const ASSIGNMENT = 1 << 26;
        const MODULE_EXPORTS = 1 << 27;

        const ENUM = Self::REGULAR_ENUM.bits() | Self::CONST_ENUM.bits();
        const VARIABLE = Self::FUNCTION_SCOPED_VARIABLE.bits() | Self::BLOCK_SCOPED_VARIABLE.bits();
        const VALUE = Self::VARIABLE.bits() | Self::PROPERTY.bits() | Self::ENUM_MEMBER.bits() | Self::OBJECT_LITERAL.bits() | Self::FUNCTION.bits() | Self::CLASS.bits() | Self::ENUM.bits() | Self::VALUE_MODULE.bits() | Self::METHOD.bits() | Self::GET_ACCESSOR.bits() | Self::SET_ACCESSOR.bits();
        const TYPE = Self::CLASS.bits() | Self::INTERFACE.bits() | Self::ENUM.bits() | Self::ENUM_MEMBER.bits() | Self::TYPE_LITERAL.bits() | Self::TYPE_PARAMETER.bits() | Self::TYPE_ALIAS.bits();
        const NAMESPACE = Self::VALUE_MODULE.bits() | Self::NAMESPACE_MODULE.bits() | Self::ENUM.bits();
        const MODULE = Self::VALUE_MODULE.bits() | Self::NAMESPACE_MODULE.bits();
        const ACCESSOR = Self::GET_ACCESSOR.bits() | Self::SET_ACCESSOR.bits();
        const FUNCTION_SCOPED_VARIABLE_EXCLUDES = Self::VALUE.bits() & !Self::FUNCTION_SCOPED_VARIABLE.bits();
        const BLOCK_SCOPED_VARIABLE_EXCLUDES = Self::VALUE.bits();
        const PARAMETER_EXCLUDES = Self::VALUE.bits();
        const PROPERTY_EXCLUDES = Self::empty().bits();
        const ENUM_MEMBER_EXCLUDES = Self::VALUE.bits() | Self::TYPE.bits();
        const FUNCTION_EXCLUDES = Self::VALUE.bits() & !(Self::FUNCTION.bits() | Self::VALUE_MODULE.bits() | Self::CLASS.bits());
        const CLASS_EXCLUDES = (Self::VALUE.bits() | Self::TYPE.bits()) & !(Self::VALUE_MODULE.bits() | Self::INTERFACE.bits() | Self::FUNCTION.bits());
        const INTERFACE_EXCLUDES = Self::TYPE.bits() & !(Self::INTERFACE.bits() | Self::CLASS.bits());
        const REGULAR_ENUM_EXCLUDES = (Self::VALUE.bits() | Self::TYPE.bits()) & !(Self::REGULAR_ENUM.bits() | Self::VALUE_MODULE.bits());
        const CONST_ENUM_EXCLUDES = (Self::VALUE.bits() | Self::TYPE.bits()) & !Self::CONST_ENUM.bits();
        const VALUE_MODULE_EXCLUDES = Self::VALUE.bits() & !(Self::FUNCTION.bits() | Self::CLASS.bits() | Self::REGULAR_ENUM.bits() | Self::VALUE_MODULE.bits());
        const NAMESPACE_MODULE_EXCLUDES = 0;
        const METHOD_EXCLUDES = Self::VALUE.bits() & !Self::METHOD.bits();
        const GET_ACCESSOR_EXCLUDES = Self::VALUE.bits() & !Self::SET_ACCESSOR.bits();
        const SET_ACCESSOR_EXCLUDES = Self::VALUE.bits() & !Self::GET_ACCESSOR.bits();
        const ACCESSOR_EXCLUDES = Self::VALUE.bits() & !Self::ACCESSOR.bits();
        const TYPE_PARAMETER_EXCLUDES = Self::TYPE.bits() & !Self::TYPE_PARAMETER.bits();
        const TYPE_ALIAS_EXCLUDES = Self::TYPE.bits();
        const ALIAS_EXCLUDES = Self::ALIAS.bits();
        const MODULE_MEMBER = Self::VARIABLE.bits() | Self::FUNCTION.bits() | Self::CLASS.bits() | Self::INTERFACE.bits() | Self::ENUM.bits() | Self::MODULE.bits() | Self::TYPE_ALIAS.bits() | Self::ALIAS.bits();
        const EXPORT_HAS_LOCAL = Self::FUNCTION.bits() | Self::CLASS.bits() | Self::ENUM.bits() | Self::VALUE_MODULE.bits();
        const BLOCK_SCOPED = Self::BLOCK_SCOPED_VARIABLE.bits() | Self::CLASS.bits() | Self::ENUM.bits();
        const PROPERTY_OR_ACCESSOR = Self::PROPERTY.bits() | Self::ACCESSOR.bits();
        const CLASS_MEMBER = Self::METHOD.bits() | Self::ACCESSOR.bits() | Self::PROPERTY.bits();
        const EXPORT_SUPPORTS_DEFAULT_MODIFIER = Self::CLASS.bits() | Self::FUNCTION.bits() | Self::INTERFACE.bits();
        const EXPORT_DOES_NOT_SUPPORT_DEFAULT_MODIFIER = !Self::EXPORT_SUPPORTS_DEFAULT_MODIFIER.bits();
        const CLASSIFIABLE = Self::CLASS.bits() | Self::ENUM.bits() | Self::TYPE_ALIAS.bits() | Self::INTERFACE.bits() | Self::TYPE_PARAMETER.bits() | Self::MODULE.bits() | Self::ALIAS.bits();
        const LATE_BINDING_CONTAINER = Self::CLASS.bits() | Self::INTERFACE.bits() | Self::TYPE_LITERAL.bits() | Self::OBJECT_LITERAL.bits() | Self::FUNCTION.bits();
    }
}

#[derive(Debug)]
pub struct Symbol {
    pub name: SymbolName,
    pub flags: SymbolFlags,
    pub(super) kind: SymbolKind,
}

impl Symbol {
    pub const ERR: SymbolID = SymbolID::root(ModuleID::root());
    pub fn new(name: SymbolName, flags: SymbolFlags, kind: SymbolKind) -> Self {
        Self { name, flags, kind }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolFnKind {
    FnDecl,
    FnExpr,
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
    Fn(FnSymbol),
    Class(ClassSymbol),
    Prop(PropSymbol),
    ElementProperty,
    Object(ObjectSymbol),
    Interface(InterfaceSymbol),
    Index(IndexSymbol),
    TyAlias(TyAliasSymbol),
    TyParam(TyParamSymbol),
}

macro_rules! as_symbol_kind {
    ($kind: ident, $ty:ty, $as_kind: ident, $expect_kind: ident, $is_kind: ident) => {
        impl Symbol {
            #[inline(always)]
            pub fn $as_kind(&self) -> Option<$ty> {
                match &self.kind {
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

as_symbol_kind!(
    Interface,
    &InterfaceSymbol,
    as_interface,
    expect_interface,
    is_interface
);
as_symbol_kind!(Index, &IndexSymbol, as_index, expect_index, is_index);
as_symbol_kind!(Object, &ObjectSymbol, as_object, expect_object, is_object);
as_symbol_kind!(Prop, &PropSymbol, as_prop, expect_prop, is_prop);
as_symbol_kind!(Fn, &FnSymbol, as_fn, expect_fn, is_fn);
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
pub struct IndexSymbol {
    pub decl: NodeID,
}
#[derive(Debug)]
pub struct InterfaceSymbol {
    pub decl: NodeID,
    pub members: FxHashMap<SymbolName, SymbolID>,
}

#[derive(Debug)]
pub struct TyParamSymbol {
    pub decl: NodeID,
}
#[derive(Debug)]
pub struct TyAliasSymbol {
    pub decl: NodeID,
}

#[derive(Debug)]
pub struct FnSymbol {
    pub kind: SymbolFnKind,
    pub decls: thin_vec::ThinVec<NodeID>,
}

#[derive(Debug)]
pub struct ClassSymbol {
    pub decl: NodeID,
    pub members: FxHashMap<SymbolName, SymbolID>,
}

#[derive(Debug)]
pub struct PropSymbol {
    pub decl: NodeID,
}

#[derive(Debug)]
pub struct ObjectSymbol {
    pub decl: NodeID,
    pub members: FxHashMap<SymbolName, SymbolID>,
}

impl Symbol {
    pub fn is_element_property(&self) -> bool {
        use SymbolKind::*;
        matches!(self.kind, ElementProperty)
    }

    #[inline(always)]
    pub fn is_variable(&self) -> bool {
        use SymbolKind::*;
        matches!(self.kind, FunctionScopedVar | BlockScopedVar)
    }

    pub fn is_value(&self) -> bool {
        use SymbolKind::*;
        self.is_variable()
            || self.is_class()
            || self.is_fn()
            || self.is_prop()
            || matches!(self.kind, Object(_))
    }

    pub fn as_str(&self) -> &'static str {
        match self.kind {
            SymbolKind::Err => "err",
            SymbolKind::FunctionScopedVar => todo!(),
            SymbolKind::BlockScopedVar => todo!(),
            SymbolKind::Fn { .. } => "function",
            SymbolKind::Class { .. } => "class",
            SymbolKind::Prop { .. } => todo!(),
            SymbolKind::Object { .. } => todo!(),
            SymbolKind::BlockContainer { .. } => todo!(),
            SymbolKind::Interface { .. } => todo!(),
            SymbolKind::Index { .. } => todo!(),
            SymbolKind::TyAlias { .. } => todo!(),
            SymbolKind::TyParam { .. } => todo!(),
            SymbolKind::ElementProperty => todo!(),
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
            Symbol::new(
                SymbolName::Normal(keyword::IDENT_EMPTY),
                SymbolFlags::empty(),
                SymbolKind::Err,
            ),
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
