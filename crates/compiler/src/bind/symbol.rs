use bolt_ts_atom::AtomId;
use bolt_ts_span::ModuleID;
use bolt_ts_utils::fx_hashmap_with_capacity;
use rustc_hash::FxHashMap;

use crate::ast::NodeID;
use crate::check::F64Represent;
use crate::keyword;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum SymbolName {
    Container,
    // TODO: merge `Normal` and `Ele`
    Normal(AtomId),
    Ele(AtomId),
    EleNum(F64Represent),
    ClassExpr,
    Array,
    /// object literal
    Object,
    /// function expression
    Fn,
    /// constructor implements
    Constructor,
    /// constructor sigs
    New,
    Call,
    Interface,
    Index,
    Type,
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

    pub fn as_numeric(&self) -> Option<f64> {
        match self {
            SymbolName::EleNum(num) => Some(Into::<f64>::into(*num)),
            _ => None,
        }
    }

    pub fn is_numeric(&self) -> bool {
        self.as_numeric().is_some()
    }
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
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
    pub(crate) kind: (SymbolKind, Option<InterfaceSymbol>, Option<NsSymbol>),
}

impl<'cx> Symbol {
    pub const ERR: SymbolID = SymbolID::root(ModuleID::root());
    pub(super) fn new(name: SymbolName, flags: SymbolFlags, kind: SymbolKind) -> Self {
        Self {
            name,
            flags,
            kind: (kind, None, None),
        }
    }
    pub(super) fn new_interface(name: SymbolName, flags: SymbolFlags, i: InterfaceSymbol) -> Self {
        Self {
            name,
            flags,
            kind: (SymbolKind::Err, Some(i), None),
        }
    }
    pub(super) fn new_ns(name: SymbolName, flags: SymbolFlags, i: NsSymbol) -> Self {
        Self {
            name,
            flags,
            kind: (SymbolKind::Err, None, Some(i)),
        }
    }

    pub fn can_have_symbol(node: crate::ast::Node<'cx>) -> bool {
        use crate::ast::Node::*;
        node.is_decl() || matches!(node, FnTy(_))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolFnKind {
    FnDecl,
    FnExpr,
    Ctor,
    Call,
    Method,
}

#[derive(Debug)]
pub(crate) enum SymbolKind {
    Err,
    BlockContainer(BlockContainerSymbol),
    /// `var` or parameter
    FunctionScopedVar(FunctionScopedVarSymbol),
    /// `let` or `const`
    BlockScopedVar {
        decl: NodeID,
    },
    Fn(FnSymbol),
    Class(ClassSymbol),
    Prop(PropSymbol),
    Object(ObjectSymbol),
    Index(IndexSymbol),
    TyAlias(TyAliasSymbol),
    TyParam(TyParamSymbol),
    TyLit(TyLitSymbol),
    Alias(AliasSymbol),
    GetterSetter(GetterSetterSymbol),
}

macro_rules! as_symbol_kind {
    ($kind: ident, $ty:ty, $as_kind: ident, $expect_kind: ident) => {
        impl Symbol {
            #[inline(always)]
            pub(super) fn $as_kind(&self) -> Option<$ty> {
                match &self.kind.0 {
                    SymbolKind::$kind(ty) => Some(ty),
                    _ => None,
                }
            }
            #[inline(always)]
            pub fn $expect_kind(&self) -> $ty {
                self.$as_kind().unwrap()
            }
        }
    };
}

impl Symbol {
    #[inline(always)]
    pub(super) fn as_interface(&self) -> Option<&InterfaceSymbol> {
        self.kind.1.as_ref()
    }
    #[inline(always)]
    pub fn expect_interface(&self) -> &InterfaceSymbol {
        self.as_interface().unwrap()
    }
    #[inline(always)]
    pub(super) fn as_ns(&self) -> Option<&NsSymbol> {
        self.kind.2.as_ref()
    }
    #[inline(always)]
    pub fn expect_ns(&self) -> &NsSymbol {
        self.as_ns().unwrap()
    }
}

as_symbol_kind!(
    BlockContainer,
    &BlockContainerSymbol,
    as_block_container,
    expect_block_container
);
as_symbol_kind!(Index, &IndexSymbol, as_index, expect_index);
as_symbol_kind!(Object, &ObjectSymbol, as_object, expect_object);
as_symbol_kind!(Prop, &PropSymbol, as_prop, expect_prop);
as_symbol_kind!(Fn, &FnSymbol, as_fn, expect_fn);
as_symbol_kind!(Class, &ClassSymbol, as_class, expect_class);
as_symbol_kind!(TyAlias, &TyAliasSymbol, as_ty_alias, expect_ty_alias);
as_symbol_kind!(TyParam, &TyParamSymbol, as_ty_param, expect_ty_param);
as_symbol_kind!(TyLit, &TyLitSymbol, as_ty_lit, expect_ty_lit);
as_symbol_kind!(Alias, &AliasSymbol, as_alias, expect_alias);
as_symbol_kind!(
    GetterSetter,
    &GetterSetterSymbol,
    as_getter_setter,
    expect_getter_setter
);

#[derive(Debug)]
pub struct AliasSymbol {
    pub decl: NodeID,
    pub source: SymbolName,
    pub target: SymbolName,
}

#[derive(Debug)]
pub struct TyLitSymbol {
    pub decl: NodeID,
    pub members: FxHashMap<SymbolName, SymbolID>,
}

#[derive(Debug)]
pub struct BlockContainerSymbol {
    pub locals: FxHashMap<SymbolName, SymbolID>,
    pub exports: FxHashMap<SymbolName, SymbolID>,
}

#[derive(Debug)]
pub struct GetterSetterSymbol {
    pub getter_decl: Option<NodeID>,
    pub setter_decl: Option<NodeID>,
}

#[derive(Debug)]
pub struct IndexSymbol {
    pub decl: NodeID,
}

#[derive(Debug)]
pub struct InterfaceSymbol {
    pub decls: thin_vec::ThinVec<NodeID>,
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
pub struct NsSymbol {
    pub decls: thin_vec::ThinVec<NodeID>,
    pub exports: FxHashMap<SymbolName, SymbolID>,
    pub members: FxHashMap<SymbolName, SymbolID>,
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
    pub exports: FxHashMap<SymbolName, SymbolID>,
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

#[derive(Debug)]
pub struct FunctionScopedVarSymbol {
    pub decl: NodeID,
}

impl Symbol {
    #[inline(always)]
    pub fn is_variable(&self) -> bool {
        self.flags.intersects(SymbolFlags::VARIABLE)
    }

    #[inline(always)]
    pub fn is_value(&self) -> bool {
        self.flags.intersects(SymbolFlags::VALUE)
    }

    #[inline(always)]
    pub fn is_type(&self) -> bool {
        self.flags.intersects(SymbolFlags::TYPE)
    }

    pub fn as_str(&self) -> &'static str {
        match self.kind.0 {
            SymbolKind::Err => "err",
            SymbolKind::FunctionScopedVar { .. } => todo!(),
            SymbolKind::BlockScopedVar { .. } => todo!(),
            SymbolKind::Fn { .. } => "function",
            SymbolKind::Class { .. } => "class",
            SymbolKind::Prop { .. } => todo!(),
            SymbolKind::Object { .. } => todo!(),
            SymbolKind::BlockContainer { .. } => todo!(),
            SymbolKind::Index { .. } => todo!(),
            SymbolKind::TyAlias { .. } => todo!(),
            SymbolKind::TyParam { .. } => todo!(),
            SymbolKind::TyLit(_) => todo!(),
            SymbolKind::Alias(_) => todo!(),
            SymbolKind::GetterSetter(_) => todo!(),
        }
    }

    pub fn opt_decl(&self) -> Option<NodeID> {
        let id = match &self.kind.0 {
            SymbolKind::FunctionScopedVar(f) => Some(f.decl),
            SymbolKind::BlockScopedVar { decl } => Some(*decl),
            SymbolKind::Class(c) => Some(c.decl),
            SymbolKind::Prop(prop) => Some(prop.decl),
            SymbolKind::Object(object) => Some(object.decl),
            SymbolKind::Index(index) => Some(index.decl),
            SymbolKind::TyAlias(alias) => Some(alias.decl),
            SymbolKind::TyParam(param) => Some(param.decl),
            SymbolKind::TyLit(ty_lit) => Some(ty_lit.decl),
            SymbolKind::Alias(alias) => Some(alias.decl),
            SymbolKind::Fn(f) => Some(f.decls[0]),
            _ => None,
        };
        id.or_else(|| self.kind.1.as_ref().and_then(|i| i.decls.first()).copied())
    }

    pub fn is_readonly_symbol(&self) -> bool {
        // TODO:
        false
    }
}

bolt_ts_utils::module_index!(SymbolID);

impl SymbolID {
    pub fn opt_decl(&self, binder: &super::Binder) -> Option<NodeID> {
        let s = binder.symbol(*self);
        let id = match &s.kind.0 {
            SymbolKind::FunctionScopedVar(f) => Some(f.decl),
            SymbolKind::BlockScopedVar { decl } => Some(*decl),
            SymbolKind::Class(c) => Some(c.decl),
            SymbolKind::Prop(prop) => Some(prop.decl),
            SymbolKind::Object(object) => Some(object.decl),
            SymbolKind::Index(index) => Some(index.decl),
            SymbolKind::TyAlias(alias) => Some(alias.decl),
            SymbolKind::TyParam(param) => Some(param.decl),
            SymbolKind::TyLit(ty_lit) => Some(ty_lit.decl),
            SymbolKind::Alias(alias) => Some(alias.decl),
            SymbolKind::Fn(f) => Some(f.decls[0]),
            _ => None,
        };
        id.or_else(|| s.kind.1.as_ref().and_then(|i| i.decls.first()).copied())
            .or_else(|| s.kind.2.as_ref().and_then(|i| i.decls.first()).copied())
    }

    pub fn decl(&self, binder: &super::Binder) -> NodeID {
        self.opt_decl(binder)
            .unwrap_or_else(|| panic!("{:#?}", binder.symbol(*self).flags))
    }

    pub(crate) fn new(module: ModuleID, index: u32) -> Self {
        debug_assert!(
            module == ModuleID::TRANSIENT,
            "only use for transient during check"
        );
        Self { module, index }
    }
}

pub struct Symbols {
    module_id: ModuleID,
    data: Vec<Symbol>,
}

impl Default for Symbols {
    fn default() -> Self {
        Self {
            module_id: ModuleID::root(),
            data: Vec::with_capacity(1024),
        }
    }
}

impl Symbols {
    pub fn new(module_id: ModuleID) -> Self {
        let mut this = Self {
            module_id,
            data: Vec::with_capacity(512),
        };
        let err = this.insert(Symbol::new(
            SymbolName::Normal(keyword::IDENT_EMPTY),
            SymbolFlags::empty(),
            SymbolKind::Err,
        ));
        assert_eq!(err.index_as_usize(), 0);
        this
    }

    pub fn insert(&mut self, symbol: Symbol) -> SymbolID {
        let index = self.data.len() as u32;
        self.data.push(symbol);
        SymbolID {
            module: self.module_id,
            index,
        }
    }

    pub fn get(&self, id: SymbolID) -> &Symbol {
        &self.data[id.index_as_usize()]
    }

    pub fn get_container(&self, module: ModuleID) -> &Symbol {
        let id = SymbolID { module, index: 1 };
        self.get(id)
    }

    pub fn get_mut(&mut self, id: SymbolID) -> &mut Symbol {
        self.data.get_mut(id.index_as_usize()).unwrap()
    }

    pub fn len(&self) -> u32 {
        self.data.len() as u32
    }
}

pub struct GlobalSymbols(FxHashMap<SymbolName, SymbolID>);

impl Default for GlobalSymbols {
    fn default() -> Self {
        Self::new()
    }
}

impl GlobalSymbols {
    pub fn new() -> Self {
        Self(fx_hashmap_with_capacity(1024 * 128))
    }

    pub fn insert(&mut self, name: SymbolName, symbol_id: SymbolID) {
        let prev = self.0.insert(name, symbol_id);
        assert!(prev.is_none(), "prev symbol: {prev:#?}")
    }

    pub fn get(&self, name: SymbolName) -> Option<SymbolID> {
        self.0.get(&name).copied()
    }
}
