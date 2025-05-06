use bolt_ts_atom::AtomId;
use bolt_ts_span::ModuleID;
use bolt_ts_utils::fx_hashmap_with_capacity;
use rustc_hash::FxHashMap;

use crate::{check::F64Represent, parser::Parser};
use bolt_ts_ast::NodeID;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum SymbolName {
    Container,
    Atom(AtomId),
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
    Missing,
    Resolving,
    ExportStar,
    ExportEquals,
    ExportDefault,
    Computed,
    ParamIdx(u32),
    ESSymbol {
        escaped_name: AtomId,
        symbol_id: SymbolID,
    },
}

impl SymbolName {
    pub fn expect_atom(&self) -> AtomId {
        self.as_atom().unwrap()
    }

    pub fn as_atom(&self) -> Option<AtomId> {
        match self {
            SymbolName::Atom(atom) => Some(*atom),
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

        const CLASS_OR_INTERFACE = Self::CLASS.bits() | Self::INTERFACE.bits();
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

impl SymbolFlags {
    pub const fn get_excluded(&self) -> Self {
        let mut result = SymbolFlags::empty();
        if self.intersects(SymbolFlags::BLOCK_SCOPED_VARIABLE) {
            result = result.union(SymbolFlags::BLOCK_SCOPED_VARIABLE_EXCLUDES);
        };
        if self.intersects(SymbolFlags::FUNCTION_SCOPED_VARIABLE) {
            result = result.union(SymbolFlags::FUNCTION_SCOPED_VARIABLE_EXCLUDES);
        };
        if self.intersects(SymbolFlags::PROPERTY) {
            result = result.union(SymbolFlags::PROPERTY_EXCLUDES);
        };
        if self.intersects(SymbolFlags::ENUM_MEMBER) {
            result = result.union(SymbolFlags::ENUM_MEMBER_EXCLUDES);
        };
        if self.intersects(SymbolFlags::FUNCTION) {
            result = result.union(SymbolFlags::FUNCTION_EXCLUDES);
        };
        if self.intersects(SymbolFlags::CLASS) {
            result = result.union(SymbolFlags::CLASS_EXCLUDES);
        };
        if self.intersects(SymbolFlags::INTERFACE) {
            result = result.union(SymbolFlags::INTERFACE_EXCLUDES);
        };
        if self.intersects(SymbolFlags::REGULAR_ENUM) {
            result = result.union(SymbolFlags::REGULAR_ENUM_EXCLUDES);
        };
        if self.intersects(SymbolFlags::CONST_ENUM) {
            result = result.union(SymbolFlags::CONST_ENUM_EXCLUDES);
        };
        if self.intersects(SymbolFlags::VALUE_MODULE) {
            result = result.union(SymbolFlags::VALUE_MODULE_EXCLUDES);
        };
        if self.intersects(SymbolFlags::METHOD) {
            result = result.union(SymbolFlags::METHOD_EXCLUDES);
        };
        if self.intersects(SymbolFlags::GET_ACCESSOR) {
            result = result.union(SymbolFlags::GET_ACCESSOR_EXCLUDES);
        };
        if self.intersects(SymbolFlags::SET_ACCESSOR) {
            result = result.union(SymbolFlags::SET_ACCESSOR_EXCLUDES);
        };
        if self.intersects(SymbolFlags::TYPE_PARAMETER) {
            result = result.union(SymbolFlags::TYPE_PARAMETER_EXCLUDES);
        };
        if self.intersects(SymbolFlags::TYPE_ALIAS) {
            result = result.union(SymbolFlags::TYPE_ALIAS_EXCLUDES);
        };
        if self.intersects(SymbolFlags::ALIAS) {
            result = result.union(SymbolFlags::ALIAS_EXCLUDES);
        };
        result
    }
}

#[derive(Debug)]
pub struct Symbol {
    pub name: SymbolName,
    pub flags: SymbolFlags,
    pub decls: Option<thin_vec::ThinVec<NodeID>>,
    pub value_decl: Option<NodeID>,
    pub members: Option<SymbolTable>,
    pub exports: Option<SymbolTable>,
    pub merged_id: Option<u32>,
    pub parent: Option<SymbolID>,
    pub export_symbol: Option<SymbolID>,
    pub const_enum_only_module: Option<bool>,
    pub is_replaceable_by_method: Option<bool>,
}

impl Symbol {
    pub fn new(name: SymbolName, flags: SymbolFlags) -> Self {
        Self {
            name,
            flags,
            decls: None,
            value_decl: None,
            members: None,
            exports: None,
            parent: None,
            merged_id: None,
            export_symbol: None,
            const_enum_only_module: None,
            is_replaceable_by_method: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable(pub FxHashMap<SymbolName, SymbolID>);

impl Default for SymbolTable {
    fn default() -> Self {
        Self(fx_hashmap_with_capacity(64))
    }
}

impl SymbolTable {
    pub fn new(cap: usize) -> Self {
        Self(fx_hashmap_with_capacity(cap))
    }
}

impl Symbol {
    pub const ERR: SymbolID = SymbolID::ERR;
    pub const GLOBAL_THIS: SymbolID = SymbolID::GLOBAL_THIS;
    pub const ARGUMENTS: SymbolID = SymbolID::ARGUMENTS;
    pub const RESOLVING: SymbolID = SymbolID::RESOLVING;
    pub const EMPTY_TYPE_LITERAL: SymbolID = SymbolID::EMPTY_TYPE_LITERAL;
    pub const UNDEFINED: SymbolID = SymbolID::UNDEFINED;

    pub fn can_have_symbol(node: bolt_ts_ast::Node<'_>) -> bool {
        use bolt_ts_ast::Node::*;
        node.is_decl() || matches!(node, FnTy(_))
    }

    pub fn get_decl_of_alias_symbol(&self, p: &Parser) -> Option<NodeID> {
        self.decls.as_ref().and_then(|decls| {
            decls
                .iter()
                .rev()
                .find(|decl| p.is_alias_symbol_decl(**decl))
                .copied()
        })
    }

    pub fn members(&self) -> Option<&SymbolTable> {
        self.members.as_ref()
    }
    pub fn exports(&self) -> Option<&SymbolTable> {
        self.exports.as_ref()
    }
    pub fn get_declaration_of_kind(
        &self,
        f: impl Fn(bolt_ts_ast::NodeID) -> bool,
    ) -> Option<bolt_ts_ast::NodeID> {
        self.decls
            .as_ref()
            .and_then(|decls| decls.iter().find(|decl| f(**decl)).copied())
    }
    pub fn opt_decl(&self) -> Option<NodeID> {
        self.decls.as_ref().and_then(|decls| decls.first()).copied()
    }
    pub fn is_shorthand_ambient_module(&self, p: &Parser) -> bool {
        self.value_decl.is_some_and(|value_decl| {
            p.node(value_decl)
                .as_module_decl()
                .is_some_and(|ns| ns.block.is_none())
        })
    }
}

bolt_ts_utils::module_index!(SymbolID);

impl SymbolID {
    pub(super) const ERR: Self = SymbolID {
        module: ModuleID::TRANSIENT,
        index: 0,
    };
    pub(super) const GLOBAL_THIS: Self = SymbolID {
        module: ModuleID::TRANSIENT,
        index: 1,
    };
    pub(super) const ARGUMENTS: Self = SymbolID {
        module: ModuleID::TRANSIENT,
        index: 2,
    };
    pub(super) const RESOLVING: Self = SymbolID {
        module: ModuleID::TRANSIENT,
        index: 3,
    };
    pub(super) const EMPTY_TYPE_LITERAL: Self = SymbolID {
        module: ModuleID::TRANSIENT,
        index: 4,
    };
    pub(super) const UNDEFINED: Self = SymbolID {
        module: ModuleID::TRANSIENT,
        index: 5,
    };

    pub fn container(module: ModuleID) -> Self {
        assert_ne!(module.as_u32(), ModuleID::TRANSIENT.as_u32());
        Self { module, index: 0 }
    }

    pub fn opt_decl(&self, binder: &super::Binder) -> Option<NodeID> {
        let s = binder.symbol(*self);
        s.opt_decl()
    }

    pub fn decl(&self, binder: &super::Binder) -> NodeID {
        self.opt_decl(binder)
            .unwrap_or_else(|| panic!("{:#?}", binder.symbol(*self).flags))
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
        assert_ne!(module_id, ModuleID::TRANSIENT);
        assert_ne!(module_id, ModuleID::DEFAULT);
        Self {
            module_id,
            data: Vec::with_capacity(512),
        }
    }

    pub fn new_transient(modules: usize) -> Self {
        Self {
            module_id: ModuleID::TRANSIENT,
            data: Vec::with_capacity(modules * 1024 * 64),
        }
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
        let idx = id.index_as_usize();
        debug_assert!(idx < self.data.len());
        unsafe { self.data.get_unchecked(idx) }
    }

    pub fn get_mut(&mut self, id: SymbolID) -> &mut Symbol {
        let idx = id.index_as_usize();
        debug_assert!(idx < self.data.len());
        unsafe { self.data.get_unchecked_mut(idx) }
    }

    pub fn len(&self) -> u32 {
        self.data.len() as u32
    }

    pub fn module(&self) -> ModuleID {
        self.module_id
    }
}

pub type GlobalSymbols = SymbolTable;

#[derive(Debug, Clone, Copy)]
pub(super) struct SymbolTableLocation {
    pub(super) container: bolt_ts_ast::NodeID,
    pub(super) kind: SymbolTableLocationKind,
}

#[derive(Debug, Clone, Copy)]
pub(super) enum SymbolTableLocationKind {
    SymbolMember,
    SymbolExports,
    ContainerLocals,
}

impl SymbolTableLocation {
    pub(super) fn locals(container: bolt_ts_ast::NodeID) -> Self {
        Self {
            container,
            kind: SymbolTableLocationKind::ContainerLocals,
        }
    }
    pub(super) fn members(container: bolt_ts_ast::NodeID) -> Self {
        Self {
            container,
            kind: SymbolTableLocationKind::SymbolMember,
        }
    }
    pub(super) fn exports(container: bolt_ts_ast::NodeID) -> Self {
        Self {
            container,
            kind: SymbolTableLocationKind::SymbolExports,
        }
    }
}
