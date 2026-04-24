use bolt_ts_ast::{self as ast, pprint_binding};
use bolt_ts_binder::{Symbol, SymbolFlags, SymbolID, SymbolName};
use bolt_ts_ty::ObjectFlags;
use bolt_ts_utils::FxIndexMap;

use super::PromiseOrAwaitableTyLinksID;
use super::TyChecker;
use super::links::{InterfaceTyLinksID, ObjectMappedTyLinksID};
use super::{Ty, TyMap};

#[derive(Debug, Clone, Copy)]
pub struct ObjectTy<'cx> {
    pub kind: ObjectTyKind<'cx>,
    pub flags: ObjectFlags,
}

#[derive(Debug, Clone, Copy)]
pub enum ObjectTyKind<'cx> {
    Anonymous(&'cx AnonymousTy<'cx>),
    SingleSigTy(&'cx SingleSigTy<'cx>),
    Tuple(&'cx TupleTy<'cx>),
    Interface(&'cx InterfaceTy<'cx>),
    Reference(&'cx ReferenceTy<'cx>),
    Mapped(&'cx MappedTy<'cx>),
    ReversedMapped(&'cx ReverseMappedTy<'cx>),
}

impl<'cx> ObjectTyKind<'cx> {
    pub fn as_generic_tuple_type(&self) -> Option<&'cx TupleTy<'cx>> {
        let ObjectTyKind::Reference(refer) = self else {
            return None;
        };
        refer
            .target
            .kind
            .as_object_tuple()
            .filter(|tup| tup.combined_flags.contains(ElementFlags::VARIADIC))
    }

    pub fn alias_ty_arguments(&self) -> Option<super::Tys<'cx>> {
        match self {
            ObjectTyKind::Mapped(ty) => ty.alias_ty_arguments,
            ObjectTyKind::Reference(ty) => ty.alias_ty_arguments,
            ObjectTyKind::Anonymous(ty) => ty.alias_ty_arguments,
            _ => None,
        }
    }

    pub fn alias_symbol(&self) -> Option<SymbolID> {
        match self {
            ObjectTyKind::Mapped(ty) => ty.alias_symbol,
            ObjectTyKind::Reference(ty) => ty.alias_symbol,
            ObjectTyKind::Anonymous(ty) => ty.alias_symbol,
            _ => None,
        }
    }

    pub fn promise_or_awaitable_ty_links(&self) -> Option<PromiseOrAwaitableTyLinksID<'cx>> {
        match self {
            ObjectTyKind::Reference(ty) => Some(ty.promise_or_awaitable_links),
            _ => None,
        }
    }
}

macro_rules! ty_kind_as_object_ty_kind {
    ($ty:ty, $name: ident) => {
        paste::paste! {
            impl<'cx> super::TyKind<'cx> {
                #[inline(always)]
                pub fn [<as_object_ $name>](&self) -> Option<$ty> {
                    self.as_object().and_then(|object| object.kind.[<as_ $name>]())
                }
                #[inline(always)]
                pub fn [<is_object_ $name>](&self) -> bool {
                    self.[<as_object_ $name>]().is_some()
                }
                #[inline(always)]
                #[track_caller]
                pub fn [<expect_object_ $name>](&self) -> $ty {
                    self.[<as_object_ $name>]().unwrap()
                }
            }
        }
    };
}

ty_kind_as_object_ty_kind!(&'cx AnonymousTy<'cx>, anonymous);
ty_kind_as_object_ty_kind!(&'cx TupleTy<'cx>, tuple);
ty_kind_as_object_ty_kind!(&'cx InterfaceTy<'cx>, interface);
ty_kind_as_object_ty_kind!(&'cx ReferenceTy<'cx>, reference);
ty_kind_as_object_ty_kind!(&'cx MappedTy<'cx>, mapped);
ty_kind_as_object_ty_kind!(&'cx ReverseMappedTy<'cx>, reverse_mapped);

macro_rules! as_object_ty_kind {
    ($kind: ident, $ty:ty, $name: ident) => {
        paste::paste! {
            impl<'cx> ObjectTyKind<'cx> {
                #[inline(always)]
                pub fn [<as_ $name>](&self) -> Option<$ty> {
                    match self {
                        ObjectTyKind::$kind(ty) => Some(ty),
                        _ => None,
                    }
                }
                #[inline(always)]
                pub fn [<is_ $name>](&self) -> bool {
                    self.[<as_ $name>]().is_some()
                }
            }
        }
    };
}

as_object_ty_kind!(Anonymous, &'cx AnonymousTy<'cx>, anonymous);
as_object_ty_kind!(Tuple, &'cx TupleTy<'cx>, tuple);
as_object_ty_kind!(Interface, &'cx InterfaceTy<'cx>, interface);
as_object_ty_kind!(Reference, &'cx ReferenceTy<'cx>, reference);
as_object_ty_kind!(Mapped, &'cx MappedTy<'cx>, mapped);
as_object_ty_kind!(ReversedMapped, &'cx ReverseMappedTy<'cx>, reverse_mapped);

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct ElementFlags: u8 {
        /// `T`
        const REQUIRED  = 1 << 0;
        /// `T?`
        const OPTIONAL  = 1 << 1;
        /// `...T[]`
        const REST      = 1 << 2;
        /// `...T`
        const VARIADIC  = 1 << 3;

        const FIXED         = Self::REQUIRED.bits() | Self::OPTIONAL.bits();
        const VARIABLE      = Self::REST.bits() | Self::VARIADIC.bits();
        const NON_REQUIRED  = Self::OPTIONAL.bits() | Self::REST.bits() | Self::VARIADIC.bits();
        const NON_REST      = Self::REQUIRED.bits() | Self::OPTIONAL.bits() | Self::VARIADIC.bits();
    }
}

#[derive(Debug)]
pub struct TupleTy<'cx> {
    /// shape (an interface type)
    pub ty: &'cx Ty<'cx>,

    // instance
    pub resolved_ty_args: super::Tys<'cx>,

    // properties
    pub min_length: usize,
    pub fixed_length: usize,
    pub element_flags: &'cx [ElementFlags],
    pub combined_flags: ElementFlags,
    pub readonly: bool,
}

impl<'cx> TupleTy<'cx> {
    pub fn ty_params(&self) -> Option<super::Tys<'cx>> {
        self.ty.kind.expect_object_interface().ty_params
    }

    pub fn get_start_elem_count(&self, flags: ElementFlags) -> usize {
        let Some(index) = self.element_flags.iter().position(|e| !e.intersects(flags)) else {
            return self.element_flags.len();
        };
        index
    }

    pub fn get_end_elem_count(&self, flags: ElementFlags) -> usize {
        let len = self.element_flags.len();
        let Some(idx) = self
            .element_flags
            .iter()
            .rev()
            .position(|e| !e.intersects(flags))
        else {
            return len;
        };
        idx
    }
}

#[derive(Debug)]
pub struct ReferenceTy<'cx> {
    pub target: &'cx Ty<'cx>,
    pub mapper: Option<&'cx dyn TyMap<'cx>>,
    pub node: Option<ast::NodeID>,
    pub alias_symbol: Option<SymbolID>,
    pub alias_ty_arguments: Option<super::Tys<'cx>>,
    pub promise_or_awaitable_links: PromiseOrAwaitableTyLinksID<'cx>,
}

impl<'cx> ReferenceTy<'cx> {
    pub fn interface_target(&self) -> Option<&'cx Ty<'cx>> {
        if self.target.kind.is_object_interface() {
            Some(self.target)
        } else if let Some(r) = self.target.kind.as_object_reference() {
            if r.target.kind.is_object_interface() {
                Some(r.target)
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct IndexInfo<'cx> {
    pub symbol: SymbolID,
    pub key_ty: &'cx Ty<'cx>,
    pub val_ty: &'cx Ty<'cx>,
    pub is_readonly: bool,
}

impl PartialEq for &IndexInfo<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

pub type IndexInfos<'cx> = &'cx [&'cx IndexInfo<'cx>];

#[derive(Debug)]
pub struct DeclaredMembers<'cx> {
    pub props: &'cx [SymbolID],
    pub index_infos: IndexInfos<'cx>,
    pub ctor_sigs: super::Sigs<'cx>,
    pub call_sigs: super::Sigs<'cx>,
}

#[derive(Debug)]
pub struct StructuredMembers<'cx> {
    pub members: &'cx FxIndexMap<SymbolName, SymbolID>,
    pub call_sigs: super::Sigs<'cx>,
    pub ctor_sigs: super::Sigs<'cx>,
    pub index_infos: self::IndexInfos<'cx>,
    pub props: &'cx [SymbolID],
}

#[derive(Debug)]
pub struct InterfaceTy<'cx> {
    pub symbol: Option<SymbolID>,
    pub ty_params: Option<super::Tys<'cx>>,
    pub outer_ty_params: Option<super::Tys<'cx>>,
    pub local_ty_params: Option<super::Tys<'cx>>,
    pub this_ty: Option<&'cx Ty<'cx>>,
    pub links: InterfaceTyLinksID<'cx>,
}

impl<'cx> ObjectTyKind<'cx> {}

#[derive(Debug)]
pub struct AnonymousTy<'cx> {
    pub symbol: Option<SymbolID>,
    pub target: Option<&'cx Ty<'cx>>,
    pub mapper: Option<&'cx dyn TyMap<'cx>>,
    // TODO: use `Option` for this links because only fresh object literal contain this.
    pub fresh_ty_links: super::FreshTyLinksID<'cx>,
    /// exist for InstantiationExpressionType
    pub node: Option<ast::NodeID>,
    pub alias_symbol: Option<SymbolID>,
    pub alias_ty_arguments: Option<super::Tys<'cx>>,
}

#[derive(Debug)]
pub struct SingleSigTy<'cx> {
    pub symbol: Option<SymbolID>,
    pub target: Option<&'cx Ty<'cx>>,
    pub mapper: Option<&'cx dyn TyMap<'cx>>,
}

#[derive(Debug)]
pub struct MappedTy<'cx> {
    pub symbol: SymbolID,
    pub decl: &'cx ast::MappedTy<'cx>,
    pub alias_symbol: Option<SymbolID>,
    pub alias_ty_arguments: Option<super::Tys<'cx>>,
    pub target: Option<&'cx Ty<'cx>>,
    pub mapper: Option<&'cx dyn TyMap<'cx>>,
    pub links: ObjectMappedTyLinksID<'cx>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MappedTyNameTyKind {
    None,
    Filtering,
    Remapping,
}

#[derive(Debug)]
pub struct ReverseMappedTy<'cx> {
    pub source: &'cx Ty<'cx>,
    pub mapped_ty: &'cx Ty<'cx>,
    pub constraint_ty: &'cx Ty<'cx>,
}
