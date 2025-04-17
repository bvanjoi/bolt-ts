use crate::bind::{SymbolFlags, SymbolID, SymbolName};
use crate::check::TyChecker;
use bolt_ts_ast::{self as ast, pprint_binding};

use super::flags::ObjectFlags;
use super::links::{InterfaceTyLinksID, ObjectMappedTyLinksID};
use super::pprint::pprint_reference_ty;
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
pub struct ReferenceTy<'cx> {
    pub target: &'cx Ty<'cx>,
    pub mapper: Option<&'cx dyn TyMap<'cx>>,
    pub node: Option<ast::NodeID>,
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
pub struct DeclaredMembers<'cx> {
    pub props: &'cx [SymbolID],
    pub index_infos: IndexInfos<'cx>,
    pub ctor_sigs: super::Sigs<'cx>,
    pub call_sigs: super::Sigs<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct StructuredMembers<'cx> {
    pub members: &'cx rustc_hash::FxHashMap<SymbolName, SymbolID>,
    pub call_sigs: super::Sigs<'cx>,
    pub ctor_sigs: super::Sigs<'cx>,
    pub index_infos: self::IndexInfos<'cx>,
    pub props: &'cx [SymbolID],
}

#[derive(Debug, Clone, Copy)]
pub struct InterfaceTy<'cx> {
    pub symbol: SymbolID,
    pub ty_params: Option<super::Tys<'cx>>,
    pub outer_ty_params: Option<super::Tys<'cx>>,
    pub local_ty_params: Option<super::Tys<'cx>>,
    pub this_ty: Option<&'cx Ty<'cx>>,
    pub links: InterfaceTyLinksID<'cx>,
}

impl<'cx> ObjectTyKind<'cx> {
    pub(super) fn to_string(&self, self_ty: &'cx Ty<'cx>, checker: &mut TyChecker<'cx>) -> String {
        match self {
            ObjectTyKind::Anonymous(a) => {
                let symbol = a.symbol.unwrap();
                let symbol = checker.binder.symbol(symbol);
                let print_fn_like_str = |checker: &mut TyChecker, sig: &super::Sig| -> String {
                    let params = sig.params;
                    let params = params
                        .iter()
                        .map(|param| {
                            let decl = checker.get_symbol_decl(*param).unwrap();
                            let name = checker.p.node(decl).ident_name().unwrap();
                            let ty = checker.get_type_of_symbol(*param);
                            format!(
                                "{name}: {ty}",
                                ty = ty.to_string(checker),
                                name = checker.atoms.get(name.name),
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    let ret = if let Some(ret) = sig.ret {
                        let ty = checker.p.node(ret);
                        let ty = checker.get_ty_from_type_node(&ty.as_ty().unwrap());
                        ty.to_string(checker)
                    } else {
                        checker.void_ty.to_string(checker)
                    };
                    format!("({params}) => {ret}")
                };
                if symbol.flags.intersects(SymbolFlags::OBJECT_LITERAL) {
                    let members = checker
                        .expect_ty_links(self_ty.id)
                        .expect_structured_members()
                        .members;
                    let members = members
                        .iter()
                        .map(|(name, symbol)| {
                            let name = if let Some(name) = name.as_atom() {
                                let name = checker.atoms.get(name).to_string();
                                if name.is_empty() {
                                    "''".to_string()
                                } else {
                                    name
                                }
                            } else if let Some(num) = name.as_numeric() {
                                num.to_string()
                            } else {
                                unreachable!()
                            };
                            let ty = checker.get_type_of_symbol(*symbol);
                            format!("{}: {}; ", name, ty.to_string(checker))
                        })
                        .collect::<Vec<_>>()
                        .join("");
                    format!("{{ {}}}", members)
                } else if symbol
                    .flags
                    .intersects(SymbolFlags::CLASS.union(SymbolFlags::VALUE_MODULE))
                {
                    let name = symbol.name.expect_atom();
                    format!("typeof {}", checker.atoms.get(name))
                } else if let Some(sig) = checker
                    .get_signatures_of_type(self_ty, super::SigKind::Call)
                    .first()
                {
                    print_fn_like_str(checker, sig)
                } else if let Some(sig) = checker
                    .expect_ty_links(self_ty.id)
                    .expect_structured_members()
                    .ctor_sigs
                    .first()
                {
                    format!("new {}", print_fn_like_str(checker, sig))
                } else if let Some(index_info) = checker
                    .expect_ty_links(self_ty.id)
                    .expect_structured_members()
                    .index_infos
                    .first()
                {
                    let decl = checker.binder.symbol(index_info.symbol).opt_decl().unwrap();
                    let key_name = checker.p.node(decl).expect_index_sig_decl().params[0].name;
                    format!(
                        "{{ [{key_name}: {key_ty}]: {val_ty} }}",
                        key_ty = index_info.key_ty.to_string(checker),
                        val_ty = index_info.val_ty.to_string(checker),
                        key_name = pprint_binding(key_name, checker.atoms),
                    )
                } else {
                    let members = checker
                        .expect_ty_links(self_ty.id)
                        .expect_structured_members()
                        .members;
                    let members = members
                        .iter()
                        .map(|(name, symbol)| {
                            let ty = checker.get_type_of_symbol(*symbol);
                            format!(
                                "{field_name}: {filed_ty}; ",
                                filed_ty = ty.to_string(checker),
                                field_name = checker.atoms.get(name.expect_atom()),
                            )
                        })
                        .collect::<Vec<_>>()
                        .join("");
                    format!("{{ {}}}", members)
                }
            }
            ObjectTyKind::Tuple(ty) => {
                assert!(ty.element_flags.is_empty());
                "[]".to_string()
            }
            ObjectTyKind::Interface(i) => checker
                .atoms
                .get(checker.binder.symbol(i.symbol).name.expect_atom())
                .to_string(),
            ObjectTyKind::Reference(_) => pprint_reference_ty(self_ty, checker),
            ObjectTyKind::SingleSigTy(_) => "single signature type".to_string(),
            ObjectTyKind::Mapped(m) => {
                if let Some(s) = m.alias_symbol {
                    checker
                        .atoms
                        .get(checker.binder.symbol(s).name.expect_atom())
                        .to_string()
                } else {
                    "mapped type".to_string()
                }
            }
            ObjectTyKind::ReversedMapped(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AnonymousTy<'cx> {
    pub symbol: Option<SymbolID>,
    pub target: Option<&'cx Ty<'cx>>,
    pub mapper: Option<&'cx dyn TyMap<'cx>>,
    // TODO: use `Option` for this links because only fresh object literal contain this.
    pub fresh_ty_links: super::FreshTyLinksID<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct SingleSigTy<'cx> {
    pub symbol: SymbolID,
    pub target: Option<&'cx Ty<'cx>>,
    pub mapper: Option<&'cx dyn TyMap<'cx>>,
    pub outer_ty_params: Option<super::Tys<'cx>>,
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
pub struct ReverseMappedTy<'cx> {
    pub source: &'cx Ty<'cx>,
    pub mapped_ty: &'cx Ty<'cx>,
    pub constraint_ty: &'cx Ty<'cx>,
}
