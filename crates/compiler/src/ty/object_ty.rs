use crate::bind::{SymbolFlags, SymbolID, SymbolName};
use crate::check::TyChecker;

use super::flags::ObjectFlags;
use super::pprint::pprint_reference_ty;
use super::Ty;

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
}

macro_rules! ty_kind_as_object_ty_kind {
    ($ty:ty, $as_kind: ident, $as_object_kind: ident, $expect_object_kind: ident, $is_object_kind: ident) => {
        impl<'cx> super::TyKind<'cx> {
            #[inline(always)]
            pub fn $as_object_kind(&self) -> Option<$ty> {
                self.as_object().and_then(|object| object.kind.$as_kind())
            }
            #[inline(always)]
            pub fn $is_object_kind(&self) -> bool {
                self.$as_object_kind().is_some()
            }
            #[inline(always)]
            pub fn $expect_object_kind(&self) -> $ty {
                self.$as_object_kind().unwrap()
            }
        }
    };
}

ty_kind_as_object_ty_kind!(
    &'cx AnonymousTy<'cx>,
    as_anonymous,
    as_object_anonymous,
    expect_object_anonymous,
    is_object_anonymous
);
ty_kind_as_object_ty_kind!(
    &'cx TupleTy<'cx>,
    as_tuple,
    as_object_tuple,
    expect_object_tuple,
    is_object_tuple
);
ty_kind_as_object_ty_kind!(
    &'cx InterfaceTy<'cx>,
    as_interface,
    as_object_interface,
    expect_object_interface,
    is_object_interface
);
ty_kind_as_object_ty_kind!(
    &'cx ReferenceTy<'cx>,
    as_reference,
    as_object_reference,
    expect_object_reference,
    is_object_reference
);

macro_rules! as_object_ty_kind {
    ($kind: ident, $ty:ty, $as_kind: ident, $is_kind: ident) => {
        impl<'cx> ObjectTyKind<'cx> {
            #[inline(always)]
            pub fn $as_kind(&self) -> Option<$ty> {
                match self {
                    ObjectTyKind::$kind(ty) => Some(ty),
                    _ => None,
                }
            }
            #[inline(always)]
            pub fn $is_kind(&self) -> bool {
                self.$as_kind().is_some()
            }
        }
    };
}

as_object_ty_kind!(Anonymous, &'cx AnonymousTy<'cx>, as_anonymous, is_anonymous);
as_object_ty_kind!(Tuple, &'cx TupleTy<'cx>, as_tuple, is_tuple);
as_object_ty_kind!(Interface, &'cx InterfaceTy<'cx>, as_interface, is_interface);
as_object_ty_kind!(Reference, &'cx ReferenceTy<'cx>, as_reference, is_reference);

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
pub struct ReferenceTy<'cx> {
    pub target: &'cx Ty<'cx>,
    pub resolved_ty_args: super::Tys<'cx>,
}

impl<'cx> ReferenceTy<'cx> {
    pub fn deep_target(&self) -> &'cx Ty<'cx> {
        let mut ty = self.target;
        loop {
            if let Some(reference) = ty.kind.as_object_reference() {
                ty = reference.target;
            } else {
                break;
            }
        }
        ty
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IndexInfo<'cx> {
    pub symbol: SymbolID,
    pub key_ty: &'cx Ty<'cx>,
    pub val_ty: &'cx Ty<'cx>,
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
    pub base_tys: super::Tys<'cx>,
    pub base_ctor_ty: Option<&'cx super::Ty<'cx>>,
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
    pub declared_members: &'cx DeclaredMembers<'cx>,
}

impl<'cx> ObjectTyKind<'cx> {
    pub(super) fn to_string(&self, self_ty: &Ty, checker: &mut TyChecker<'cx>) -> String {
        match self {
            ObjectTyKind::Anonymous(a) => {
                let symbol = a.symbol;
                let symbol = checker.binder.symbol(symbol);
                let print_fn_like_str = |checker: &mut TyChecker, sig: &super::Sig| -> String {
                    let params = sig.params;
                    let params = params
                        .iter()
                        .map(|param| {
                            let decl = param.decl(checker.binder);
                            let name = checker.p.node(decl).ident_name().unwrap();
                            let ty = checker.get_type_of_symbol(*param);
                            format!(
                                "{}: {}",
                                checker.atoms.get(name.name),
                                ty.to_string(checker)
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    let ret = if let Some(ret) = sig.ret {
                        let ty = checker.p.node(ret);
                        let ty = checker.get_ty_from_type_node(&ty.as_ty().unwrap());
                        ty.to_string(checker)
                    } else {
                        checker.void_ty().to_string(checker)
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
                            let name = checker.atoms.get(name.expect_atom());
                            let ty = checker.get_type_of_symbol(*symbol);
                            format!("{}: {}; ", name, ty.to_string(checker))
                        })
                        .collect::<Vec<_>>()
                        .join("");
                    format!("{{ {}}}", members)
                } else if symbol.flags.intersects(SymbolFlags::CLASS) {
                    let name = symbol.name.expect_atom();
                    format!("typeof {}", checker.atoms.get(name))
                } else if let Some(sig) = checker
                    .expect_ty_links(self_ty.id)
                    .expect_structured_members()
                    .call_sigs
                    .first()
                {
                    print_fn_like_str(checker, &sig)
                } else if let Some(sig) = checker
                    .expect_ty_links(self_ty.id)
                    .expect_structured_members()
                    .ctor_sigs
                    .first()
                {
                    format!("new {}", print_fn_like_str(checker, &sig))
                } else if let Some(index_info) = checker
                    .expect_ty_links(self_ty.id)
                    .expect_structured_members()
                    .index_infos
                    .first()
                {
                    let decl = checker.binder.symbol(index_info.symbol).expect_index().decl;
                    let key_name = checker.p.node(decl).expect_index_sig_decl().params[0].name;
                    let key_name = checker.atoms.get(key_name.name);
                    format!(
                        "{{ [{key_name}: {}]: {} }}",
                        index_info.key_ty.to_string(checker),
                        index_info.val_ty.to_string(checker)
                    )
                } else {
                    let members = checker
                        .expect_ty_links(self_ty.id)
                        .expect_structured_members()
                        .members;
                    let members = members
                        .iter()
                        .map(|(name, symbol)| {
                            let name = checker.atoms.get(name.expect_atom());
                            let ty = checker.get_type_of_symbol(*symbol);
                            format!("{}: {}; ", name, ty.to_string(checker))
                        })
                        .collect::<Vec<_>>()
                        .join("");
                    format!("{{ {}}}", members)
                }
            }
            ObjectTyKind::Tuple(t) => {
                format!(
                    "[{}]",
                    t.resolved_ty_args
                        .iter()
                        .map(|ty| ty.to_string(checker))
                        .collect::<Vec<_>>()
                        .join(",")
                )
            }
            ObjectTyKind::Interface(i) => checker
                .atoms
                .get(checker.binder.symbol(i.symbol).name.expect_atom())
                .to_string(),
            ObjectTyKind::Reference(refer) => pprint_reference_ty(refer, checker),
            ObjectTyKind::SingleSigTy(_) => "single signature type".to_string(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AnonymousTy<'cx> {
    pub symbol: SymbolID,
    pub target: Option<&'cx Ty<'cx>>,
    pub mapper: Option<&'cx super::TyMapper<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct SingleSigTy<'cx> {
    pub symbol: SymbolID,
    pub target: Option<&'cx Ty<'cx>>,
    pub mapper: Option<&'cx super::TyMapper<'cx>>,
    pub outer_ty_params: Option<super::Tys<'cx>>,
}
