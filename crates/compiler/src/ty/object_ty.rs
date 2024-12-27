use rustc_hash::FxHashMap;

use crate::bind::{SymbolID, SymbolName};
use crate::check::TyChecker;

use super::{Sig, Ty};

#[derive(Debug, Clone, Copy)]
pub struct ObjectTy<'cx> {
    pub kind: ObjectTyKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ObjectTyKind<'cx> {
    Class(&'cx ClassTy),
    Fn(&'cx FnTy<'cx>),
    ObjectLit(&'cx ObjectLitTy<'cx>),
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
    &'cx ClassTy,
    as_class,
    as_object_class,
    expect_object_class,
    is_object_class
);
ty_kind_as_object_ty_kind!(
    &'cx FnTy<'cx>,
    as_fn,
    as_object_fn,
    expect_object_fn,
    is_object_fn
);
ty_kind_as_object_ty_kind!(
    &'cx ObjectLitTy<'cx>,
    as_object_lit,
    as_object_lit,
    expect_object_lit,
    is_object_lit
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

as_object_ty_kind!(Class, &'cx ClassTy, as_class, is_class);
as_object_ty_kind!(Fn, &'cx FnTy<'cx>, as_fn, is_fn);
as_object_ty_kind!(
    ObjectLit,
    &'cx ObjectLitTy<'cx>,
    as_object_lit,
    is_object_lit
);
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
    pub tys: super::Tys<'cx>,
    pub element_flags: &'cx [ElementFlags],
    pub combined_flags: ElementFlags,
    pub shape: &'cx TupleShape<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct TupleShape<'cx> {
    pub declared_props: &'cx [SymbolID],
    pub fixed_length: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct ReferenceTy<'cx> {
    pub ty_args: super::Tys<'cx>,
    pub target: &'cx Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct IndexInfo<'cx> {
    pub symbol: SymbolID,
    pub key_ty: &'cx Ty<'cx>,
    pub val_ty: &'cx Ty<'cx>,
}

pub type IndexInfos<'cx> = &'cx [&'cx IndexInfo<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct DeclaredInfos<'cx> {
    pub declared_props: &'cx [SymbolID],
    pub declared_index_infos: IndexInfos<'cx>,
    pub declared_ctor_sigs: super::Sigs<'cx>,
    pub declared_call_sigs: super::Sigs<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct InterfaceTy<'cx> {
    pub symbol: SymbolID,
    pub members: &'cx FxHashMap<SymbolName, SymbolID>,

    pub ty_params: super::Tys<'cx>,
    pub outer_ty_params: super::Tys<'cx>,
    pub local_ty_params: super::Tys<'cx>,

    pub base_tys: super::Tys<'cx>,
    pub base_ctor_ty: Option<&'cx Ty<'cx>>,

    pub declared_infos: &'cx DeclaredInfos<'cx>,

    pub props: &'cx [SymbolID],
    pub index_infos: IndexInfos<'cx>,
    pub ctor_sigs: super::Sigs<'cx>,
    pub call_sigs: super::Sigs<'cx>,
}

impl<'cx> ObjectTyKind<'cx> {
    pub(super) fn to_string(&self, checker: &mut TyChecker) -> String {
        match self {
            ObjectTyKind::Class(_) => "class".to_string(),
            ObjectTyKind::Fn(f) => {
                let params = f.declared_sigs[0].params;
                let params = params
                    .iter()
                    .map(|param| {
                        let decl = checker.binder.symbol(*param).decl();
                        let name = checker.p.node(decl).ident_name().unwrap();
                        let ty = checker.get_type_of_symbol(*param);
                        format!(
                            "{}: {}",
                            checker.atoms.get(name.name),
                            ty.to_string(checker)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                let ret = if let Some(ret) = f.declared_sigs[0].ret {
                    let ty = checker.p.node(ret);
                    let ty = checker.get_ty_from_type_node(&ty.as_ty().unwrap());
                    ty.to_string(checker)
                } else {
                    checker.any_ty().to_string(checker)
                };
                format!("({params}) => {ret}")
            }
            ObjectTyKind::ObjectLit(_) => "Object".to_string(),
            ObjectTyKind::Tuple(TupleTy { tys, .. }) => {
                format!(
                    "[{}]",
                    tys.iter()
                        .map(|ty| ty.to_string(checker))
                        .collect::<Vec<_>>()
                        .join(",")
                )
            }
            ObjectTyKind::Interface(i) => checker
                .atoms
                .get(checker.binder.symbol(i.symbol).name.expect_atom())
                .to_string(),
            ObjectTyKind::Reference(refer) => {
                let ty = refer.target.to_string(checker);
                ty
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ClassTy {
    pub symbol: SymbolID,
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectLitTy<'cx> {
    pub members: &'cx FxHashMap<SymbolName, SymbolID>,
    pub declared_props: &'cx [SymbolID],
    pub symbol: SymbolID,
}

#[derive(Debug, Clone, Copy)]
pub struct FnTy<'cx> {
    pub symbol: SymbolID,
    pub declared_sigs: &'cx [&'cx Sig<'cx>],
}
