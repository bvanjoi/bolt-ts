use bolt_ts_span::ModuleID;
use rustc_hash::FxHashMap;

use crate::atoms::AtomMap;
use crate::bind::{Binder, SymbolID, SymbolName};

use super::Ty;

#[derive(Debug, Clone, Copy)]
pub struct ObjectTy<'cx> {
    pub kind: ObjectTyKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ObjectTyKind<'cx> {
    Class(&'cx ClassTy),
    Fn(&'cx FnTy<'cx>),
    ObjectLit(&'cx ObjectLitTy<'cx>),
    Array(&'cx ArrayTy<'cx>),
    Tuple(&'cx TupleTy<'cx>),
    Interface(&'cx InterfaceTy<'cx>),
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
    &'cx ArrayTy<'cx>,
    as_array,
    as_object_array,
    expect_object_array,
    is_object_array
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
as_object_ty_kind!(Array, &'cx ArrayTy<'cx>, as_array, is_array);
as_object_ty_kind!(Tuple, &'cx TupleTy<'cx>, as_tuple, is_tuple);
as_object_ty_kind!(Interface, &'cx InterfaceTy<'cx>, as_interface, is_interface);

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
    pub refer: &'cx TyReference<'cx>
}

#[derive(Debug, Clone, Copy)]
pub struct TyReference<'cx> {
    pub ty_args: super::Tys<'cx>
}

#[derive(Debug, Clone, Copy)]
pub struct ArrayTy<'cx> {
    pub ty: &'cx Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct IndexInfo<'cx> {
    pub key_ty: &'cx Ty<'cx>,
    pub val_ty: &'cx Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct InterfaceTy<'cx> {
    pub module: ModuleID,
    pub symbol: SymbolID,
    pub members: &'cx FxHashMap<SymbolName, SymbolID>,
    pub declared_props: &'cx [SymbolID],
    pub base_tys: &'cx [&'cx Ty<'cx>],
    pub index_infos: &'cx [&'cx IndexInfo<'cx>],
    pub base_ctor_ty: Option<&'cx Ty<'cx>>,
}

impl<'cx> ObjectTyKind<'cx> {
    pub(super) fn to_string(&self, binder: &Binder, atoms: &AtomMap<'cx>) -> String {
        match self {
            ObjectTyKind::Class(_) => "class".to_string(),
            ObjectTyKind::Fn(_) => "function".to_string(),
            ObjectTyKind::ObjectLit(_) => "Object".to_string(),
            ObjectTyKind::Array(ArrayTy { ty }) => {
                format!("{}[]", ty.kind.to_string(binder, atoms))
            }
            ObjectTyKind::Tuple(TupleTy { tys, .. }) => {
                format!(
                    "[{}]",
                    tys.iter()
                        .map(|ty| ty.kind.to_string(binder, atoms))
                        .collect::<Vec<_>>()
                        .join(",")
                )
            }
            ObjectTyKind::Interface(i) => atoms
                .get(
                    binder
                        .get(i.module)
                        .symbols
                        .get(i.symbol)
                        .name
                        .expect_atom(),
                )
                .to_string(),
        }
    }

    pub fn is_reference(&self) -> bool {
        self.is_interface()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ClassTy {
    pub module: ModuleID,
    pub symbol: SymbolID,
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectLitTy<'cx> {
    pub members: &'cx FxHashMap<SymbolName, SymbolID>,
    pub declared_props: &'cx [SymbolID],
    pub module: ModuleID,
    pub symbol: SymbolID,
}

#[derive(Debug, Clone, Copy)]
pub struct FnTy<'cx> {
    pub params: &'cx [&'cx Ty<'cx>],
    pub ret: &'cx Ty<'cx>,
    pub module: ModuleID,
    pub symbol: SymbolID,
}
