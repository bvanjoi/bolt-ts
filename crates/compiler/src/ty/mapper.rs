use std::fmt::Debug;

use crate::check::TyChecker;

use super::{Ty, Tys};

#[derive(Debug)]
pub enum TyMapper<'cx> {
    Simple(SimpleTyMapper<'cx>),
    Array(ArrayTyMapper<'cx>),
    Composite(CompositeTyMapper<'cx>),
    Merged(MergedTyMapper<'cx>),
}

impl<'cx> TyMapper<'cx> {
    pub fn make_unary(source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> TyMapper<'cx> {
        TyMapper::Simple(SimpleTyMapper { source, target })
    }
}

macro_rules! ty_mapper {
    ($kind: ident, $ty: ty, $as_kind: ident, $is_kind: ident) => {
        impl<'cx> TyMapper<'cx> {
            #[inline(always)]
            pub fn $as_kind(&self) -> Option<$ty> {
                match self {
                    TyMapper::$kind(ty) => Some(ty),
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

ty_mapper!(Simple, &SimpleTyMapper<'cx>, as_simple, is_simple);
ty_mapper!(Array, &ArrayTyMapper<'cx>, as_array, is_array);
// ty_mapper!(Fn, &FnTyMapper<'cx>, as_fn, is_fn);
ty_mapper!(
    Composite,
    &CompositeTyMapper<'cx>,
    as_composite,
    is_composite
);
ty_mapper!(Merged, &MergedTyMapper<'cx>, as_merged, is_merged);

#[derive(Clone, Copy, Debug)]
pub struct SimpleTyMapper<'cx> {
    pub source: &'cx Ty<'cx>,
    pub target: &'cx Ty<'cx>,
}

#[derive(Clone, Copy, Debug)]
pub struct ArrayTyMapper<'cx> {
    pub sources: Tys<'cx>,
    pub targets: Option<Tys<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct CompositeTyMapper<'cx> {
    pub mapper1: &'cx dyn TyMap<'cx>,
    pub mapper2: &'cx dyn TyMap<'cx>,
}

#[derive(Clone, Copy, Debug)]
pub struct MergedTyMapper<'cx> {
    pub mapper1: &'cx TyMapper<'cx>,
    pub mapper2: &'cx TyMapper<'cx>,
}

pub trait TyMap<'cx>: Debug {
    fn get_mapped_ty(&self, ty: &'cx Ty<'cx>, checker: &mut TyChecker<'cx>) -> &'cx Ty<'cx>;
}

impl<'cx> TyMap<'cx> for SimpleTyMapper<'cx> {
    fn get_mapped_ty(&self, ty: &'cx Ty<'cx>, _: &mut TyChecker<'cx>) -> &'cx Ty<'cx> {
        if ty == self.source {
            self.target
        } else {
            ty
        }
    }
}

impl<'cx> TyMap<'cx> for ArrayTyMapper<'cx> {
    fn get_mapped_ty(&self, ty: &'cx Ty<'cx>, checker: &mut TyChecker<'cx>) -> &'cx Ty<'cx> {
        for (idx, source) in self.sources.iter().enumerate() {
            assert!(source.kind.is_param());
            if source.eq(&ty) {
                if let Some(targets) = &self.targets {
                    return targets[idx];
                } else {
                    return checker.any_ty;
                }
            }
        }
        ty
    }
}

impl<'cx> TyMap<'cx> for CompositeTyMapper<'cx> {
    fn get_mapped_ty(&self, ty: &'cx Ty<'cx>, checker: &mut TyChecker<'cx>) -> &'cx Ty<'cx> {
        let t1 = self.mapper1.get_mapped_ty(ty, checker);
        if t1 != ty {
            checker.instantiate_ty(t1, Some(self.mapper2))
        } else {
            self.mapper2.get_mapped_ty(t1, checker)
        }
    }
}

impl<'cx> TyMap<'cx> for MergedTyMapper<'cx> {
    fn get_mapped_ty(&self, _: &'cx Ty<'cx>, _: &mut TyChecker<'cx>) -> &'cx Ty<'cx> {
        todo!()
    }
}

impl<'cx> TyMap<'cx> for TyMapper<'cx> {
    fn get_mapped_ty(&self, ty: &'cx Ty<'cx>, checker: &mut TyChecker<'cx>) -> &'cx Ty<'cx> {
        match self {
            TyMapper::Simple(n) => n.get_mapped_ty(ty, checker),
            TyMapper::Array(n) => n.get_mapped_ty(ty, checker),
            TyMapper::Composite(n) => n.get_mapped_ty(ty, checker),
            TyMapper::Merged(n) => n.get_mapped_ty(ty, checker),
        }
    }
}
