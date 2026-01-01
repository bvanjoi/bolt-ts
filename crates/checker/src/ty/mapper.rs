use super::Ty;
use crate::check::TyChecker;

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

#[derive(Clone, Copy, Debug)]
pub struct SimpleTyMapper<'cx> {
    pub source: &'cx Ty<'cx>,
    pub target: &'cx Ty<'cx>,
}

#[derive(Clone, Copy, Debug)]
pub struct ArrayTyMapper<'cx> {
    pub mapper: &'cx [(&'cx Ty<'cx>, &'cx Ty<'cx>)],
}

#[derive(Debug, Clone)]
pub struct CompositeTyMapper<'cx> {
    pub mapper1: &'cx dyn TyMap<'cx>,
    pub mapper2: &'cx dyn TyMap<'cx>,
}

#[derive(Clone, Debug)]
pub struct MergedTyMapper<'cx> {
    pub mapper1: &'cx dyn TyMap<'cx>,
    pub mapper2: &'cx dyn TyMap<'cx>,
}

pub trait TyMap<'cx>: std::fmt::Debug {
    fn get_mapped_ty(&self, ty: &'cx Ty<'cx>, checker: &mut TyChecker<'cx>) -> &'cx Ty<'cx>;
}

impl<'cx> TyMap<'cx> for SimpleTyMapper<'cx> {
    fn get_mapped_ty(&self, ty: &'cx Ty<'cx>, _: &mut TyChecker<'cx>) -> &'cx Ty<'cx> {
        if ty == self.source { self.target } else { ty }
    }
}

impl<'cx> TyMap<'cx> for ArrayTyMapper<'cx> {
    fn get_mapped_ty(&self, ty: &'cx Ty<'cx>, _: &mut TyChecker<'cx>) -> &'cx Ty<'cx> {
        debug_assert!(
            self.mapper
                .is_sorted_by_key(|(source, _)| source.id.as_u32()),
            "mapper must be sorted by source type, but got {:#?}",
            self.mapper
        );
        self.mapper
            .binary_search_by_key(&ty.id.as_u32(), |(source, _)| source.id.as_u32())
            .map_or(ty, |idx| self.mapper[idx].1)
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
    fn get_mapped_ty(&self, ty: &'cx Ty<'cx>, checker: &mut TyChecker<'cx>) -> &'cx Ty<'cx> {
        let t1 = self.mapper1.get_mapped_ty(ty, checker);
        self.mapper2.get_mapped_ty(t1, checker)
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
