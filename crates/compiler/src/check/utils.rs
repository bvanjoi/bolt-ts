use crate::ty;

use super::TyChecker;

pub fn append_if_unique<'a, T>(array: &mut Vec<&'a T>, value: &'a T) {
    if array.iter().all(|item| !std::ptr::eq(item, &value)) {
        array.push(value);
    }
}

pub fn insert_ty<'cx>(tys: &mut Vec<&'cx ty::Ty<'cx>>, ty: &'cx ty::Ty<'cx>) -> bool {
    let id = ty.id.as_u32();
    if let Err(pos) = tys.binary_search_by(|probe| probe.id.as_u32().cmp(&id)) {
        tys.insert(pos, ty);
        true
    } else {
        false
    }
}

pub fn contains_ty<'cx>(tys: &[&'cx ty::Ty<'cx>], ty: &'cx ty::Ty<'cx>) -> bool {
    debug_assert!(tys.is_sorted_by_key(|ty| ty.id.as_u32()));
    let id = ty.id.as_u32();
    tys.binary_search_by(|probe| probe.id.as_u32().cmp(&id))
        .is_ok()
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn filter_type(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        f: impl Fn(&'cx ty::Ty<'cx>) -> bool,
    ) -> &'cx ty::Ty<'cx> {
        if ty.kind.as_union().is_some() {
            // TODO:
            ty
        } else if ty.flags.intersects(ty::TypeFlags::NEVER) {
            self.never_ty
        } else if f(ty) {
            ty
        } else {
            self.never_ty
        }
    }

    pub(super) fn every_type(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        f: impl Fn(&mut Self, &'cx ty::Ty<'cx>) -> bool,
    ) -> bool {
        if let Some(union) = ty.kind.as_union() {
            union.tys.iter().all(|ty| f(self, ty))
        } else {
            f(self, ty)
        }
    }

    fn same_map<T: PartialEq<U> + Copy, U: PartialEq<T> + Copy>(
        &mut self,
        array: Option<&'cx [T]>,
        f: impl Fn(&mut Self, T, usize) -> U,
    ) -> SameMapperResult<'cx, U>
    where
        T: Into<U>,
    {
        let Some(array) = array else {
            return SameMapperResult::Old;
        };
        for i in 0..array.len() {
            let item = array[i];
            let mapped = f(self, item, i);
            if item != mapped {
                let mut result = Vec::with_capacity(array.len());
                result.extend(array[0..i].iter().map(|item| (*item).into()));
                result.push(mapped);
                let start = i + 1;
                for j in start..array.len() {
                    let item = f(self, array[j], j);
                    result.push(item);
                }
                assert_eq!(result.len(), array.len());
                let result = self.alloc(result);
                return SameMapperResult::New(result);
            }
        }
        SameMapperResult::Old
    }

    pub fn same_map_tys(
        &mut self,
        input: Option<ty::Tys<'cx>>,
        f: impl Fn(&mut Self, &'cx ty::Ty<'cx>, usize) -> &'cx ty::Ty<'cx>,
    ) -> Option<ty::Tys<'cx>> {
        match self.same_map(input, |this, ty, i| f(this, ty, i)) {
            SameMapperResult::Old => input,
            SameMapperResult::New(tys) => Some(tys),
        }
    }
}

enum SameMapperResult<'cx, T> {
    Old,
    New(&'cx [T]),
}
