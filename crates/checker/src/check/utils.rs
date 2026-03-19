use bolt_ts_ty::TypeFlags;

use super::SymbolID;
use super::TyChecker;
use super::symbol_info::SymbolInfo;
use super::ty;

pub fn append_if_unique<'a, T: PartialEq>(array: &mut Vec<&'a T>, value: &'a T) {
    if array.iter().all(|item| !value.eq(item)) {
        array.push(value);
    }
}

pub fn insert_ty<'cx>(tys: &mut Vec<&'cx ty::Ty<'cx>>, ty: &'cx ty::Ty<'cx>) -> bool {
    debug_assert!(tys.is_sorted_by_key(|ty| ty.id.as_u32()));
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
        f: impl Fn(&mut Self, &'cx ty::Ty<'cx>) -> bool,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(u) = ty.kind.as_union() {
            let filtered = self.filter(u.tys, |this, t| f(this, t));
            if std::ptr::eq(filtered, u.tys) {
                return ty;
            }
            let mut new_origin = None;
            if let Some(origin) = u.origin
                && let Some(origin_u) = origin.kind.as_union()
            {
                let origin_filtered = self.filter(origin_u.tys, |this, t| {
                    t.flags.contains(TypeFlags::UNION) || f(this, t)
                });
                if origin_u.tys.len() - origin_filtered.len() == u.tys.len() - filtered.len() {
                    if origin_filtered.len() == 1 {
                        return origin_filtered[0];
                    }
                    new_origin =
                        Some(self.create_origin_union_or_intersection_ty(
                            TypeFlags::UNION,
                            origin_filtered,
                        ));
                }
            }
            self.get_union_ty_from_sorted_list::<false>(
                filtered.to_vec(),
                ty.get_object_flags()
                    & (ty::ObjectFlags::PRIMITIVE_UNION
                        .union(ty::ObjectFlags::CONTAINS_INTERSECTIONS)),
                None,
                None,
                new_origin,
            )
        } else if ty.flags.contains(ty::TypeFlags::NEVER) || f(self, ty) {
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

    pub(super) fn some_type(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        f: impl Fn(&mut Self, &'cx ty::Ty<'cx>) -> bool,
    ) -> bool {
        if let Some(union) = ty.kind.as_union() {
            union.tys.iter().any(|ty| f(self, ty))
        } else {
            f(self, ty)
        }
    }

    fn same_map<T, U>(
        &mut self,
        array: Option<&'cx [T]>,
        f: impl Fn(&mut Self, T, usize) -> U,
    ) -> SameMapperResult<'cx, U>
    where
        T: Into<U> + PartialEq<U> + Copy,
        U: PartialEq<T> + Copy,
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
                for (j, item) in array.iter().enumerate().skip(start) {
                    let item = f(self, *item, j);
                    result.push(item);
                }
                assert_eq!(result.len(), array.len());
                let result = self.alloc(result);
                return SameMapperResult::New(result);
            }
        }
        SameMapperResult::Old
    }

    pub(super) fn same_map_tys(
        &mut self,
        input: Option<ty::Tys<'cx>>,
        f: impl Fn(&mut Self, &'cx ty::Ty<'cx>, usize) -> &'cx ty::Ty<'cx>,
    ) -> Option<ty::Tys<'cx>> {
        match self.same_map(input, |this, ty, i| f(this, ty, i)) {
            SameMapperResult::Old => input,
            SameMapperResult::New(tys) => Some(tys),
        }
    }

    pub(super) fn same_map_sigs(
        &mut self,
        input: Option<ty::Sigs<'cx>>,
        f: impl Fn(&mut Self, &'cx ty::Sig<'cx>, usize) -> &'cx ty::Sig<'cx>,
    ) -> Option<ty::Sigs<'cx>> {
        match self.same_map(input, |this, t, i| f(this, t, i)) {
            SameMapperResult::Old => input,
            SameMapperResult::New(tys) => Some(tys),
        }
    }

    pub(super) fn same_map_index_infos(
        &mut self,
        input: Option<ty::IndexInfos<'cx>>,
        f: impl Fn(&mut Self, &'cx ty::IndexInfo<'cx>, usize) -> &'cx ty::IndexInfo<'cx>,
    ) -> Option<ty::IndexInfos<'cx>> {
        match self.same_map(input, |this, t, i| f(this, t, i)) {
            SameMapperResult::Old => input,
            SameMapperResult::New(tys) => Some(tys),
        }
    }

    pub(super) fn map_union_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        union: &'cx ty::UnionTy<'cx>,
        mapper: impl Fn(&mut Self, &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> + Copy,
        no_reduction: bool,
    ) -> Option<&'cx ty::Ty<'cx>> {
        debug_assert!(std::ptr::eq(ty.kind.expect_union(), union));
        let tys = if let Some(origin) = union.origin
            && let Some(origin_u) = origin.kind.as_union()
        {
            origin_u.tys
        } else {
            union.tys
        };
        let mut mapped_tys: Option<Vec<_>> = None;
        let mut changed = false;
        for t in tys {
            let mapped = if t.kind.is_union() {
                self.map_ty(t, mapper, no_reduction)
            } else {
                mapper(self, t)
            };
            if let Some(mapped) = mapped {
                changed |= !mapped.eq(ty);
                if let Some(mapped_tys) = &mut mapped_tys {
                    mapped_tys.push(mapped);
                } else {
                    let mut v = Vec::with_capacity(tys.len());
                    v.push(mapped);
                    mapped_tys = Some(v);
                };
            }
        }
        if changed {
            if let Some(mapped_tys) = mapped_tys {
                let reduction = if no_reduction {
                    ty::UnionReduction::None
                } else {
                    ty::UnionReduction::Lit
                };
                Some(self.get_union_ty::<false>(&mapped_tys, reduction, None, None, None))
            } else {
                None
            }
        } else {
            Some(ty)
        }
    }

    pub(super) fn map_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: impl Fn(&mut Self, &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> + Copy,
        no_reduction: bool,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if ty.flags.contains(ty::TypeFlags::NEVER) {
            Some(self.never_ty)
        } else if let Some(u) = ty.kind.as_union() {
            self.map_union_ty(ty, u, mapper, no_reduction)
        } else {
            mapper(self, ty)
        }
    }

    pub(super) fn each_ty_contained_in(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        tys: ty::Tys<'cx>,
    ) -> bool {
        if let Some(u) = source.kind.as_union() {
            u.tys.iter().all(|t| tys.contains(t))
        } else {
            tys.contains(&source)
        }
    }

    pub(super) fn map_ty_with_alias(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: impl Fn(&mut Self, &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> + Copy,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(u) = ty.kind.as_union()
            && alias_symbol.is_some()
        {
            let tys: Vec<_> = u
                .tys
                .iter()
                .map(|ty| self.map_ty(ty, mapper, false).unwrap())
                .collect::<Vec<_>>();
            return Some(self.get_union_ty::<false>(
                &tys,
                ty::UnionReduction::Lit,
                alias_symbol,
                alias_ty_arguments,
                None,
            ));
        }

        self.map_ty(ty, mapper, false)
    }

    pub(super) fn for_each_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mut f: impl FnMut(&mut Self, &'cx ty::Ty<'cx>),
    ) {
        if let Some(u) = ty.kind.as_union() {
            for ty in u.tys {
                f(self, ty)
            }
        } else {
            f(self, ty)
        }
    }

    pub(super) fn has_ty_param_default(&self, ty_param: &'cx ty::ParamTy<'cx>) -> bool {
        self.ty_param_nodes(ty_param).iter().any(|decl| {
            let ty_param_node = self.p.node(*decl).expect_ty_param();
            ty_param_node.default.is_some()
        })
    }

    pub(super) fn array_is_equal<T: PartialEq>(&self, a1: Option<&[T]>, a2: Option<&[T]>) -> bool {
        let (Some(a1), Some(a2)) = (a1, a2) else {
            return false;
        };
        if a1.len() != a2.len() {
            return false;
        }
        for i in 0..a1.len() {
            // TODO: custom compare fn
            if a1[i] != a2[i] {
                return false;
            }
        }
        true
    }

    pub(super) fn filter<T>(
        &mut self,
        array: &'cx [&'cx T],
        f: impl Fn(&mut Self, &'cx T) -> bool,
    ) -> &'cx [&'cx T] {
        let len = array.len();
        let mut i = 0;
        while i < len && f(self, array[i]) {
            i += 1;
        }
        if i < len {
            let mut result = array[0..i].to_vec();
            i += 1;
            while i < len {
                let item = array[i];
                if f(self, item) {
                    result.push(item);
                }
                i += 1;
            }
            self.alloc(result)
        } else {
            array
        }
    }

    pub(super) fn extract_unit_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if let Some(i) = ty.kind.as_intersection() {
            if let Some(ty) = i.tys.iter().find(|t| t.is_unit()) {
                ty
            } else {
                ty
            }
        } else {
            ty
        }
    }
}

enum SameMapperResult<'cx, T> {
    Old,
    New(&'cx [T]),
}

pub fn uncapitalize(s: &str) -> String {
    let mut chars = s.chars();

    match chars.next() {
        Some(first) => first.to_lowercase().chain(chars).collect(),
        None => String::new(),
    }
}

pub fn capitalize(s: &str) -> String {
    let mut chars = s.chars();

    match chars.next() {
        Some(first) => first.to_uppercase().chain(chars).collect(),
        None => String::new(),
    }
}
