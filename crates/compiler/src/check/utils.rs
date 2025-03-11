use crate::{
    bind::SymbolID,
    ty::{self, ObjectFlags},
};

use super::TyChecker;

pub fn append_if_unique<'a, T>(array: &mut Vec<&'a T>, value: &'a T) {
    if array.iter().all(|item| !std::ptr::eq(item, &value)) {
        array.push(value);
    }
}

pub fn insert_ty<'cx>(tys: &mut Vec<&'cx ty::Ty<'cx>>, ty: &'cx ty::Ty<'cx>) -> bool {
    let id = ty.id.as_u32();
    debug_assert!(tys.is_sorted_by_key(|ty| ty.id.as_u32()));
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
            let filtered = u
                .tys
                .iter()
                .filter(|t| f(self, t))
                .copied()
                .collect::<Vec<_>>();
            // TODO: filter should reduce alloc
            // TODO: handle origin
            self.get_union_ty_from_sorted_list(
                filtered,
                ty.get_object_flags()
                    & (ObjectFlags::PRIMITIVE_UNION | ObjectFlags::CONTAINS_INTERSECTIONS),
            )
        } else if ty.flags.intersects(ty::TypeFlags::NEVER) {
            self.never_ty
        } else if f(self, ty) {
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

    pub(super) fn map_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: impl Fn(&mut Self, &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> + Copy,
        no_reduction: bool,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if ty.flags.intersects(ty::TypeFlags::NEVER) {
            Some(self.never_ty)
        } else if let Some(u) = ty.kind.as_union() {
            // TODO: union.origin
            let tys = u.tys;
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
                    Some(self.get_union_ty(&mapped_tys, reduction))
                } else {
                    None
                }
            } else {
                Some(ty)
            }
        } else {
            mapper(self, ty)
        }
    }

    pub(super) fn map_ty_with_alias(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: impl Fn(&mut Self, &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> + Copy,
        alias_symbol: Option<SymbolID>,
        alias_symbol_ty_args: Option<ty::Tys<'cx>>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(u) = ty.kind.as_union() {
            if let Some(alias_symbol) = alias_symbol {
                let tys: Vec<_> = u
                    .tys
                    .iter()
                    .map(|ty| self.map_ty(ty, mapper, false).unwrap())
                    .collect::<Vec<_>>();
                return Some(self.get_union_ty(&tys, ty::UnionReduction::Lit));
            }
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
        let param = self.ty_param_node(ty_param);
        param.default.is_some()
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
}

enum SameMapperResult<'cx, T> {
    Old,
    New(&'cx [T]),
}

pub fn is_external_module_relative(module_name: &str) -> bool {
    path_is_relative(module_name) || is_rooted_disk_path(module_name)
}

pub fn path_is_relative(path: &str) -> bool {
    let re = regex::Regex::new(r"^\.\.?(?:$|[\\/])").unwrap();
    re.is_match(path)
}

pub fn is_rooted_disk_path(path: &str) -> bool {
    get_encoded_root_length(path) > 0
}

fn is_volume_character(ch: u8) -> bool {
    ch.is_ascii_uppercase() || ch.is_ascii_lowercase()
}

fn get_file_url_volume_separator_end(path: &str, start: usize) -> isize {
    let bytes = path.as_bytes();
    if bytes.get(start).is_some_and(|c| *c == b':') {
        return (start + 1) as isize;
    }
    if bytes.get(start).is_some_and(|c| *c == b'%')
        && bytes.get(start + 1).is_some_and(|c| *c == 0x33)
        && bytes
            .get(start + 2)
            .is_some_and(|c| *c == b'a' || *c == b'A')
    {
        return (start + 3) as isize;
    }
    -1
}

fn get_encoded_root_length(path: &str) -> isize {
    if path.is_empty() {
        return 0;
    }
    let bytes = path.as_bytes();
    let ch0 = bytes[0];

    const SLASH: u8 = b'/';
    const BACKSLASH: u8 = b'\\';

    // POSIX or UNC
    if ch0 == SLASH || ch0 == BACKSLASH {
        if bytes.get(1).is_none_or(|b| *b != ch0) {
            return 1;
        }
        let target_sep = if ch0 == SLASH { SLASH } else { BACKSLASH };
        if let Some(rel_index) = path[2..].find(target_sep as char) {
            let p1 = 2 + rel_index;
            return (p1 + 1) as isize;
        } else {
            return path.len() as isize;
        }
    }

    // DOS
    if is_volume_character(ch0) && bytes.get(1).is_some_and(|b| *b == b':') {
        if bytes
            .get(2)
            .copied()
            .is_some_and(|c| c == SLASH || c == BACKSLASH)
        {
            return 3;
        }
        if path.len() == 2 {
            return 2;
        }
    }

    // URL
    let url_scheme_separator = "://";
    let Some(scheme_end) = path.find(url_scheme_separator) else {
        // relative
        return 0;
    };
    let authority_start = scheme_end + url_scheme_separator.len();
    let Some(rel_authority_end) = path[authority_start..].find('/') else {
        return -(path.len() as isize + 1);
    };
    let authority_end = authority_start + rel_authority_end;
    let scheme = &path[..scheme_end];
    let authority = &path[authority_start..authority_end];
    if scheme == "file" && (authority.is_empty() || authority == "localhost") {
        if let Some(next_byte) = bytes.get(authority_end + 1).copied() {
            if is_volume_character(next_byte) {
                let volume_separator_end =
                    get_file_url_volume_separator_end(path, authority_end + 2);
                if volume_separator_end != -1 {
                    if let Some(b) = path.as_bytes().get(volume_separator_end as usize) {
                        if *b == SLASH {
                            return -((volume_separator_end + 1) + 1);
                        }
                    }
                    if volume_separator_end as usize == path.len() {
                        return -((volume_separator_end) + 1);
                    }
                }
            }
        }
    }
    -((authority_end + 1) as isize + 1)
}
