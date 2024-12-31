use crate::{
    bind::SymbolFlags,
    ty::{self, TyMapper},
};

use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    pub(super) fn instantiate_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: Option<&'cx ty::TyMapper<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(mapper) = mapper {
            self.instantiate_ty_with_alias(ty, mapper)
        } else {
            ty
        }
    }

    pub fn instantiate_list<T: PartialEq + Copy>(
        &mut self,
        list: &'cx [T],
        mapper: &'cx ty::TyMapper<'cx>,
        f: impl Fn(&mut Self, T, &'cx ty::TyMapper<'cx>) -> T,
    ) -> &'cx [T] {
        let len = list.len();
        for i in 0..len {
            let item = list[i];
            let mapped = f(self, item, mapper);
            if item != mapped {
                let mut result = Vec::with_capacity(list.len());
                result.extend(list[0..i].iter());
                for j in i..len {
                    result.push(f(self, list[j], mapper));
                }
                assert_eq!(result.len(), list.len());
                let result = self.alloc(result);
                return result;
            }
        }
        list
    }

    fn instantiate_tys(
        &mut self,
        tys: ty::Tys<'cx>,
        mapper: &'cx ty::TyMapper<'cx>,
    ) -> ty::Tys<'cx> {
        self.instantiate_list(tys, mapper, |this, item, mapper| {
            this.instantiate_ty(item, Some(mapper))
        })
    }

    pub(super) fn could_contain_ty_var(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        if ty.kind.is_instantiable_non_primitive() {
            true
        } else if let Some(object) = ty.kind.as_object() {
            // TODO: !isNonGenericTopLevelType(type)
            if object.kind.is_reference() {
                true
            } else {
                object.kind.is_anonymous()
            }
        } else if ty.kind.is_union_or_intersection() {
            // TODO: condition
            false
        } else {
            false
        }
    }

    pub(super) fn instantiate_ty_with_alias(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx ty::TyMapper<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if !self.could_contain_ty_var(ty) {
            return ty;
        }
        self.instantiate(ty, mapper)
    }

    fn instantiate(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx ty::TyMapper<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        use ty::TyKind::*;
        match ty.kind {
            Param(_) => self.get_mapped_ty(mapper, ty),
            IndexedAccess(indexed) => {
                let object_ty = self.instantiate_ty_with_alias(indexed.object_ty, mapper);
                let index_ty = self.instantiate_ty_with_alias(indexed.index_ty, mapper);
                self.get_indexed_access_ty(object_ty, index_ty, Some(indexed.access_flags))
            }
            Cond(_) => self.get_cond_ty_instantiation(ty, mapper),
            Object(_) => self.get_object_ty_instantiation(ty, mapper),
            _ => {
                todo!("{:?}", ty.kind);
            }
        }
    }

    fn instantiate_anonymous_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx ty::TyMapper<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let target = ty.kind.expect_object_anonymous();
        self.create_anonymous_ty(ty::AnonymousTy {
            symbol: target.symbol,
            target: Some(ty),
            mapper: Some(mapper),
        })
    }

    fn get_object_ty_instantiation(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx ty::TyMapper<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        use ty::ObjectTyKind::*;
        if let Some(refer) = ty.kind.as_object_reference() {
            let ty_args = self.instantiate_tys(refer.resolved_ty_args, mapper);
            let object_ty = refer.target.kind.expect_object();
            match &object_ty.kind {
                Tuple(tuple) => {
                    if !std::ptr::eq(ty_args, refer.resolved_ty_args) {
                        self.create_normalized_tuple_ty(
                            ty_args,
                            tuple.element_flags,
                            tuple.combined_flags,
                        )
                    } else {
                        refer.target
                    }
                }
                _ => self.create_reference_ty(ty::ReferenceTy {
                    target: refer.target,
                    resolved_ty_args: ty_args,
                }),
            }
        } else if let Some(a) = ty.kind.as_object_anonymous() {
            let s = self.binder.symbol(a.symbol);
            let flags = s.flags;
            let decl = if flags.intersects(SymbolFlags::FUNCTION) {
                s.expect_fn().decls[0]
            } else if flags.intersects(SymbolFlags::TYPE_LITERAL) {
                s.expect_ty_lit().decl
            } else {
                return ty;
            };
            // TODO: use `node_links`
            let outer_params = self.get_outer_ty_params(decl);
            let ty_params = outer_params.unwrap_or_default();
            if !ty_params.is_empty() {
                let combined_mapper = self.combine_ty_mappers(a.mapper, *mapper);
                let ty_args = ty_params
                    .iter()
                    .map(|t| self.get_mapped_ty(&combined_mapper, t))
                    .collect::<Vec<_>>();
                let new_mapper = TyMapper::create(self.alloc(ty_params), self.alloc(ty_args));
                self.instantiate_anonymous_ty(ty, self.alloc(new_mapper))
            } else {
                ty
            }
        } else {
            ty
        }
    }

    fn get_cond_ty_instantiation(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx ty::TyMapper<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let Some(cond_ty) = ty.kind.as_cond_ty() else {
            unreachable!()
        };
        if let Some(outer_ty_params) = cond_ty.root.outer_ty_params {
            let ty_args = outer_ty_params
                .iter()
                .map(|t| self.get_mapped_ty(mapper, t))
                .collect::<Vec<_>>();
            let mapper = self.alloc(TyMapper::create(outer_ty_params, self.alloc(ty_args)));
            self.get_cond_ty(cond_ty.root, Some(mapper))
        } else {
            ty
        }
    }
}
