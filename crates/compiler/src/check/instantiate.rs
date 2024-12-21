use crate::ty::{self, TyMapper};

use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    pub(super) fn instantiate_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: Option<&ty::TyMapper<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(mapper) = mapper {
            self.instantiate_ty_with_alias(ty, mapper)
        } else {
            ty
        }
    }

    fn instantiate_tys(&mut self, tys: ty::Tys<'cx>, mapper: &ty::TyMapper<'cx>) -> ty::Tys<'cx> {
        let len = tys.len();
        for i in 0..len {
            let item = tys[i];
            let mapped = self.instantiate_ty(item, Some(mapper));
            if item != mapped {
                let mut result = tys[0..i].to_vec();
                for j in i..len {
                    result.push(self.instantiate(tys[j], mapper));
                }
                assert!(result.len() == tys.len());
                let result = self.alloc(result);
                return result;
            }
        }
        tys
    }

    fn could_contain_ty_var(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        if ty.kind.is_lit() {
            false
        } else {
            true
        }
    }

    pub(super) fn instantiate_ty_with_alias(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &ty::TyMapper<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if !self.could_contain_ty_var(ty) {
            return ty;
        }
        self.instantiate(ty, mapper)
    }

    fn instantiate(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &ty::TyMapper<'cx>,
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

    fn get_object_ty_instantiation(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &ty::TyMapper<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let Some(refer) = ty.kind.as_object_reference() else {
            return ty;
        };
        use ty::ObjectTyKind::*;
        let ty_args = self.instantiate_tys(refer.ty_args, mapper);
        let object_ty = refer.target.kind.expect_object();
        match &object_ty.kind {
            Tuple(tuple) => {
                if !std::ptr::eq(ty_args, refer.ty_args) {
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
                ty_args,
            }),
        }
    }

    fn get_cond_ty_instantiation(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &ty::TyMapper<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let Some(cond_ty) = ty.kind.as_cond_ty() else {
            unreachable!()
        };
        if let Some(outer_ty_params) = cond_ty.root.outer_ty_params {
            let ty_args = outer_ty_params
                .iter()
                .map(|t| self.get_mapped_ty(mapper, t))
                .collect::<Vec<_>>();
            let mapper = TyMapper::create(outer_ty_params, self.alloc(ty_args));
            self.get_cond_ty(cond_ty.root, Some(mapper))
        } else {
            ty
        }
    }
}
