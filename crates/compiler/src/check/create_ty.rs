use crate::ty::{self, TyID, UnionReduction};

use super::{relation::RelationKind, TyChecker};

impl<'cx> TyChecker<'cx> {
    pub(super) fn new_ty(&mut self, kind: ty::TyKind<'cx>) -> &'cx ty::Ty<'cx> {
        let id = TyID::new(self.tys.len() as u32);
        let ty = self.alloc(ty::Ty::new(id, kind));
        self.tys.push(ty);
        ty
    }

    pub(super) fn create_ty_var(&mut self) -> &'cx ty::Ty<'cx> {
        let id = self.next_ty_var_id;
        self.next_ty_var_id = self.next_ty_var_id.next();
        self.new_ty(ty::TyKind::Var(id))
    }

    fn create_object_ty(&mut self, ty: ty::ObjectTyKind<'cx>) -> &'cx ty::Ty<'cx> {
        let kind = ty::TyKind::Object(self.alloc(ty::ObjectTy { kind: ty }));
        self.new_ty(kind)
    }

    pub(super) fn create_object_lit_ty(&mut self, ty: ty::ObjectLitTy<'cx>) -> &'cx ty::Ty<'cx> {
        self.create_object_ty(ty::ObjectTyKind::ObjectLit(self.alloc(ty)))
    }

    pub(super) fn create_tuple_ty(&mut self, ty: ty::TupleTy<'cx>) -> &'cx ty::Ty<'cx> {
        assert_eq!(ty.tys.len(), ty.element_flags.len());
        debug_assert!(ty.element_flags.iter().all(|flag| {
            let flag = flag.bits();
            // is variant
            (flag & (flag - 1)) == 0
        }));
        self.create_object_ty(ty::ObjectTyKind::Tuple(self.alloc(ty)))
    }

    pub(super) fn create_reference_ty(&mut self, ty: ty::ReferenceTy<'cx>) -> &'cx ty::Ty<'cx> {
        let ty = self.create_object_ty(ty::ObjectTyKind::Reference(self.alloc(ty)));
        self.resolve_structured_type_members(ty);
        ty
    }

    pub(super) fn crate_interface_ty(&mut self, ty: ty::InterfaceTy<'cx>) -> &'cx ty::Ty<'cx> {
        self.create_object_ty(ty::ObjectTyKind::Interface(self.alloc(ty)))
    }

    pub(super) fn create_anonymous_ty(&mut self, ty: ty::AnonymousTy<'cx>) -> &'cx ty::Ty<'cx> {
        assert!(ty.target.is_none() || ty.target.as_ref().unwrap().kind.is_object_anonymous());
        let ty = self.create_object_ty(ty::ObjectTyKind::Anonymous(self.alloc(ty)));
        self.resolve_structured_type_members(ty);
        ty
    }

    pub(super) fn create_param_ty(&mut self, ty: ty::ParamTy) -> &'cx ty::Ty<'cx> {
        let parm_ty = self.alloc(ty);
        self.new_ty(ty::TyKind::Param(parm_ty))
    }

    // fn add_types_to_union(set: &mut Vec<&'cx ty::Ty<'cx>>, includes: TypeFlags, tys: &[&'cx ty::Ty<'cx>]) {
    //     let mut last_ty = None;
    //     for ty in tys {
    //         if let Some(last_ty) = last_ty {
    //             if last_ty != ty {
    //             }
    //         } else {
    //             last_ty = Some(ty)
    //         }
    //     }
    // }

    pub(super) fn create_union_type(
        &mut self,
        mut tys: Vec<&'cx ty::Ty<'cx>>,
        reduction: UnionReduction,
    ) -> &'cx ty::Ty<'cx> {
        // if tys.is_empty() {
        //     // TODO: never type
        // }

        tys.dedup();
        if reduction == UnionReduction::Subtype {
            tys = self.remove_subtypes(tys)
        }
        if tys.len() == 1 {
            tys[0]
        } else {
            let union = self.alloc(ty::UnionTy {
                tys: self.alloc(tys),
            });
            self.new_ty(ty::TyKind::Union(union))
        }
    }

    fn remove_subtypes(&mut self, mut tys: Vec<&'cx ty::Ty<'cx>>) -> Vec<&'cx ty::Ty<'cx>> {
        let len = tys.len();
        if len < 2 {
            return tys;
        }
        let mut i = len;
        while i > 0 {
            i -= 1;
            let source = tys[i];
            if source.kind.is_structured_or_instantiable() {
                for target in tys.iter() {
                    if !source.eq(target)
                        && self.is_type_related_to(source, target, RelationKind::StrictSubtype)
                    {
                        tys.remove(i);
                        break;
                    }
                }
            }
        }
        tys
    }
}
