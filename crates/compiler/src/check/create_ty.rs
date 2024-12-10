use crate::ty::{self};

use super::{relation::RelationKind, TyChecker};

impl<'cx> TyChecker<'cx> {
    pub(super) fn new_ty(&mut self, kind: ty::TyKind<'cx>) -> &'cx ty::Ty<'cx> {
        let id = self.next_ty_id;
        self.next_ty_id = self.next_ty_id.next();

        let ty = self.alloc(ty::Ty::new(id, kind));
        assert_eq!(id.as_usize(), self.tys.len());
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
        assert!(ty.element_flags.iter().all(|flag| {
            let flag = flag.bits();
            // is variant
            (flag & (flag - 1)) == 0
        }));
        self.create_object_ty(ty::ObjectTyKind::Tuple(self.alloc(ty)))
    }

    pub(super) fn create_array_ty(&mut self, ty: ty::ArrayTy<'cx>) -> &'cx ty::Ty<'cx> {
        self.create_object_ty(ty::ObjectTyKind::Array(self.alloc(ty)))
    }

    pub(super) fn create_class_ty(&mut self, ty: ty::ClassTy) -> &'cx ty::Ty<'cx> {
        self.create_object_ty(ty::ObjectTyKind::Class(self.alloc(ty)))
    }

    pub(super) fn crate_interface_ty(&mut self, ty: ty::InterfaceTy<'cx>) -> &'cx ty::Ty<'cx> {
        self.create_object_ty(ty::ObjectTyKind::Interface(self.alloc(ty)))
    }

    pub(super) fn create_fn_ty(&mut self, ty: ty::FnTy<'cx>) -> &'cx ty::Ty<'cx> {
        self.create_object_ty(ty::ObjectTyKind::Fn(self.alloc(ty)))
    }

    pub(super) fn create_union_type(&mut self, tys: Vec<&'cx ty::Ty<'cx>>) -> &'cx ty::Ty<'cx> {
        let tys = self.remove_subtypes(tys);
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
                    if !std::ptr::eq(source, *target) {
                        if self.is_type_related_to(source, target, RelationKind::StrictSubtype) {
                            tys.remove(i);
                            break;
                        }
                    }
                }
            }
        }
        tys
    }
}
