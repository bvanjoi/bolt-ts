use super::ty;
use super::TyChecker;
use crate::bind::SymbolID;

impl<'cx> TyChecker<'cx> {
    pub(super) fn base_types(&self, ty: &'cx ty::Ty<'cx>) -> ty::Tys<'cx> {
        if ty.kind.is_object_interface() || ty.kind.is_object_reference() {
            self.expect_ty_links(ty.id)
                .expect_structured_members()
                .base_tys
        } else {
            &[]
        }
    }

    pub(super) fn index_infos_of_ty(&self, ty: &'cx ty::Ty<'cx>) -> ty::IndexInfos<'cx> {
        if let Some(i) = ty.kind.as_object_interface() {
            self.expect_ty_links(ty.id)
                .expect_structured_members()
                .index_infos
        } else if ty.kind.is_object_reference() {
            self.expect_ty_links(ty.id)
                .expect_structured_members()
                .index_infos
        } else if ty.kind.is_object_anonymous() {
            self.expect_ty_links(ty.id)
                .expect_structured_members()
                .index_infos
        } else {
            &[]
        }
    }

    pub(super) fn properties_of_ty(&self, ty: &'cx ty::Ty<'cx>) -> &'cx [SymbolID] {
        self.properties_of_object_type(ty)
    }

    pub(super) fn properties_of_object_type(&self, ty: &'cx ty::Ty<'cx>) -> &'cx [SymbolID] {
        ty.kind
            .is_object()
            .then(|| {
                self.expect_ty_links(ty.id)
                    .expect_structured_members()
                    .props
            })
            .unwrap_or_default()
    }

    pub(super) fn signatures_of_structured_type(
        &self,
        ty: &'cx ty::Ty<'cx>,
        kind: ty::SigKind,
    ) -> ty::Sigs<'cx> {
        ty.kind
            .is_structured()
            .then(|| {
                // TODO: remove this
                let Some(ty_links) = self.ty_links.get(&ty.id) else {
                    return Default::default();
                };
                // TODO: remove this
                let Some(resolved) = ty_links.get_structured_members() else {
                    return Default::default();
                };
                if matches!(kind, ty::SigKind::Call) {
                    resolved.call_sigs
                } else {
                    resolved.ctor_sigs
                }
            })
            .unwrap_or_default()
    }

    pub(super) fn signatures_of_type(
        &self,
        ty: &'cx ty::Ty<'cx>,
        kind: ty::SigKind,
    ) -> ty::Sigs<'cx> {
        let sigs = self.signatures_of_structured_type(ty, kind);
        if matches!(kind, ty::SigKind::Call)
            && sigs.is_empty()
            && matches!(ty.kind, ty::TyKind::Union(_))
        {
            // TODO: handle this branch
            sigs
        } else {
            sigs
        }
    }

    pub(super) fn this_ty(&self, ty: &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(i) = ty.kind.as_object_interface() {
            i.this_ty
        } else if let Some(refer) = ty.kind.as_object_reference() {
            self.this_ty(refer.target)
        } else {
            None
        }
    }
}
