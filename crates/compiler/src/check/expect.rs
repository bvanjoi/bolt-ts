use super::ty;
use super::TyChecker;
use crate::bind::SymbolID;

impl<'cx> TyChecker<'cx> {
    pub(super) fn base_types(&self, ty: &'cx ty::Ty<'cx>) -> ty::Tys<'cx> {
        if ty.kind.as_object_interface().is_some() {
            self.ty_structured_members[&ty.id].base_tys
        } else if ty.kind.is_object_reference() {
            self.ty_structured_members[&ty.id].base_tys
        } else {
            &[]
        }
    }

    pub(super) fn index_infos_of_ty(&self, ty: &'cx ty::Ty<'cx>) -> ty::IndexInfos<'cx> {
        if let Some(i) = ty.kind.as_object_interface() {
            self.ty_structured_members[&ty.id].index_infos
        } else if ty.kind.is_object_reference() {
            self.ty_structured_members[&ty.id].index_infos
        } else if ty.kind.is_object_anonymous() {
            self.ty_structured_members[&ty.id].index_infos
        } else {
            &[]
        }
    }

    pub(super) fn properties_of_object_type(&self, ty: &'cx ty::Ty<'cx>) -> &'cx [SymbolID] {
        ty.kind
            .is_object()
            .then(|| self.ty_structured_members[&ty.id].props)
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
                if !self.ty_structured_members.contains_key(&ty.id) {
                    return Default::default();
                }
                let resolved = self.ty_structured_members[&ty.id];
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
            // TODO: handle
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
