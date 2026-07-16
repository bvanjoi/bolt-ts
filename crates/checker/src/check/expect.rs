use super::TyChecker;
use super::ty;
use super::ty::TypeFlags;

use bolt_ts_binder::SymbolID;

impl<'cx> TyChecker<'cx> {
    pub(super) fn index_infos_of_ty(&self, ty: &'cx ty::Ty<'cx>) -> ty::IndexInfos<'cx> {
        if ty.flags.intersects(TypeFlags::STRUCTURED_TYPE) {
            self.expect_ty_links(ty.id)
                .expect_structured_members()
                .index_infos
        } else {
            self.empty_array()
        }
    }

    pub(super) fn properties_of_ty(&self, ty: &'cx ty::Ty<'cx>) -> &'cx [SymbolID] {
        self.properties_of_object_type(ty)
    }

    pub(super) fn properties_of_object_type(&self, ty: &'cx ty::Ty<'cx>) -> &'cx [SymbolID] {
        if ty.kind.is_object() {
            {
                self.expect_ty_links(ty.id)
                    .expect_structured_members()
                    .props
            }
        } else {
            Default::default()
        }
    }

    pub(super) fn signatures_of_structured_type(
        &self,
        ty: &'cx ty::Ty<'cx>,
        kind: ty::SigKind,
    ) -> ty::Sigs<'cx> {
        if ty.kind.is_structured() {
            {
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
            }
        } else {
            self.empty_array()
        }
    }

    pub(super) fn signatures_of_type(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        kind: ty::SigKind,
    ) -> ty::Sigs<'cx> {
        let sigs = self.signatures_of_structured_type(ty, kind);
        if matches!(kind, ty::SigKind::Call)
            && sigs.is_empty()
            && let ty::TyKind::Union(u) = ty.kind
        {
            if let Some(sigs) =
                self.union_ty_links_arena[u.union_ty_links].get_array_fallback_sigs()
            {
                return sigs;
            }
            let mut member_name = None;
            if u.tys.iter().all(|t| {
                t.symbol().is_some_and(|symbol| {
                    let s = self.symbol(symbol);
                    let symbol_name = s.name;
                    if let Some(parent) = s.parent
                        && self.is_array_or_tuple_symbol(parent)
                        && match member_name {
                            Some(member_name) => member_name == symbol_name,
                            None => {
                                member_name = Some(symbol_name);
                                true
                            }
                        }
                    {
                        true
                    } else {
                        false
                    }
                })
            }) {
                let array_argument = self
                    .map_union_ty(
                        ty,
                        u,
                        |this, t| {
                            let symbol = t.symbol().unwrap();
                            let parent = this.symbol(symbol).parent;
                            let array_ty = if let Some(parent) = parent
                                && this.is_readonly_array_symbol(parent)
                            {
                                this.global_readonly_array_ty()
                            } else {
                                this.global_array_ty()
                            };
                            let ty_params = array_ty
                                .kind
                                .expect_object_reference()
                                .interface_target()
                                .unwrap()
                                .kind
                                .expect_object_interface()
                                .ty_params
                                .unwrap();
                            let mapper = t.kind.expect_object_anonymous().mapper.unwrap();
                            Some(this.get_mapped_ty(mapper, ty_params[0]))
                        },
                        true,
                    )
                    .unwrap();
                let readonly = u.tys.iter().any(|t| {
                    let s = t.symbol().unwrap();
                    let p = self.symbol(s).parent;
                    p.is_some_and(|parent| self.is_readonly_array_symbol(parent))
                });
                let array_ty = self.create_array_ty(array_argument, readonly);
                let ty = self
                    .get_ty_of_prop_of_ty(array_ty, member_name.unwrap())
                    .unwrap();
                let sigs = self.get_signatures_of_type(ty, ty::SigKind::Call);
                self.union_ty_links_arena[u.union_ty_links].set_array_fallback_sigs(sigs);
                return sigs;
            }
        }
        sigs
    }

    fn is_readonly_array_symbol(&mut self, symbol: SymbolID) -> bool {
        let Some(global_readonly_array_ty_symbol) = self.global_readonly_array_ty().symbol() else {
            return false;
        };
        self.get_symbol_if_same_reference(symbol, global_readonly_array_ty_symbol)
            .is_some()
    }

    fn is_array_or_tuple_symbol(&mut self, symbol: SymbolID) -> bool {
        let Some(global_array_ty_symbol) = self.global_array_ty().symbol() else {
            return false;
        };
        let Some(global_readonly_array_ty_symbol) = self.global_readonly_array_ty().symbol() else {
            return false;
        };
        self.get_symbol_if_same_reference(symbol, global_array_ty_symbol)
            .is_some()
            || self
                .get_symbol_if_same_reference(symbol, global_readonly_array_ty_symbol)
                .is_some()
    }

    pub(super) fn this_ty(ty: &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(i) = ty.kind.as_object_interface() {
            i.this_ty
        } else if let Some(refer) = ty.kind.as_object_reference() {
            Self::this_ty(refer.target)
        } else {
            None
        }
    }
}
