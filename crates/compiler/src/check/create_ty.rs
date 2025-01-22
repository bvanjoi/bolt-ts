use rustc_hash::FxHashMap;

use crate::bind::{Symbol, SymbolID, SymbolName};
use crate::check::links::TyLinks;
use crate::ty::{self, ObjectFlags, TyID, UnionReduction};

use super::Ternary;
use super::{relation::RelationKind, TyChecker};

impl<'cx> TyChecker<'cx> {
    pub(super) fn new_ty(&mut self, kind: ty::TyKind<'cx>) -> &'cx ty::Ty<'cx> {
        let id = TyID::new(self.tys.len() as u32);
        let ty = self.alloc(ty::Ty::new(id, kind));
        self.tys.push(ty);
        ty
    }

    fn create_object_ty(
        &mut self,
        ty: ty::ObjectTyKind<'cx>,
        object_flags: ObjectFlags,
    ) -> &'cx ty::Ty<'cx> {
        let kind = ty::TyKind::Object(self.alloc(ty::ObjectTy {
            kind: ty,
            flags: object_flags,
        }));
        self.new_ty(kind)
    }

    pub(super) fn create_tuple_ty(&mut self, ty: ty::TupleTy<'cx>) -> &'cx ty::Ty<'cx> {
        assert_eq!(ty.tys.len(), ty.element_flags.len());
        debug_assert!(ty.element_flags.iter().all(|flag| {
            let flag = flag.bits();
            // is variant
            (flag & (flag - 1)) == 0
        }));
        self.create_object_ty(
            ty::ObjectTyKind::Tuple(self.alloc(ty)),
            ObjectFlags::empty(),
        )
    }

    pub(super) fn create_reference_ty(
        &mut self,
        ty: ty::ReferenceTy<'cx>,
        flags: ObjectFlags,
    ) -> &'cx ty::Ty<'cx> {
        let object_flags = flags | ty::Ty::get_propagating_flags_of_tys(ty.resolved_ty_args, None);
        let ty = self.create_object_ty(ty::ObjectTyKind::Reference(self.alloc(ty)), object_flags);
        ty
    }

    pub(super) fn crate_interface_ty(&mut self, ty: ty::InterfaceTy<'cx>) -> &'cx ty::Ty<'cx> {
        self.create_object_ty(
            ty::ObjectTyKind::Interface(self.alloc(ty)),
            ObjectFlags::empty(),
        )
    }

    pub(super) fn create_anonymous_ty(
        &mut self,
        symbol: SymbolID,
        object_flags: ObjectFlags,
    ) -> &'cx ty::Ty<'cx> {
        let ty = self.alloc(ty::AnonymousTy {
            symbol,
            target: None,
            mapper: None,
        });
        let ty = self.create_object_ty(
            ty::ObjectTyKind::Anonymous(ty),
            object_flags | ObjectFlags::ANONYMOUS,
        );
        ty
    }

    pub(super) fn create_anonymous_ty_with_resolved(
        &mut self,
        symbol: Option<SymbolID>,
        object_flags: ObjectFlags,
        members: &'cx FxHashMap<SymbolName, SymbolID>,
        call_sigs: ty::Sigs<'cx>,
        ctor_sigs: ty::Sigs<'cx>,
        index_infos: ty::IndexInfos<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let symbol = symbol.unwrap_or(Symbol::ERR);
        let ty = self.create_anonymous_ty(symbol, object_flags);
        let props = self.get_props_from_members(members);
        let prev = self.ty_links.insert(
            ty.id,
            TyLinks::default().with_structured_members(self.alloc(ty::StructuredMembers {
                members,
                props,
                call_sigs,
                ctor_sigs,
                index_infos,
                base_tys: &[],
                base_ctor_ty: None,
            })),
        );
        assert!(prev.is_none());
        ty
    }

    pub(super) fn create_instantiating_anonymous_ty(
        &mut self,
        symbol: SymbolID,
        target: &'cx ty::Ty<'cx>,
        mapper: &'cx ty::TyMapper<'cx>,
        object_flags: ObjectFlags,
    ) -> &'cx ty::Ty<'cx> {
        assert!(target.kind.is_object_anonymous());
        let ty = self.alloc(ty::AnonymousTy {
            symbol,
            target: Some(target),
            mapper: Some(mapper),
        });
        let ty = self.create_object_ty(
            ty::ObjectTyKind::Anonymous(ty),
            object_flags | ObjectFlags::ANONYMOUS,
        );
        ty
    }

    pub(super) fn create_single_sig_ty(&mut self, ty: ty::SingleSigTy<'cx>) -> &'cx ty::Ty<'cx> {
        let ty = self.create_object_ty(
            ty::ObjectTyKind::SingleSigTy(self.alloc(ty)),
            ObjectFlags::empty(),
        );
        ty
    }

    pub(super) fn clone_param_ty(&mut self, old: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        let old_param = old.kind.expect_param();
        let param_ty = self.alloc(ty::ParamTy {
            target: Some(old),
            ..*old_param
        });
        self.new_ty(ty::TyKind::Param(param_ty))
    }

    pub(super) fn create_param_ty(
        &mut self,
        symbol: SymbolID,
        offset: usize,
        is_this_ty: bool,
    ) -> &'cx ty::Ty<'cx> {
        let ty = ty::ParamTy {
            symbol,
            offset,
            target: None,
            is_this_ty,
        };
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

    pub(super) fn create_array_ty(&mut self, element_ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        let refer = self.global_array_ty().kind.expect_object_reference();
        self.create_reference_ty(
            ty::ReferenceTy {
                target: refer.target,
                resolved_ty_args: self.alloc(vec![element_ty]),
            },
            ObjectFlags::empty(),
        )
    }

    pub(super) fn create_union_type(
        &mut self,
        mut tys: Vec<&'cx ty::Ty<'cx>>,
        reduction: UnionReduction,
    ) -> &'cx ty::Ty<'cx> {
        // if tys.is_empty() {
        //     // TODO: never type
        // }

        tys.dedup();
        if reduction != UnionReduction::None && reduction == UnionReduction::Subtype {
            tys = self.remove_subtypes(tys)
        }

        if tys.len() == 1 {
            tys[0]
        } else {
            let object_flags = ty::Ty::get_propagating_flags_of_tys(&tys, None);
            let union = self.alloc(ty::UnionTy {
                tys: self.alloc(tys),
                object_flags,
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
                            != Ternary::FALSE
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
