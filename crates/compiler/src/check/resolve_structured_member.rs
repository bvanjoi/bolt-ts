use rustc_hash::FxHashMap;

use super::{SymbolLinks, TyChecker};
use crate::bind::{SymbolFlags, SymbolID, SymbolName};
use crate::ty::{self, CheckFlags, SigKind, TyMapper};

impl<'cx> TyChecker<'cx> {
    pub(super) fn members(&self, symbol: SymbolID) -> &FxHashMap<SymbolName, SymbolID> {
        let s = self.binder.symbol(symbol);
        if s.flags == SymbolFlags::CLASS {
            let c = s.expect_class();
            &c.members
        } else if s.flags.intersects(SymbolFlags::INTERFACE) {
            let i = s.expect_interface();
            &i.members
        } else {
            unreachable!()
        }
    }

    fn add_inherited_members(
        &self,
        members: &mut FxHashMap<SymbolName, SymbolID>,
        base_symbols: &'cx [SymbolID],
    ) {
        for base_id in base_symbols {
            let base = self.binder.symbol(*base_id);
            members.entry(base.name).or_insert(*base_id);
        }
    }

    fn range_eq<T: PartialEq>(arr1: &[T], arr2: &[T], start: usize, end: usize) -> bool {
        for index in start..end {
            if let Some(item2) = arr2.get(index) {
                if arr1[index].eq(item2) {
                    continue;
                }
            }
            return false;
        }
        true
    }

    fn get_index_symbol(&self, symbol: SymbolID) -> Option<SymbolID> {
        self.members(symbol).get(&SymbolName::Index).copied()
    }

    pub(super) fn get_index_infos(&mut self, symbol: SymbolID) -> &'cx [&'cx ty::IndexInfo<'cx>] {
        let index_infos = self
            .get_index_symbol(symbol)
            .map(|index_symbol| {
                let decl = self.binder.symbol(index_symbol).expect_index().decl;
                let decl = self.p.node(decl).expect_index_sig_decl();
                let val_ty = self.get_ty_from_type_node(decl.ty);
                decl.params
                    .iter()
                    .map(|param| {
                        let Some(ty) = param.ty else { unreachable!() };
                        let key_ty = self.get_ty_from_type_node(ty);
                        self.alloc(ty::IndexInfo {
                            key_ty,
                            val_ty,
                            symbol: index_symbol,
                        })
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        self.alloc(index_infos)
    }

    fn in_this_less(&self, symbol: SymbolID) -> bool {
        let s = self.binder.symbol(symbol);
        if s.flags.intersects(SymbolFlags::PROPERTY) {
            // let p = s.expect_prop();
            // c.this_ty.is_none()
            false
        } else if s.flags == SymbolFlags::INTERFACE {
            // let i = s.expect_interface();
            // i.this_ty.is_none()
            false
        } else {
            false
        };
        false
    }

    fn create_instantiated_symbol_table(
        &mut self,
        declared_props: &'cx [SymbolID],
        mapper: &'cx TyMapper<'cx>,
        mapping_only_this: bool,
    ) -> FxHashMap<SymbolName, SymbolID> {
        declared_props
            .iter()
            .map(|symbol| {
                let name = self.binder.symbol(*symbol).name;
                if mapping_only_this && self.in_this_less(*symbol) {
                    (name, *symbol)
                } else {
                    (name, self.instantiate_symbol(*symbol, mapper))
                }
            })
            .collect::<FxHashMap<_, _>>()
    }

    fn instantiate_symbol(&mut self, symbol: SymbolID, mapper: &'cx TyMapper<'cx>) -> SymbolID {
        // let links = self.get_symbol_links(symbol);
        let s = self.binder.symbol(symbol);
        let name = s.name;
        let symbol_flags = s.flags;
        let check_flags = CheckFlags::INSTANTIATED
            | (self.check_flags(symbol)
                & (CheckFlags::READONLY
                    | CheckFlags::LATE
                    | CheckFlags::OPTIONAL_PARAMETER
                    | CheckFlags::REST_PARAMETER));
        let links = SymbolLinks::default()
            .with_check_flags(check_flags)
            .with_ty_mapper(mapper)
            .with_target(symbol);

        self.binder
            .create_transient_symbol(name, symbol_flags, Some(symbol), links)
    }

    fn instantiate_sigs(
        &mut self,
        sigs: ty::Sigs<'cx>,
        mapper: &'cx TyMapper<'cx>,
    ) -> ty::Sigs<'cx> {
        self.instantiate_list(sigs, mapper, |this, sig, mapper| {
            this.instantiate_sig(sig, mapper)
        })
    }

    fn instantiate_sig(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        mapper: &'cx TyMapper<'cx>,
    ) -> &'cx ty::Sig<'cx> {
        self.alloc(ty::Sig {
            target: Some(sig),
            mapper: Some(mapper),
            ..*sig
        })
    }

    fn resolve_object_type_members(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        declared: &'cx ty::DeclaredMembers<'cx>,
        ty_params: &'cx [&'cx ty::Ty<'cx>],
        ty_args: &'cx [&'cx ty::Ty<'cx>],
    ) {
        let symbol = ty.symbol().unwrap();
        let (base_ctor_ty, base_tys) = self.get_base_tys(symbol);
        // TODO: remove this
        if self.ty_structured_members.get(&ty.id).is_some() {
            return;
        }

        let mut members;
        let mut call_sigs;
        let mut ctor_sigs;
        let mut index_infos;
        if Self::range_eq(ty_params, ty_args, 0, ty_params.len()) {
            members = self.members(symbol).clone();
            if base_tys.is_empty() {
                let m = self.alloc(ty::StructuredMembers {
                    members: self.alloc(members),
                    base_tys: &[],
                    base_ctor_ty: None,
                    call_sigs: declared.call_sigs,
                    ctor_sigs: declared.ctor_sigs,
                    index_infos: declared.index_infos,
                    props: declared.props,
                });
                let prev = self.ty_structured_members.insert(ty.id, m);
                assert!(prev.is_none());
                return;
            }
            call_sigs = declared.call_sigs.to_vec();
            ctor_sigs = declared.ctor_sigs.to_vec();
            index_infos = declared.index_infos.to_vec();
        } else {
            let mapper = self.alloc(TyMapper::create(ty_params, ty_args));
            members =
                self.create_instantiated_symbol_table(declared.props, mapper, ty_params.len() == 1);
            // TODO: instantiate call_sigs, index_info, ctor_sigs
            call_sigs = declared.call_sigs.to_vec();
            ctor_sigs = declared.ctor_sigs.to_vec();
            index_infos = declared.index_infos.to_vec();
        }

        for base_ty in base_tys {
            self.add_inherited_members(&mut members, self.get_props_of_ty(base_ty));
            call_sigs.extend(self.signatures_of_type(base_ty, SigKind::Call).iter());
            ctor_sigs.extend(
                self.signatures_of_type(base_ty, SigKind::Constructor)
                    .iter(),
            );
            index_infos.extend(self.index_infos(base_ty).iter());
        }

        let props = self.get_props_from_members(&members);
        let m = self.alloc(ty::StructuredMembers {
            members: self.alloc(members),
            base_tys,
            base_ctor_ty,
            call_sigs: self.alloc(call_sigs),
            ctor_sigs: self.alloc(ctor_sigs),
            index_infos: self.alloc(index_infos),
            props,
        });
        let prev = self.ty_structured_members.insert(ty.id, m);
        assert!(prev.is_none());
    }

    fn resolve_interface_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let i = ty.kind.as_object_interface().unwrap();
        self.resolve_object_type_members(ty, i.declared, &[], &[]);
    }

    fn resolve_reference_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let Some(refer) = ty.kind.as_object_reference() else {
            unreachable!()
        };
        let Some(i) = refer.target.kind.as_object_interface() else {
            // TODO: handle more case.
            return;
        };
        let ty_params = {
            let mut ty_params = i.ty_params.unwrap().to_vec();
            ty_params.push(i.this_ty.unwrap());
            self.alloc(ty_params)
        };
        let ty_args = refer.resolved_ty_args;
        let padded_type_arguments = if ty_params.len() == ty_args.len() {
            ty_args
        } else {
            let mut padded_type_arguments = ty_args.to_vec();
            padded_type_arguments.push(refer.target);
            self.alloc(padded_type_arguments)
        };
        self.resolve_object_type_members(ty, i.declared, ty_params, padded_type_arguments);
    }

    fn resolve_anonymous_type_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let Some(a) = ty.kind.as_object_anonymous() else {
            unreachable!()
        };
        let call_sigs;
        if let Some(target) = a.target {
            let mapper = a.mapper.unwrap();
            call_sigs =
                self.instantiate_sigs(self.signatures_of_type(target, SigKind::Call), mapper);
            // TODO: `constructor_sigs`, `index_infos`, `members` and instantiate them.
            // TODO: remove this
        } else {
            call_sigs = self.get_sigs_of_symbol(a.symbol);
            // TODO: `constructor_sigs`, `index_infos`, `members`
        }
        let m = self.alloc(ty::StructuredMembers {
            members: self.alloc(FxHashMap::default()),
            base_tys: &[],
            base_ctor_ty: None,
            call_sigs,
            ctor_sigs: Default::default(),
            index_infos: Default::default(),
            props: Default::default(),
        });
        let prev = self.ty_structured_members.insert(ty.id, m);
        assert!(prev.is_none());
    }

    fn resolve_union_type_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let Some(union) = ty.kind.as_union() else {
            unreachable!()
        };
        let call_sigs = union
            .tys
            .iter()
            .map(|ty| self.get_sigs_of_ty(ty, SigKind::Call))
            .collect::<Vec<_>>();
        let ctor_sigs = union
            .tys
            .iter()
            .map(|ty| self.get_sigs_of_ty(ty, SigKind::Constructor))
            .collect::<Vec<_>>();
    }

    pub(super) fn resolve_structured_type_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        if self.ty_structured_members.get(&ty.id).is_some() {
            return;
        }

        if ty.kind.is_object_reference() {
            self.resolve_reference_members(ty);
        } else if ty.kind.is_object_interface() {
            self.resolve_interface_members(ty);
        } else if ty.kind.is_object_anonymous() {
            self.resolve_anonymous_type_members(ty);
        } else if ty.kind.is_union() {
            self.resolve_union_type_members(ty);
        }
    }
}
