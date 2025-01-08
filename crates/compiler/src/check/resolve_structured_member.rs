use rustc_hash::FxHashMap;

use super::{SymbolLinks, TyChecker};
use crate::ast;
use crate::bind::{SymbolFlags, SymbolID, SymbolName};
use crate::ty::{self, CheckFlags, SigID, SigKind, TyMapper};

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

    fn instantiate_symbol(
        &mut self,
        mut symbol: SymbolID,
        mut mapper: &'cx TyMapper<'cx>,
    ) -> SymbolID {
        if let Some(ty) = self.get_symbol_links(symbol).get_ty() {
            if !self.could_contain_ty_var(ty) {
                // TODO: handle write_ty and setAccessor
                return symbol;
            }
        }

        if self
            .check_flags(symbol)
            .intersects(CheckFlags::INSTANTIATED)
        {
            let links = self.get_symbol_links(symbol);
            symbol = links.get_target().unwrap();
            let ty_mapper = links.get_ty_mapper();
            mapper = self.combine_ty_mappers(ty_mapper, mapper);
        }

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
            this.instantiate_sig(sig, mapper, false)
        })
    }

    fn instantiate_index_infos(
        &mut self,
        index_infos: ty::IndexInfos<'cx>,
        mapper: &'cx TyMapper<'cx>,
    ) -> ty::IndexInfos<'cx> {
        self.instantiate_list(index_infos, mapper, |this, index_info, mapper| {
            this.instantiate_index_info(index_info, mapper)
        })
    }

    fn instantiate_index_info(
        &mut self,
        info: &'cx ty::IndexInfo<'cx>,
        mapper: &'cx TyMapper<'cx>,
    ) -> &'cx ty::IndexInfo<'cx> {
        let ty = self.instantiate_ty(info.val_ty, Some(mapper));
        self.alloc(ty::IndexInfo {
            val_ty: ty,
            ..*info
        })
    }

    pub(super) fn instantiate_sig(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        mut mapper: &'cx TyMapper<'cx>,
        erase_ty_params: bool,
    ) -> &'cx ty::Sig<'cx> {
        let mut fresh_ty_params = None;
        if !erase_ty_params {
            if let Some(ty_params) = &sig.ty_params {
                let new_ty_params = ty_params
                    .iter()
                    .map(|ty| self.clone_param_ty(ty))
                    .collect::<Vec<_>>();
                let new_ty_params: ty::Tys<'cx> = self.alloc(new_ty_params);
                fresh_ty_params = Some(new_ty_params);
                let new_mapper = self.alloc(TyMapper::create(ty_params, new_ty_params));
                mapper = self.combine_ty_mappers(Some(new_mapper), mapper);
                for ty in new_ty_params {
                    let prev = self.param_ty_mapper.insert(ty.id, mapper);
                    assert!(prev.is_none());
                }
            }
        }

        let params = self.instantiate_list(sig.params, mapper, |this, symbol, mapper| {
            this.instantiate_symbol(symbol, mapper)
        });

        let sig = self.new_sig(ty::Sig {
            target: Some(sig),
            mapper: Some(mapper),
            params,
            ty_params: fresh_ty_params,
            id: SigID::dummy(),
            ..*sig
        });
        sig
    }

    fn resolve_object_type_members(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        declared: &'cx ty::DeclaredMembers<'cx>,
        ty_params: ty::Tys<'cx>,
        ty_args: ty::Tys<'cx>,
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
            call_sigs = self.instantiate_sigs(declared.call_sigs, mapper).to_vec();
            ctor_sigs = self.instantiate_sigs(declared.ctor_sigs, mapper).to_vec();
            index_infos = self
                .instantiate_index_infos(declared.index_infos, mapper)
                .to_vec();
        }

        for base_ty in base_tys {
            let props = self.get_props_of_ty(base_ty);
            self.add_inherited_members(&mut members, props);
            // TODO: instantiate them
            call_sigs.extend(self.signatures_of_type(base_ty, SigKind::Call).iter());
            ctor_sigs.extend(
                self.signatures_of_type(base_ty, SigKind::Constructor)
                    .iter(),
            );
            let inherited_index_infos = self
                .index_infos_of_ty(base_ty)
                .into_iter()
                .filter(|info| self.find_index_info(&index_infos, info.key_ty).is_none())
                .collect::<Vec<_>>();
            index_infos.extend(inherited_index_infos);
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

    fn get_default_construct_sigs(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx [&'cx ty::Sig<'cx>] {
        let r = ty.kind.expect_object_reference();
        let i = r.target.kind.expect_object_interface();
        let symbol = i.symbol;
        let mut flags = ty::SigFlags::empty();
        let class_node_id = self.binder.symbol(symbol).expect_class().decl;
        if let Some(c) = self.p.node(class_node_id).as_class_decl() {
            if let Some(mods) = c.modifiers {
                if mods.flags.contains(ast::ModifierKind::Abstract) {
                    flags.insert(ty::SigFlags::HAS_ABSTRACT);
                }
            }
        }
        let sig = self.new_sig(ty::Sig {
            flags,
            ty_params: None,
            params: &[],
            min_args_count: 0,
            ret: None,
            node_id: None,
            target: None,
            mapper: None,
            id: SigID::dummy(),
            class_decl: Some(class_node_id),
        });
        self.sig_ret_ty.insert(sig.id, ty);
        self.alloc([sig])
    }

    fn resolve_anonymous_type_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let Some(a) = ty.kind.as_object_anonymous() else {
            unreachable!()
        };
        let symbol = self.binder.symbol(a.symbol);
        let symbol_flags = symbol.flags;

        let members;
        let call_sigs;
        let mut ctor_sigs;
        let index_infos: ty::IndexInfos<'cx>;

        if let Some(target) = a.target {
            let mapper = a.mapper.unwrap();
            let sigs = self.get_sigs_of_ty(target, SigKind::Call);
            call_sigs = self.instantiate_sigs(sigs, mapper);
            let sigs = self.get_sigs_of_ty(target, SigKind::Constructor);
            ctor_sigs = self.instantiate_sigs(sigs, mapper);
            members = FxHashMap::default();
            index_infos = &[];
            // TODO:  `index_infos`, `members` and instantiate them.
        } else if symbol_flags.intersects(SymbolFlags::FUNCTION) {
            call_sigs = self.get_sigs_of_symbol(a.symbol);
            ctor_sigs = &[];
            members = FxHashMap::default();
            index_infos = &[];
            // TODO: `constructor_sigs`, `index_infos`
        } else if symbol_flags.intersects(SymbolFlags::CLASS) {
            call_sigs = &[];
            members = symbol.expect_class().exports.clone();
            if let Some(symbol) = symbol.expect_class().members.get(&SymbolName::Constructor) {
                ctor_sigs = self.get_sigs_of_symbol(*symbol)
            } else {
                ctor_sigs = &[];
            }
            let ty = self.get_declared_ty_of_symbol(a.symbol);
            index_infos = self.index_infos_of_ty(ty);
            if ctor_sigs.is_empty() {
                ctor_sigs = self.get_default_construct_sigs(ty);
            };
        } else if symbol_flags.intersects(SymbolFlags::TYPE_LITERAL) {
            members = symbol.expect_ty_lit().members.clone();
            call_sigs = members
                .get(&SymbolName::Call)
                .map(|s| self.get_sigs_of_symbol(*s))
                .unwrap_or_default();
            // TODO: `constructor_sigs`, `index_infos`
            ctor_sigs = &[];
            index_infos = &[]
        } else {
            members = FxHashMap::default();
            call_sigs = &[];
            ctor_sigs = &[];
            index_infos = &[]
        };
        let props = self.get_props_from_members(&members);
        let m = self.alloc(ty::StructuredMembers {
            members: self.alloc(members),
            base_tys: &[],
            base_ctor_ty: None,
            call_sigs,
            ctor_sigs,
            index_infos,
            props,
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
            .flat_map(|ty| self.get_sigs_of_ty(ty, SigKind::Call))
            .copied()
            .collect::<Vec<_>>();
        let ctor_sigs = union
            .tys
            .iter()
            .flat_map(|ty| self.get_sigs_of_ty(ty, SigKind::Constructor))
            .copied()
            .collect::<Vec<_>>();
        let m = self.alloc(ty::StructuredMembers {
            members: self.alloc(FxHashMap::default()),
            base_tys: &[],
            base_ctor_ty: None,
            call_sigs: self.alloc(call_sigs),
            ctor_sigs: self.alloc(ctor_sigs),
            index_infos: Default::default(),
            props: Default::default(),
        });
        let prev = self.ty_structured_members.insert(ty.id, m);
        assert!(prev.is_none());
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
