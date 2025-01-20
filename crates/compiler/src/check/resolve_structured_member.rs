use bolt_ts_span::Span;
use rustc_hash::FxHashMap;

use super::cycle_check::{Cycle, ResolutionKey};
use super::links::SigLinks;
use super::{errors, SymbolLinks, TyChecker};
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
        } else if s.flags.intersects(SymbolFlags::TYPE_LITERAL) {
            let t = s.expect_ty_lit();
            &t.members
        } else if s.flags.intersects(SymbolFlags::OBJECT_LITERAL) {
            // TODO: remove
            let t = s.expect_object();
            &t.members
        } else {
            unreachable!("s: {s:#?}")
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
                    let prev = self.ty_links.insert(
                        ty.id,
                        super::TyLinks::default().with_param_ty_mapper(mapper),
                    );
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

    fn report_circular_base_ty(
        &mut self,
        decl: ast::NodeID,
        ty: &'cx ty::Ty<'cx>,
        base_defined_span: Option<Span>,
    ) {
        let error = errors::TypeXRecursivelyReferencesItselfAsABaseType {
            span: self.p.node(decl).ident_name().unwrap().span,
            x: self.print_ty(ty).to_string(),
            base_defined_span,
        };
        self.push_error(Box::new(error));
    }

    fn get_interface_base_ty_nodes(
        &self,
        decl: ast::NodeID,
    ) -> Option<&'cx [&'cx ast::ReferTy<'cx>]> {
        use ast::Node::*;
        let InterfaceDecl(interface) = self.p.node(decl) else {
            unreachable!()
        };
        interface.extends.map(|extends| extends.list)
    }

    fn has_base_ty(&mut self, ty: &'cx ty::Ty<'cx>, check_base: &'cx ty::Ty<'cx>) -> bool {
        fn check<'cx>(
            this: &mut TyChecker<'cx>,
            ty: &'cx ty::Ty<'cx>,
            self_ty: &'cx ty::Ty<'cx>,
            check_base: &'cx ty::Ty<'cx>,
        ) -> bool {
            if ty.kind.is_object_interface() {
                self_ty == check_base
                    || this
                        .get_base_tys(ty)
                        .iter()
                        .any(|base_ty| check(this, base_ty, base_ty, check_base))
            } else if let Some(r) = ty.kind.as_object_reference() {
                check(this, r.target, ty, check_base)
            } else {
                false
            }
        }
        check(self, ty, ty, check_base)
    }

    fn resolve_base_tys_of_interface(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        cycle_reported: &mut bool,
    ) -> &'cx [&'cx ty::Ty<'cx>] {
        let symbol = ty.symbol().unwrap();
        let s = self.binder.symbol(symbol);
        let i = s.expect_interface();
        let decl = i.decls[0];
        assert!(self.p.node(decl).is_interface_decl());
        let Some(ty_nodes) = self.get_interface_base_ty_nodes(decl) else {
            return &[];
        };
        let mut tys = thin_vec::ThinVec::with_capacity(ty_nodes.len());
        for node in ty_nodes {
            let base_ty = self.get_ty_from_ty_reference(*node);
            if ty != base_ty && !self.has_base_ty(base_ty, ty) {
                tys.push(base_ty);
            } else {
                *cycle_reported = true;
                self.report_circular_base_ty(decl, ty, Some(node.name.span()));
            }
        }
        self.alloc(tys)
    }

    fn get_base_type_node_of_class(
        &self,
        i: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ast::ClassExtendsClause<'cx>> {
        let symbol = i.symbol().unwrap();
        let decl = self.binder.symbol(symbol).expect_class().decl;
        self.get_effective_base_type_node(decl)
    }

    fn are_all_outer_parameters_applied(ty: &'cx ty::Ty<'cx>) -> bool {
        let i = if let Some(i) = ty.kind.as_object_interface() {
            i
        } else if let Some(r) = ty.kind.as_object_reference() {
            r.target.kind.expect_object_interface()
        } else {
            unreachable!()
        };
        if let Some(outer_ty_params) = i.outer_ty_params {
            let last = outer_ty_params.len() - 1;
            if let Some(ty_args) = ty.kind.as_object_reference() {
                outer_ty_params[last].symbol() != ty_args.resolved_ty_args[last].symbol()
            } else {
                unreachable!()
            }
        } else {
            true
        }
    }

    fn resolve_base_tys_of_class(&mut self, ty: &'cx ty::Ty<'cx>) -> ty::Tys<'cx> {
        let base_ctor_ty = self.get_base_constructor_type_of_class(ty);
        if !(base_ctor_ty.kind.is_object() || base_ctor_ty.kind.is_any()) {
            return &[];
        }
        let base_ty_node = self.get_base_type_node_of_class(ty);
        let original_base_ty = base_ctor_ty
            .symbol()
            .map(|symbol| self.get_declared_ty_of_symbol(symbol));

        let base_ty;
        if base_ctor_ty.symbol().is_some()
            && self
                .binder
                .symbol(base_ctor_ty.symbol().unwrap())
                .flags
                .intersects(SymbolFlags::CLASS)
            && Self::are_all_outer_parameters_applied(original_base_ty.unwrap())
        {
            let symbol = base_ctor_ty.symbol().unwrap();
            base_ty = self.get_ty_from_class_or_interface_refer(base_ty_node.unwrap(), symbol);
        } else if base_ctor_ty.kind.is_any() {
            base_ty = base_ctor_ty;
        } else {
            // TODO:
            base_ty = base_ctor_ty;
        }
        if base_ty == self.error_ty() {
            return &[];
        }
        let tys = self.alloc([base_ty]);
        tys
    }

    fn get_base_tys(&mut self, ty: &'cx ty::Ty<'cx>) -> ty::Tys<'cx> {
        if let Some(tys) = self.get_ty_links(ty.id).get_resolved_base_tys() {
            return tys;
        }
        let id = ty.symbol().unwrap();
        if self.push_ty_resolution(ResolutionKey::ResolvedBaseTypes(ty.id)) {
            let symbol = self.binder.symbol(id);
            let mut cycle_reported = false;
            let tys = if symbol.flags.intersects(SymbolFlags::CLASS) {
                self.resolve_base_tys_of_class(ty)
            } else if symbol.flags.intersects(SymbolFlags::INTERFACE) {
                self.resolve_base_tys_of_interface(ty, &mut cycle_reported)
            } else {
                unreachable!()
            };
            if !cycle_reported {
                if let Cycle::Some(_) = self.pop_ty_resolution() {
                    if let Some(decl) = id.opt_decl(self.binder) {
                        let p = self.p.node(decl);
                        if p.is_class_decl() || p.is_interface_decl() {
                            self.report_circular_base_ty(decl, ty, None);
                        }
                    }
                }
            }
            self.get_mut_ty_links(ty.id).set_resolved_base_tys(tys);
            tys
        } else {
            &[]
        }
    }

    fn resolve_object_type_members(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        declared: &'cx ty::DeclaredMembers<'cx>,
        ty_params: ty::Tys<'cx>,
        ty_args: ty::Tys<'cx>,
    ) {
        if self.get_ty_links(ty.id).get_structured_members().is_some() {
            return;
        }
        let symbol = ty.symbol().unwrap();

        let base_tys = self.get_base_tys(ty);

        let base_ctor_ty = if self
            .binder
            .symbol(symbol)
            .flags
            .intersects(SymbolFlags::CLASS)
        {
            Some(self.get_base_constructor_type_of_class(ty))
        } else {
            None
        };

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
                self.get_mut_ty_links(ty.id).set_structured_members(m);
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
                .iter()
                .filter(|info| self.find_index_info(&index_infos, info.key_ty).is_none())
                .collect::<Vec<_>>();
            index_infos.extend(inherited_index_infos);
        }

        if self.get_ty_links(ty.id).get_structured_members().is_some() {
            return;
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
        self.get_mut_ty_links(ty.id).set_structured_members(m);
    }

    fn resolve_interface_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let declared_members = ty.kind.expect_object_interface().declared_members;
        self.resolve_object_type_members(ty, declared_members, &[], &[]);
    }

    fn resolve_reference_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let Some(refer) = ty.kind.as_object_reference() else {
            unreachable!()
        };
        fn depp_target<'cx>(ty: &'cx ty::Ty<'cx>) -> Option<&'cx ty::InterfaceTy<'cx>> {
            if let Some(refer) = ty.kind.as_object_reference() {
                depp_target(refer.target)
            } else {
                // TODO: handle more case
                ty.kind.as_object_interface()
            }
        }
        let Some(i) = depp_target(ty) else { return };
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
        let declared_members = i.declared_members;
        self.resolve_object_type_members(ty, declared_members, ty_params, padded_type_arguments);
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
        let prev = self
            .sig_links
            .insert(sig.id, SigLinks::default().with_resolved_ret_ty(ty));
        assert!(prev.is_none());
        self.alloc([sig])
    }

    fn resolve_anonymous_type_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let Some(a) = ty.kind.as_object_anonymous() else {
            unreachable!()
        };
        let symbol = self.binder.symbol(a.symbol);
        let symbol_flags = symbol.flags;

        let mut members;
        let call_sigs;
        let mut ctor_sigs;
        let index_infos: ty::IndexInfos<'cx>;

        if let Some(target) = a.target {
            let mapper = a.mapper.unwrap();
            members = {
                let props = self.get_props_of_ty(target);
                self.create_instantiated_symbol_table(props, mapper, false)
            };
            let sigs = self.get_signatures_of_type(target, SigKind::Call);
            call_sigs = self.instantiate_sigs(sigs, mapper);
            let sigs = self.get_signatures_of_type(target, SigKind::Constructor);
            ctor_sigs = self.instantiate_sigs(sigs, mapper);
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
            // TODO: `get_exports_of_symbol`
            members = symbol.expect_class().exports.clone();
            if let Some(symbol) = symbol.expect_class().members.get(&SymbolName::Constructor) {
                ctor_sigs = self.get_sigs_of_symbol(*symbol)
            } else {
                ctor_sigs = &[];
            }

            let class_ty = self.get_declared_ty_of_symbol(a.symbol);
            self.resolve_structured_type_members(class_ty);
            let base_ctor_ty = self.get_base_constructor_type_of_class(class_ty);
            if base_ctor_ty.kind.is_object() {
                let props = self.get_props_of_ty(base_ctor_ty);
                self.add_inherited_members(&mut members, props);
            }

            index_infos = self.index_infos_of_ty(class_ty);
            if ctor_sigs.is_empty() {
                ctor_sigs = self.get_default_construct_sigs(class_ty);
            };
        } else if symbol_flags.intersects(SymbolFlags::TYPE_LITERAL) {
            members = symbol.expect_ty_lit().members.clone();
            call_sigs = members
                .get(&SymbolName::Call)
                .map(|s| self.get_sigs_of_symbol(*s))
                .unwrap_or_default();
            ctor_sigs = members
                .get(&SymbolName::New)
                .map(|s| self.get_sigs_of_symbol(*s))
                .unwrap_or_default();
            index_infos = self.get_index_infos_of_symbol(a.symbol);
        } else if symbol_flags.intersects(SymbolFlags::OBJECT_LITERAL) {
            unreachable!("Object literal should be resolved during check");
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
        self.get_mut_ty_links(ty.id).set_structured_members(m);
    }

    fn resolve_union_type_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let Some(union) = ty.kind.as_union() else {
            unreachable!()
        };
        let call_sigs = union
            .tys
            .iter()
            .flat_map(|ty| self.get_signatures_of_type(ty, SigKind::Call))
            .copied()
            .collect::<Vec<_>>();
        let ctor_sigs = union
            .tys
            .iter()
            .flat_map(|ty| self.get_signatures_of_type(ty, SigKind::Constructor))
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
        self.get_mut_ty_links(ty.id).set_structured_members(m);
    }

    pub(super) fn resolve_structured_type_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        if self.get_ty_links(ty.id).get_structured_members().is_some() {
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
