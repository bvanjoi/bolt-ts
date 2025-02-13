use bolt_ts_span::Span;
use indexmap::map;
use rustc_hash::FxHashMap;

use super::cycle_check::{Cycle, ResolutionKey};
use super::links::SigLinks;
use super::{errors, SymbolLinks, TyChecker};
use crate::ast::{self, MappedTyModifiers, ObjectMethodMember};
use crate::bind::{Symbol, SymbolFlags, SymbolID, SymbolName};
use crate::ty::{self, CheckFlags, ObjectFlags, SigID, SigKind, TypeFlags};

impl<'cx> TyChecker<'cx> {
    pub(super) fn members(&self, symbol: SymbolID) -> &FxHashMap<SymbolName, SymbolID> {
        let s = self.binder.symbol(symbol);
        if s.flags.intersects(SymbolFlags::CLASS) {
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
            let base_name = self.symbol(*base_id).name();
            members.entry(base_name).or_insert(*base_id);
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
        let flags = self.symbol(symbol).flags();
        if flags.intersects(SymbolFlags::PROPERTY) {
            // let p = s.expect_prop();
            // c.this_ty.is_none()
            false
        } else if flags == SymbolFlags::INTERFACE {
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
        mapper: &'cx dyn ty::TyMap<'cx>,
        mapping_only_this: bool,
    ) -> FxHashMap<SymbolName, SymbolID> {
        declared_props
            .iter()
            .map(|symbol| {
                let name = self.symbol(*symbol).name();
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
        mut mapper: &'cx dyn ty::TyMap<'cx>,
    ) -> SymbolID {
        if let Some(ty) = self.get_symbol_links(symbol).get_ty() {
            if !self.could_contain_ty_var(ty) {
                // TODO: handle write_ty and setAccessor
                return symbol;
            }
        }

        if self
            .get_check_flags(symbol)
            .intersects(CheckFlags::INSTANTIATED)
        {
            let links = self.get_symbol_links(symbol);
            symbol = links.get_target().unwrap();
            let ty_mapper = links.get_ty_mapper();
            mapper = self.combine_ty_mappers(ty_mapper, mapper);
        }

        let check_flags = CheckFlags::INSTANTIATED
            | (self.get_check_flags(symbol)
                & (CheckFlags::READONLY
                    | CheckFlags::LATE
                    | CheckFlags::OPTIONAL_PARAMETER
                    | CheckFlags::REST_PARAMETER));
        let links = SymbolLinks::default()
            .with_check_flags(check_flags)
            .with_ty_mapper(mapper)
            .with_target(symbol);

        let s = self.symbol(symbol);
        let name = s.name();
        let symbol_flags = s.flags();

        self.create_transient_symbol(name, symbol_flags, Some(symbol), links)
    }

    fn instantiate_sigs(
        &mut self,
        sigs: ty::Sigs<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
    ) -> ty::Sigs<'cx> {
        self.instantiate_list(sigs, mapper, |this, sig, mapper| {
            this.instantiate_sig(sig, mapper, false)
        })
    }

    fn instantiate_index_infos(
        &mut self,
        index_infos: ty::IndexInfos<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
    ) -> ty::IndexInfos<'cx> {
        self.instantiate_list(index_infos, mapper, |this, index_info, mapper| {
            this.instantiate_index_info(index_info, mapper)
        })
    }

    fn instantiate_index_info(
        &mut self,
        info: &'cx ty::IndexInfo<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
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
        mut mapper: &'cx dyn ty::TyMap<'cx>,
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
                let new_mapper = self.create_ty_mapper(ty_params, new_ty_params);
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

    pub(super) fn has_base_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        check_base: &'cx ty::Ty<'cx>,
    ) -> bool {
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

    fn are_all_outer_parameters_applied(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        let i = if let Some(i) = ty.kind.as_object_interface() {
            i
        } else if let Some(r) = ty.kind.as_object_reference() {
            r.target.kind.expect_object_interface()
        } else {
            unreachable!()
        };
        if let Some(outer_ty_params) = i.outer_ty_params {
            let last = outer_ty_params.len() - 1;
            let ty_args = self.get_ty_arguments(ty);
            outer_ty_params[last].symbol() != ty_args[last].symbol()
        } else {
            true
        }
    }

    fn resolve_base_tys_of_class(&mut self, ty: &'cx ty::Ty<'cx>) -> ty::Tys<'cx> {
        let base_ctor_ty = self.get_base_constructor_type_of_class(ty);
        if !(base_ctor_ty.kind.is_object() || base_ctor_ty.flags.intersects(TypeFlags::ANY)) {
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
            && self.are_all_outer_parameters_applied(original_base_ty.unwrap())
        {
            let symbol = base_ctor_ty.symbol().unwrap();
            base_ty = self.get_ty_from_class_or_interface_refer(base_ty_node.unwrap(), symbol);
        } else if base_ctor_ty.flags.intersects(TypeFlags::ANY) {
            base_ty = base_ctor_ty;
        } else {
            // TODO:
            base_ty = base_ctor_ty;
        }
        if base_ty == self.error_ty {
            return &[];
        }
        let tys = self.alloc([base_ty]);
        tys
    }

    fn get_tuple_base_ty(&mut self, ty: &'cx ty::TupleTy<'cx>) -> &'cx ty::Ty<'cx> {
        let element_tys = ty
            .ty_params()
            .unwrap_or_default()
            .iter()
            .enumerate()
            .map(|(idx, ty_arg)| {
                if ty.element_flags[idx].intersects(ty::ElementFlags::VARIADIC) {
                    self.get_indexed_access_ty(ty_arg, self.number_ty, None, None)
                } else {
                    ty_arg
                }
            })
            .collect::<Vec<_>>();
        let tys = if element_tys.is_empty() {
            self.empty_array()
        } else {
            &element_tys
        };
        let ty = self.get_union_ty(tys, ty::UnionReduction::Lit);
        self.create_array_ty(ty, false)
    }

    fn get_base_tys(&mut self, ty: &'cx ty::Ty<'cx>) -> ty::Tys<'cx> {
        if let Some(tys) = self.get_ty_links(ty.id).get_resolved_base_tys() {
            return tys;
        }
        if self.push_ty_resolution(ResolutionKey::ResolvedBaseTypes(ty.id)) {
            if let Some(t) = ty.as_tuple() {
                let base_ty = self.get_tuple_base_ty(t);
                let tys = self.alloc(vec![base_ty]);
                self.get_mut_ty_links(ty.id).set_resolved_base_tys(tys);
                return tys;
            }
            let id = ty.symbol().unwrap();
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
        source: &'cx ty::Ty<'cx>,
        declared: &'cx ty::DeclaredMembers<'cx>,
        ty_params: ty::Tys<'cx>,
        ty_args: ty::Tys<'cx>,
    ) {
        if self.get_ty_links(ty.id).get_structured_members().is_some() {
            return;
        }

        let mapper: Option<&'cx dyn ty::TyMap<'cx>>;
        // TODO: use source type
        let base_tys = self.get_base_tys(ty);

        let base_ctor_ty = if ty.symbol().is_some_and(|symbol| {
            self.binder
                .symbol(symbol)
                .flags
                .intersects(SymbolFlags::CLASS)
        }) {
            Some(self.get_base_constructor_type_of_class(ty))
        } else {
            None
        };

        let mut members;
        let mut call_sigs;
        let mut ctor_sigs;
        let mut index_infos;
        if Self::range_eq(ty_params, ty_args, 0, ty_params.len()) {
            mapper = None;
            members = ty
                .symbol()
                .map(|symbol| self.members(symbol).clone())
                .unwrap_or_default();
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
            let m = self.create_ty_mapper(ty_params, ty_args);
            mapper = Some(m);
            members =
                self.create_instantiated_symbol_table(declared.props, m, ty_params.len() == 1);
            call_sigs = self.instantiate_sigs(declared.call_sigs, m).to_vec();
            ctor_sigs = self.instantiate_sigs(declared.ctor_sigs, m).to_vec();
            index_infos = self
                .instantiate_index_infos(declared.index_infos, m)
                .to_vec();
        }

        let this_arg = ty_params.last();
        for base_ty in base_tys {
            let instantiated_base_ty = if let Some(this_arg) = this_arg {
                let ty = self.instantiate_ty(base_ty, mapper);
                self.get_ty_with_this_arg(ty, Some(this_arg))
            } else {
                base_ty
            };
            let props = self.get_props_of_ty(instantiated_base_ty);
            self.add_inherited_members(&mut members, props);
            // TODO: instantiate them
            call_sigs.extend(
                self.signatures_of_type(instantiated_base_ty, SigKind::Call)
                    .iter(),
            );
            ctor_sigs.extend(
                self.signatures_of_type(instantiated_base_ty, SigKind::Constructor)
                    .iter(),
            );
            let inherited_index_infos = self
                .index_infos_of_ty(instantiated_base_ty)
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
        self.resolve_object_type_members(ty, ty, declared_members, &[], &[]);
    }

    fn resolve_reference_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let target = if let Some(refer) = ty.kind.as_object_reference() {
            refer.deep_target()
        } else if let Some(t) = ty.kind.as_object_tuple() {
            t.ty
        } else {
            unreachable!("{:#?}", ty)
        };

        let ty_params = if let Some(i) = target.kind.as_object_interface() {
            let mut ty_params = i.ty_params.unwrap_or_default().to_vec();
            ty_params.push(i.this_ty.unwrap());
            self.alloc(ty_params)
        } else if let Some(t) = target.kind.as_object_tuple() {
            let i = t.ty.kind.expect_object_interface();
            let mut ty_params = i.ty_params.unwrap_or_default().to_vec();
            ty_params.push(i.this_ty.unwrap());
            self.alloc(ty_params)
        } else {
            // TODO: handle more case
            return;
        };

        let ty_args = self.get_ty_arguments(ty);
        let padded_type_arguments = if ty_params.len() == ty_args.len() {
            ty_args
        } else {
            let mut padded_type_arguments = ty_args.to_vec();
            padded_type_arguments.push(target);
            self.alloc(padded_type_arguments)
        };
        let declared_members = if let Some(i) = target.kind.as_object_interface() {
            i.declared_members
        } else if let Some(t) = target.kind.as_object_tuple() {
            t.ty.kind.expect_object_interface().declared_members
        } else {
            unreachable!()
        };
        self.resolve_object_type_members(
            ty,
            target,
            declared_members,
            ty_params,
            padded_type_arguments,
        );
    }

    pub(super) fn get_default_constraint_of_cond_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let cond_ty = ty.kind.expect_cond_ty();
        if let Some(ty) = self.get_ty_links(ty.id).get_resolved_default_constraint() {
            return ty;
        }
        let true_constraint = self.get_inferred_true_ty_from_cond_ty(ty, cond_ty);
        let false_constraint = self.get_false_ty_from_cond_ty(ty, cond_ty);
        let res = if self.is_type_any(Some(true_constraint)) {
            false_constraint
        } else if self.is_type_any(Some(false_constraint)) {
            true_constraint
        } else {
            self.get_union_ty(
                &[true_constraint, false_constraint],
                ty::UnionReduction::Lit,
            )
        };
        self.get_mut_ty_links(ty.id)
            .set_resolved_default_constraint(res);
        res
    }

    pub(super) fn get_inferred_true_ty_from_cond_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        cond_ty: &'cx ty::CondTy<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(ty) = self.get_ty_links(ty.id).get_resolved_inferred_true_ty() {
            return ty;
        }
        let res = if let Some(m) = cond_ty.combined_mapper {
            let ty = self.get_ty_from_type_node(cond_ty.root.node.true_ty);
            self.instantiate_ty(ty, Some(m))
        } else {
            self.get_true_ty_from_cond_ty(ty, cond_ty)
        };
        self.get_mut_ty_links(ty.id)
            .set_resolved_inferred_true_ty(res);
        res
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
                    flags.insert(ty::SigFlags::ABSTRACT);
                }
            }
        }
        let sig = self.new_sig(ty::Sig {
            flags,
            ty_params: i.local_ty_params,
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

    pub(super) fn get_exports_of_symbol(
        &mut self,
        symbol: SymbolID,
    ) -> Option<&FxHashMap<SymbolName, SymbolID>> {
        let flags = self.binder.symbol(symbol).flags;
        if flags.intersects(SymbolFlags::CLASS) {
            let c = self.binder.symbol(symbol).expect_class();
            Some(&c.exports)
        } else if flags.intersects(SymbolFlags::MODULE) {
            let ns = self.binder.symbol(symbol).expect_ns();
            Some(&ns.exports)
        } else {
            None
        }
    }

    fn resolve_anonymous_type_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let a = ty.kind.expect_object_anonymous();
        let symbol = self.binder.symbol(a.symbol);
        assert!(
            !symbol.flags.intersects(SymbolFlags::OBJECT_LITERAL),
            "Object literal should be resolved during check"
        );

        if let Some(target) = a.target {
            let mapper = a.mapper.unwrap();
            let members = {
                let props = self.get_props_of_ty(target);
                self.create_instantiated_symbol_table(props, mapper, false)
            };
            let sigs = self.get_signatures_of_type(target, SigKind::Call);
            let call_sigs = self.instantiate_sigs(sigs, mapper);
            let sigs = self.get_signatures_of_type(target, SigKind::Constructor);
            let ctor_sigs = self.instantiate_sigs(sigs, mapper);
            let index_infos = &[];
            // TODO:  `index_infos`, `members` and instantiate them.
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
            return;
        } else if symbol.flags.intersects(SymbolFlags::TYPE_LITERAL) {
            let members = symbol.expect_ty_lit().members.clone();
            let call_sigs = members
                .get(&SymbolName::Call)
                .map(|s| self.get_sigs_of_symbol(*s))
                .unwrap_or_default();
            let ctor_sigs = members
                .get(&SymbolName::New)
                .map(|s| self.get_sigs_of_symbol(*s))
                .unwrap_or_default();
            let index_infos = self.get_index_infos_of_symbol(a.symbol);
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
            return;
        }

        let symbol = self.binder.symbol(a.symbol);
        let symbol_flags = symbol.flags;

        let mut members = self
            .get_exports_of_symbol(a.symbol)
            .cloned()
            .unwrap_or_default();
        let call_sigs;
        let mut ctor_sigs: ty::Sigs<'cx>;
        let index_infos: ty::IndexInfos<'cx>;

        if symbol_flags.intersects(SymbolFlags::FUNCTION | SymbolFlags::METHOD) {
            call_sigs = self.get_sigs_of_symbol(a.symbol);
            ctor_sigs = &[];
            index_infos = &[];
            // TODO: `constructor_sigs`, `index_infos`
        } else if symbol_flags.intersects(SymbolFlags::CLASS) {
            call_sigs = &[];
            if let Some(symbol) = symbol.expect_class().members.get(&SymbolName::Constructor) {
                ctor_sigs = self.get_sigs_of_symbol(*symbol)
            } else {
                ctor_sigs = &[];
            }
            // let mut base_ctor_index_info = None;
            let class_ty = self.get_declared_ty_of_symbol(a.symbol);
            self.resolve_structured_type_members(class_ty);
            let base_ctor_ty = self.get_base_constructor_type_of_class(class_ty);
            if base_ctor_ty.kind.is_object() || base_ctor_ty.kind.is_type_variable() {
                let props = self.get_props_of_ty(base_ctor_ty);
                self.add_inherited_members(&mut members, props);
            } else if base_ctor_ty == self.any_ty {
                // base_ctor_index_info = Some(self.any_base);
            }

            if let Some(index_symbol) = members.get(&SymbolName::Index) {
                index_infos = self.get_index_infos_of_index_symbol(*index_symbol);
            } else {
                index_infos = &[];
            }

            if ctor_sigs.is_empty() {
                ctor_sigs = self.get_default_construct_sigs(class_ty);
            };
        } else {
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

    pub(super) fn get_name_ty_from_mapped_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let mapped_ty = ty.kind.expect_object_mapped();
        if let Some(name_ty) = mapped_ty.decl.name_ty {
            if let Some(ty) = self.get_ty_links(ty.id).get_named_ty() {
                return Some(ty);
            }
            let name_ty = self.get_ty_from_type_node(name_ty);
            let name_ty = self.instantiate_ty(name_ty, mapped_ty.mapper);
            self.get_mut_ty_links(ty.id).set_named_ty(name_ty);
            Some(name_ty)
        } else {
            None
        }
    }

    pub(super) fn get_mapped_ty_name_ty_kind(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> ty::MappedTyNameTyKind {
        let mapped_ty = ty.kind.expect_object_mapped();
        let name_ty = self.get_name_ty_from_mapped_ty(ty);
        if let Some(name_ty) = name_ty {
            if self.is_type_assignable_to(name_ty, mapped_ty.ty_param) {
                ty::MappedTyNameTyKind::Filtering
            } else {
                ty::MappedTyNameTyKind::Remapping
            }
        } else {
            ty::MappedTyNameTyKind::None
        }
    }

    pub(super) fn get_template_ty_from_mapped_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let mapped_ty = ty.kind.expect_object_mapped();
        if let Some(template_ty) = self.get_ty_links(ty.id).get_template_ty() {
            return template_ty;
        }
        let template_ty = if let Some(decl_ty) = mapped_ty.decl.ty {
            let decl_ty = self.get_ty_from_type_node(decl_ty);
            let is_optional = mapped_ty
                .decl
                .get_modifiers()
                .intersects(MappedTyModifiers::INCLUDE_OPTIONAL);
            let t = self.add_optionality(decl_ty, true, is_optional);
            self.instantiate_ty(t, mapped_ty.mapper)
        } else {
            self.error_ty
        };
        self.get_mut_ty_links(ty.id).set_template_ty(template_ty);
        template_ty
    }

    fn get_constraint_decl_for_mapped_ty(
        &self,
        ty: &'cx ty::MappedTy<'cx>,
    ) -> Option<&'cx ast::Ty<'cx>> {
        self.get_effective_constraint_of_ty_param(ty.decl.ty_param)
    }

    pub(super) fn is_mapped_ty_with_keyof_constraint_decl(
        &self,
        ty: &'cx ty::MappedTy<'cx>,
    ) -> bool {
        let constraint_decl = self.get_constraint_decl_for_mapped_ty(ty).unwrap();
        if let ast::TyKind::TyOp(t) = constraint_decl.kind {
            t.op == ast::TyOpKind::Keyof
        } else {
            false
        }
    }

    pub(super) fn get_modifier_ty_from_mapped_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let mapped_ty = ty.kind.expect_object_mapped();
        if let Some(ty) = self.get_ty_links(ty.id).get_modifiers_ty() {
            return ty;
        }
        let modifiers_ty = if self.is_mapped_ty_with_keyof_constraint_decl(mapped_ty) {
            let ty_node = if let ast::TyKind::TyOp(t) = self
                .get_constraint_decl_for_mapped_ty(mapped_ty)
                .unwrap()
                .kind
            {
                t.ty
            } else {
                unimplemented!()
            };
            let t = self.get_ty_from_type_node(ty_node);
            self.instantiate_ty(t, mapped_ty.mapper)
        } else {
            let declare_ty = self.get_ty_from_mapped_ty_node(mapped_ty.decl);
            let constraint = declare_ty.kind.expect_object_mapped().constraint_ty;
            let extended_constraint = if constraint.flags.intersects(ty::TypeFlags::TYPE_PARAMETER)
            {
                self.get_constraint_of_ty_param(constraint)
            } else {
                Some(constraint)
            };
            extended_constraint
                .and_then(|extended_constraint| {
                    if let Some(index) = extended_constraint.kind.as_index_ty() {
                        Some(self.instantiate_ty(index.ty, mapped_ty.mapper))
                    } else {
                        None
                    }
                })
                .unwrap_or(self.undefined_ty)
        };
        self.get_mut_ty_links(ty.id).set_modifiers_ty(modifiers_ty);
        modifiers_ty
    }

    fn resolve_mapped_ty_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let mapped_ty = ty.kind.expect_object_mapped();
        let ty_param = mapped_ty.ty_param;
        let constraint_ty = mapped_ty.constraint_ty;
        let (name_ty, should_link_prop_decls, template_ty) = {
            let target = mapped_ty.target.unwrap_or(ty);
            assert!(target.kind.is_object_mapped());
            let name_ty = self.get_name_ty_from_mapped_ty(target);
            let should_link_prop_decls =
                self.get_mapped_ty_name_ty_kind(target) != ty::MappedTyNameTyKind::Remapping;
            let template_ty = self.get_template_ty_from_mapped_ty(target);
            (name_ty, should_link_prop_decls, template_ty)
        };
        let modifiers_ty = {
            let ty = self.get_modifier_ty_from_mapped_ty(ty);
            self.get_apparent_ty(ty)
        };
        let template_modifier = mapped_ty.decl.get_modifiers();

        let mut members: FxHashMap<SymbolName, SymbolID> = FxHashMap::default();
        let mut index_infos = vec![];

        let include = TypeFlags::STRING_OR_NUMBER_LITERAL_OR_UNIQUE;
        let mut add_member_for_key_ty_worker =
            |this: &mut Self, key_ty: &'cx ty::Ty<'cx>, prop_name_ty: &'cx ty::Ty<'cx>| {
                if prop_name_ty.useable_as_prop_name() {
                    let prop_name = this.get_prop_name_from_ty(prop_name_ty).unwrap();
                    let symbol_name = this.get_symbol_name_from_prop_name(prop_name);
                    if let Some(existing_prop) = members.get(&symbol_name) {
                        let named_ty = {
                            let old = this.get_symbol_links(*existing_prop).expect_named_ty();
                            this.get_union_ty(&[old, prop_name_ty], ty::UnionReduction::Lit)
                        };
                        this.get_mut_symbol_links(*existing_prop)
                            .override_name_ty(named_ty);

                        let key_ty = {
                            let old = this.get_symbol_links(*existing_prop).expect_key_ty();
                            this.get_union_ty(&[old, key_ty], ty::UnionReduction::Lit)
                        };
                        this.get_mut_symbol_links(*existing_prop)
                            .override_key_ty(key_ty);
                    } else {
                        let modifiers_prop = if key_ty.useable_as_prop_name() {
                            let prop_name = this.get_prop_name_from_ty(key_ty).unwrap();
                            let symbol_name = this.get_symbol_name_from_prop_name(prop_name);
                            this.get_prop_of_ty(modifiers_ty, symbol_name)
                        } else {
                            None
                        };
                        let is_optional = template_modifier
                            .intersects(MappedTyModifiers::INCLUDE_OPTIONAL)
                            || !(template_modifier.intersects(MappedTyModifiers::EXCLUDE_OPTIONAL)
                                && modifiers_prop.is_some_and(|m| {
                                    this.symbol(m).flags().intersects(SymbolFlags::OPTIONAL)
                                }));
                        let is_readonly = template_modifier
                            .intersects(MappedTyModifiers::INCLUDE_READONLY)
                            || !(template_modifier.intersects(MappedTyModifiers::EXCLUDE_READONLY)
                                && modifiers_prop.is_some_and(|m| this.is_readonly_symbol(m)));
                        let strip_optional = false; // TODO: strict_null_checks;
                        let late_flag = modifiers_prop
                            .map_or(ty::CheckFlags::default(), |m| this.get_late_flag(m));
                        let symbol_flags = SymbolFlags::PROPERTY
                            | if is_optional {
                                SymbolFlags::OPTIONAL
                            } else {
                                SymbolFlags::empty()
                            };
                        let links = SymbolLinks::default()
                            .with_mapped_ty(ty)
                            .with_name_ty(prop_name_ty)
                            .with_key_ty(key_ty)
                            .with_check_flags(
                                late_flag
                                    | CheckFlags::MAPPED
                                    | if is_readonly {
                                        CheckFlags::READONLY
                                    } else {
                                        CheckFlags::empty()
                                    }
                                    | if strip_optional {
                                        CheckFlags::STRIP_OPTIONAL
                                    } else {
                                        CheckFlags::empty()
                                    },
                            );
                        let symbol =
                            this.create_transient_symbol(symbol_name, symbol_flags, None, links);
                        let prev = members.insert(symbol_name, symbol);
                        assert!(prev.is_none());
                    }
                } else if this.is_valid_index_key_ty(prop_name_ty)
                    || prop_name_ty
                        .flags
                        .intersects(TypeFlags::ANY | TypeFlags::ENUM)
                {
                    let index_key_ty = if prop_name_ty
                        .flags
                        .intersects(TypeFlags::ANY | TypeFlags::STRING)
                    {
                        this.string_ty
                    } else if prop_name_ty
                        .flags
                        .intersects(TypeFlags::NUMBER | TypeFlags::ENUM)
                    {
                        this.number_ty
                    } else {
                        prop_name_ty
                    };
                    let prop_ty = {
                        let mapper = this.append_ty_mapping(mapped_ty.mapper, ty_param, key_ty);
                        this.instantiate_ty(template_ty, Some(mapper))
                    };
                    let modifiers_index_info =
                        this.get_applicable_index_info(modifiers_ty, prop_name_ty);
                    let is_readonly = template_modifier
                        .intersects(MappedTyModifiers::INCLUDE_READONLY)
                        || !(template_modifier.intersects(MappedTyModifiers::EXCLUDE_READONLY)
                            && modifiers_index_info.is_some_and(|i| i.is_readonly));
                    let index_info = this.alloc(ty::IndexInfo {
                        key_ty: index_key_ty,
                        val_ty: prop_ty,
                        is_readonly,
                        symbol: Symbol::ERR,
                    });
                    index_infos.push(index_info);
                }
            };
        let mut add_member_for_key_ty = |this: &mut Self, key_ty: &'cx ty::Ty<'cx>| {
            let prop_name_ty = if let Some(name_ty) = name_ty {
                let mapper = this.append_ty_mapping(mapped_ty.mapper, ty_param, name_ty);
                this.instantiate_ty(name_ty, Some(mapper))
            } else {
                key_ty
            };
            this.for_each_ty(prop_name_ty, |this, t| {
                add_member_for_key_ty_worker(this, key_ty, t)
            });
        };
        if self.is_mapped_ty_with_keyof_constraint_decl(mapped_ty) {
            self.for_each_mapped_ty_prop_key_ty_and_index_sig_key_ty(
                modifiers_ty,
                include,
                false,
                |this, key_ty| add_member_for_key_ty(this, key_ty),
            );
        } else {
            let ty = self.get_lower_bound_of_key_ty(constraint_ty);
            self.for_each_ty(ty, |this, ty| {
                add_member_for_key_ty(this, ty);
            });
        };
        let props = self.get_props_from_members(&members);
        let m = self.alloc(ty::StructuredMembers {
            members: self.alloc(members),
            base_tys: &[],
            base_ctor_ty: None,
            call_sigs: self.alloc(vec![]),
            ctor_sigs: self.alloc(vec![]),
            index_infos: if index_infos.is_empty() {
                self.empty_array()
            } else {
                self.alloc(index_infos)
            },
            props,
        });
        self.get_mut_ty_links(ty.id).set_structured_members(m);
    }

    pub(super) fn resolve_structured_type_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        if self.get_ty_links(ty.id).get_structured_members().is_some() {
            return;
        }
        if ty.kind.is_object() {
            if ty.get_object_flags().intersects(ObjectFlags::REFERENCE) {
                self.resolve_reference_members(ty);
            } else if ty.kind.is_object_interface() {
                self.resolve_interface_members(ty);
            } else if ty.kind.is_object_anonymous() {
                self.resolve_anonymous_type_members(ty);
            } else if ty.kind.is_object_mapped() {
                self.resolve_mapped_ty_members(ty);
            } else {
                // TODO: unreachable!()
            }
        } else if ty.kind.is_union() {
            self.resolve_union_type_members(ty);
        }
    }
}
