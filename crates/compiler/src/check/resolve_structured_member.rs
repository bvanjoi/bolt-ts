use bolt_ts_ast::{self as ast, MappedTyModifiers};
use bolt_ts_span::Span;
use bolt_ts_utils::fx_hashmap_with_capacity;
use rustc_hash::FxHashMap;

use super::check_type_related_to::RecursionFlags;
use super::create_ty::IntersectionFlags;
use super::cycle_check::{Cycle, ResolutionKey};
use super::infer::InferenceFlags;
use super::links::SigLinks;
use super::symbol_info::SymbolInfo;
use super::{SymbolLinks, Ternary, TyChecker, errors};
use crate::bind::{Symbol, SymbolFlags, SymbolID, SymbolName, SymbolTable};
use crate::ty::{self, CheckFlags, IndexFlags, ObjectFlags, SigID, SigKind, TypeFlags};

#[derive(Debug, Clone, Copy)]
pub(super) enum MemberOrExportsResolutionKind {
    ResolvedExports,
    ResolvedMembers,
}

impl<'cx> TyChecker<'cx> {
    fn add_inherited_members(
        &self,
        members: &mut FxHashMap<SymbolName, SymbolID>,
        base_symbols: &'cx [SymbolID],
    ) {
        for base_id in base_symbols {
            let base_id = *base_id;
            let base_name = self.symbol(base_id).name;
            members.entry(base_name).or_insert(base_id);
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

    fn create_instantiated_symbol_table(
        &mut self,
        declared_props: &'cx [SymbolID],
        mapper: &'cx dyn ty::TyMap<'cx>,
        mapping_only_this: bool,
    ) -> FxHashMap<SymbolName, SymbolID> {
        declared_props
            .iter()
            .map(|symbol| {
                let name = self.symbol(*symbol).name;
                if mapping_only_this && self.is_this_less(*symbol) {
                    (name, *symbol)
                } else {
                    (name, self.instantiate_symbol(*symbol, mapper))
                }
            })
            .collect::<FxHashMap<_, _>>()
    }

    fn instantiate_symbol(
        &mut self,
        symbol: SymbolID,
        mapper: &'cx dyn ty::TyMap<'cx>,
    ) -> SymbolID {
        if let Some(ty) = self.get_symbol_links(symbol).get_ty() {
            if !self.could_contain_ty_var(ty) {
                if !self
                    .symbol(symbol)
                    .flags
                    .intersects(SymbolFlags::SET_ACCESSOR)
                {
                    return symbol;
                }
                if let Some(write_ty) = self.get_symbol_links(symbol).get_write_ty() {
                    if !self.could_contain_ty_var(write_ty) {
                        return symbol;
                    }
                }
            }
        }

        let check_flags = self.get_check_flags(symbol);
        let (symbol, mapper, check_flags) = if check_flags.intersects(CheckFlags::INSTANTIATED) {
            let links = self.get_symbol_links(symbol);
            let ty_mapper = links.get_ty_mapper();
            let symbol = links.get_target().unwrap();
            let mapper = self.combine_ty_mappers(ty_mapper, mapper);
            let check_flags = self.get_check_flags(symbol);
            (symbol, mapper, check_flags)
        } else {
            (symbol, mapper, check_flags)
        };

        let check_flags = CheckFlags::INSTANTIATED
            | (check_flags
                & CheckFlags::READONLY
                    .union(CheckFlags::LATE)
                    .union(CheckFlags::OPTIONAL_PARAMETER)
                    .union(CheckFlags::REST_PARAMETER));
        let links = SymbolLinks::default()
            .with_check_flags(check_flags)
            .with_ty_mapper(mapper)
            .with_target(symbol);
        let s = self.symbol(symbol);
        let name = s.name;
        let flags = s.flags;
        let decls = s.decls.clone();
        let value_declaration = s.value_decl;
        let id = self.create_transient_symbol(
            name,
            flags | SymbolFlags::TRANSIENT,
            links,
            decls,
            value_declaration,
        );
        id
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

    pub(super) fn get_interface_base_ty_nodes(
        &self,
        decl: ast::NodeID,
    ) -> Option<&'cx [&'cx ast::ReferTy<'cx>]> {
        use bolt_ts_ast::Node::*;
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

    fn resolve_base_tys_of_interface(&mut self, ty: &'cx ty::Ty<'cx>, cycle_reported: &mut bool) {
        if self.get_ty_links(ty.id).get_resolved_base_tys().is_none() {
            let tys = self.empty_array();
            self.get_mut_ty_links(ty.id).set_resolved_base_tys(tys);
        }
        let symbol = ty.symbol().unwrap();
        let s = self.binder.symbol(symbol);
        let Some(decls) = s.decls.as_ref() else {
            return;
        };

        let mut tys = Vec::with_capacity(decls.len() * 4);
        for decl in decls.clone() {
            if self.p.node(decl).is_interface_decl() {
                let Some(ty_nodes) = self.get_interface_base_ty_nodes(decl) else {
                    continue;
                };
                for node in ty_nodes {
                    let base_ty = self.get_ty_from_ty_reference(*node);
                    if !self.is_error(base_ty) {
                        if self.is_valid_base_ty(base_ty) {
                            if ty != base_ty && !self.has_base_ty(base_ty, ty) {
                                tys.push(base_ty);
                            } else {
                                *cycle_reported = true;
                                self.report_circular_base_ty(decl, ty, Some(node.name.span()));
                            }
                        } else {
                            // TODO: ERROR: An interface can only extend an object...
                        }
                    }
                }
            }
        }

        let now = self.get_ty_links(ty.id).expect_resolved_base_tys();
        assert!(std::ptr::addr_eq(now, self.empty_array::<ty::Tys<'cx>>()));
        let resolved_tys = self.alloc(tys);
        self.get_mut_ty_links(ty.id)
            .override_resolved_base_tys(resolved_tys);
    }

    fn get_base_type_node_of_class(
        &self,
        i: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ast::ClassExtendsClause<'cx>> {
        let symbol = i.symbol().unwrap();
        let decl = self.binder.symbol(symbol).decls.as_ref().unwrap()[0];
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
            let base_ty_node = base_ty_node.unwrap();
            let span = base_ty_node.span;
            let ty_args = base_ty_node.expr_with_ty_args.ty_args;
            base_ty = self.get_ty_from_class_or_interface_refer(span, ty_args, symbol);
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
        let readonly = ty.readonly;
        let element_tys = self.same_map_tys(ty.ty_params(), |this, t, i| {
            if ty.element_flags[i].intersects(ty::ElementFlags::VARIADIC) {
                this.get_indexed_access_ty(t, self.number_ty, None, None)
            } else {
                t
            }
        });
        let ty = if let Some(element_tys) = element_tys {
            self.get_union_ty(element_tys, ty::UnionReduction::Lit)
        } else {
            self.never_ty
        };
        self.create_array_ty(ty, readonly)
    }

    fn get_base_tys(&mut self, ty: &'cx ty::Ty<'cx>) -> ty::Tys<'cx> {
        if let Some(tys) = self.get_ty_links(ty.id).get_resolved_base_tys() {
            return tys;
        }
        if self.push_ty_resolution(ResolutionKey::ResolvedBaseTypes(ty.id)) {
            if let Some(t) = ty.as_tuple() {
                if let Some(old) = self.get_ty_links(ty.id).get_resolved_base_tys() {
                    return old;
                }
                let base_ty = self.get_tuple_base_ty(t);
                let tys = self.alloc(vec![base_ty]);
                self.get_mut_ty_links(ty.id).set_resolved_base_tys(tys);
                return tys;
            }
            let id = ty.symbol().unwrap();
            let symbol = self.binder.symbol(id);
            let mut cycle_reported = false;
            if symbol.flags.intersects(SymbolFlags::CLASS) {
                let tys = self.resolve_base_tys_of_class(ty);
                self.get_mut_ty_links(ty.id).set_resolved_base_tys(tys);
            } else if symbol.flags.intersects(SymbolFlags::INTERFACE) {
                self.resolve_base_tys_of_interface(ty, &mut cycle_reported);
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
            self.get_mut_ty_links(ty.id).set_base_tys_resolved(true);
        }

        self.get_ty_links(ty.id).expect_resolved_base_tys()
    }

    fn resolve_object_type_members(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        source: &'cx ty::Ty<'cx>,
        declared: &'cx ty::DeclaredMembers<'cx>,
        ty_params: ty::Tys<'cx>,
        ty_args: ty::Tys<'cx>,
    ) {
        let mapper: Option<&'cx dyn ty::TyMap<'cx>>;
        let base_tys = self.get_base_tys(source);

        let mut members;
        let mut call_sigs;
        let mut ctor_sigs;
        let mut index_infos;
        if Self::range_eq(ty_params, ty_args, 0, ty_params.len()) {
            mapper = None;
            members = ty
                .symbol()
                .map(|symbol| {
                    // TODO: remove clone
                    self.get_members_of_symbol(symbol).clone().0
                })
                .unwrap_or_default();
            if base_tys.is_empty() {
                let m = self.alloc(ty::StructuredMembers {
                    members: self.alloc(members),
                    base_tys: &[],
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
            // TODO: parallel?
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
                self.get_ty_with_this_arg(ty, Some(this_arg), false)
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

        let props = self.get_props_from_members(&members);
        let m = self.alloc(ty::StructuredMembers {
            members: self.alloc(members),
            base_tys,
            call_sigs: self.alloc(call_sigs),
            ctor_sigs: self.alloc(ctor_sigs),
            index_infos: self.alloc(index_infos),
            props,
        });
        self.get_mut_ty_links(ty.id).set_structured_members(m);
    }

    fn resolve_declared_members(
        &mut self,
        i: &'cx ty::InterfaceTy<'cx>,
    ) -> &'cx ty::DeclaredMembers<'cx> {
        if let Some(c) = self.interface_ty_links_arena[i.links].get_declared_members() {
            return c;
        }
        let members = &self.get_members_of_symbol(i.symbol).0;
        let props = self.get_props_from_members(members);
        let call_sigs = members
            .get(&SymbolName::Call)
            .copied()
            .map(|s| self.get_sigs_of_symbol(s))
            .unwrap_or_default();
        let ctor_sigs = members
            .get(&SymbolName::New)
            .copied()
            .map(|s| self.get_sigs_of_symbol(s))
            .unwrap_or_default();
        let index_infos = self.get_index_infos_of_symbol(i.symbol);
        let declared_members = self.alloc(ty::DeclaredMembers {
            props,
            index_infos,
            ctor_sigs,
            call_sigs,
        });
        self.interface_ty_links_arena[i.links].set_declared_members(declared_members);
        declared_members
    }

    fn resolve_interface_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let interface_ty = ty.kind.expect_object_interface();
        let declared_members = self.resolve_declared_members(interface_ty);
        self.resolve_object_type_members(ty, ty, declared_members, &[], &[]);
    }

    fn resolve_reference_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let target = if let Some(t) = ty.as_tuple() {
            t.ty
        } else if let Some(refer) = ty.kind.as_object_reference() {
            refer.interface_target().unwrap()
        } else {
            unreachable!("{:#?}", ty)
        };

        let i = target.kind.expect_object_interface();
        let mut ty_params = i.ty_params.unwrap_or_default().to_vec();
        ty_params.push(i.this_ty.unwrap());
        let ty_params = self.alloc(ty_params);

        let ty_args = self.get_ty_arguments(ty);
        let padded_type_arguments = if ty_params.len() == ty_args.len() {
            ty_args
        } else {
            let mut padded_type_arguments = ty_args.to_vec();
            padded_type_arguments.push(target);
            self.alloc(padded_type_arguments)
        };
        let declared_members = if let Some(i) = target.kind.as_object_interface() {
            self.resolve_declared_members(i)
        } else if let Some(t) = target.kind.as_object_tuple() {
            let i = t.ty.kind.expect_object_interface();
            self.resolve_declared_members(i)
        } else {
            unreachable!()
        };
        let source = if let Some(refer) = ty.kind.as_object_reference() {
            if refer.target.kind.is_object_interface() {
                ty
            } else {
                refer.target
            }
        } else if ty.kind.is_object_tuple() {
            ty
        } else {
            unreachable!("{:#?}", ty)
        };
        self.resolve_object_type_members(
            ty,
            source,
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
        let class_node_id = self.binder.symbol(symbol).decls.as_ref().unwrap()[0];
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

    pub(super) fn get_members_of_symbol(&mut self, symbol: SymbolID) -> &'cx SymbolTable {
        let flags = self.binder.symbol(symbol).flags;
        if flags.intersects(SymbolFlags::LATE_BINDING_CONTAINER) {
            self.get_resolved_members_or_exports_of_symbol(
                symbol,
                MemberOrExportsResolutionKind::ResolvedMembers,
            )
        } else {
            let members = self.members_of_symbol(symbol);
            if members.0.is_empty() {
                self.empty_symbols
            } else {
                members
            }
        }
    }

    pub(super) fn infer_reverse_mapped_ty(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        constraint: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let constraint_index_ty = constraint.kind.expect_index_ty();
        // TODO: cache
        self.reverse_mapped_source_stack.push(source);
        self.reverse_mapped_target_stack.push(target);
        let save_expanding_flags = self.reverse_expanding_flags;
        if self.is_deeply_nested_type(source, self.reverse_mapped_source_stack.as_ref(), 2) {
            self.reverse_expanding_flags |= RecursionFlags::SOURCE;
        }

        if self.is_deeply_nested_type(source, self.reverse_mapped_target_stack.as_ref(), 2) {
            self.reverse_expanding_flags |= RecursionFlags::TARGET;
        }
        let ty = if self.reverse_expanding_flags != RecursionFlags::BOTH {
            let index_ty = self.get_ty_param_from_mapped_ty(target);
            let ty_param = self.get_indexed_access_ty(constraint_index_ty.ty, index_ty, None, None);
            let template_ty = self.get_template_ty_from_mapped_ty(target);
            let inference =
                self.create_inference_context(&[ty_param], None, InferenceFlags::empty());
            self.infer_tys(inference, source, template_ty, None, false);
            assert!(self.inference(inference).inferences.len() == 1);
            Some(
                self.get_ty_from_inference(inference, 0)
                    .unwrap_or(self.unknown_ty),
            )
        } else {
            None
        };

        self.reverse_mapped_target_stack.push(target);
        self.reverse_mapped_source_stack.push(source);
        self.reverse_expanding_flags = save_expanding_flags;
        // TODO: cache
        ty
    }

    fn get_limited_constraint(
        &mut self,
        ty: &'cx ty::ReverseMappedTy<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let c = self.get_constraint_ty_from_mapped_ty(ty.mapped_ty);
        if !c
            .flags
            .intersects(TypeFlags::UNION.union(TypeFlags::INTERSECTION))
        {
            return None;
        }
        None
        // TODO: origin;
    }

    fn replace_indexed_access(
        &mut self,
        instantiable: &'cx ty::Ty<'cx>,
        object_ty: &'cx ty::Ty<'cx>,
        index_ty: &'cx ty::Ty<'cx>,
        replacement: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let sources = self.alloc([index_ty, object_ty]);
        let targets = {
            let a = self.get_number_literal_type(0.);
            let tys = self.alloc([replacement]);
            let b = self.create_tuple_ty(tys, None, false);
            self.alloc([a, b])
        };
        let mapper = self.create_ty_mapper(sources, targets);
        self.instantiate_ty(instantiable, Some(mapper))
    }

    fn resolve_reverse_mapped_ty_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let r = ty.kind.expect_object_reverse_mapped();
        let modifiers = r.mapped_ty.kind.expect_object_mapped().decl.get_modifiers();
        let readonly_mask = modifiers.intersects(MappedTyModifiers::INCLUDE_READONLY);
        let optional_mask = if modifiers.intersects(MappedTyModifiers::INCLUDE_OPTIONAL) {
            SymbolFlags::empty()
        } else {
            SymbolFlags::OPTIONAL
        };
        let index_infos =
            if let Some(index_info) = self.get_index_info_of_ty(r.source, self.string_ty) {
                let val_ty = self
                    .infer_reverse_mapped_ty(index_info.val_ty, r.mapped_ty, r.constraint_ty)
                    .unwrap_or(self.unknown_ty);
                let index_info = self.alloc(ty::IndexInfo {
                    symbol: Symbol::ERR,
                    key_ty: self.string_ty,
                    val_ty,
                    is_readonly: readonly_mask && index_info.is_readonly,
                });
                self.alloc(vec![index_info])
            } else {
                self.empty_array()
            };

        let props = self.get_props_of_ty(r.source);
        let mut members = SymbolTable::new(props.len());
        let limited_constraint = self.get_limited_constraint(r);

        for prop in props {
            if let Some(limited_constraint) = limited_constraint {
                todo!()
            };
            let prop = *prop;
            let check_flags = CheckFlags::REVERSE_MAPPED
                | if readonly_mask && self.is_readonly_symbol(prop) {
                    CheckFlags::READONLY
                } else {
                    CheckFlags::empty()
                };
            let p = self.symbol(prop);
            let name = p.name;
            let flags = (SymbolFlags::PROPERTY | p.flags & optional_mask) | SymbolFlags::TRANSIENT;
            let decls = p.decls.clone();
            let value_decl = p.value_decl;
            let prop_ty = self.get_type_of_symbol(prop);

            let mut mapped_ty = r.mapped_ty;
            let mut constraint_ty = r.constraint_ty;

            let c_index_ty = r.constraint_ty.kind.expect_index_ty();
            if let Some(t) = c_index_ty.ty.kind.as_indexed_access() {
                if t.object_ty.flags.intersects(TypeFlags::TYPE_PARAMETER)
                    && t.index_ty.flags.intersects(TypeFlags::TYPE_PARAMETER)
                {
                    let new_ty_param = t.object_ty;
                    let new_mapped_ty = self.replace_indexed_access(
                        r.mapped_ty,
                        t.object_ty,
                        t.index_ty,
                        new_ty_param,
                    );
                    mapped_ty = new_mapped_ty;
                    constraint_ty = self.get_index_ty(new_ty_param, IndexFlags::empty());
                }
            }

            let links = SymbolLinks::default()
                .with_prop_ty(prop_ty)
                .with_check_flags(check_flags)
                .with_mapped_ty(mapped_ty)
                .with_constraint_ty(constraint_ty);
            let links = if let Some(named_ty) = self.get_symbol_links(prop).get_name_ty() {
                links.with_name_ty(named_ty)
            } else {
                links
            };
            let inferred_prop = self.create_transient_symbol(name, flags, links, decls, value_decl);
            members.0.insert(name, inferred_prop);
        }

        let props = self.get_props_from_members(&members.0);
        let m = self.alloc(ty::StructuredMembers {
            members: self.alloc(members.0),
            base_tys: &[],
            call_sigs: self.empty_array(),
            ctor_sigs: self.empty_array(),
            index_infos,
            props,
        });
        self.get_mut_ty_links(ty.id).set_structured_members(m);
    }

    fn resolve_anonymous_type_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let a = ty.kind.expect_object_anonymous();
        let symbol_id = a.symbol.unwrap();
        let symbol = self.symbol(symbol_id);
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
            let index_infos = {
                let index_infos = self.get_index_infos_of_ty(target);
                self.instantiate_index_infos(index_infos, mapper)
            };
            // TODO: instantiate `members`.
            let props = self.get_props_from_members(&members);
            let m = self.alloc(ty::StructuredMembers {
                members: self.alloc(members),
                base_tys: &[],
                call_sigs,
                ctor_sigs,
                index_infos,
                props,
            });
            self.get_mut_ty_links(ty.id).set_structured_members(m);
            return;
        } else if symbol.flags.intersects(SymbolFlags::TYPE_LITERAL) {
            let members = self.get_members_of_symbol(symbol_id);
            let call_sigs = members
                .0
                .get(&SymbolName::Call)
                .map(|s| self.get_sigs_of_symbol(*s))
                .unwrap_or_default();
            let ctor_sigs = members
                .0
                .get(&SymbolName::New)
                .map(|s| self.get_sigs_of_symbol(*s))
                .unwrap_or_default();
            let index_infos = self.get_index_infos_of_symbol(symbol_id);
            let props = self.get_props_from_members(&members.0);
            let m = self.alloc(ty::StructuredMembers {
                members: &members.0,
                base_tys: &[],
                call_sigs,
                ctor_sigs,
                index_infos,
                props,
            });
            self.get_mut_ty_links(ty.id).set_structured_members(m);
            return;
        }

        // TODO: remove this clone.
        let mut members = self.get_exports_of_symbol(symbol_id).0.clone();
        let index_infos: ty::IndexInfos<'cx>;
        if symbol_id == self.global_this_symbol {
            // TODO:
        }

        let call_sigs;
        let mut ctor_sigs: ty::Sigs<'cx>;

        let symbol = self.symbol(symbol_id);
        let symbol_flags = symbol.flags;

        if symbol_flags.intersects(SymbolFlags::FUNCTION.union(SymbolFlags::METHOD)) {
            call_sigs = self.get_sigs_of_symbol(symbol_id);
            ctor_sigs = &[];
            index_infos = &[];
            // TODO: `constructor_sigs`, `index_infos`
        } else if symbol_flags.intersects(SymbolFlags::CLASS) {
            call_sigs = &[];
            if let Some(symbol) = symbol
                .members()
                .and_then(|m| m.0.get(&SymbolName::Constructor))
            {
                ctor_sigs = self.get_sigs_of_symbol(*symbol)
            } else {
                ctor_sigs = &[];
            }
            // let mut base_ctor_index_info = None;
            let class_ty = self.get_declared_ty_of_symbol(symbol_id);
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
            call_sigs,
            ctor_sigs,
            index_infos,
            props,
        });
        self.get_mut_ty_links(ty.id).set_structured_members(m);
    }

    fn resolve_union_type_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let union = ty.kind.expect_union();
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
            call_sigs: self.alloc(call_sigs),
            ctor_sigs: self.alloc(ctor_sigs),
            index_infos: Default::default(),
            props: Default::default(),
        });
        self.get_mut_ty_links(ty.id).set_structured_members(m);
    }

    fn resolve_intersection_type_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let t = ty.kind.expect_intersection();
        let mut call_sigs = Vec::with_capacity(t.tys.len());
        // let ctor_sigs = Vec::with_capacity(t.tys.len());
        let mut index_infos = Vec::with_capacity(t.tys.len());
        // TODO: mixin
        for i in 0..t.tys.len() {
            let t = t.tys[i];
            // let sigs = self.get_signatures_of_type(t, ty::SigKind::Call);

            let sigs = self.get_signatures_of_type(t, ty::SigKind::Call);
            self.append_sigs(&mut call_sigs, sigs);
            for index_info in self.get_index_infos_of_ty(t) {
                self.append_index_info(&mut index_infos, index_info, false);
            }
        }

        let m = self.alloc(ty::StructuredMembers {
            members: self.alloc(FxHashMap::default()),
            base_tys: &[],
            call_sigs: if call_sigs.is_empty() {
                self.empty_array()
            } else {
                self.alloc(call_sigs)
            },
            ctor_sigs: self.empty_array(),
            index_infos: if index_infos.is_empty() {
                self.empty_array()
            } else {
                self.alloc(index_infos)
            },
            props: Default::default(),
        });
        self.get_mut_ty_links(ty.id).set_structured_members(m);
    }

    fn append_sigs(&mut self, sigs: &mut Vec<&'cx ty::Sig<'cx>>, new_sigs: &[&'cx ty::Sig<'cx>]) {
        for new_sig in new_sigs {
            if sigs.iter().all(|s| {
                !self.compare_sigs_identical(s, new_sig, false, false, false, |this, s, t| {
                    this.compare_types_identical(s, t)
                }) != Ternary::FALSE
            }) {
                sigs.push(*new_sig);
            }
        }
    }

    pub(super) fn compare_sigs_identical(
        &mut self,
        mut source: &'cx ty::Sig<'cx>,
        target: &'cx ty::Sig<'cx>,
        partial_match: bool,
        ignore_this_tys: bool,
        ignore_return_tys: bool,
        compare_tys: impl Fn(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>) -> Ternary,
    ) -> Ternary {
        if source == target {
            return Ternary::TRUE;
        } else if !self.is_matching_sig(source, target, partial_match) {
            return Ternary::FALSE;
        } else if source.ty_params.map(|t| t.len()) != target.ty_params.map(|t| t.len()) {
            return Ternary::FALSE;
        }
        if let Some(target_ty_params) = target.ty_params {
            let source_ty_params = source.ty_params.unwrap();
            let mapper = self.create_ty_mapper(source_ty_params, target_ty_params);
            for i in 0..target_ty_params.len() {
                let s = source_ty_params[i];
                let t = target_ty_params[i];
                if !(s == t
                    || ({
                        let s = self
                            .get_constraint_of_ty_param(s)
                            .map(|s| self.instantiate_ty(s, Some(mapper)))
                            .unwrap_or(self.unknown_ty);
                        let t = self
                            .get_constraint_of_ty_param(t)
                            .map(|t| self.instantiate_ty(t, Some(mapper)))
                            .unwrap_or(self.unknown_ty);
                        compare_tys(self, s, t) != Ternary::FALSE
                    }) && {
                        let s = self
                            .get_default_ty_from_ty_param(s)
                            .map(|s| self.instantiate_ty(s, Some(mapper)))
                            .unwrap_or(self.unknown_ty);
                        let t = self
                            .get_default_ty_from_ty_param(t)
                            .map(|t| self.instantiate_ty(t, Some(mapper)))
                            .unwrap_or(self.unknown_ty);
                        compare_tys(self, s, t) != Ternary::FALSE
                    })
                {
                    return Ternary::FALSE;
                }
            }
            source = self.instantiate_sig(source, mapper, true);
        }

        let mut result = Ternary::TRUE;
        if !ignore_this_tys {
            // TODO:
        }

        let target_len = target.get_param_count(self);
        for i in 0..target_len {
            let s = self.get_ty_at_pos(source, i);
            let t = self.get_ty_at_pos(target, i);
            let related = compare_tys(self, s, t);
            if related == Ternary::FALSE {
                return Ternary::FALSE;
            }
            result &= related;
        }

        if !ignore_return_tys {
            let source_ty_pred = self.get_ty_predicate_of_sig(source);
            let target_ty_pred = self.get_ty_predicate_of_sig(target);
            result &= if source_ty_pred.is_some() || target_ty_pred.is_some() {
                // TODO:
                Ternary::TRUE
            } else {
                let source_ret_ty = self.get_ret_ty_of_sig(source);
                let target_ret_ty = self.get_ret_ty_of_sig(target);
                compare_tys(self, source_ret_ty, target_ret_ty)
            }
        }

        result
    }

    fn is_matching_sig(
        &mut self,
        source: &'cx ty::Sig<'cx>,
        target: &'cx ty::Sig<'cx>,
        partial_match: bool,
    ) -> bool {
        let source_min_argument_count = self.get_min_arg_count(source);
        let target_min_argument_count = self.get_min_arg_count(target);
        if partial_match && source_min_argument_count <= target_min_argument_count {
            return true;
        } else if source_min_argument_count != target_min_argument_count {
            return false;
        }
        let source_param_count = source.get_param_count(self);
        let target_param_count = target.get_param_count(self);
        if source_param_count != target_param_count {
            return false;
        }
        let source_has_rest_param = self.has_effective_rest_param(source);
        let target_has_rest_param = self.has_effective_rest_param(target);
        if source_has_rest_param != target_has_rest_param {
            return false;
        }
        true
    }

    fn append_index_info(
        &mut self,
        infos: &mut Vec<&'cx ty::IndexInfo<'cx>>,
        new_info: &'cx ty::IndexInfo<'cx>,
        union: bool,
    ) {
        for i in 0..infos.len() {
            let info = infos[i];
            if info.key_ty == new_info.key_ty {
                let val_ty = if union {
                    self.get_union_ty(&[info.val_ty, new_info.val_ty], ty::UnionReduction::Lit)
                } else {
                    self.get_intersection_ty(
                        &[info.val_ty, new_info.val_ty],
                        IntersectionFlags::None,
                        None,
                        None,
                    )
                };
                let is_readonly = if union {
                    info.is_readonly || new_info.is_readonly
                } else {
                    info.is_readonly && new_info.is_readonly
                };
                infos[i] = self.alloc(ty::IndexInfo {
                    key_ty: info.key_ty,
                    val_ty,
                    symbol: info.symbol,
                    is_readonly,
                });
                return;
            }
        }
        infos.push(new_info);
    }

    pub(super) fn get_mapped_ty_name_ty_kind(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> ty::MappedTyNameTyKind {
        let name_ty = self.get_name_ty_from_mapped_ty(ty);
        if let Some(name_ty) = name_ty {
            let target = self.get_ty_param_from_mapped_ty(ty);
            if self.is_type_assignable_to(name_ty, target) {
                ty::MappedTyNameTyKind::Filtering
            } else {
                ty::MappedTyNameTyKind::Remapping
            }
        } else {
            ty::MappedTyNameTyKind::None
        }
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

    pub(super) fn get_modifiers_ty_from_mapped_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let mapped_ty = ty.kind.expect_object_mapped();
        if let Some(t) = self.object_mapped_ty_links_arena[mapped_ty.links].get_modifiers_ty() {
            return t;
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
            let constraint = self.get_constraint_ty_from_mapped_ty(declare_ty);
            let extended_constraint = if constraint.flags.intersects(ty::TypeFlags::TYPE_PARAMETER)
            {
                self.get_constraint_of_ty_param(constraint)
            } else {
                Some(constraint)
            };
            extended_constraint
                .and_then(|extended_constraint| {
                    extended_constraint
                        .kind
                        .as_index_ty()
                        .map(|index| self.instantiate_ty(index.ty, mapped_ty.mapper))
                })
                .unwrap_or(self.undefined_ty)
        };
        self.object_mapped_ty_links_arena[mapped_ty.links].set_modifiers_ty(modifiers_ty);
        modifiers_ty
    }

    fn resolve_mapped_ty_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let mapped_ty = ty.kind.expect_object_mapped();
        let ty_param = self.get_ty_param_from_mapped_ty(ty);
        let constraint_ty = self.get_constraint_ty_from_mapped_ty(ty);
        let (name_ty, should_link_prop_decls, template_ty) = {
            let target = mapped_ty.target.unwrap_or(ty);
            let name_ty = self.get_name_ty_from_mapped_ty(target);
            let should_link_prop_decls =
                self.get_mapped_ty_name_ty_kind(target) != ty::MappedTyNameTyKind::Remapping;
            let template_ty = self.get_template_ty_from_mapped_ty(target);
            (name_ty, should_link_prop_decls, template_ty)
        };
        let modifiers_ty = {
            let ty = self.get_modifiers_ty_from_mapped_ty(ty);
            self.get_apparent_ty(ty)
        };
        let template_modifier = mapped_ty.decl.get_modifiers();

        let mut members: FxHashMap<SymbolName, SymbolID> = fx_hashmap_with_capacity(16);
        let mut index_infos = Vec::with_capacity(4);

        let include = TypeFlags::STRING_OR_NUMBER_LITERAL_OR_UNIQUE;
        let mut add_member_for_key_ty_worker =
            |this: &mut Self, key_ty: &'cx ty::Ty<'cx>, prop_name_ty: &'cx ty::Ty<'cx>| {
                if prop_name_ty.useable_as_prop_name() {
                    let symbol_name = this.get_prop_name_from_ty(prop_name_ty).unwrap();
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
                            let symbol_name = this.get_prop_name_from_ty(key_ty).unwrap();
                            this.get_prop_of_ty(modifiers_ty, symbol_name)
                        } else {
                            None
                        };
                        let is_optional = template_modifier
                            .intersects(MappedTyModifiers::INCLUDE_OPTIONAL)
                            || !template_modifier.intersects(MappedTyModifiers::EXCLUDE_OPTIONAL)
                                && modifiers_prop.is_some_and(|m| {
                                    this.symbol(m).flags.intersects(SymbolFlags::OPTIONAL)
                                });
                        let is_readonly = template_modifier
                            .intersects(MappedTyModifiers::INCLUDE_READONLY)
                            || !template_modifier.intersects(MappedTyModifiers::EXCLUDE_READONLY)
                                && modifiers_prop.is_some_and(|m| this.is_readonly_symbol(m));
                        let strip_optional = *self.config.strict_null_checks()
                            && !is_optional
                            && modifiers_prop.is_some_and(|m| {
                                this.symbol(m).flags.intersects(SymbolFlags::OPTIONAL)
                            });
                        let late_flag = modifiers_prop
                            .map_or(ty::CheckFlags::default(), |m| this.get_late_flag(m));
                        let symbol_flags = SymbolFlags::PROPERTY.union(SymbolFlags::TRANSIENT)
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
                        let declarations = modifiers_prop
                            .and_then(|p| {
                                if should_link_prop_decls {
                                    Some(this.symbol(p).decls.clone())
                                } else {
                                    None
                                }
                            })
                            .unwrap_or_default();
                        let symbol = this.create_transient_symbol(
                            symbol_name,
                            symbol_flags,
                            links,
                            declarations,
                            None,
                        );
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
                    this.append_index_info(&mut index_infos, index_info, true);
                }
            };
        let mut add_member_for_key_ty = |this: &mut Self, key_ty: &'cx ty::Ty<'cx>| {
            let prop_name_ty = if let Some(name_ty) = name_ty {
                let mapper = this.append_ty_mapping(mapped_ty.mapper, ty_param, key_ty);
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
                &mut add_member_for_key_ty,
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
            let object_flags = ty.get_object_flags();
            if object_flags.intersects(ObjectFlags::REFERENCE) {
                self.resolve_reference_members(ty);
            } else if ty.kind.is_object_interface() {
                self.resolve_interface_members(ty);
            } else if ty.kind.is_object_reverse_mapped() {
                self.resolve_reverse_mapped_ty_members(ty)
            } else if ty.kind.is_object_anonymous() {
                self.resolve_anonymous_type_members(ty);
            } else if ty.kind.is_object_mapped() {
                self.resolve_mapped_ty_members(ty);
            } else {
                unreachable!()
            }
        } else if ty.kind.is_union() {
            self.resolve_union_type_members(ty);
        } else if ty.kind.is_intersection() {
            self.resolve_intersection_type_members(ty);
        } else {
            unreachable!("{:#?}", ty)
        };
        assert!(self.get_ty_links(ty.id).get_structured_members().is_some());
    }
}
