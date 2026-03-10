use bolt_ts_ast::{self as ast, MappedTyModifiers};
use bolt_ts_binder::{Symbol, SymbolFlags, SymbolID, SymbolName, SymbolTable};
use bolt_ts_span::Span;
use bolt_ts_utils::{FxIndexMap, fx_indexmap_with_capacity};

use super::check_type_related_to::RecursionFlags;
use super::create_ty::IntersectionFlags;
use super::cycle_check::{Cycle, ResolutionKey};
use super::infer::InferenceFlags;
use super::links::SigLinks;
use super::symbol_info::SymbolInfo;
use super::ty::{self, CheckFlags, IndexFlags, ObjectFlags, SigFlags, SigID, SigKind, TypeFlags};
use super::{SymbolLinks, Ternary, TyChecker, errors};

#[derive(Debug, Clone, Copy)]
pub(super) enum MemberOrExportsResolutionKind {
    ResolvedExports,
    ResolvedMembers,
}

impl<'cx> TyChecker<'cx> {
    fn add_inherited_members(
        &self,
        members: &mut FxIndexMap<SymbolName, SymbolID>,
        base_symbols: &'cx [SymbolID],
    ) {
        for &base_id in base_symbols {
            let base_name = self.symbol(base_id).name;
            members.entry(base_name).or_insert(base_id);
        }
    }

    fn range_eq<T: PartialEq>(arr1: &[T], arr2: &[T], start: usize, end: usize) -> bool {
        for index in start..end {
            if let Some(item2) = arr2.get(index)
                && arr1[index].eq(item2)
            {
                continue;
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
    ) -> FxIndexMap<SymbolName, SymbolID> {
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
            .collect()
    }

    fn instantiate_symbol(
        &mut self,
        symbol: SymbolID,
        mapper: &'cx dyn ty::TyMap<'cx>,
    ) -> SymbolID {
        if let Some(ty) = self.get_symbol_links(symbol).get_ty()
            && !self.could_contain_ty_var(ty)
        {
            if !self
                .symbol(symbol)
                .flags
                .intersects(SymbolFlags::SET_ACCESSOR)
            {
                return symbol;
            }
            if let Some(write_ty) = self.get_symbol_links(symbol).get_write_ty()
                && !self.could_contain_ty_var(write_ty)
            {
                return symbol;
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
        self.create_transient_symbol(
            name,
            flags | SymbolFlags::TRANSIENT,
            links,
            decls,
            value_declaration,
        )
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
        let ty = self.instantiate_ty_worker(info.val_ty, mapper);
        self.alloc(ty::IndexInfo {
            val_ty: ty,
            ..*info
        })
    }

    fn instantiate_sig_worker(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        mut mapper: &'cx dyn ty::TyMap<'cx>,
        erase_ty_params: bool,
    ) -> (ty::Sig<'cx>, Option<ty::Tys<'cx>>) {
        let mut fresh_ty_params = None;
        if !erase_ty_params && let Some(ty_params) = self.get_sig_links(sig.id).get_ty_params() {
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
        let this_param = sig.this_param.map(|s| self.instantiate_symbol(s, mapper));
        let params = self.instantiate_list(sig.params, mapper, |this, symbol, mapper| {
            this.instantiate_symbol(symbol, mapper)
        });
        let sig = ty::Sig {
            id: SigID::dummy(),
            target: Some(sig),
            mapper: Some(mapper),
            params,
            this_param,
            node_id: sig.node_id,
            flags: sig.flags & SigFlags::PROPAGATING_FLAGS,
            min_args_count: sig.min_args_count,
            ret: sig.ret,
            class_decl: sig.class_decl,
            composite_sigs: None,
            composite_kind: None,
        };
        (sig, fresh_ty_params)
    }

    pub(super) fn instantiate_sig(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
        erase_ty_params: bool,
    ) -> &'cx ty::Sig<'cx> {
        let (sig, fresh_ty_params) = self.instantiate_sig_worker(sig, mapper, erase_ty_params);
        let sig = self.new_sig(sig);
        if let Some(fresh_ty_params) = fresh_ty_params {
            let links = SigLinks::default().with_ty_params(fresh_ty_params);
            let prev = self.sig_links.insert(sig.id, links);
            debug_assert!(prev.is_none());
        }
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
                    let base_ty = self.get_reduced_ty(base_ty);
                    if !self.is_error(base_ty) {
                        if self.is_valid_base_ty(base_ty) {
                            if ty != base_ty && !self.has_base_ty(base_ty, ty) {
                                tys.push(base_ty);
                            } else {
                                *cycle_reported = true;
                                self.report_circular_base_ty(decl, ty, Some(node.name.span()));
                            }
                        } else {
                            let error = errors::AnInterfaceCanOnlyExtendAnObjectTypeOrIntersectionOfObjectTypesWithStaticallyKnownMembers {
                                span: node.span,
                            };
                            self.push_error(Box::new(error));
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

    pub(super) fn get_base_type_node_of_class(
        &self,
        i: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ast::ClassExtendsClause<'cx>> {
        let symbol = i.symbol().unwrap();
        self.get_class_like_decl_of_symbol(symbol)
            .and_then(|decl| self.get_effective_base_type_node(decl))
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
        let base_ctor_ty = self.get_apparent_ty(base_ctor_ty);
        if !base_ctor_ty.flags.intersects(
            TypeFlags::OBJECT
                .union(TypeFlags::INTERSECTION)
                .union(TypeFlags::ANY),
        ) {
            return &[];
        }
        let base_ty_node = self.get_base_type_node_of_class(ty);
        let base_ctor_ty_symbol = base_ctor_ty.symbol();
        let original_base_ty =
            base_ctor_ty_symbol.map(|symbol| self.get_declared_ty_of_symbol(symbol));

        let base_ty;
        if let Some(s) = base_ctor_ty_symbol
            && self.binder.symbol(s).flags.contains(SymbolFlags::CLASS)
            && self.are_all_outer_parameters_applied(original_base_ty.unwrap())
        {
            let base_ty_node = base_ty_node.unwrap();
            let span = base_ty_node.expr_with_ty_args.span;
            let ty_args = base_ty_node.expr_with_ty_args.ty_args;
            base_ty = self.get_ty_from_class_or_interface_reference(
                base_ty_node.id,
                span,
                ty_args,
                None,
                s,
            );
        } else if base_ctor_ty.flags.contains(TypeFlags::ANY) {
            base_ty = base_ctor_ty;
        } else {
            let base_ty_node = base_ty_node.unwrap();
            let ty_args = base_ty_node.expr_with_ty_args.ty_args;
            let ctors = self.get_instantiated_constructors_for_ty_args(
                base_ctor_ty,
                ty_args,
                base_ty_node.id,
            );
            if ctors.is_empty() {
                // TODO: error
                return self.empty_array();
            }
            base_ty = self.get_ret_ty_of_sig(ctors[0]);
        }
        if base_ty == self.error_ty {
            return &[];
        }

        self.alloc([base_ty])
    }

    fn get_tuple_base_ty(&mut self, ty: &'cx ty::TupleTy<'cx>) -> &'cx ty::Ty<'cx> {
        let readonly = ty.readonly;
        let element_tys = self.same_map_tys(ty.ty_params(), |this, t, i| {
            if ty.element_flags[i].intersects(ty::ElementFlags::VARIADIC) {
                this.get_indexed_access_ty(t, self.number_ty, None, None, None, None)
            } else {
                t
            }
        });
        let ty = if let Some(element_tys) = element_tys {
            self.get_union_ty::<false>(element_tys, ty::UnionReduction::Lit, None, None, None)
        } else {
            self.never_ty
        };
        self.create_array_ty(ty, readonly)
    }

    pub(super) fn get_base_tys(&mut self, ty: &'cx ty::Ty<'cx>) -> ty::Tys<'cx> {
        if !ty
            .get_object_flags()
            .intersects(ObjectFlags::CLASS_OR_INTERFACE.union(ObjectFlags::REFERENCE))
        {
            return self.empty_array();
        }
        if let Some(tys) = self.get_ty_links(ty.id).get_resolved_base_tys() {
            return tys;
        }
        if self.push_ty_resolution(ResolutionKey::ResolvedBaseTypes(ty.id)) {
            if let Some(t) = ty.as_tuple() {
                if let Some(old) = self.get_ty_links(ty.id).get_resolved_base_tys() {
                    return old;
                }
                let base_ty = self.get_tuple_base_ty(t);
                let tys = self.alloc([base_ty]);
                self.get_mut_ty_links(ty.id).set_resolved_base_tys(tys);
                return tys;
            }
            let id = ty.symbol().unwrap();
            let symbol = self.binder.symbol(id);
            let mut cycle_reported = false;
            if symbol.flags.contains(SymbolFlags::CLASS) {
                let tys = self.resolve_base_tys_of_class(ty);
                self.get_mut_ty_links(ty.id).set_resolved_base_tys(tys);
            } else if symbol.flags.contains(SymbolFlags::INTERFACE) {
                self.resolve_base_tys_of_interface(ty, &mut cycle_reported);
            } else {
                unreachable!()
            };
            if !cycle_reported
                && let Cycle::Some(_) = self.pop_ty_resolution()
                && let Some(decl) = id.opt_decl(self.binder)
            {
                let p = self.p.node(decl);
                if p.is_class_decl() || p.is_interface_decl() {
                    self.report_circular_base_ty(decl, ty, None);
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

        for base_ty in base_tys {
            let instantiated_base_ty = if let Some(this_arg) = ty_args.last() {
                let ty = self.instantiate_ty(base_ty, mapper);
                self.get_ty_with_this_arg(ty, Some(this_arg), false)
            } else {
                base_ty
            };
            let props = self.get_props_of_ty(instantiated_base_ty);
            self.add_inherited_members(&mut members, props);
            call_sigs.extend(
                self.signatures_of_type(instantiated_base_ty, SigKind::Call)
                    .iter(),
            );
            ctor_sigs.extend(
                self.signatures_of_type(instantiated_base_ty, SigKind::Constructor)
                    .iter(),
            );
            if instantiated_base_ty != self.any_ty {
                let instantiated_index_infos = self
                    .get_index_infos_of_ty(instantiated_base_ty)
                    .into_iter()
                    .filter(|info| self.find_index_info(&index_infos, info.key_ty).is_none())
                    .collect::<Vec<_>>();
                index_infos.extend(instantiated_index_infos);
            } else if {
                let key_ty = self.any_base_type_index_info().key_ty;
                self.find_index_info(&index_infos, key_ty).is_none()
            } {
                index_infos.push(self.any_base_type_index_info());
            }
        }

        let props = self.get_props_from_members(&members);
        let m = self.alloc(ty::StructuredMembers {
            members: self.alloc(members),
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

    pub(super) fn get_class_like_decl_of_symbol(&self, symbol: SymbolID) -> Option<ast::NodeID> {
        let decls = self.binder.symbol(symbol).decls.as_ref()?;
        decls
            .iter()
            .find(|decl| self.p.node(**decl).is_class_like())
            .copied()
    }

    pub(super) fn clone_sig(&mut self, sig: &'cx ty::Sig<'cx>) -> &'cx ty::Sig<'cx> {
        let next = ty::Sig {
            id: SigID::dummy(),
            flags: sig.flags & SigFlags::PROPAGATING_FLAGS,
            params: sig.params,
            this_param: sig.this_param,
            min_args_count: sig.min_args_count,
            ret: sig.ret,
            node_id: sig.node_id,
            target: sig.target,
            mapper: sig.mapper,
            class_decl: sig.class_decl,
            composite_sigs: sig.composite_sigs,
            composite_kind: sig.composite_kind,
        };
        if let Some(ty_params) = self.get_sig_links(sig.id).get_ty_params() {
            let links = SigLinks::default().with_ty_params(ty_params);
            let prev = self.sig_links.insert(next.id, links);
            debug_assert!(prev.is_none());
        }

        self.new_sig(next)
    }

    fn get_default_construct_sigs(
        &mut self,
        class_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx [&'cx ty::Sig<'cx>] {
        let base_ctor_ty = self.get_base_constructor_type_of_class(class_ty);
        let base_sigs = self.get_signatures_of_type(base_ctor_ty, SigKind::Constructor);
        let r = class_ty.kind.expect_object_reference();
        let i = r.target.kind.expect_object_interface();
        let decl = self.get_class_like_decl_of_symbol(i.symbol);
        let is_abstract = decl.is_some_and(|decl| {
            self.p
                .node(decl)
                .has_syntactic_modifier(ast::ModifierKind::Abstract.into())
        });
        if base_sigs.is_empty() {
            let sig = self.new_sig(ty::Sig {
                flags: if is_abstract {
                    SigFlags::ABSTRACT
                } else {
                    SigFlags::empty()
                },
                this_param: None,
                params: self.empty_array(),
                min_args_count: 0,
                ret: None,
                node_id: None,
                target: None,
                mapper: None,
                id: SigID::dummy(),
                class_decl: decl,
                composite_sigs: None,
                composite_kind: None,
            });
            let links = SigLinks::default().with_resolved_ret_ty(class_ty);
            let links = if let Some(ty_params) = i.local_ty_params {
                links.with_ty_params(ty_params)
            } else {
                links
            };
            let prev = self.sig_links.insert(sig.id, links);
            debug_assert!(prev.is_none());
            self.alloc([sig])
        } else if let Some(base_ty_node) = self.get_base_type_node_of_class(class_ty) {
            let is_js = false;
            let ty_args = self.ty_args_from_ty_refer_node(base_ty_node.expr_with_ty_args.ty_args);
            let ty_arg_count = ty_args.map(|t| t.len()).unwrap_or_default();
            let mut res = Vec::with_capacity(base_sigs.len());
            for base_sig in base_sigs {
                let base_sig_ty_params = self.get_sig_links(base_sig.id).get_ty_params();
                let min_ty_argument_count = self.get_min_ty_arg_count(base_sig_ty_params);
                let ty_param_count = base_sig_ty_params.map(|t| t.len()).unwrap_or_default();
                if is_js || ty_arg_count >= min_ty_argument_count && ty_arg_count <= ty_param_count
                {
                    let (sig, fallback_ty_params) = if ty_param_count > 0 {
                        let ty_args = self.fill_missing_ty_args(
                            ty_args,
                            base_sig_ty_params,
                            min_ty_argument_count,
                        );
                        let ty_params = self.get_ty_params_for_mapper(base_sig);
                        let mapper = self.create_ty_mapper_with_optional_target(ty_params, ty_args);
                        let (mut sig, fresh_ty_params) =
                            self.instantiate_sig_worker(base_sig, mapper, true);
                        sig.flags = if is_abstract {
                            sig.flags | SigFlags::ABSTRACT
                        } else {
                            sig.flags & !SigFlags::ABSTRACT
                        };
                        (self.new_sig(sig), fresh_ty_params)
                    } else {
                        let flags = if is_abstract {
                            (base_sig.flags & SigFlags::PROPAGATING_FLAGS) | SigFlags::ABSTRACT
                        } else {
                            (base_sig.flags & SigFlags::PROPAGATING_FLAGS) & !SigFlags::ABSTRACT
                        };
                        let next = ty::Sig {
                            id: SigID::dummy(),
                            flags,
                            params: base_sig.params,
                            this_param: base_sig.this_param,
                            min_args_count: base_sig.min_args_count,
                            ret: base_sig.ret,
                            node_id: base_sig.node_id,
                            target: base_sig.target,
                            mapper: base_sig.mapper,
                            class_decl: base_sig.class_decl,
                            composite_sigs: base_sig.composite_sigs,
                            composite_kind: base_sig.composite_kind,
                        };
                        let ty_params = self.get_sig_links(base_sig.id).get_ty_params();
                        (self.new_sig(next), ty_params)
                    };
                    let links = SigLinks::default().with_resolved_ret_ty(class_ty);
                    let ty_params = i.local_ty_params.or(fallback_ty_params);
                    let links = if let Some(ty_params) = ty_params {
                        links.with_ty_params(ty_params)
                    } else {
                        links
                    };
                    let prev = self.sig_links.insert(sig.id, links);
                    debug_assert!(prev.is_none());
                    res.push(sig);
                }
            }
            self.alloc(res)
        } else {
            unreachable!()
        }
    }

    pub(super) fn get_members_of_symbol(&mut self, symbol: SymbolID) -> &'cx SymbolTable {
        let flags = self.symbol(symbol).flags;
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
            let target_mapped_ty = target.kind.expect_object_mapped();
            let index_ty = self.get_ty_param_from_mapped_ty(target_mapped_ty);
            let ty_param = self.get_indexed_access_ty(
                constraint_index_ty.ty,
                index_ty,
                None,
                None,
                None,
                None,
            );
            let template_ty = self.get_template_ty_from_mapped_ty(target_mapped_ty);
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
        let c = self.get_constraint_ty_from_mapped_ty(ty.mapped_ty.kind.expect_object_mapped());
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
            let a = self.get_number_literal_type_from_number(0.);
            let tys = self.alloc([replacement]);
            let b = self.create_tuple_ty(tys, None, false);
            self.alloc([a, b])
        };
        let mapper = self.create_ty_mapper(sources, targets);
        self.instantiate_ty_worker(instantiable, mapper)
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
                self.alloc([index_info])
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
            if let Some(t) = c_index_ty.ty.kind.as_indexed_access()
                && t.object_ty.flags.intersects(TypeFlags::TYPE_PARAMETER)
                && t.index_ty.flags.intersects(TypeFlags::TYPE_PARAMETER)
            {
                let new_ty_param = t.object_ty;
                let new_mapped_ty =
                    self.replace_indexed_access(r.mapped_ty, t.object_ty, t.index_ty, new_ty_param);
                mapped_ty = new_mapped_ty;
                constraint_ty = self.get_index_ty(new_ty_param, IndexFlags::empty());
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
        if let Some(target) = a.target {
            let mapper = a.mapper.unwrap();
            let members = {
                let props = self.get_props_of_object_ty(target);
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
                call_sigs,
                ctor_sigs,
                index_infos,
                props,
            });
            self.get_mut_ty_links(ty.id).set_structured_members(m);
            return;
        }
        assert!(
            !symbol.flags.contains(SymbolFlags::OBJECT_LITERAL),
            "Object literal should be resolved during check"
        );
        // TODO: get_merged_symbol
        // let symbol_id = self.get_merged_symbol(symbol_id);
        // let symbol = self.symbol(symbol_id);
        if symbol.flags.contains(SymbolFlags::TYPE_LITERAL) {
            let placeholder = self.structure_members_placeholder;
            self.get_mut_ty_links(ty.id)
                .set_structured_members(placeholder);
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
                call_sigs,
                ctor_sigs,
                index_infos,
                props,
            });
            self.get_mut_ty_links(ty.id).override_structured_members(m);
            return;
        }

        // TODO: remove this clone.
        let mut members = self.get_exports_of_symbol(symbol_id).0.clone();
        let index_infos: ty::IndexInfos<'cx>;
        if symbol_id == self.global_this_symbol {
            // TODO:
        }

        let placeholder = self.structure_members_placeholder;
        self.get_mut_ty_links(ty.id)
            .set_structured_members(placeholder);

        let call_sigs: ty::Sigs<'cx>;
        let mut ctor_sigs: ty::Sigs<'cx>;

        let symbol_flags = self.symbol(symbol_id).flags;

        if symbol_flags.intersects(SymbolFlags::FUNCTION.union(SymbolFlags::METHOD)) {
            call_sigs = self.get_sigs_of_symbol(symbol_id);
            ctor_sigs = self.empty_array();
            index_infos = self.empty_array();
        } else if symbol_flags.contains(SymbolFlags::CLASS) {
            call_sigs = self.empty_array();
            if let Some(symbol) = self
                .symbol(symbol_id)
                .members()
                .and_then(|m| m.0.get(&SymbolName::Constructor))
            {
                ctor_sigs = self.get_sigs_of_symbol(*symbol)
            } else {
                ctor_sigs = self.empty_array();
            }
            // let mut base_ctor_index_info = None;
            let class_ty = self.get_declared_ty_of_symbol(symbol_id);
            let base_ctor_ty = self.get_base_constructor_type_of_class(class_ty);
            if base_ctor_ty.flags.intersects(
                TypeFlags::OBJECT
                    .union(TypeFlags::INTERSECTION)
                    .union(TypeFlags::TYPE_VARIABLE),
            ) {
                let props = self.get_props_of_ty(base_ctor_ty);
                self.add_inherited_members(&mut members, props);
            }

            if let Some(index_symbol) = members.get(&SymbolName::Index) {
                index_infos = self.get_index_infos_of_index_symbol(*index_symbol);
            } else {
                index_infos = self.empty_array();
            }

            if ctor_sigs.is_empty() {
                ctor_sigs = self.get_default_construct_sigs(class_ty);
            };
        } else if symbol_flags.intersects(SymbolFlags::ENUM)
            && (self
                .get_declared_ty_of_symbol(symbol_id)
                .flags
                .contains(TypeFlags::ENUM)
                || members.values().any(|prop| {
                    self.get_type_of_symbol(*prop)
                        .flags
                        .intersects(TypeFlags::NUMBER_LIKE)
                }))
        {
            call_sigs = self.empty_array();
            ctor_sigs = self.empty_array();
            index_infos = self.alloc([self.enum_number_index_info()]);
        } else {
            call_sigs = self.empty_array();
            ctor_sigs = self.empty_array();
            index_infos = self.empty_array()
        };

        let props = self.get_props_from_members(&members);
        let m = self.alloc(ty::StructuredMembers {
            members: self.alloc(members),
            call_sigs,
            ctor_sigs,
            index_infos,
            props,
        });
        self.get_mut_ty_links(ty.id).override_structured_members(m);
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
        let index_infos = self.get_union_index_infos(union.tys);
        let m = self.alloc(ty::StructuredMembers {
            members: self.alloc(FxIndexMap::default()),
            call_sigs: self.alloc(call_sigs),
            ctor_sigs: self.alloc(ctor_sigs),
            index_infos,
            props: Default::default(),
        });
        self.get_mut_ty_links(ty.id).set_structured_members(m);
    }

    fn find_mixins(&mut self, tys: ty::Tys<'cx>) -> Vec<bool> {
        let ctor_ty_count = tys.iter().fold(0, |acc, t| {
            if self
                .get_signatures_of_type(t, SigKind::Constructor)
                .is_empty()
            {
                acc
            } else {
                acc + 1
            }
        });
        let mut count = 0;
        let mut mixin_flags = tys
            .iter()
            .map(|ty| {
                if self.is_mixin_constructor_ty(ty) {
                    count += 1;
                    true
                } else {
                    false
                }
            })
            .collect::<Vec<_>>();
        if ctor_ty_count > 0
            && ctor_ty_count == count
            && let Some(first_mixin_index) = mixin_flags.iter().position(|&is_mixin| is_mixin)
        {
            mixin_flags[first_mixin_index] = false;
        }
        mixin_flags
    }

    fn include_mixin_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        tys: ty::Tys<'cx>,
        mixin_flags: &[bool],
        index: usize,
    ) -> &'cx ty::Ty<'cx> {
        let mut mixed_tys = Vec::with_capacity(tys.len());
        for i in 0..tys.len() {
            if i == index {
                mixed_tys.push(ty);
            } else if mixin_flags[i] {
                let sigs = self.get_signatures_of_type(tys[i], SigKind::Constructor);
                let sig = sigs[0];
                let t = self.get_ret_ty_of_sig(sig);
                mixed_tys.push(t);
            }
        }
        self.get_intersection_ty(&mixed_tys, IntersectionFlags::None, None, None)
    }

    fn resolve_intersection_type_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let i = ty.kind.expect_intersection();
        let mut call_sigs = Vec::with_capacity(i.tys.len() / 2);
        let mut ctor_sigs = Vec::with_capacity(i.tys.len() / 2);
        let mut index_infos = Vec::with_capacity(i.tys.len());
        let mixin_flags = self.find_mixins(i.tys);
        let mixin_count = mixin_flags.iter().filter(|&&is_mixin| is_mixin).count();
        for index in 0..i.tys.len() {
            let t = i.tys[index];
            if !mixin_flags[index] {
                let sigs = self.get_signatures_of_type(t, ty::SigKind::Constructor);
                if !sigs.is_empty() && mixin_count > 0 {
                    for sig in sigs {
                        let cloned = self.clone_sig(sig);
                        let ret_ty = self.get_ret_ty_of_sig(sig);
                        let resolved_ret_ty =
                            self.include_mixin_ty(ret_ty, i.tys, &mixin_flags, index);
                        // ensure signature links exist
                        self.get_sig_links(cloned.id);
                        self.get_mut_sig_links(cloned.id)
                            .set_resolved_ret_ty(resolved_ret_ty);
                        self.append_sig(&mut ctor_sigs, cloned);
                    }
                }
            }

            let sigs = self.get_signatures_of_type(t, ty::SigKind::Call);
            self.append_sigs(&mut call_sigs, sigs);
            for index_info in self.get_index_infos_of_ty(t) {
                self.append_index_info(&mut index_infos, index_info, false);
            }
        }

        let m = self.alloc(ty::StructuredMembers {
            members: self.alloc(FxIndexMap::default()),
            call_sigs: if call_sigs.is_empty() {
                self.empty_array()
            } else {
                self.alloc(call_sigs)
            },
            ctor_sigs: if ctor_sigs.is_empty() {
                self.empty_array()
            } else {
                self.alloc(ctor_sigs)
            },
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
            self.append_sig(sigs, new_sig);
        }
    }

    fn append_sig(&mut self, sigs: &mut Vec<&'cx ty::Sig<'cx>>, new_sig: &'cx ty::Sig<'cx>) {
        if sigs.iter().all(|s| {
            self.compare_sigs_identical(s, new_sig, false, false, false, |this, s, t| {
                this.compare_types_identical(s, t)
            }) == Ternary::FALSE
        }) {
            sigs.push(new_sig);
        }
    }

    // TODO: duplicate `TypeRelatedChecker::compare_sigs_identical`
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
        }
        let source_ty_params = self.get_sig_links(source.id).get_ty_params();
        let target_ty_params = self.get_sig_links(target.id).get_ty_params();
        if source_ty_params.map(|t| t.len()) != target_ty_params.map(|t| t.len()) {
            return Ternary::FALSE;
        }
        if let Some(target_ty_params) = target_ty_params {
            let source_ty_params = source_ty_params.unwrap();
            let mapper = self.create_ty_mapper(source_ty_params, target_ty_params);
            for i in 0..target_ty_params.len() {
                let s = source_ty_params[i];
                let t = target_ty_params[i];
                if !(s == t
                    || ({
                        let s = self
                            .get_constraint_of_ty_param(s)
                            .map(|s| self.instantiate_ty_worker(s, mapper))
                            .unwrap_or(self.unknown_ty);
                        let t = self
                            .get_constraint_of_ty_param(t)
                            .map(|t| self.instantiate_ty_worker(t, mapper))
                            .unwrap_or(self.unknown_ty);
                        compare_tys(self, s, t) != Ternary::FALSE
                    }) && {
                        let s = self
                            .get_default_ty_from_ty_param(s)
                            .map(|s| self.instantiate_ty_worker(s, mapper))
                            .unwrap_or(self.unknown_ty);
                        let t = self
                            .get_default_ty_from_ty_param(t)
                            .map(|t| self.instantiate_ty_worker(t, mapper))
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

    pub(super) fn is_matching_sig(
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
        for (i, info) in infos.iter().enumerate() {
            if info.key_ty == new_info.key_ty {
                let val_ty = if union {
                    self.get_union_ty::<false>(
                        &[info.val_ty, new_info.val_ty],
                        ty::UnionReduction::Lit,
                        None,
                        None,
                        None,
                    )
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
        let m = ty.kind.expect_object_mapped();
        let name_ty = self.get_name_ty_from_mapped_ty(m);
        if let Some(name_ty) = name_ty {
            let target = self.get_ty_param_from_mapped_ty(m);
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
        if let ast::TyKind::TypeOp(t) = constraint_decl.kind {
            t.op == ast::TyOpKind::Keyof
        } else {
            false
        }
    }

    pub(super) fn get_modifiers_ty_from_mapped_ty(
        &mut self,
        ty: &'cx ty::MappedTy<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(t) = self.object_mapped_ty_links_arena[ty.links].get_modifiers_ty() {
            return t;
        }
        let modifiers_ty = if self.is_mapped_ty_with_keyof_constraint_decl(ty) {
            let ty_node = if let ast::TyKind::TypeOp(t) =
                self.get_constraint_decl_for_mapped_ty(ty).unwrap().kind
            {
                t.ty
            } else {
                unreachable!()
            };
            let t = self.get_ty_from_type_node(ty_node);
            self.instantiate_ty(t, ty.mapper)
        } else {
            let declare_ty = self.get_ty_from_mapped_ty_node(ty.decl);
            let constraint =
                self.get_constraint_ty_from_mapped_ty(declare_ty.kind.expect_object_mapped());
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
                        .map(|index| self.instantiate_ty(index.ty, ty.mapper))
                })
                .unwrap_or(self.undefined_ty)
        };
        self.object_mapped_ty_links_arena[ty.links].set_modifiers_ty(modifiers_ty);
        modifiers_ty
    }

    fn resolve_mapped_ty_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        let placeholder = self.structure_members_placeholder;
        self.get_mut_ty_links(ty.id)
            .set_structured_members(placeholder);

        let mapped_ty = ty.kind.expect_object_mapped();
        let ty_param = self.get_ty_param_from_mapped_ty(mapped_ty);
        let constraint_ty = self.get_constraint_ty_from_mapped_ty(mapped_ty);
        let (name_ty, should_link_prop_decls, template_ty) = {
            let target = mapped_ty.target.unwrap_or(ty);
            let target_mapped_ty = target.kind.expect_object_mapped();
            let name_ty = self.get_name_ty_from_mapped_ty(target_mapped_ty);
            let should_link_prop_decls =
                self.get_mapped_ty_name_ty_kind(target) != ty::MappedTyNameTyKind::Remapping;
            let template_ty = self.get_template_ty_from_mapped_ty(target_mapped_ty);
            (name_ty, should_link_prop_decls, template_ty)
        };
        let modifiers_ty = {
            let ty = self.get_modifiers_ty_from_mapped_ty(mapped_ty);
            self.get_apparent_ty(ty)
        };
        let template_modifier = mapped_ty.decl.get_modifiers();

        let mut members: FxIndexMap<SymbolName, SymbolID> = fx_indexmap_with_capacity(16);
        let mut index_infos = Vec::with_capacity(4);

        let include = TypeFlags::STRING_OR_NUMBER_LITERAL_OR_UNIQUE;
        let mut add_member_for_key_ty_worker =
            |this: &mut Self, key_ty: &'cx ty::Ty<'cx>, prop_name_ty: &'cx ty::Ty<'cx>| {
                if prop_name_ty.useable_as_prop_name() {
                    let symbol_name = this.get_prop_name_from_ty(prop_name_ty);
                    if let Some(existing_prop) = members.get(&symbol_name) {
                        let named_ty = {
                            let old = this.get_symbol_links(*existing_prop).expect_named_ty();
                            this.get_union_ty::<false>(
                                &[old, prop_name_ty],
                                ty::UnionReduction::Lit,
                                None,
                                None,
                                None,
                            )
                        };
                        this.get_mut_symbol_links(*existing_prop)
                            .override_name_ty(named_ty);

                        let key_ty = {
                            let old = this.get_symbol_links(*existing_prop).expect_key_ty();
                            this.get_union_ty::<false>(
                                &[old, key_ty],
                                ty::UnionReduction::Lit,
                                None,
                                None,
                                None,
                            )
                        };
                        this.get_mut_symbol_links(*existing_prop)
                            .override_key_ty(key_ty);
                    } else {
                        let modifiers_prop = if key_ty.useable_as_prop_name() {
                            let symbol_name = this.get_prop_name_from_ty(key_ty);
                            this.get_prop_of_ty(modifiers_ty, symbol_name)
                        } else {
                            None
                        };
                        let is_optional = template_modifier
                            .contains(MappedTyModifiers::INCLUDE_OPTIONAL)
                            || !template_modifier.contains(MappedTyModifiers::EXCLUDE_OPTIONAL)
                                && modifiers_prop.is_some_and(|m| {
                                    this.symbol(m).flags.intersects(SymbolFlags::OPTIONAL)
                                });
                        let is_readonly = template_modifier
                            .contains(MappedTyModifiers::INCLUDE_READONLY)
                            || !template_modifier.contains(MappedTyModifiers::EXCLUDE_READONLY)
                                && modifiers_prop.is_some_and(|m| this.is_readonly_symbol(m));
                        let strip_optional = self.config.strict_null_checks()
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
                        .intersects(TypeFlags::ANY.union(TypeFlags::ENUM))
                {
                    let index_key_ty = if prop_name_ty
                        .flags
                        .intersects(TypeFlags::ANY.union(TypeFlags::STRING))
                    {
                        this.string_ty
                    } else if prop_name_ty
                        .flags
                        .intersects(TypeFlags::NUMBER.union(TypeFlags::ENUM))
                    {
                        this.number_ty
                    } else {
                        prop_name_ty
                    };
                    let val_ty = {
                        let mapper = this.append_ty_mapping(mapped_ty.mapper, ty_param, key_ty);
                        this.instantiate_ty_worker(template_ty, mapper)
                    };
                    let modifiers_index_info =
                        this.get_applicable_index_info(modifiers_ty, prop_name_ty);
                    let is_readonly = template_modifier
                        .contains(MappedTyModifiers::INCLUDE_READONLY)
                        || !template_modifier.contains(MappedTyModifiers::EXCLUDE_READONLY)
                            && modifiers_index_info.is_some_and(|i| i.is_readonly);
                    let index_info = this.alloc(ty::IndexInfo {
                        key_ty: index_key_ty,
                        val_ty,
                        is_readonly,
                        symbol: Symbol::ERR,
                    });
                    this.append_index_info(&mut index_infos, index_info, true);
                }
            };
        let mut add_member_for_key_ty = |this: &mut Self, key_ty: &'cx ty::Ty<'cx>| {
            let prop_name_ty = if let Some(name_ty) = name_ty {
                let mapper = this.append_ty_mapping(mapped_ty.mapper, ty_param, key_ty);
                this.instantiate_ty_worker(name_ty, mapper)
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
            call_sigs: self.empty_array(),
            ctor_sigs: self.empty_array(),
            index_infos: if index_infos.is_empty() {
                self.empty_array()
            } else {
                self.alloc(index_infos)
            },
            props,
        });
        self.get_mut_ty_links(ty.id).override_structured_members(m);
    }

    pub(super) fn resolve_structured_type_members(&mut self, ty: &'cx ty::Ty<'cx>) {
        if self.get_ty_links(ty.id).get_structured_members().is_some() {
            return;
        }
        if ty.kind.is_object() {
            let object_flags = ty.get_object_flags();
            if object_flags.contains(ObjectFlags::REFERENCE) {
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
