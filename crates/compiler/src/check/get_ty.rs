use std::net::ToSocketAddrs;

use bolt_ts_atom::AtomId;

use super::ty::{self, Ty, TyKind};
use super::{errors, Ternary};
use super::{CheckMode, F64Represent, InferenceContextId, PropName, TyChecker};
use crate::ast;

use crate::bind::{SymbolFlags, SymbolID, SymbolName};
use crate::ty::{
    AccessFlags, CheckFlags, ElementFlags, IndexFlags, ObjectFlags, TyMapper, TypeFlags,
};

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_non_missing_type_of_symbol(&mut self, id: SymbolID) -> &'cx Ty<'cx> {
        let ty = self.get_type_of_symbol(id);
        // TODO: resolving missing.
        ty
    }

    pub(crate) fn get_type_of_symbol(&mut self, id: SymbolID) -> &'cx Ty<'cx> {
        if let Some(t) = self.binder.get_transient(id) {
            if let Some(ty) = t.links.get_ty() {
                return ty;
            }
        };

        let check_flags = self.check_flags(id);
        let symbol = self.binder.symbol(id);

        let ty = if check_flags.intersects(CheckFlags::INSTANTIATED) {
            self.get_type_of_instantiated_symbol(id)
        } else if symbol.flags.intersects(SymbolFlags::CLASS) {
            let ty = self.get_type_of_class_decl(id);
            // TODO: delete
            if let Some(ty) = self.get_symbol_links(id).get_ty() {
                return ty;
            }
            self.get_mut_symbol_links(id).set_ty(ty);
            // ---
            ty
        } else if symbol.flags.intersects(SymbolFlags::FUNCTION) {
            self.get_type_of_func_decl(id)
        } else if symbol.is_variable() || symbol.flags.intersects(SymbolFlags::PROPERTY) {
            self.get_type_of_var_like(id)
        } else if symbol.flags == SymbolFlags::OBJECT_LITERAL {
            let ty = self.get_type_of_object(id);
            // TODO: delete
            if let Some(ty) = self.get_symbol_links(id).get_ty() {
                return ty;
            }
            self.get_mut_symbol_links(id).set_ty(ty);
            // ---
            ty
        } else {
            self.any_ty()
        };

        ty
    }

    pub(super) fn get_write_type_of_symbol(&mut self, symbol: SymbolID) -> &'cx Ty<'cx> {
        self.get_type_of_symbol(symbol)
    }

    fn get_type_of_instantiated_symbol(&mut self, symbol: SymbolID) -> &'cx Ty<'cx> {
        let links = self.get_symbol_links(symbol);
        if let Some(ty) = links.get_ty() {
            return ty;
        }

        let target = links.get_target().unwrap();
        let mapper = links.get_ty_mapper();

        let ty = self.get_type_of_symbol(target);
        let ty = self.instantiate_ty(ty, mapper);
        self.get_mut_symbol_links(symbol).set_ty(ty);
        ty
    }

    fn get_type_of_object(&mut self, symbol: SymbolID) -> &'cx Ty<'cx> {
        self.undefined_ty()
    }

    fn get_base_type_variable_of_class(&mut self, symbol: SymbolID) -> Option<&'cx Ty<'cx>> {
        let class_ty = self.get_declared_ty_of_symbol(symbol);
        let base_ctor_ty = self.get_base_constructor_type_of_class(class_ty);
        Some(base_ctor_ty)
    }

    // fn get_intersection_ty(&mut self, tys: &'cx [&'cx Ty<'cx>]) -> &'cx Ty<'cx> {

    // }

    fn get_type_of_class_decl(&mut self, symbol: SymbolID) -> &'cx Ty<'cx> {
        let ty = self.create_anonymous_ty(symbol, ObjectFlags::empty());
        if let Some(base) = self.get_base_type_variable_of_class(symbol) {
            // TODO: get_intersection_ty
            base
        } else {
            ty
        };
        ty
    }

    fn get_type_of_func_decl(&mut self, symbol: SymbolID) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(symbol).get_ty() {
            return ty;
        }
        let ty = self.create_anonymous_ty(symbol, ObjectFlags::empty());
        self.get_mut_symbol_links(symbol).set_ty(ty);
        ty
    }

    pub(crate) fn get_ty_from_inference(
        &mut self,
        inference: InferenceContextId,
        idx: usize,
    ) -> Option<&'cx Ty<'cx>> {
        let inference = &self.inferences[inference.as_usize()].inferences[idx];
        if let Some(tys) = &inference.candidates {
            Some(self.create_union_type(tys.to_vec(), ty::UnionReduction::Subtype))
        } else if let Some(_) = &inference.contra_candidates {
            todo!("intersection")
        } else {
            None
        }
    }

    pub(crate) fn get_ty_from_type_node(&mut self, ty: &ast::Ty<'cx>) -> &'cx Ty<'cx> {
        // TODO: cache
        use ast::TyKind::*;
        match ty.kind {
            Refer(refer) => self.get_ty_from_ty_reference(refer),
            Array(array) => self.get_ty_from_array_node(array),
            Tuple(tuple) => self.get_ty_from_tuple_node(tuple),
            Fn(node) => self.get_ty_from_object_lit_or_fn_or_ctor_ty_node(node.id),
            ObjectLit(node) => self.get_ty_from_object_lit_or_fn_or_ctor_ty_node(node.id),
            Ctor(node) => self.get_ty_from_object_lit_or_fn_or_ctor_ty_node(node.id),
            NumLit(num) => self.get_number_literal_type(num.val),
            StringLit(s) => self.get_string_literal_type(s.val),
            Rest(rest) => self.get_ty_from_rest_ty_node(rest),
            IndexedAccess(node) => self.get_ty_from_indexed_access_node(node),
            Cond(node) => self.get_ty_from_cond_node(node),
            Union(node) => self.get_ty_from_union_ty_node(node),
            Typeof(node) => self.get_ty_from_typeof_node(node),
            Intersection(_) => self.undefined_ty(),
            BooleanLit(_) => todo!(),
            NullLit(_) => todo!(),
            Mapped(_) => self.undefined_ty(),
            TyOp(_) => self.undefined_ty(),
            Pred(_) => self.boolean_ty(),
        }
    }

    fn get_ty_from_object_lit_or_fn_or_ctor_ty_node(&mut self, node: ast::NodeID) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_node_links(node).get_resolved_ty() {
            return ty;
        }
        let ty = self.create_anonymous_ty(self.binder.final_res(node), ObjectFlags::empty());
        self.get_mut_node_links(node).set_resolved_ty(ty);
        ty
    }

    fn get_ty_from_typeof_node(&mut self, node: &'cx ast::TypeofTy<'cx>) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_node_links(node.id).get_resolved_ty() {
            return ty;
        }
        let ty = match node.name.kind {
            ast::EntityNameKind::Ident(ident) => self.check_ident(ident),
            ast::EntityNameKind::Qualified(_) => {
                // TODO: fix
                self.undefined_ty()
            }
        };
        // TODO: ty args
        self.get_mut_node_links(node.id).set_resolved_ty(ty);
        ty
    }

    fn get_ty_from_union_ty_node(&mut self, node: &'cx ast::UnionTy<'cx>) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_node_links(node.id).get_resolved_ty() {
            return ty;
        }
        // let alias_symbol = self.get_alias_symbol_for_ty_node(node.id);
        let tys = node
            .tys
            .iter()
            .map(|ty| self.get_ty_from_type_node(ty))
            .collect::<Vec<_>>();
        let ty = self.create_union_type(tys, ty::UnionReduction::Lit);
        self.get_mut_node_links(node.id).set_resolved_ty(ty);
        ty
    }

    fn get_ty_from_rest_ty_node(&mut self, rest: &'cx ast::RestTy<'cx>) -> &'cx Ty<'cx> {
        self.get_ty_from_type_node(rest.ty)
    }

    fn get_prop_name_from_ty(&self, ty: &'cx Ty<'cx>) -> Option<PropName> {
        if let Some(lit) = ty.kind.as_string_lit() {
            Some(PropName::String(lit.val))
        } else if let Some(lit) = ty.kind.as_number_lit() {
            Some(PropName::Num(lit.val))
        } else {
            None
        }
    }

    fn get_prop_name_from_index(&self, index_ty: &'cx Ty<'cx>) -> Option<PropName> {
        self.get_prop_name_from_ty(index_ty)
    }

    fn get_prop_ty_for_index_ty(
        &mut self,
        origin_object_ty: &'cx Ty<'cx>,
        object_ty: &'cx Ty<'cx>,
        index_ty: &'cx Ty<'cx>,
        access_node: Option<ast::NodeID>,
    ) -> Option<&'cx Ty<'cx>> {
        let access_expr = access_node.filter(|n| self.p.node(*n).is_ele_access_expr());
        let prop_name = self.get_prop_name_from_index(index_ty);
        let symbol_name = if let Some(prop_name) = prop_name {
            match prop_name {
                PropName::String(atom_id) => Some(SymbolName::Ele(atom_id)),
                PropName::Num(num) => {
                    if let Some(tuple) = object_ty.kind.as_object_tuple() {
                        assert!(num.fract() == 0.0);
                        let idx = num as usize;
                        return if idx >= tuple.resolved_ty_args.len() {
                            Some(self.undefined_ty())
                        } else {
                            Some(tuple.resolved_ty_args[idx])
                        };
                    }
                    Some(SymbolName::EleNum(num.into()))
                }
            }
        } else {
            None
        };
        let symbol =
            symbol_name.and_then(|symbol_name| self.get_prop_of_ty(object_ty, symbol_name));
        if let Some(symbol) = symbol {
            return Some(self.get_type_of_symbol(symbol));
        }

        if !index_ty.kind.is_nullable()
            && self.is_type_assignable_to_kind(
                index_ty,
                |t| t.kind.is_string_like() || t.kind.is_number_like(),
                TypeFlags::STRING_LIKE | TypeFlags::NUMBER_LIKE | TypeFlags::ES_SYMBOL_LIKE,
                false,
            ) != Ternary::FALSE
        {
            if object_ty.kind.is_any() {
                return Some(object_ty);
            }
            let index_info = self
                .get_applicable_index_info(object_ty, index_ty)
                .or_else(|| self.get_index_info_of_ty(object_ty, self.string_ty()));
            if let Some(index_info) = index_info {
                return Some(index_info.val_ty);
            }

            if let Some(_) = access_expr {
                return None;
            }
        }

        if let Some(access_node) = access_node {
            let index_node = self.get_index_node_for_access_expr(access_node);
            let span = self.p.node(index_node).span();
            let error = errors::TypeCannotBeUsedAsAnIndexType {
                span,
                ty: self.print_ty(index_ty).to_string(),
            };
            self.push_error(Box::new(error));
        }

        None
    }

    fn get_index_node_for_access_expr(&self, id: ast::NodeID) -> ast::NodeID {
        use ast::Node::*;
        match self.p.node(id) {
            EleAccessExpr(node) => node.arg.id(),
            IndexedAccessTy(node) => node.index_ty.id(),
            _ => id,
        }
    }

    pub(super) fn get_indexed_access_ty(
        &mut self,
        object_ty: &'cx Ty<'cx>,
        index_ty: &'cx Ty<'cx>,
        access_flags: Option<AccessFlags>,
        access_node: Option<ast::NodeID>,
    ) -> Option<&'cx Ty<'cx>> {
        let mut access_flags = access_flags.unwrap_or(AccessFlags::empty());
        let is_generic_index = if index_ty.kind.is_generic_index_ty() {
            true
        } else if let Some(access_node) = access_node {
            if !self.p.node(access_node).is_indexed_access_ty() {
                object_ty.kind.is_generic_tuple_type()
            } else {
                object_ty.kind.is_generic_object()
            }
        } else {
            object_ty.kind.is_generic_object()
        };

        if is_generic_index {
            access_flags.insert(AccessFlags::PERSISTENT);
            let ty = self.alloc(ty::IndexedAccessTy {
                object_ty,
                index_ty,
                access_flags,
            });
            return Some(self.new_ty(ty::TyKind::IndexedAccess(ty)));
        };

        let apparent_object_ty = self.get_reduced_apparent_ty(object_ty);

        if let Some(union) = index_ty.kind.as_union() {
            let mut prop_tys = Vec::with_capacity(union.tys.len());
            for t in union.tys.iter() {
                if let Some(prop_ty) =
                    self.get_prop_ty_for_index_ty(object_ty, apparent_object_ty, t, access_node)
                {
                    prop_tys.push(prop_ty);
                } else if access_node.is_none() {
                    return None;
                }
            }

            return if access_flags.intersects(AccessFlags::WRITING) {
                todo!()
            } else {
                Some(self.create_union_type(prop_tys, ty::UnionReduction::Lit))
            };
        }

        self.get_prop_ty_for_index_ty(object_ty, apparent_object_ty, index_ty, access_node)
    }

    pub(super) fn get_ty_from_indexed_access_node(
        &mut self,
        node: &'cx ast::IndexedAccessTy<'cx>,
    ) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_node_links(node.id).get_resolved_ty() {
            return ty;
        }
        let object_ty = self.get_ty_from_type_node(node.ty);
        let index_ty = self.get_ty_from_type_node(node.index_ty);
        let ty = self
            .get_indexed_access_ty(object_ty, index_ty, None, Some(node.id))
            .unwrap();
        self.get_mut_node_links(node.id).set_resolved_ty(ty);
        ty
    }

    pub(super) fn get_alias_symbol_for_ty_node(&self, node: ast::NodeID) -> Option<SymbolID> {
        assert!(self.p.node(node).is_ty());
        let mut host = self.p.parent(node);
        while let Some(node_id) = host {
            let node = self.p.node(node);
            if node.is_paren_type_node() {
                host = self.p.parent(node_id);
            } else {
                break;
            }
        }
        host.and_then(|node_id| self.p.node(node_id).is_type_decl().then_some(node_id))
            .map(|node_id| {
                let symbol = self.binder.final_res(node_id);
                assert!(self.binder.symbol(symbol).flags == SymbolFlags::TYPE_ALIAS);
                symbol
            })
    }

    pub(super) fn get_local_ty_params_of_class_or_interface_or_type_alias(
        &mut self,
        symbol: SymbolID,
    ) -> Option<ty::Tys<'cx>> {
        let s = self.binder.symbol(symbol);
        let decl = if s.flags == SymbolFlags::TYPE_ALIAS {
            let alias = s.expect_ty_alias();
            alias.decl
        } else if s.flags.intersects(SymbolFlags::INTERFACE) {
            let i = s.expect_interface();
            i.decls[0]
        } else if s.flags.intersects(SymbolFlags::CLASS) {
            let c = s.expect_class();
            c.decl
        } else {
            return None;
        };
        let mut res = vec![];
        let ty_params = self.get_effective_ty_param_decls(decl);
        self.append_ty_params(&mut res, ty_params);
        if res.is_empty() {
            None
        } else {
            Some(self.alloc(res))
        }
    }

    fn is_deferred_ty(&self, ty: &'cx Ty<'cx>, check_tuples: bool) -> bool {
        ty.kind.is_generic()
            || (check_tuples
                && ty.kind.as_object_tuple().map_or(false, |tup| {
                    tup.resolved_ty_args.iter().any(|ty| ty.kind.is_generic())
                }))
    }

    pub(super) fn get_restrictive_ty_param(&self, ty: &'cx Ty<'cx>) -> &'cx Ty<'cx> {
        // TODO:
        ty
    }

    pub fn combine_ty_mappers(
        &self,
        m1: Option<&'cx TyMapper<'cx>>,
        m2: &'cx TyMapper<'cx>,
    ) -> &'cx TyMapper<'cx> {
        if let Some(m1) = m1 {
            self.alloc(TyMapper::make_composite(m1, m2))
        } else {
            m2
        }
    }

    pub fn create_ty_mapper_with_optional_target(
        &self,
        sources: ty::Tys<'cx>,
        targets: Option<ty::Tys<'cx>>,
    ) -> &'cx TyMapper<'cx> {
        if let Some(targets) = targets {
            self.alloc(TyMapper::create(sources, targets))
        } else if sources.len() == 1 {
            let mapper = ty::SimpleTyMapper {
                source: sources[0],
                target: self.any_ty(),
            };
            self.alloc(TyMapper::Simple(mapper))
        } else {
            let mapper = ty::ArrayTyMapper {
                sources,
                targets: None,
            };
            self.alloc(TyMapper::Array(mapper))
        }
    }

    pub fn create_ty_eraser(&self, sources: ty::Tys<'cx>) -> &'cx TyMapper<'cx> {
        self.create_ty_mapper_with_optional_target(sources, None)
    }

    pub(super) fn get_mapped_ty(&mut self, mapper: &TyMapper<'cx>, ty: &'cx Ty) -> &'cx Ty<'cx> {
        match mapper {
            TyMapper::Simple(mapper) => {
                if ty == mapper.source {
                    mapper.target
                } else {
                    ty
                }
            }
            TyMapper::Array(mapper) => {
                for (idx, source) in mapper.sources.iter().enumerate() {
                    assert!(source.kind.is_param());
                    if source.eq(&ty) {
                        if let Some(targets) = &mapper.targets {
                            return targets[idx];
                        } else {
                            return self.any_ty();
                        }
                    }
                }
                ty
            }
            TyMapper::Fn(_) => todo!(),
            TyMapper::Composite(mapper) => {
                let t1 = self.get_mapped_ty(mapper.mapper1, ty);
                if t1 != ty {
                    self.instantiate_ty_with_alias(t1, mapper.mapper2)
                } else {
                    self.get_mapped_ty(mapper.mapper2, t1)
                }
            }
            TyMapper::Merged(_) => todo!(),
        }
    }

    pub(super) fn get_cond_ty(
        &mut self,
        mut root: &'cx ty::CondTyRoot<'cx>,
        mut mapper: Option<&'cx TyMapper<'cx>>,
    ) -> &'cx Ty<'cx> {
        let can_tail_recurse =
            |this: &mut Self, new_ty: &'cx Ty<'cx>, new_mapper: Option<&'cx TyMapper<'cx>>| {
                if let Some(cond) = new_ty.kind.as_cond_ty() {
                    if let Some(new_mapper) = new_mapper {
                        let new_root = cond.root;
                        if let Some(out_ty_params) = new_root.outer_ty_params {
                            let ty_param_mapper = this.combine_ty_mappers(cond.mapper, new_mapper);
                            let ty_args = out_ty_params
                                .iter()
                                .map(|t| this.get_mapped_ty(ty_param_mapper, t))
                                .collect::<Vec<_>>();
                            let new_root_mapper =
                                this.alloc(TyMapper::create(out_ty_params, this.alloc(ty_args)));
                            let use_new = if new_root.is_distributive {
                                let new_check_ty =
                                    this.get_mapped_ty(new_root_mapper, new_root.check_ty);
                                new_check_ty == new_root.check_ty || !new_check_ty.kind.is_union()
                            } else {
                                true
                            };
                            if use_new {
                                return Some((new_root, new_root_mapper));
                            }
                        }
                    }
                }
                None
            };
        let mut tailed = 0;
        let mut extra_tys = Vec::with_capacity(16);
        let result = loop {
            if tailed > 100 {
                panic!()
            }
            let check_ty = self.instantiate_ty(root.check_ty, mapper);
            let extends_ty = self.instantiate_ty(root.extends_ty, mapper);

            let check_tuples = false;
            let effective_check_ty = check_ty;
            let check_ty_deferred = self.is_deferred_ty(effective_check_ty, check_tuples);
            let inferred_extends_ty = extends_ty;

            if !check_ty_deferred && !self.is_deferred_ty(inferred_extends_ty, check_tuples) {
                if !inferred_extends_ty.kind.is_any_or_unknown()
                    && (effective_check_ty.kind.is_any()
                        || self.is_type_assignable_to(
                            self.get_permissive_instantiation(effective_check_ty),
                            self.get_permissive_instantiation(inferred_extends_ty),
                        ) == Ternary::FALSE)
                {
                    if effective_check_ty.kind.is_any() {
                        let ty = self.get_ty_from_type_node(root.node.true_ty);
                        let ty = self.instantiate_ty(ty, mapper);
                        extra_tys.push(ty);
                    }
                    let false_ty = self.get_ty_from_type_node(root.node.false_ty);
                    if false_ty.kind.as_cond_ty().is_some() {
                        if let Some((new_root, new_root_mapper)) =
                            can_tail_recurse(self, false_ty, mapper)
                        {
                            root = new_root;
                            mapper = Some(new_root_mapper);
                            tailed += 1;
                            continue;
                        }
                    }
                    break self.instantiate_ty(false_ty, mapper);
                }

                if inferred_extends_ty.kind.is_any_or_unknown()
                    || self.is_type_assignable_to(
                        self.get_restrictive_instantiation(effective_check_ty),
                        self.get_restrictive_instantiation(inferred_extends_ty),
                    ) != Ternary::FALSE
                {
                    let true_ty = self.get_ty_from_type_node(root.node.true_ty);
                    break self.instantiate_ty(true_ty, mapper);
                }
            }

            let check_ty = self.instantiate_ty(root.check_ty, mapper);
            let extends_ty = self.instantiate_ty(root.extends_ty, mapper);
            let cond_ty = self.alloc(ty::CondTy {
                root,
                check_ty,
                extends_ty,
                mapper: if let Some(mapper) = mapper {
                    Some(mapper)
                } else {
                    None
                },
            });
            break self.new_ty(ty::TyKind::Cond(cond_ty));
        };
        if !extra_tys.is_empty() {
            extra_tys.push(result);
            self.create_union_type(extra_tys, ty::UnionReduction::Lit)
        } else {
            result
        }
    }

    fn is_ty_param_possibly_referenced(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        true
    }

    fn get_ty_from_cond_node(&mut self, node: &'cx ast::CondTy<'cx>) -> &'cx Ty<'cx> {
        let check_ty = self.get_ty_from_type_node(node.check_ty);
        let alias_symbol = self.get_alias_symbol_for_ty_node(node.id);
        let alias_ty_args = alias_symbol.and_then(|symbol| {
            self.get_local_ty_params_of_class_or_interface_or_type_alias(symbol)
        });
        let all_outer_ty_params = self.get_outer_ty_params(node.id, true);
        let outer_ty_params: Option<ty::Tys<'cx>> = if alias_ty_args.is_some() {
            if let Some(all_outer_ty_params) = all_outer_ty_params {
                Some(self.alloc(all_outer_ty_params))
            } else {
                None
            }
        } else {
            all_outer_ty_params.map(|all_outer_ty_params| {
                let ty_params = all_outer_ty_params
                    .into_iter()
                    .filter(|tp| self.is_ty_param_possibly_referenced(tp))
                    .collect::<Vec<_>>();
                let ty_params: ty::Tys<'cx> = self.alloc(ty_params);
                ty_params
            })
        };
        let extends_ty = self.get_ty_from_type_node(node.extends_ty);
        let root = self.alloc(ty::CondTyRoot {
            check_ty,
            extends_ty,
            outer_ty_params,
            node,
            is_distributive: check_ty.kind.is_param(),
        });
        self.get_cond_ty(root, None)
    }

    fn get_ty_from_array_node(&mut self, node: &'cx ast::ArrayTy<'cx>) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_node_links(node.id).get_resolved_ty() {
            return ty;
        }

        let element_ty = self.get_ty_from_type_node(node.ele);
        let ty = self.create_array_ty(element_ty);
        self.get_mut_node_links(node.id).set_resolved_ty(ty);
        ty
    }

    pub(super) fn get_array_ele_ty_node(node: &ast::Ty<'cx>) -> Option<&'cx ast::Ty<'cx>> {
        use ast::TyKind::*;
        match node.kind {
            Tuple(tup) if tup.tys.len() == 1 => {
                let node = tup.tys[0];
                if let Rest(rest) = node.kind {
                    Self::get_array_ele_ty_node(rest.ty)
                } else {
                    None
                }
            }
            Array(arr) => Some(arr.ele),
            _ => None,
        }
    }

    fn get_rest_ty_ele_flags(node: &'cx ast::RestTy<'cx>) -> ElementFlags {
        if Self::get_array_ele_ty_node(node.ty).is_some() {
            ElementFlags::REST
        } else {
            ElementFlags::VARIADIC
        }
    }

    pub fn get_tuple_element_flags(node: &'cx ast::Ty<'cx>) -> ElementFlags {
        use ast::TyKind::*;
        match node.kind {
            Rest(rest) => Self::get_rest_ty_ele_flags(rest),
            _ => ElementFlags::REQUIRED,
        }
    }

    fn get_ty_from_tuple_node(&mut self, node: &'cx ast::TupleTy<'cx>) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_node_links(node.id).get_resolved_ty() {
            return ty;
        }

        let ty_node = self.p.node(node.id).as_ty().unwrap();
        let target = if Self::get_array_ele_ty_node(&ty_node).is_some() {
            self.global_array_ty()
        } else {
            let element_flags: Vec<_> = node
                .tys
                .iter()
                .map(|ty| Self::get_tuple_element_flags(ty))
                .collect();
            let element_flags = self.alloc(element_flags);
            self.get_tuple_target_ty(element_flags, false)
        };

        let ty = if target == self.empty_generic_ty() {
            self.empty_object_ty()
        } else {
            // TODO: deferred type
            let element_tys = node
                .tys
                .iter()
                .map(|ty| self.get_ty_from_type_node(ty))
                .collect::<Vec<_>>();
            let element_tys: ty::Tys<'cx> = self.alloc(element_tys);
            self.create_normalized_ty_reference(target, element_tys)
        };
        self.get_mut_node_links(node.id).set_resolved_ty(ty);
        ty
    }

    pub(super) fn get_number_literal_type(&mut self, val: f64) -> &'cx Ty<'cx> {
        let key = F64Represent::new(val);
        if let Some(id) = self.num_lit_tys.get(&key) {
            self.tys[id.as_usize()]
        } else {
            let kind = TyKind::NumberLit(self.alloc(ty::NumberLitTy { val }));
            let ty = self.new_ty(kind);
            self.num_lit_tys.insert(key, ty.id);
            ty
        }
    }

    pub(super) fn get_string_literal_type(&mut self, val: AtomId) -> &'cx Ty<'cx> {
        if let Some(id) = self.string_lit_tys.get(&val) {
            self.tys[id.as_usize()]
        } else {
            let kind = TyKind::StringLit(self.alloc(ty::StringLitTy { val }));
            let ty = self.new_ty(kind);
            self.string_lit_tys.insert(val, ty.id);
            ty
        }
    }

    pub(super) fn get_global_type(&mut self, name: SymbolName) -> &'cx Ty<'cx> {
        let Some(s) = self.global_symbols.get(name) else {
            unreachable!()
        };
        self.get_declared_ty_of_symbol(s)
    }

    fn get_ret_ty_of_ty_tag(&mut self, id: ast::NodeID) -> Option<&'cx Ty<'cx>> {
        self.get_sig_of_ty_tag(id).map(|_| unreachable!())
    }

    pub fn get_ret_ty_from_anno(&mut self, id: ast::NodeID) -> Option<&'cx ty::Ty<'cx>> {
        let n = self.p.node(id);
        if n.as_class_ctor().is_some() {
            let class = self.p.parent(id).unwrap();
            assert!(self.p.node(class).is_class_like());
            let symbol = self.get_symbol_of_decl(class);
            Some(self.get_declared_ty_of_symbol(symbol))
        } else if let Some(ty_node) = n.ret_ty() {
            Some(self.get_ty_from_type_node(ty_node))
        } else {
            self.get_ret_ty_of_ty_tag(id)
        }
    }

    pub fn get_ret_ty_from_body(&mut self, id: ast::NodeID) -> &'cx ty::Ty<'cx> {
        let n = self.p.node(id);
        let Some(body) = n.fn_body() else {
            return self.error_ty();
        };

        let flags = n.fn_flags();
        let ret_ty;
        if let ast::ArrowFnExprBody::Expr(expr) = body {
            let old = if let Some(check_mode) = self.check_mode {
                let old = self.check_mode;
                self.check_mode = Some(check_mode & !CheckMode::SKIP_GENERIC_FUNCTIONS);
                old
            } else {
                None
            };
            ret_ty = self.check_expr_with_cache(expr);
            self.check_mode = old;
        } else if let ast::ArrowFnExprBody::Block(body) = body {
            let Some(tys) = self.check_and_aggregate_ret_expr_tys(id, body) else {
                return self.undefined_ty();
            };
            if tys.is_empty() {
                if let Some(contextual_ret_ty) = self.get_contextual_ret_ty(id, None) {
                    ret_ty = contextual_ret_ty;
                } else {
                    ret_ty = self.void_ty();
                }
            } else {
                ret_ty = self.create_union_type(tys, ty::UnionReduction::Subtype)
            }
        } else {
            unreachable!()
        };
        ret_ty
    }

    pub(super) fn get_ty_params_for_mapper(&mut self, sig: &'cx ty::Sig<'cx>) -> ty::Tys<'cx> {
        let tys = sig
            .ty_params
            .unwrap_or_default()
            .iter()
            .map(|tp| {
                if let Some(mapper) = self.param_ty_mapper(tp) {
                    self.instantiate_ty(tp, Some(mapper))
                } else {
                    tp
                }
            })
            .collect::<Vec<_>>();
        self.alloc(tys)
    }

    pub(super) fn get_ty_of_expr(&mut self, expr: &'cx ast::Expr<'cx>) -> &'cx ty::Ty<'cx> {
        // TODO: quick check
        let old_check_mode = self.check_mode;
        let ty = self.check_expr(expr);
        self.check_mode = old_check_mode;
        ty
    }

    fn get_literal_ty_from_props(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        include: TypeFlags,
        include_origin: bool,
    ) -> &'cx ty::Ty<'cx> {
        let mut property_tys = self
            .get_props_of_ty(ty)
            .iter()
            .map(|prop| self.get_lit_ty_from_prop(*prop))
            .collect::<Vec<_>>();
        let index_key_tys = self.get_index_infos_of_ty(ty).iter().map(|info| {
            if info.key_ty == self.string_ty() && include.intersects(TypeFlags::NUMBER) {
                self.string_or_number_ty()
            } else {
                info.key_ty
            }
        });
        property_tys.extend(index_key_tys);
        let tys = property_tys;
        self.create_union_type(tys, ty::UnionReduction::Lit)
    }

    fn should_defer_index_ty(&self, ty: &'cx ty::Ty<'cx>, index_flags: IndexFlags) -> bool {
        ty.kind.is_instantiable_non_primitive() || ty.kind.is_generic_tuple_type()
    }

    fn get_index_ty_for_generic_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        index_flags: IndexFlags,
    ) -> &'cx ty::Ty<'cx> {
        if index_flags.intersects(IndexFlags::STRINGS_ONLY) {
            if let Some(ty) = self.get_ty_links(ty.id).get_resolved_string_index_ty() {
                ty
            } else {
                let index_ty = self.create_index_ty(ty, IndexFlags::STRINGS_ONLY);
                self.get_mut_ty_links(ty.id)
                    .set_resolved_string_index_ty(index_ty);
                index_ty
            }
        } else {
            if let Some(ty) = self.get_ty_links(ty.id).get_resolved_index_ty() {
                ty
            } else {
                let index_ty = self.create_index_ty(ty, IndexFlags::empty());
                self.get_mut_ty_links(ty.id).set_resolved_index_ty(index_ty);
                index_ty
            }
        }
    }

    pub(super) fn get_index_ty_of_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        key_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        self.get_index_info_of_ty(ty, key_ty).map(|ty| ty.val_ty)
    }

    pub(super) fn get_index_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        index_flags: IndexFlags,
    ) -> &'cx ty::Ty<'cx> {
        if self.should_defer_index_ty(ty, index_flags) {
            self.get_index_ty_for_generic_ty(ty, index_flags)
        } else {
            let include = if index_flags.intersects(IndexFlags::NO_INDEX_SIGNATURES) {
                TypeFlags::STRING_LITERAL
            } else {
                TypeFlags::STRING_LIKE
            } | if index_flags.intersects(IndexFlags::STRINGS_ONLY) {
                TypeFlags::empty()
            } else {
                TypeFlags::NUMBER_LIKE | TypeFlags::ES_SYMBOL_LIKE
            };
            self.get_literal_ty_from_props(ty, include, index_flags == IndexFlags::empty())
        }
    }
}
