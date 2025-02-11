use bolt_ts_atom::AtomId;

use super::create_ty::IntersectionFlags;
use super::infer::{InferenceFlags, InferencePriority};
use super::ty::{self, Ty, TyKind};
use super::{errors, IndexedAccessTyMap, ResolutionKey};
use super::{CheckMode, F64Represent, InferenceContextId, PropName, TyChecker};
use crate::ast::{self, EntityNameKind};

use crate::bind::{SymbolFlags, SymbolID, SymbolName};
use crate::keyword::is_prim_ty_name;
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
        if let Some(t) = self.get_transient(id) {
            if let Some(ty) = t.links.get_ty() {
                return ty;
            }
        };

        let check_flags = self.get_check_flags(id);
        if check_flags.intersects(CheckFlags::INSTANTIATED) {
            return self.get_type_of_instantiated_symbol(id);
        } else if check_flags.intersects(CheckFlags::MAPPED) {
            // return self.get_type_of_resolved_symbol(id);
        }

        let symbol = self.binder.symbol(id);

        let ty = if symbol.flags.intersects(
            SymbolFlags::FUNCTION
                | SymbolFlags::METHOD
                | SymbolFlags::CLASS
                | SymbolFlags::ENUM
                | SymbolFlags::VALUE_MODULE,
        ) {
            self.get_ty_of_func_class_enum_module(id)
        } else if symbol.is_variable() || symbol.flags.intersects(SymbolFlags::PROPERTY) {
            self.get_type_for_var_like(id)
        } else if symbol.flags == SymbolFlags::OBJECT_LITERAL {
            unreachable!("type literal")
        } else if symbol.flags.intersects(SymbolFlags::ACCESSOR) {
            self.get_ty_of_accessor(id)
        } else {
            self.error_ty
        };

        ty
    }

    fn get_ty_of_func_class_enum_module(&mut self, symbol: SymbolID) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(symbol).get_ty() {
            return ty;
        };
        let s = self.binder.symbol(symbol);
        let decl = symbol.decl(self.binder);
        let ty = self.create_anonymous_ty(symbol, ObjectFlags::empty());

        if s.flags.intersects(SymbolFlags::CLASS) {
            if let Some(base) = self.get_base_type_variable_of_class(symbol) {
                // TODO: get_intersection_ty(&[ty, base, ty])
                ty
            } else {
                ty
            };

            // TODO: delete this branch
            if let Some(ty) = self.get_symbol_links(symbol).get_ty() {
                return ty;
            };
        }
        self.get_mut_symbol_links(symbol).set_ty(ty);
        ty
    }

    pub(super) fn get_ty_of_accessor(&mut self, symbol: SymbolID) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(symbol).get_ty() {
            return ty;
        };
        if !self.push_ty_resolution(ResolutionKey::Type(symbol)) {
            return self.error_ty;
        }
        let s = self.binder.symbol(symbol).expect_getter_setter();
        let getter = s.getter_decl;
        let setter = s.setter_decl;
        let ty = if let Some(getter_ty) = getter
            .and_then(|getter| {
                let getter = self.p.node(getter).expect_getter_decl();
                getter.ty
            })
            .map(|getter_ty| self.get_ty_from_type_node(getter_ty))
        {
            Some(getter_ty)
        } else {
            setter
                .and_then(|setter| {
                    let setter = self.p.node(setter).expect_setter_decl();
                    setter.params[0].ty
                })
                .map(|setter_ty| self.get_ty_from_type_node(setter_ty))
        };

        let ty = if let Some(ty) = ty {
            ty
        } else {
            // TODO: error
            self.any_ty
        };
        if self.pop_ty_resolution().has_cycle() {
            todo!("cycle")
        }
        self.get_mut_symbol_links(symbol).set_ty(ty);
        ty
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

    fn get_base_type_variable_of_class(&mut self, symbol: SymbolID) -> Option<&'cx Ty<'cx>> {
        let class_ty = self.get_declared_ty_of_symbol(symbol);
        let base_ctor_ty = self.get_base_constructor_type_of_class(class_ty);
        Some(base_ctor_ty)
    }

    pub(crate) fn get_ty_from_inference(
        &mut self,
        inference: InferenceContextId,
        idx: usize,
    ) -> Option<&'cx Ty<'cx>> {
        let inference = &self.inferences[inference.as_usize()].inferences[idx];
        if let Some(tys) = &inference.candidates {
            let tys = tys.clone();
            Some(self.get_union_ty(&tys, ty::UnionReduction::Subtype))
        } else if inference.contra_candidates.is_some() {
            todo!("intersection")
        } else {
            None
        }
    }

    pub(crate) fn get_ty_from_type_node(&mut self, node: &ast::Ty<'cx>) -> &'cx Ty<'cx> {
        use ast::TyKind::*;
        let ty = match node.kind {
            Refer(node) => self.get_ty_from_ty_reference(node),
            Array(node) => self.get_ty_from_array_node(node),
            Tuple(node) => self.get_ty_from_tuple_node(node),
            Fn(node) => self.get_ty_from_object_lit_or_fn_or_ctor_ty_node(node.id),
            ObjectLit(node) => self.get_ty_from_object_lit_or_fn_or_ctor_ty_node(node.id),
            Ctor(node) => self.get_ty_from_object_lit_or_fn_or_ctor_ty_node(node.id),
            Rest(rest) => self.get_ty_from_rest_ty_node(rest),
            IndexedAccess(node) => self.get_ty_from_indexed_access_node(node),
            Cond(node) => self.get_ty_from_cond_node(node),
            Union(node) => self.get_ty_from_union_ty_node(node),
            Typeof(node) => self.get_ty_from_typeof_node(node),
            Intersection(node) => self.get_ty_from_intersection_ty_node(node),
            TyOp(node) => self.get_ty_from_ty_op(node),
            Pred(_) => self.boolean_ty(),
            Lit(node) => {
                use ast::LitTyKind::*;
                match node.kind {
                    Null => self.null_ty,
                    True => self.true_ty,
                    False => self.false_ty,
                    Undefined => self.undefined_ty,
                    Void => self.void_ty,
                    Num(n) => self.get_number_literal_type(n),
                    String(n) => self.get_string_literal_type(n),
                }
            }
            Paren(n) => self.get_ty_from_type_node(n.ty),
            Infer(n) => self.get_ty_from_infer_ty_node(n),
            Mapped(n) => self.get_ty_from_mapped_ty_node(n),
        };

        self.get_conditional_flow_of_ty(ty, node.id())
    }

    pub(super) fn get_ty_from_mapped_ty_node(
        &mut self,
        node: &'cx ast::MappedTy<'cx>,
    ) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_node_links(node.id).get_resolved_ty() {
            return ty;
        };
        let alias_symbol = self.get_alias_symbol_for_ty_node(node.id);
        let alias_ty_arguments = self.get_ty_args_for_alias_symbol(alias_symbol);
        let ty_param = {
            let param_ty = self.get_symbol_of_decl(node.ty_param.id);
            self.get_declared_ty_of_ty_param(param_ty)
        };
        let constraint_ty = self
            .get_constraint_of_ty_param(ty_param)
            .unwrap_or(self.error_ty);
        let ty = self.create_mapper_ty(
            node,
            alias_symbol,
            alias_ty_arguments,
            ty_param,
            constraint_ty,
        );
        self.get_mut_node_links(node.id).set_resolved_ty(ty);
        ty
    }

    fn get_ty_from_infer_ty_node(&mut self, node: &'cx ast::InferTy<'cx>) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_node_links(node.id).get_resolved_ty() {
            return ty;
        }
        let symbol = self.get_symbol_of_decl(node.ty_param.id);
        let ty = self.get_declared_ty_of_ty_param(symbol);
        self.get_mut_node_links(node.id).set_resolved_ty(ty);
        ty
    }

    fn get_actual_ty_variable(&self, ty: &'cx Ty<'cx>) -> &'cx Ty<'cx> {
        if let Some(sub) = ty.kind.as_substitution_ty() {
            return self.get_actual_ty_variable(sub.base_ty);
        } else if let Some(indexed_access) = ty.kind.as_indexed_access() {
            if indexed_access
                .object_ty
                .flags
                .intersects(TypeFlags::SUBSTITUTION)
                || indexed_access
                    .index_ty
                    .flags
                    .intersects(TypeFlags::SUBSTITUTION)
            {
                // TODO:
            }
        }
        ty
    }

    fn get_implied_constraint(
        &mut self,
        ty: &'cx Ty<'cx>,
        check_ty: &'cx ast::Ty<'cx>,
        extends_ty: &'cx ast::Ty<'cx>,
    ) -> Option<&'cx Ty<'cx>> {
        if let Some(check_ty) = check_ty.as_unary_tuple_ty() {
            if let Some(extends_ty) = extends_ty.as_unary_tuple_ty() {
                return self.get_implied_constraint(ty, check_ty, extends_ty);
            }
        }
        let ty_of_check_ty = self.get_ty_from_type_node(check_ty);
        if self.get_actual_ty_variable(ty_of_check_ty) == ty {
            Some(self.get_ty_from_type_node(extends_ty))
        } else {
            None
        }
    }

    pub fn get_conditional_flow_of_ty(
        &mut self,
        ty: &'cx Ty<'cx>,
        mut node: ast::NodeID,
    ) -> &'cx Ty<'cx> {
        let mut constraints = Vec::with_capacity(8);
        let mut covariant = true;
        while !self.p.node(node).is_stmt() {
            let Some(parent) = self.p.parent(node) else {
                break;
            };
            let parent_node = self.p.node(parent);
            if parent_node.is_param_decl() {
                covariant = !covariant;
            }
            if covariant || ty.flags.intersects(TypeFlags::TYPE_VARIABLE) {
                if let Some(cond) = parent_node.as_cond_ty() {
                    if cond.true_ty.id() == node {
                        if let Some(constraint) =
                            self.get_implied_constraint(ty, cond.check_ty, cond.extends_ty)
                        {
                            constraints.push(constraint);
                        }
                    }
                }
            } else if ty.flags.intersects(TypeFlags::TYPE_PARAMETER) {
                // TODO: mapped_ty
            }
            node = parent;
        }
        if !constraints.is_empty() {
            let constraint =
                self.get_intersection_ty(&constraints, IntersectionFlags::None, None, None);
            self.get_substitution_ty(ty, constraint)
        } else {
            ty
        }
    }

    fn get_ty_from_ty_op(&mut self, node: &'cx ast::TyOp<'cx>) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_node_links(node.id).get_resolved_ty() {
            return ty;
        }
        let ty = match node.op {
            ast::TyOpKind::Keyof => self.undefined_ty,
            ast::TyOpKind::Unique => self.undefined_ty,
            ast::TyOpKind::Readonly => self.get_ty_from_type_node(node.ty),
        };
        self.get_mut_node_links(node.id).set_resolved_ty(ty);
        ty
    }

    fn get_ty_from_intersection_ty_node(
        &mut self,
        node: &'cx ast::IntersectionTy<'cx>,
    ) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_node_links(node.id).get_resolved_ty() {
            return ty;
        }
        let alias_symbol = self.get_alias_symbol_for_ty_node(node.id);
        let tys = node
            .tys
            .iter()
            .map(|ty| self.get_ty_from_type_node(ty))
            .collect::<Vec<_>>();
        let empty_index = if tys.len() == 2 {
            tys.iter().position(|ty| *ty == self.empty_ty_literal_ty())
        } else {
            None
        };
        let t = if let Some(empty_index) = empty_index {
            tys[1 - empty_index]
        } else {
            self.unknown_ty
        };
        let no_super_ty_reduction = t
            .flags
            .intersects(TypeFlags::STRING | TypeFlags::NUMBER | TypeFlags::BIG_INT);
        let flags = if no_super_ty_reduction {
            IntersectionFlags::NoSuperTypeReduction
        } else {
            IntersectionFlags::None
        };
        let alias_symbol_ty_args = self.get_ty_args_for_alias_symbol(alias_symbol);
        let ty = self.get_intersection_ty(&tys, flags, alias_symbol, alias_symbol_ty_args);
        self.get_mut_node_links(node.id).set_resolved_ty(ty);
        ty
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
                self.undefined_ty
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
        let ty = self.get_union_ty(&tys, ty::UnionReduction::Lit);
        self.get_mut_node_links(node.id).set_resolved_ty(ty);
        ty
    }

    fn get_ty_from_rest_ty_node(&mut self, rest: &'cx ast::RestTy<'cx>) -> &'cx Ty<'cx> {
        self.get_ty_from_type_node(rest.ty)
    }

    pub(super) fn get_prop_name_from_ty(&self, ty: &'cx Ty<'cx>) -> Option<PropName> {
        if let Some(lit) = ty.kind.as_string_lit() {
            Some(PropName::String(lit.val))
        } else {
            ty.kind.as_number_lit().map(|lit| PropName::Num(lit.val))
        }
    }

    fn get_prop_name_from_index(&self, index_ty: &'cx Ty<'cx>) -> Option<PropName> {
        self.get_prop_name_from_ty(index_ty)
    }

    pub(super) fn get_symbol_name_from_prop_name(&self, prop_name: PropName) -> SymbolName {
        match prop_name {
            PropName::String(atom_id) => SymbolName::Normal(atom_id),
            PropName::Num(num) => SymbolName::EleNum(num.into()),
        }
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
                PropName::String(atom_id) => {
                    let name = if object_ty
                        .symbol()
                        .map(|symbol| {
                            self.binder
                                .symbol(symbol)
                                .flags
                                .intersects(SymbolFlags::MODULE)
                        })
                        .unwrap_or_default()
                    {
                        SymbolName::Normal(atom_id)
                    } else {
                        SymbolName::Ele(atom_id)
                    };
                    Some(name)
                }
                PropName::Num(num) => {
                    if object_ty.is_tuple() {
                        let resolved_ty_args = self.get_ty_arguments(object_ty);
                        assert!(num.fract() == 0.0);
                        let idx = num as usize;
                        return if idx >= resolved_ty_args.len() {
                            Some(self.undefined_ty)
                        } else {
                            Some(resolved_ty_args[idx])
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

        if !index_ty.flags.intersects(TypeFlags::NULLABLE)
            && self.is_type_assignable_to_kind(
                index_ty,
                TypeFlags::STRING_LIKE | TypeFlags::NUMBER_LIKE | TypeFlags::ES_SYMBOL_LIKE,
                false,
            )
        {
            if object_ty.flags.intersects(TypeFlags::ANY)
                || object_ty.flags.intersects(TypeFlags::NEVER)
            {
                return Some(object_ty);
            }
            let index_info = self
                .get_applicable_index_info(object_ty, index_ty)
                .or_else(|| self.get_index_info_of_ty(object_ty, self.string_ty));
            if let Some(index_info) = index_info {
                return Some(index_info.val_ty);
            }

            if access_expr.is_some() {
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
    ) -> &'cx Ty<'cx> {
        if let Some(ty) =
            self.get_indexed_access_ty_or_undefined(object_ty, index_ty, access_flags, access_node)
        {
            ty
        } else if access_node.is_some() {
            self.error_ty
        } else {
            self.undefined_ty
        }
    }

    pub(super) fn get_indexed_access_ty_or_undefined(
        &mut self,
        mut object_ty: &'cx Ty<'cx>,
        index_ty: &'cx Ty<'cx>,
        access_flags: Option<AccessFlags>,
        access_node: Option<ast::NodeID>,
    ) -> Option<&'cx Ty<'cx>> {
        if object_ty == self.wildcard_ty || index_ty == self.wildcard_ty {
            return Some(self.undefined_ty);
        }
        object_ty = self.get_reduced_ty(object_ty);
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
            let persistent_access_flags = access_flags.intersection(AccessFlags::PERSISTENT);
            let id = IndexedAccessTyMap::create_id(persistent_access_flags, object_ty, index_ty);
            if let Some(ty) = self.indexed_access_tys.get(id) {
                return Some(ty);
            }
            let ty = self.alloc(ty::IndexedAccessTy {
                object_ty,
                index_ty,
                access_flags: persistent_access_flags,
            });
            let ty = self.new_ty(ty::TyKind::IndexedAccess(ty), TypeFlags::INDEXED_ACCESS);
            self.indexed_access_tys.insert(id, ty);
            return Some(ty);
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
                Some(self.get_union_ty(&prop_tys, ty::UnionReduction::Lit))
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
        let ty = self.get_indexed_access_ty(object_ty, index_ty, None, Some(node.id));
        self.get_mut_node_links(node.id).set_resolved_ty(ty);
        ty
    }

    pub fn may_resolve_ty_alias(&self, node: ast::NodeID) -> bool {
        use ast::Node::*;
        match self.p.node(node) {
            ReferTy(n) => {
                if let EntityNameKind::Ident(i) = n.name.kind {
                    if is_prim_ty_name(i.name) {
                        return false;
                    }
                }
                let id = n.name.id();
                let s = self.binder.final_res(id);
                self.symbol(s).flags().intersects(SymbolFlags::TYPE_ALIAS)
            }
            TyOp(n) => n.op != ast::TyOpKind::Unique && self.may_resolve_ty_alias(n.ty.id()),
            RestTy(n) => {
                if let ast::TyKind::Array(t) = n.ty.kind {
                    self.may_resolve_ty_alias(t.ele.id())
                } else {
                    true
                }
            }
            UnionTy(n) => n.tys.iter().any(|ty| self.may_resolve_ty_alias(ty.id())),
            IntersectionTy(n) => n.tys.iter().any(|ty| self.may_resolve_ty_alias(ty.id())),
            IndexedAccessTy(n) => {
                self.may_resolve_ty_alias(n.ty.id()) || self.may_resolve_ty_alias(n.index_ty.id())
            }
            CondTy(n) => {
                self.may_resolve_ty_alias(n.check_ty.id())
                    || self.may_resolve_ty_alias(n.extends_ty.id())
                    || self.may_resolve_ty_alias(n.true_ty.id())
                    || self.may_resolve_ty_alias(n.false_ty.id())
            }
            _ => false,
        }
    }

    pub(super) fn is_deferred_ty_reference_node(
        &self,
        node: ast::NodeID,
        has_default_ty_arguments: bool,
    ) -> bool {
        assert!(self.p.node(node).is_ty());
        self.get_alias_symbol_for_ty_node(node).is_some()
            || self.p.is_resolved_by_ty_alias(node) && {
                use ast::Node::*;
                let n = self.p.node(node);
                match n {
                    ArrayTy(n) => self.may_resolve_ty_alias(n.ele.id()),
                    TupleTy(n) => n.tys.iter().any(|n| self.may_resolve_ty_alias(n.id())),
                    _ if has_default_ty_arguments => true,
                    _ => {
                        if let Some(ty_args) = n.ty_args() {
                            ty_args
                                .list
                                .iter()
                                .any(|n| self.may_resolve_ty_alias(n.id()))
                        } else {
                            false
                        }
                    }
                }
            }
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

    pub(super) fn get_ty_args_for_alias_symbol(
        &mut self,
        symbol: Option<SymbolID>,
    ) -> Option<ty::Tys<'cx>> {
        symbol
            .and_then(|symbol| self.get_local_ty_params_of_class_or_interface_or_type_alias(symbol))
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

    pub(super) fn get_ty_reference_arity(ty: &'cx ty::ReferenceTy<'cx>) -> usize {
        let ty_params = if let Some(t) = ty.target.kind.as_object_tuple() {
            t.ty_params()
        } else if let Some(i) = ty.target.kind.as_object_interface() {
            i.ty_params
        } else if let Some(r) = ty.target.kind.as_object_reference() {
            return Self::get_ty_reference_arity(r);
        } else {
            unreachable!()
        };
        ty_params.map_or(0, |ty_params| ty_params.len())
    }

    fn get_element_tys(&mut self, ty: &'cx Ty<'cx>) -> ty::Tys<'cx> {
        if !ty.is_tuple() {
            return Default::default();
        };
        let Some(r) = ty.kind.as_object_reference() else {
            return Default::default();
        };
        let arity = Self::get_ty_reference_arity(r);
        let ty_args = self.get_ty_arguments(ty);
        if ty_args.len() == arity {
            ty_args
        } else {
            &ty_args[0..arity]
        }
    }

    fn is_deferred_ty(&mut self, ty: &'cx Ty<'cx>, check_tuples: bool) -> bool {
        ty.kind.is_generic()
            || (check_tuples
                && ty.is_tuple()
                && self.get_element_tys(ty).iter().any(|t| t.kind.is_generic()))
    }

    pub fn combine_ty_mappers(
        &self,
        m1: Option<&'cx dyn ty::TyMap<'cx>>,
        m2: &'cx dyn ty::TyMap<'cx>,
    ) -> &'cx dyn ty::TyMap<'cx> {
        if let Some(m1) = m1 {
            let mapper = ty::CompositeTyMapper {
                mapper1: m1,
                mapper2: m2,
            };
            self.alloc(mapper)
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
            self.create_ty_mapper(sources, targets)
        } else if sources.len() == 1 {
            let mapper = TyMapper::make_unary(sources[0], self.any_ty);
            self.alloc(mapper)
        } else {
            let mapper = ty::ArrayTyMapper::new(sources, None, self);
            self.alloc(TyMapper::Array(mapper))
        }
    }

    pub fn create_ty_eraser(&self, sources: ty::Tys<'cx>) -> &'cx TyMapper<'cx> {
        self.create_ty_mapper_with_optional_target(sources, None)
    }

    pub(super) fn get_mapped_ty(
        &mut self,
        mapper: &'cx dyn ty::TyMap<'cx>,
        ty: &'cx Ty<'cx>,
    ) -> &'cx Ty<'cx> {
        mapper.get_mapped_ty(ty, self)
    }

    pub(super) fn get_true_ty_from_cond_ty(
        &mut self,
        ty: &'cx Ty<'cx>,
        cond_ty: &'cx ty::CondTy<'cx>,
    ) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_ty_links(ty.id).get_resolved_true_ty() {
            return ty;
        }
        let true_ty = self.get_ty_from_type_node(cond_ty.root.node.true_ty);
        let true_ty = self.instantiate_ty(true_ty, cond_ty.mapper);
        self.get_mut_ty_links(ty.id).set_resolved_true_ty(true_ty);
        true_ty
    }

    pub(super) fn get_false_ty_from_cond_ty(
        &mut self,
        ty: &'cx Ty<'cx>,
        cond_ty: &'cx ty::CondTy<'cx>,
    ) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_ty_links(ty.id).get_resolved_false_ty() {
            return ty;
        }
        let false_ty = self.get_ty_from_type_node(cond_ty.root.node.false_ty);
        let false_ty = self.instantiate_ty(false_ty, cond_ty.mapper);
        self.get_mut_ty_links(ty.id).set_resolved_false_ty(false_ty);
        false_ty
    }

    pub(super) fn get_constraint_of_distributive_cond_ty(
        &mut self,
        ty: &'cx Ty<'cx>,
    ) -> Option<&'cx Ty<'cx>> {
        let cond_ty = ty.kind.expect_cond_ty();
        if let Some(ty) = self
            .get_ty_links(ty.id)
            .get_resolved_constraint_of_distribute()
        {
            return ty;
        }
        if cond_ty.root.is_distributive
            && self.ty_links[&ty.id]
                .get_restrictive_instantiation()
                .map_or(true, |t| t != ty)
        {
            let simplified = self.get_simplified_ty(cond_ty.check_ty, false);
            let constraint = if simplified == cond_ty.check_ty {
                self.get_constraint_of_ty(simplified)
            } else {
                None
            };
            if let Some(constraint) = constraint {
                if constraint != cond_ty.check_ty {
                    let mapper =
                        self.prepend_ty_mapping(cond_ty.root.check_ty, constraint, cond_ty.mapper);
                    let instantiated = self.get_cond_ty_instantiation(ty, mapper, None, None);
                    self.get_mut_ty_links(ty.id)
                        .set_resolved_constraint_of_distribute(Some(instantiated));
                    return Some(instantiated);
                }
            }
        }
        self.get_mut_ty_links(ty.id)
            .set_resolved_constraint_of_distribute(None);
        None
    }

    pub(super) fn get_constraint_of_cond_ty(&mut self, ty: &'cx Ty<'cx>) -> Option<&'cx Ty<'cx>> {
        if self.has_non_circular_constraint(ty) {
            self.get_constraint_from_cond_ty(ty)
        } else {
            None
        }
    }

    pub(super) fn get_cond_ty(
        &mut self,
        mut root: &'cx ty::CondTyRoot<'cx>,
        mut mapper: Option<&'cx dyn ty::TyMap<'cx>>,
        mut alias_symbol: Option<SymbolID>,
        mut alias_ty_args: Option<ty::Tys<'cx>>,
    ) -> &'cx Ty<'cx> {
        let can_tail_recurse =
            |this: &mut Self, new_ty: &'cx Ty<'cx>, new_mapper: Option<&'cx dyn ty::TyMap<'cx>>| {
                let Some(cond) = new_ty.kind.as_cond_ty() else {
                    return None;
                };
                let Some(new_mapper) = new_mapper else {
                    return None;
                };
                let new_root = cond.root;
                let Some(outer_ty_params) = new_root.outer_ty_params else {
                    return None;
                };
                let ty_param_mapper = this.combine_ty_mappers(cond.mapper, new_mapper);
                let ty_args = outer_ty_params
                    .iter()
                    .map(|t| this.get_mapped_ty(ty_param_mapper, t))
                    .collect::<Vec<_>>();
                let new_root_mapper = this.create_ty_mapper(outer_ty_params, this.alloc(ty_args));
                let new_check_ty = new_root
                    .is_distributive
                    .then(|| this.get_mapped_ty(new_root_mapper, new_root.check_ty));
                let use_new = if let Some(new_check_ty) = new_check_ty {
                    new_check_ty == new_root.check_ty
                        || !new_check_ty
                            .flags
                            .intersects(TypeFlags::UNION | TypeFlags::NEVER)
                } else {
                    true
                };
                if use_new {
                    Some((new_root, new_root_mapper))
                } else {
                    None
                }
            };
        let mut tailed = 0;
        let mut extra_tys = Vec::with_capacity(16);
        let result = loop {
            if tailed > 100 {
                panic!()
            }
            let check_ty = self.instantiate_ty(self.get_actual_ty_variable(root.check_ty), mapper);
            let extends_ty = self.instantiate_ty(root.extends_ty, mapper);
            if check_ty == self.error_ty || extends_ty == self.error_ty {
                return self.error_ty;
            } else if check_ty == self.wildcard_ty || extends_ty == self.wildcard_ty {
                return self.wildcard_ty;
            }
            let check_ty_node = root.node.check_ty.skip_ty_parens();
            let extends_ty_node = root.node.extends_ty.skip_ty_parens();
            let check_tuples =
                check_ty_node.is_simple_tuple_ty() && extends_ty_node.is_simple_tuple_ty() && {
                    let ast::TyKind::Tuple(a) = check_ty_node.kind else {
                        unreachable!()
                    };
                    let ast::TyKind::Tuple(b) = check_ty_node.kind else {
                        unreachable!()
                    };
                    a.tys.len() == b.tys.len()
                };
            let check_ty_deferred = self.is_deferred_ty(check_ty, check_tuples);
            let mut combined_mapper = None;
            if let Some(infer_ty_params) = root.infer_ty_params {
                let context =
                    self.create_inference_context(infer_ty_params, None, InferenceFlags::empty());
                if let Some(mapper) = mapper {
                    let non_fixing_mapper = self.inference(context).non_fixing_mapper;
                    let m = self.combine_ty_mappers(Some(non_fixing_mapper), mapper);
                    self.inferences[context.as_usize()].non_fixing_mapper = m;
                }
                if !check_ty_deferred {
                    self.infer_tys(
                        context,
                        check_ty,
                        extends_ty,
                        Some(InferencePriority::NO_CONSTRAINTS | InferencePriority::ALWAYS_STRICT),
                        false,
                    );
                }

                let m = self.inference(context).mapper;
                combined_mapper = if let Some(mapper) = mapper {
                    Some(self.combine_ty_mappers(Some(m), mapper))
                } else {
                    Some(m)
                };
            }
            let inferred_extends_ty = if let Some(combined_mapper) = combined_mapper {
                self.instantiate_ty(root.extends_ty, Some(combined_mapper))
            } else {
                extends_ty
            };
            if !check_ty_deferred && !self.is_deferred_ty(inferred_extends_ty, check_tuples) {
                if !inferred_extends_ty
                    .flags
                    .intersects(TypeFlags::ANY_OR_UNKNOWN)
                    && (check_ty.flags.intersects(TypeFlags::ANY) || {
                        let source = self.get_permissive_instantiation(check_ty);
                        let target = self.get_permissive_instantiation(inferred_extends_ty);
                        !self.is_type_assignable_to(source, target)
                    })
                {
                    if check_ty.flags.intersects(TypeFlags::ANY) {
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

                if inferred_extends_ty
                    .flags
                    .intersects(TypeFlags::ANY_OR_UNKNOWN)
                    || {
                        let source = self.get_restrictive_instantiation(check_ty);
                        let target = self.get_restrictive_instantiation(inferred_extends_ty);
                        self.is_type_assignable_to(source, target)
                    }
                {
                    let true_ty = self.get_ty_from_type_node(root.node.true_ty);
                    let true_mapper = combined_mapper.or(mapper);
                    if let Some((new_root, new_root_mapper)) =
                        can_tail_recurse(self, true_ty, true_mapper)
                    {
                        root = new_root;
                        mapper = Some(new_root_mapper);
                        tailed += 1;
                        continue;
                    }
                    break self.instantiate_ty(true_ty, true_mapper);
                }
            }

            let check_ty = self.instantiate_ty(root.check_ty, mapper);
            let extends_ty = self.instantiate_ty(root.extends_ty, mapper);
            let cond_ty = self.alloc(ty::CondTy {
                root,
                check_ty,
                extends_ty,
                mapper,
                combined_mapper,
                alias_symbol,
                alias_ty_args,
            });
            break self.new_ty(ty::TyKind::Cond(cond_ty), TypeFlags::CONDITIONAL);
        };
        if !extra_tys.is_empty() {
            extra_tys.push(result);
            self.get_union_ty(&extra_tys, ty::UnionReduction::Lit)
        } else {
            result
        }
    }

    fn is_ty_param_possibly_referenced(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        // TODO:
        true
    }

    pub(super) fn get_infer_ty_params(
        &mut self,
        node: &'cx ast::CondTy<'cx>,
    ) -> Option<ty::Tys<'cx>> {
        let id = node.id;
        let locals = self.binder.locals(id)?;
        let ty_params = locals
            .iter()
            .flat_map(|(_, symbol)| {
                let s = self.binder.symbol(*symbol);
                if s.flags.intersects(SymbolFlags::TYPE_PARAMETER) {
                    Some(self.get_declared_ty_of_symbol(*symbol))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        if ty_params.is_empty() {
            None
        } else {
            Some(self.alloc(ty_params))
        }
    }

    fn get_ty_from_cond_node(&mut self, node: &'cx ast::CondTy<'cx>) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_node_links(node.id).get_resolved_ty() {
            return ty;
        }
        let check_ty = self.get_ty_from_type_node(node.check_ty);
        let alias_symbol = self.get_alias_symbol_for_ty_node(node.id);
        let alias_ty_args = self.get_ty_args_for_alias_symbol(alias_symbol);
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
        let infer_ty_params = self.get_infer_ty_params(node);
        let root = self.alloc(ty::CondTyRoot {
            check_ty,
            extends_ty,
            outer_ty_params,
            node,
            is_distributive: check_ty.kind.is_param(),
            infer_ty_params,
        });
        let ty = self.get_cond_ty(root, None, alias_symbol, alias_ty_args);
        self.get_mut_node_links(node.id).set_resolved_ty(ty);
        ty
    }

    fn get_ty_from_array_node(&mut self, node: &'cx ast::ArrayTy<'cx>) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_node_links(node.id).get_resolved_ty() {
            return ty;
        }
        let element_ty = self.get_ty_from_type_node(node.ele);
        // TODO: defer type
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
        } else if !node
            .tys
            .iter()
            .any(|e| Self::get_tuple_element_flags(e).intersects(ElementFlags::VARIADIC))
            && self.is_deferred_ty_reference_node(node.id, false)
        {
            if node.tys.is_empty() {
                target
            } else {
                self.create_deferred_ty_reference(target, node.id, None)
            }
        } else {
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
        if let Some(ty) = self.num_lit_tys.get(&key) {
            ty
        } else {
            let kind = TyKind::NumberLit(self.alloc(ty::NumberLitTy { val }));
            let ty = self.new_ty(kind, TypeFlags::NUMBER_LITERAL);
            self.num_lit_tys.insert(key, ty);
            ty
        }
    }

    pub(super) fn get_string_literal_type(&mut self, val: AtomId) -> &'cx Ty<'cx> {
        if let Some(ty) = self.string_lit_tys.get(&val) {
            ty
        } else {
            let kind = TyKind::StringLit(self.alloc(ty::StringLitTy { val }));
            let ty = self.new_ty(kind, TypeFlags::STRING_LITERAL);
            self.string_lit_tys.insert(val, ty);
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
            return self.error_ty;
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
                return self.undefined_ty;
            };
            if tys.is_empty() {
                if let Some(contextual_ret_ty) = self.get_contextual_ret_ty(id, None) {
                    ret_ty = contextual_ret_ty;
                } else {
                    ret_ty = self.void_ty;
                }
            } else {
                ret_ty = self.get_union_ty(&tys, ty::UnionReduction::Subtype)
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
            if info.key_ty == self.string_ty && include.intersects(TypeFlags::NUMBER) {
                self.string_or_number_ty()
            } else {
                info.key_ty
            }
        });
        property_tys.extend(index_key_tys);
        let tys = property_tys;
        self.get_union_ty(&tys, ty::UnionReduction::Lit)
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
        } else if let Some(ty) = self.get_ty_links(ty.id).get_resolved_index_ty() {
            ty
        } else {
            let index_ty = self.create_index_ty(ty, IndexFlags::empty());
            self.get_mut_ty_links(ty.id).set_resolved_index_ty(index_ty);
            index_ty
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
        let ty = self.get_reduced_ty(ty);
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

    pub(crate) fn get_ty_arguments(&mut self, ty: &'cx ty::Ty<'cx>) -> ty::Tys<'cx> {
        if let Some(ty) = self.get_ty_links(ty.id).get_resolved_ty_args() {
            return ty;
        };

        let Some(r) = ty.kind.as_object_reference() else {
            if let Some(t) = ty.kind.as_object_tuple() {
                return t.resolved_ty_args;
            } else {
                unreachable!()
            }
        };

        if !self.push_ty_resolution(ResolutionKey::ResolvedTypeArguments(ty.id)) {
            let i = r.target.kind.expect_object_interface();
            return self.concatenate(i.outer_ty_params, i.local_ty_params);
        }

        let ty_args = if let Some(node) = r.node {
            use ast::Node::*;
            match self.p.node(node) {
                ReferTy(_) => {
                    let i = r.target.kind.expect_object_interface();
                    let local_args = self.get_effective_ty_args(node, i.local_ty_params.unwrap());
                    self.concatenate(i.outer_ty_params, local_args)
                }
                ArrayTy(n) => {
                    let ty = self.get_ty_from_type_node(n.ele);
                    self.alloc([ty])
                }
                TupleTy(n) => {
                    let tys = n
                        .tys
                        .iter()
                        .map(|t| self.get_ty_from_type_node(t))
                        .collect::<Vec<_>>();
                    self.alloc(tys)
                }
                _ => unreachable!(),
            }
        } else {
            self.empty_array()
        };
        let ty_args = if self.pop_ty_resolution().has_cycle() {
            let i = r.target.kind.expect_object_interface();
            // TODO: throw error
            self.concatenate(i.outer_ty_params, i.local_ty_params)
        } else if let Some(mapper) = r.mapper {
            self.instantiate_tys(ty_args, mapper)
        } else {
            ty_args
        };
        self.get_mut_ty_links(ty.id).set_resolved_ty_args(ty_args);
        ty_args
    }
}
