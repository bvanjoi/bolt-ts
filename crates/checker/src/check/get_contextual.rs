use bolt_ts_ast::FnFlags;
use bolt_ts_binder::SymbolFlags;
use bolt_ts_binder::SymbolName;

use super::IterationTypeKind;
use super::Ternary;
use super::TyChecker;
use super::ast;
use super::create_ty::IntersectionFlags;
use super::symbol_info::SymbolInfo;
use super::ty;
use super::ty::MappedTyNameTyKind;
use super::ty::ObjectFlags;
use super::ty::TypeFlags;

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub(super) struct ContextFlags: u8 {
        const SIGNATURE             = 1 << 0;
        const NO_CONSTRAINTS        = 1 << 1;
        const COMPLETIONS           = 1 << 2;
        const SKIP_BINDING_PATTERNS = 1 << 3;
    }
}

enum AssignmentDeclarationKind {
    None,
    /// `exports.name = expr`
    /// `module.exports.name = expr`
    ExportsProperty,
    /// `module.exports = expr`
    ModuleExports,
    /// `className.prototype.name = expr`
    PrototypeProperty,
    /// `this.name = expr`
    ThisProperty,
    // `F.name = expr`
    Property,
    // `F.prototype = { ... }`
    Prototype,
    // `Object.defineProperty(x, 'name', { value: any, writable?: boolean (false by default) });`
    // `Object.defineProperty(x, 'name', { get: Function, set: Function });`
    // `Object.defineProperty(x, 'name', { get: Function });`
    // `Object.defineProperty(x, 'name', { set: Function });`
    ObjectDefinePropertyValue,
    // `Object.defineProperty(exports || module.exports, 'name', ...);`
    ObjectDefinePropertyExports,
    // `Object.defineProperty(Foo.prototype, 'name', ...);`
    ObjectDefinePrototypeProperty,
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_contextual_ty(
        &mut self,
        id: ast::NodeID,
        flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let includes_caches = flags.is_none_or(|flags| ContextFlags::empty() == flags);
        if let Some(ctx) = self.find_context_node(id, includes_caches) {
            return ctx.ty;
        }
        let Some(parent_id) = self.parent(id) else {
            unreachable!()
        };
        let parent = self.p.node(parent_id);
        use bolt_ts_ast::Node::*;
        match parent {
            VarDecl(parent) => self.get_contextual_ty_for_var_decl(parent, id),
            ParamDecl(parent) => self.get_contextual_ty_for_param_decl(parent, id),
            ArrayBinding(parent) => self.get_contextual_ty_for_array_binding(parent, id, flags),
            ArrowFnExpr(_) | RetStmt(_) => self.get_contextual_ty_for_return_expr(id, flags),
            AssignExpr(parent) if id == parent.right.id() => {
                self.get_contextual_ty_for_assign(parent, flags)
            }
            ArrayLit(parent) => {
                let ty = self.get_apparent_ty_of_contextual_ty(parent.id, flags);
                let element_idx = self.p.index_of_node(parent.elems, id);
                let (first, last) = {
                    // TODO: cache
                    let mut first = None;
                    let mut last = None;
                    for i in 0..parent.elems.len() {
                        if matches!(parent.elems[i].kind, ast::ExprKind::SpreadElement(_)) {
                            if first.is_none() {
                                first = Some(i);
                            }
                            last = Some(i);
                        }
                    }
                    (first, last)
                };
                self.get_contextual_ty_for_element_expr(
                    ty,
                    element_idx,
                    Some(parent.elems.len()),
                    first,
                    last,
                )
            }
            // TODO: type assertion
            AsExpr(parent) => {
                debug_assert!(parent.id == parent_id);
                if parent.ty.is_const_ty_refer() {
                    self.get_contextual_ty(parent.id, flags)
                } else {
                    Some(self.get_ty_from_type_node(parent.ty))
                }
            }
            SpreadAssignment(_) => {
                let parent_parent = self.parent(parent_id).unwrap();
                self.get_contextual_ty(parent_parent, flags)
            }
            ObjectPropAssignment(parent) => {
                self.get_contextual_ty_for_object_literal_ele(parent, flags)
            }
            ObjectShorthandMember(parent) => {
                self.get_contextual_ty_for_object_literal_ele(parent, flags)
            }
            ParenExpr(parent) => {
                // TODO: is_in_js_file
                self.get_contextual_ty(parent.id, flags)
            }
            _ => None,
        }
    }

    fn get_contextual_ty_for_return_expr(
        &mut self,
        id: ast::NodeID,
        flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let f = self.node_query(id.module()).get_containing_fn(id)?;
        let mut contextual_return_ty = self.get_contextual_ret_ty(f, flags)?;
        let fn_flags = self.p.node(f).fn_flags();
        if fn_flags.contains(FnFlags::GENERATOR) {
            let is_async_generator = fn_flags.contains(FnFlags::ASYNC);
            if contextual_return_ty.flags.contains(TypeFlags::UNION) {
                contextual_return_ty = self.filter_type(contextual_return_ty, |this, t| {
                    this.get_iteration_ty_of_generator_fn_return_ty(
                        IterationTypeKind::Return,
                        t,
                        is_async_generator,
                    )
                    .is_some()
                });
            }
            let Some(iteration_ret_ty) = self.get_iteration_ty_of_generator_fn_return_ty(
                IterationTypeKind::Return,
                contextual_return_ty,
                is_async_generator,
            ) else {
                return None;
            };
            contextual_return_ty = iteration_ret_ty;
        }
        if fn_flags.contains(FnFlags::ASYNC) {
            return self
                .map_ty(
                    contextual_return_ty,
                    |this, t| this.get_awaited_ty_no_alias(t),
                    false,
                )
                .and_then(|contextual_await_ty| {
                    let ty = self.create_promise_like_ty(contextual_await_ty);
                    Some(self.get_union_ty::<false>(
                        &[contextual_await_ty, ty],
                        ty::UnionReduction::Lit,
                        None,
                        None,
                        None,
                    ))
                });
        }
        Some(contextual_return_ty)
    }

    fn create_promise_like_ty(&mut self, mut promised_ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        let global_promise_ty = self.get_global_promise_ty::<true>();
        if global_promise_ty != self.empty_generic_ty() {
            let ty = self.unwrap_awaited_ty(promised_ty);
            promised_ty = self.get_awaited_ty_no_alias(ty).unwrap_or(self.unknown_ty);
            let resolved_ty_args = self.alloc([promised_ty]);
            self.create_reference_ty(
                global_promise_ty,
                Some(resolved_ty_args),
                ObjectFlags::empty(),
            )
        } else {
            self.unknown_ty
        }
    }

    fn get_contextual_ty_for_object_literal_ele(
        &mut self,
        element: &'cx impl ast::r#trait::ObjectLitElementLike<'cx>,
        context_flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let id = element.id();
        let object_literal = {
            let p = self.parent(id).unwrap();
            self.p.node(p).expect_object_lit()
        };
        let ty = self.get_apparent_ty_of_contextual_ty(object_literal.id, context_flags)?;
        if self.has_bindable_name(id) {
            let symbol = self.get_symbol_of_decl(id);
            let name = self.symbol(symbol).name;
            let name_ty = self.get_symbol_links(symbol).get_name_ty();
            return self.get_ty_of_prop_of_contextual_ty(ty, name, name_ty);
        }
        let nq = self.node_query(id.module());
        if nq.has_dynamic_name(id)
            && let Some(name) = nq.get_name_of_decl(id)
            && let ast::DeclarationName::Computed(name) = name
            && let expr_ty = self.check_expr(name.expr)
            && expr_ty.useable_as_prop_name()
            && let prop_name = self.get_prop_name_from_ty(expr_ty)
            && let Some(prop_ty) = self.get_ty_of_prop_of_contextual_ty(ty, prop_name, None)
        {
            Some(prop_ty)
        } else if let Some(name) = element.name() {
            let name_ty = self.get_lit_ty_from_prop_name(&name);
            self.map_ty(
                ty,
                |this, t| {
                    let index_infos = this.get_index_infos_of_structured_ty(t);
                    match this.find_applicable_index_info(index_infos, name_ty) {
                        Some(index_info) => Some(index_info.val_ty),
                        None => None,
                    }
                },
                true,
            )
        } else {
            None
        }
    }

    fn get_contextual_ty_for_object_literal_method(
        &mut self,
        n: &'cx ast::ObjectMethodMember<'cx>,
        context_flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        // TODO: NodeFlags.InWithStatement
        self.get_contextual_ty_for_object_literal_ele(n, context_flags)
    }

    fn is_resolving_ret_ty_of_sig(&mut self, sig: &'cx ty::Sig<'cx>) -> bool {
        // TODO: signature.compositeSignatures
        self.get_sig_links(sig.id).get_resolved_ret_ty().is_none()
            && self
                .find_resolution_cycle_start_index(super::ResolutionKey::ResolvedReturnType(sig.id))
                .is_some()
    }

    pub(super) fn get_contextual_ret_ty(
        &mut self,
        id: ast::NodeID,
        flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(ret_ty) = self.get_ret_ty_from_anno(id) {
            return Some(ret_ty);
        }

        if let Some(sig) = self.get_contextual_sig_for_fn_like_decl(id)
            && !self.is_resolving_ret_ty_of_sig(sig)
        {
            let ret_ty = self.get_ret_ty_of_sig(sig);
            let fn_flags = self.p.node(id).fn_flags();
            return Some(if fn_flags.contains(FnFlags::GENERATOR) {
                self.filter_type(ret_ty, |this, t| {
                    t.flags.intersects(
                        TypeFlags::ANY_OR_UNKNOWN
                            .union(TypeFlags::VOID)
                            .union(TypeFlags::INSTANTIABLE_NON_PRIMITIVE),
                    ) || this
                        .check_generator_instantiation_assignability_to_return_ty(t, fn_flags, None)
                })
            } else if fn_flags.contains(FnFlags::ASYNC) {
                self.filter_type(ret_ty, |this, t| {
                    t.flags.intersects(
                        TypeFlags::ANY_OR_UNKNOWN
                            .union(TypeFlags::VOID)
                            .union(TypeFlags::INSTANTIABLE_NON_PRIMITIVE),
                    ) || this.get_awaited_ty_of_promise(t, None).is_none()
                })
            } else {
                ret_ty
            });
        }

        if self.node_query(id.module()).get_iife(id).is_some() {
            self.get_contextual_ty(id, flags)
        } else {
            None
        }
    }

    fn get_assignment_declaration_prop_access_kind(
        &mut self,
        access: ast::NodeID,
    ) -> AssignmentDeclarationKind {
        let access_node = self.p.node(access);
        let expr = match access_node {
            ast::Node::PropAccessExpr(n) => n.expr,
            ast::Node::EleAccessExpr(n) => n.expr,
            _ => unreachable!(),
        };
        if matches!(expr.kind, ast::ExprKind::This(_)) {
            return AssignmentDeclarationKind::ThisProperty;
        }
        // TODO: module_exports
        if expr.is_bindable_static_name_expr::<true>() {
            if expr.is_prototype_access() {
                return AssignmentDeclarationKind::PrototypeProperty;
            }
            // TODO: exportsProperty

            if access_node.is_bindable_static_name_expr::<true>()
                || access_node
                    .as_ele_access_expr()
                    .is_some_and(|ele| ele.is_dynamic_name())
            {
                return AssignmentDeclarationKind::Property;
            }
        }
        AssignmentDeclarationKind::None
    }

    fn get_assignment_declaration_kind_for_assign_expr(
        &mut self,
        expr: &'cx ast::AssignExpr<'cx>,
    ) -> AssignmentDeclarationKind {
        if expr.op != ast::AssignOp::Eq
            || !expr.left.kind.is_access_expr()
            || self
                .p
                .node(
                    self.node_query(expr.id.module())
                        .get_right_most_assigned_expr(expr.id),
                )
                .is_void_zero_expr()
        {
            AssignmentDeclarationKind::None
        } else {
            // TODO: prototype
            self.get_assignment_declaration_prop_access_kind(expr.left.id())
        }
    }

    fn get_contextual_ty_for_assign(
        &mut self,
        parent: &'cx ast::AssignExpr<'cx>,
        flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let kind = self.get_assignment_declaration_kind_for_assign_expr(parent);
        match kind {
            AssignmentDeclarationKind::None | AssignmentDeclarationKind::ThisProperty => {
                let lhs_symbol = self.get_symbol_for_expr(parent.left.id());
                if let Some(lhs_symbol) = lhs_symbol
                    && let Some(decl) = self.symbol(lhs_symbol).value_decl
                {
                    let n = self.p.node(decl);
                    if let ast::Node::ClassPropElem(ast::ClassPropElem { ty, .. })
                    | ast::Node::PropSignature(ast::PropSignature { ty, .. }) = n
                    {
                        return match ty {
                            Some(ty) => {
                                let ty = self.get_ty_from_type_node(*ty);
                                let mapper = self.get_symbol_links(lhs_symbol).get_ty_mapper();
                                Some(self.instantiate_ty(ty, mapper))
                            }
                            None => {
                                if let Some(n) = n.as_class_prop_elem()
                                    && n.init.is_some()
                                {
                                    Some(self.get_ty_of_expr(parent.left))
                                } else {
                                    None
                                }
                            }
                        };
                    }
                }
                if matches!(kind, AssignmentDeclarationKind::None) {
                    let ret = self.get_ty_of_expr(parent.left);
                    Some(ret)
                } else {
                    // TODO: get_contextual_ty_for_this_property_assignment
                    None
                }
            }
            _ => {
                // TODO: other case
                None
            }
        }
    }

    fn get_contextual_ty_for_param_decl(
        &mut self,
        parent: &'cx ast::ParamDecl<'cx>,
        node: ast::NodeID,
    ) -> Option<&'cx ty::Ty<'cx>> {
        debug_assert!(self.parent(node).is_some_and(|p| p == parent.id));
        if parent.init.is_some()
            && let Some(decl_ty) = parent.ty
        {
            // TODO: js
            Some(self.get_ty_from_type_node(decl_ty))
        } else {
            None
        }
    }

    fn get_contextual_ty_for_var_decl(
        &mut self,
        parent: &'cx ast::VarDecl<'cx>,
        node: ast::NodeID,
    ) -> Option<&'cx ty::Ty<'cx>> {
        debug_assert!(self.parent(node).is_some_and(|p| p == parent.id));
        if parent.init.is_some()
            && let Some(decl_ty) = parent.ty
        {
            // TODO: js
            Some(self.get_ty_from_type_node(decl_ty))
        } else {
            None
        }
    }

    fn get_contextual_ty_for_array_binding(
        &mut self,
        parent: &'cx ast::ArrayBinding<'cx>,
        node: ast::NodeID,
        context_flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        debug_assert!(self.parent(node).is_some_and(|p| p == parent.id));
        if parent.init.is_some_and(|init| init.id() == node) {
            // TODO: js
            let name = parent.name;
            let parent_parent_id = self.parent(parent.id).unwrap();
            debug_assert!(self.p.node(parent_parent_id).is_array_pat());
            let parent_parent_parent_id = self.parent(parent_parent_id).unwrap();
            let parent_parent_parent_ty = match self.p.node(parent_parent_parent_id) {
                ast::Node::VarDecl(n) => {
                    // TODO: init_ty
                    n.ty.map(|ty| self.get_ty_from_type_node(ty))
                }
                _ => {
                    // TODO: other cases
                    None
                }
            }?;
            let index = self
                .p
                .node(parent_parent_id)
                .expect_array_pat()
                .elems
                .iter()
                .position(|e| match e.kind {
                    ast::ArrayBindingElemKind::Omit(_) => false,
                    ast::ArrayBindingElemKind::Binding(item) => std::ptr::eq(item, parent),
                })?;
            self.get_contextual_ty_for_element_expr(
                Some(parent_parent_parent_ty),
                index,
                None,
                None,
                None,
            )
        } else {
            None
        }
    }

    pub(super) fn get_contextual_ty_for_element_expr(
        &mut self,
        ty: Option<&'cx ty::Ty<'cx>>,
        index: usize,
        length: Option<usize>,
        first_spread_index: Option<usize>,
        last_spread_index: Option<usize>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        self.map_ty(
            ty?,
            |this, t| {
                if let Some(tup) = t.as_tuple() {
                    if first_spread_index
                        .is_none_or(|first_spread_index| index < first_spread_index)
                        && index < tup.fixed_length
                    {
                        let ty = this.get_ty_arguments(t)[index];
                        let is_optional =
                            tup.element_flags[index].contains(ty::ElementFlags::OPTIONAL);
                        return Some(this.remove_missing_ty(ty, is_optional));
                    }
                    let offset = if last_spread_index.is_none_or(|last| index > last) {
                        length.map_or(0, |len| len.saturating_sub(index))
                    } else {
                        0
                    };
                    let fixed_end_length = if offset > 0
                        && tup.combined_flags.intersects(ty::ElementFlags::VARIABLE)
                    {
                        tup.get_end_elem_count(ty::ElementFlags::FIXED)
                    } else {
                        0
                    };
                    if offset > 0 && offset <= fixed_end_length {
                        let idx = Self::get_ty_reference_arity(t) - offset;
                        return this.get_ty_arguments(t).get(idx).copied();
                    }
                }
                if first_spread_index.is_none_or(|first_spread_index| index < first_spread_index)
                    && let Some(t) = this.get_ty_of_prop_of_contextual_ty(
                        t,
                        SymbolName::EleNum(index.into()),
                        None,
                    )
                {
                    return Some(t);
                }
                // TODO: this.get_iterated_ty_or_element_ty()
                this.get_index_ty_of_ty(t, this.number_ty)
            },
            true,
        )
    }

    pub(super) fn get_ty_of_prop_of_contextual_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        name: SymbolName,
        name_ty: Option<&'cx ty::Ty<'cx>>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        self.map_ty(
            ty,
            |this, t| {
                if let Some(i) = t.kind.as_intersection() {
                    let mut tys = Vec::with_capacity(i.tys.len());
                    let mut index_info_candidates = Vec::with_capacity(i.tys.len());
                    let mut ignore_index_infos = false;
                    for constituent_ty in i.tys {
                        if !constituent_ty.kind.is_object() {
                            continue;
                        }
                        if this.is_generic_mapped_ty(constituent_ty)
                            && this.get_mapped_ty_name_ty_kind(constituent_ty)
                                != ty::MappedTyNameTyKind::Remapping
                        {
                            let sub = this.get_indexed_mapped_type_substituted_ty_of_contextual_ty(
                                constituent_ty,
                                name,
                                name_ty,
                            );
                            this.append_contextual_prop_ty_constituent(&mut tys, sub);
                            continue;
                        }
                        let prop_ty = this.get_ty_of_concrete_prop_of_contextual_ty(ty, name);
                        if let Some(prop_ty) = prop_ty {
                            ignore_index_infos = true;
                            index_info_candidates.clear();
                            this.append_contextual_prop_ty_constituent(&mut tys, Some(prop_ty));
                        } else {
                            if !ignore_index_infos {
                                index_info_candidates.push(constituent_ty);
                            }
                            continue;
                        }
                    }

                    if !index_info_candidates.is_empty() {
                        for candidate in index_info_candidates {
                            let index_info_ty = this
                                .get_ty_from_index_infos_of_contextual_ty(candidate, name, name_ty);
                            this.append_contextual_prop_ty_constituent(&mut tys, index_info_ty);
                        }
                    }
                    if tys.is_empty() {
                        None
                    } else if tys.len() == 1 {
                        Some(tys[0])
                    } else {
                        Some(this.get_intersection_ty(&tys, IntersectionFlags::None, None, None))
                    }
                } else if !t.flags.intersects(TypeFlags::OBJECT) {
                    None
                } else if this.is_generic_mapped_ty(t)
                    && this.get_mapped_ty_name_ty_kind(t) != MappedTyNameTyKind::Remapping
                {
                    this.get_indexed_mapped_type_substituted_ty_of_contextual_ty(t, name, name_ty)
                } else {
                    this.get_ty_of_concrete_prop_of_contextual_ty(t, name)
                        .or_else(|| this.get_ty_from_index_infos_of_contextual_ty(t, name, name_ty))
                }
            },
            true,
        )
    }

    fn get_ty_from_index_infos_of_contextual_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        name: SymbolName,
        name_ty: Option<&'cx ty::Ty<'cx>>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(tup) = ty.as_tuple()
            && name.as_numeric().is_some_and(|n| n >= 0.)
            && let Some(rest_ty) = self.get_element_ty_of_slice_of_tuple_ty(
                ty,
                tup.fixed_length,
                Some(0),
                Some(false),
                Some(true),
            )
        {
            return Some(rest_ty);
        }
        let index_infos = self.get_index_infos_of_structured_ty(ty);
        let key_ty = name_ty.unwrap_or_else(|| {
            if let Some(atom) = name.as_atom() {
                self.get_string_literal_type_from_string(atom)
            } else if let Some(n) = name.as_numeric() {
                self.get_number_literal_type_from_number(n)
            } else {
                unreachable!()
            }
        });
        self.find_applicable_index_info(index_infos, key_ty)
            .map(|t| t.val_ty)
    }

    fn get_ty_of_concrete_prop_of_contextual_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        name: SymbolName,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let prop = self.get_prop_of_ty(ty, name)?;
        if self.is_circular_mapped_prop(prop) {
            return None;
        }
        let t = self.get_type_of_symbol(prop);
        let is_optional = self.symbol(prop).flags.contains(SymbolFlags::OPTIONAL);
        Some(self.remove_missing_ty(t, is_optional))
    }

    fn append_contextual_prop_ty_constituent(
        &self,
        tys: &mut Vec<&'cx ty::Ty<'cx>>,
        ty: Option<&'cx ty::Ty<'cx>>,
    ) {
        if let Some(ty) = ty {
            if ty.flags.intersects(TypeFlags::ANY) {
                tys.push(self.unknown_ty);
            } else {
                tys.push(ty);
            }
        }
    }

    fn is_excluded_mapped_property_name(
        &mut self,
        constraint: &'cx ty::Ty<'cx>,
        property_name_ty: &'cx ty::Ty<'cx>,
    ) -> bool {
        if let Some(c) = constraint.kind.as_cond_ty() {
            let true_ty = {
                let t = self.get_true_ty_from_cond_ty(c);
                self.get_reduced_ty(t)
            };
            let false_ty = {
                let t = self.get_false_ty_from_cond_ty(c);
                self.get_actual_ty_variable(t)
            };
            true_ty.flags.intersects(TypeFlags::NEVER)
                && false_ty == self.get_actual_ty_variable(c.check_ty)
                && self.is_type_assignable_to(property_name_ty, c.extends_ty)
        } else if let Some(i) = constraint.kind.as_intersection() {
            i.tys
                .iter()
                .any(|t| self.is_excluded_mapped_property_name(t, property_name_ty))
        } else {
            false
        }
    }

    fn get_indexed_mapped_type_substituted_ty_of_contextual_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        name: SymbolName,
        name_ty: Option<&'cx ty::Ty<'cx>>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let m = ty.kind.expect_object_mapped();
        let property_name_ty = name_ty.unwrap_or_else(|| {
            if let Some(atom) = name.as_atom() {
                self.get_string_literal_type_from_string(atom)
            } else if let Some(n) = name.as_numeric() {
                self.get_number_literal_type_from_number(n)
            } else {
                unreachable!()
            }
        });
        let constraint = self.get_constraint_ty_from_mapped_ty(m);

        if self.object_mapped_ty_links_arena[m.links]
            .get_named_ty()
            .is_some_and(|name_ty| self.is_excluded_mapped_property_name(name_ty, property_name_ty))
            || self.is_excluded_mapped_property_name(constraint, property_name_ty)
        {
            return None;
        }
        let constraint_of_constraint = self
            .get_base_constraint_of_ty(constraint)
            .unwrap_or(constraint);
        if !self.is_type_assignable_to(property_name_ty, constraint_of_constraint) {
            return None;
        }
        Some(self.substitute_indexed_mapped_ty(ty, property_name_ty))
    }

    pub(super) fn get_apparent_ty_of_contextual_ty(
        &mut self,
        node: ast::NodeID,
        flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let contextual_ty = if let Some(n) = self.p.node(node).as_object_method_member() {
            self.get_contextual_ty_for_object_literal_method(n, flags)
        } else {
            self.get_contextual_ty(node, flags)
        };
        let instantiated_ty = self.instantiate_contextual_ty(contextual_ty, node, flags)?;
        if flags.is_none_or(|flags| {
            !(flags.contains(ContextFlags::NO_CONSTRAINTS)
                && instantiated_ty.kind.is_type_variable())
        }) {
            let apparent_ty = self.map_ty(
                instantiated_ty,
                |this, t| {
                    if t.get_object_flags().contains(ObjectFlags::MAPPED) {
                        Some(t)
                    } else {
                        Some(this.get_apparent_ty(t))
                    }
                },
                true,
            );
            // TODO: union
            return apparent_ty;
        }
        None
    }

    fn is_arity_smaller(&mut self, sig: &'cx ty::Sig<'cx>, id: ast::NodeID) -> bool {
        let mut target_params_count = 0;
        let params = self.p.node(id).params().unwrap_or_default();
        while target_params_count < params.len() {
            let param = params[target_params_count];
            if param.init.is_some() || param.question.is_some() || param.dotdotdot.is_some() {
                break;
            }
            target_params_count += 1;
        }
        self.has_effective_rest_param(sig) && sig.get_param_count(self) < target_params_count
    }

    fn get_contextual_call_sig(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        id: ast::NodeID,
    ) -> Option<&'cx ty::Sig<'cx>> {
        let sigs = self
            .get_signatures_of_type(ty, ty::SigKind::Call)
            .iter()
            .filter(|sig| !self.is_arity_smaller(sig, id))
            .collect::<Vec<_>>();
        if sigs.is_empty() {
            // TODO: get_intersected_sigs
            None
        } else {
            Some(sigs[0])
        }
    }

    pub(super) fn get_contextual_sig_for_fn_like_decl(
        &mut self,
        id: ast::NodeID,
    ) -> Option<&'cx ty::Sig<'cx>> {
        let n = self.p.node(id);
        if n.is_fn_expr() || n.is_arrow_fn_expr() || n.is_object_method_member() {
            self.get_contextual_sig(id)
        } else {
            None
        }
    }

    fn create_union_sigs(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        union_sigs: ty::Sigs<'cx>,
    ) -> &'cx ty::Sig<'cx> {
        let next = ty::Sig {
            id: ty::SigID::dummy(),
            flags: sig.flags & ty::SigFlags::PROPAGATING_FLAGS,
            params: sig.params,
            this_param: sig.this_param,
            min_args_count: sig.min_args_count,
            ret: sig.ret,
            node_id: sig.node_id,
            target: None,
            mapper: None,
            class_decl: sig.class_decl,
            composite_sigs: Some(union_sigs),
            composite_kind: Some(ty::TypeFlags::UNION),
        };
        if let Some(ty_params) = self.get_sig_links(sig.id).get_ty_params() {
            let links = super::links::SigLinks::default().with_ty_params(ty_params);
            let prev = self.sig_links.insert(next.id, links);
            debug_assert!(prev.is_none());
        }
        self.new_sig(next)
    }

    pub(super) fn get_contextual_sig(&mut self, id: ast::NodeID) -> Option<&'cx ty::Sig<'cx>> {
        debug_assert!(!self.p.node(id).is_class_method_elem());
        if let Some(ty_tag_sig) = self.get_sig_of_ty_tag(id) {
            return Some(ty_tag_sig);
        }
        let ty = self.get_apparent_ty_of_contextual_ty(id, Some(ContextFlags::SIGNATURE))?;

        if let Some(u) = ty.kind.as_union() {
            let mut sigs = Vec::with_capacity(u.tys.len());
            for current in u.tys {
                if let Some(sig) = self.get_contextual_call_sig(current, id) {
                    if sigs.is_empty() {
                        sigs.push(sig);
                    } else if self.compare_sigs_identical(
                        sigs[0],
                        sig,
                        false,
                        true,
                        true,
                        |this, s, t| this.compare_types_identical(s, t),
                    ) == Ternary::FALSE
                    {
                        return None;
                    } else {
                        sigs.push(sig);
                    }
                }
            }
            if !sigs.is_empty() {
                if sigs.len() == 1 {
                    return Some(sigs[0]);
                } else {
                    // todo: self.create_union_sigs
                    let sigs = self.alloc(sigs);
                    self.create_union_sigs(sigs[0], sigs);
                }
            }
        } else {
            return self.get_contextual_call_sig(ty, id);
        }
        None
    }

    pub(super) fn is_context_sensitive_fn_or_object_literal_method(&self, id: ast::NodeID) -> bool {
        let node = self.p.node(id);
        if node.is_fn_expr_or_arrow_fnc_expr() || self.p.is_object_lit_method(id) {
            // TODO: isContextSensitiveFunctionLikeDeclaration
            false
        } else {
            false
        }
    }

    pub(super) fn get_contextual_iteration_ty(
        &mut self,
        kind: IterationTypeKind,
        fn_decl: ast::NodeID,
        is_async: bool,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let contextual_ret_ty = self.get_contextual_ret_ty(fn_decl, None)?;
        self.get_iteration_ty_of_generator_fn_return_ty(kind, contextual_ret_ty, is_async)
    }
}
