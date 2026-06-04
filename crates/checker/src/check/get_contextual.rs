use bolt_ts_ast::FnFlags;
use bolt_ts_ast::keyword;
use bolt_ts_binder::AssignmentDeclarationKind;
use bolt_ts_binder::Symbol;
use bolt_ts_binder::SymbolFlags;
use bolt_ts_binder::SymbolID;
use bolt_ts_binder::SymbolName;
use bolt_ts_ty::CheckFlags;

use super::IterationTypeKind;
use super::Ternary;
use super::TyChecker;
use super::ast;
use super::check_expr::IterationUse;
use super::create_ty::IntersectionFlags;
use super::get_effective_node::EffectiveCallArgument;
use super::get_effective_node::EffectiveCallArguments;
use super::links;
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

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub(super) struct DiscriminateContextualTyByObjectLiteral {
    node: ast::NodeID,
    ty: ty::TyID,
}

impl DiscriminateContextualTyByObjectLiteral {
    fn new(node: &ast::ObjectLit, ty: &ty::Ty<'_>) -> Self {
        Self {
            node: node.id,
            ty: ty.id,
        }
    }
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
            VarDecl(parent) => self.get_contextual_ty_for_var_decl(parent, id, flags),
            ParamDecl(parent) => self.get_contextual_ty_for_param_decl(parent, id, flags),
            ClassPropElem(parent) => {
                self.get_contextual_ty_for_class_property_element(parent, flags)
            }
            ArrayBinding(parent) => self.get_contextual_ty_for_array_binding(parent, id, flags),
            ArrowFnExpr(_) | RetStmt(_) => self.get_contextual_ty_for_return_expr(id, flags),
            ArrayLit(parent) => {
                let ty = self.get_apparent_ty_of_contextual_ty(parent.id, flags);
                let element_idx = self.p.index_of_node(parent.elems, id);
                // get_spread_indices
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
            AssignExpr(parent) => {
                // get_contextual_type_for_binary_operand
                if matches!(
                    parent.op,
                    ast::AssignOp::Eq
                        | ast::AssignOp::LogicalOrEq
                        | ast::AssignOp::LogicalAndEq
                        | ast::AssignOp::NullishEq
                ) && id == parent.right.id()
                {
                    self.get_contextual_ty_for_assign(parent, flags)
                } else {
                    None
                }
            }
            BinExpr(parent) => {
                // get_contextual_type_for_binary_operand
                match parent.op.kind {
                    ast::BinOpKind::LogicalOr | ast::BinOpKind::Nullish => {
                        let ty = self.get_contextual_ty(parent.id, flags);
                        if id == parent.right.id() {
                            if let Some(ty) = ty {
                                Some(if ty.pattern().is_some() {
                                    self.get_ty_of_expr(parent.left)
                                } else {
                                    ty
                                })
                            } else {
                                // TODO: if !isDefaultedExpandoInitializer(binaryExpression) { self.get_ty_of_expr(parent.left) }
                                None
                            }
                        } else {
                            None
                        }
                    }
                    ast::BinOpKind::LogicalAnd | ast::BinOpKind::Comma
                        if id == parent.right.id() =>
                    {
                        self.get_contextual_ty(parent.id, flags)
                    }
                    _ => None,
                }
                // switch (operatorToken.kind) {
                //     case SyntaxKind.EqualsToken:
                //     case SyntaxKind.AmpersandAmpersandEqualsToken:
                //     case SyntaxKind.BarBarEqualsToken:
                //     case SyntaxKind.QuestionQuestionEqualsToken:
                //         return node === right ? getContextualTypeForAssignmentDeclaration(binaryExpression) : undefined;
                //     case SyntaxKind.BarBarToken:
                //     case SyntaxKind.QuestionQuestionToken:
                //         // When an || expression has a contextual type, the operands are contextually typed by that type, except
                //         // when that type originates in a binding pattern, the right operand is contextually typed by the type of
                //         // the left operand. When an || expression has no contextual type, the right operand is contextually typed
                //         // by the type of the left operand, except for the special case of Javascript declarations of the form
                //         // `namespace.prop = namespace.prop || {}`.
                //         const type = getContextualType(binaryExpression, contextFlags);
                //         return node === right && (type && type.pattern || !type && !isDefaultedExpandoInitializer(binaryExpression)) ?
                //             getTypeOfExpression(left) : type;
                //     case SyntaxKind.AmpersandAmpersandToken:
                //     case SyntaxKind.CommaToken:
                //         return node === right ? getContextualType(binaryExpression, contextFlags) : undefined;
                //     default:
                //         return undefined;
                // }
            }
            SpreadAssignment(_) => {
                let parent_parent = self.parent(parent_id).unwrap();
                self.get_contextual_ty(parent_parent, flags)
            }
            ObjectPropAssignment(parent) => {
                self.get_contextual_type_for_object_literal_element(parent, flags)
            }
            ObjectShorthandMember(parent) => {
                self.get_contextual_type_for_object_literal_element(parent, flags)
            }
            ParenExpr(n) => {
                if self.node_query(n.id.module()).is_in_js_file(n.id) {
                    todo!()
                }
                self.get_contextual_ty(n.id, flags)
            }
            CallExpr(n) => self.get_contextual_ty_for_argument(n, id),
            NewExpr(n) => self.get_contextual_ty_for_argument(n, id),
            _ => None,
        }
    }

    fn get_contextual_ty_for_argument(
        &mut self,
        call: &impl ast::r#trait::CallLike<'cx>,
        argument: ast::NodeID,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let args = self.get_effective_call_arguments(call);
        let argument_index = match args {
            EffectiveCallArguments::Borrowed(args) => {
                args.iter().position(|arg| arg.id() == argument)?
            }
            EffectiveCallArguments::Owned(args) => args.iter().position(|arg| match arg {
                EffectiveCallArgument::Expression(expr) => expr.id() == argument,
                EffectiveCallArgument::Synthetic(_) => false,
            })?,
        };
        self.get_contextual_ty_for_argument_at_index(call, argument_index)
    }

    fn get_contextual_ty_for_argument_at_index(
        &mut self,
        call: &impl ast::r#trait::CallLike<'cx>,
        argument_index: usize,
    ) -> Option<&'cx ty::Ty<'cx>> {
        // TODO: import call

        let sig = if self.get_node_links(call.id()).get_resolved_sig() == Some(self.resolving_sig())
        {
            self.resolving_sig()
        } else {
            self.get_resolved_signature(call.id(), None)
        };

        // TODO: jsx
        Some(
            if sig.has_rest_param() && argument_index + 1 >= sig.params.len() {
                let rest_index = sig.params.len() - 1;
                let object_ty = self.get_type_of_symbol(sig.params[rest_index]);
                let index_ty = self
                    .get_number_literal_type::<false>((argument_index - rest_index).into(), None);
                self.get_indexed_access_ty(
                    object_ty,
                    index_ty,
                    Some(ty::AccessFlags::Contextual),
                    None,
                    None,
                    None,
                )
            } else {
                self.get_ty_at_pos(sig, argument_index)
            },
        )
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
                        None,
                    ))
                });
        }
        Some(contextual_return_ty)
    }

    fn get_contextual_type_for_object_literal_element(
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
            let symbol = self.get_symbol_of_declaration(id);
            let name = self.symbol(symbol).name;
            let name_ty = self.get_symbol_links(symbol).get_name_ty();
            return self.get_ty_of_property_of_contextual_ty(ty, name, name_ty);
        }
        let nq = self.node_query(id.module());
        if nq.has_dynamic_name(id)
            && let Some(name) = nq.get_name_of_decl(id)
            && let ast::DeclarationName::Computed(name) = name
            && let expr_ty = self.check_expression(name.expr, None)
            && expr_ty.usable_as_prop_name()
            && let prop_name = self.get_prop_name_from_ty(expr_ty)
            && let Some(prop_ty) = self.get_ty_of_property_of_contextual_ty(ty, prop_name, None)
        {
            Some(prop_ty)
        } else if let Some(name) = element.name() {
            let name_ty = self.get_literal_ty_from_prop_name(&name);
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
        self.get_contextual_type_for_object_literal_element(n, context_flags)
    }

    pub(super) fn is_resolving_ret_ty_of_sig(&mut self, sig: &'cx ty::Sig<'cx>) -> bool {
        if let Some(composite_sigs) = sig.composite_sigs
            && composite_sigs
                .iter()
                .any(|s| self.is_resolving_ret_ty_of_sig(s))
        {
            return true;
        }
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
            let ret_ty = self.get_return_type_of_signature(sig);
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
                    ) || this.get_awaited_ty_of_promise(t, None).is_some()
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

    fn get_contextual_ty_for_assign(
        &mut self,
        parent: &'cx ast::AssignExpr<'cx>,
        flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let kind = self
            .node_query(parent.id.module())
            .get_assignment_declaration_kind_for_assign_expr(parent);
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
            AssignmentDeclarationKind::Property => {
                // TODO: isPossiblyAliasedThisProperty
                // TODO: !can_have_symbol
                // TODO: let decl = self.final_res(parent.left.id());
                None
            }
            _ => {
                // TODO: other case
                None
            }
        }
    }

    fn get_contextual_ty_for_class_property_element(
        &mut self,
        n: &'cx ast::ClassPropElem<'cx>,
        flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if n.init.is_some() {
            if let Some(ty) = n.ty {
                // TODO: js
                return Some(self.get_ty_from_type_node(ty));
            } else if n
                .modifiers
                .is_some_and(|m| m.flags.contains(ast::ModifierFlags::STATIC))
            {
                // get_contextual_type_for_static_property_declaration
                if let Some(parent) = self.parent(n.id)
                    && self.p.node(parent).is_expression()
                    && let Some(parent_ty) = self.get_contextual_ty(parent, flags)
                {
                    let symbol = self.get_symbol_of_declaration(n.id);
                    let name = self.symbol(symbol).name;
                    return self.get_ty_of_property_of_contextual_ty(parent_ty, name, None);
                }
            }
        }
        None
    }

    fn get_contextual_ty_for_param_decl(
        &mut self,
        parent: &'cx ast::ParamDecl<'cx>,
        node: ast::NodeID,
        flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        debug_assert!(self.parent(node).is_some_and(|p| p == parent.id));
        if parent.init.is_some() {
            if let Some(ty) = parent.ty {
                // TODO: js
                return Some(self.get_ty_from_type_node(ty));
            } else if let Some(ty) = self.get_contextually_typed_parameter_ty(parent) {
                return Some(ty);
            } else if flags.is_some_and(|flags| flags.contains(ContextFlags::SKIP_BINDING_PATTERNS))
            {
                match parent.name.kind {
                    ast::BindingKind::ObjectPat(pat) if !pat.elems.is_empty() => {
                        return Some(self.get_ty_from_binding_pattern::<true>(parent.name));
                    }
                    ast::BindingKind::ArrayPat(pat) if !pat.elems.is_empty() => {
                        return Some(self.get_ty_from_binding_pattern::<true>(parent.name));
                    }
                    _ => {}
                }
            }
        }
        None
    }

    fn get_contextual_ty_for_var_decl(
        &mut self,
        parent: &'cx ast::VarDecl<'cx>,
        node: ast::NodeID,
        flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        debug_assert!(self.parent(node).is_some_and(|p| p == parent.id));
        if parent.init.is_some() {
            if let Some(decl_ty) = parent.ty {
                // TODO: js
                return Some(self.get_ty_from_type_node(decl_ty));
            } else if !flags
                .is_some_and(|flags| flags.contains(ContextFlags::SKIP_BINDING_PATTERNS))
            {
                match parent.name.kind {
                    ast::BindingKind::ObjectPat(pat) if !pat.elems.is_empty() => {
                        return Some(self.get_ty_from_binding_pattern::<true>(parent.name));
                    }
                    ast::BindingKind::ArrayPat(pat) if !pat.elems.is_empty() => {
                        return Some(self.get_ty_from_binding_pattern::<true>(parent.name));
                    }
                    _ => {}
                }
            };
        }
        None
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
                    && let Some(t) = this.get_ty_of_property_of_contextual_ty(
                        t,
                        SymbolName::EleNum(index.into()),
                        None,
                    )
                {
                    return Some(t);
                }
                // TODO: this.get_iterated_ty_or_element_ty()
                this.get_iterated_ty_or_element_ty(
                    IterationUse::ELEMENT,
                    t,
                    this.undefined_ty,
                    None,
                    true,
                )
            },
            true,
        )
    }

    pub(super) fn get_ty_of_property_of_contextual_ty(
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
                        let prop_ty =
                            this.get_ty_of_concrete_prop_of_contextual_ty(constituent_ty, name);
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
                } else if !t.flags.contains(TypeFlags::OBJECT) {
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
            && let Some(rest_ty) =
                self.get_element_ty_of_slice_of_tuple_ty::<false, true>(ty, tup.fixed_length, 0)
        {
            Some(rest_ty)
        } else {
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
    }

    fn get_ty_of_concrete_prop_of_contextual_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        name: SymbolName,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let prop = self.get_prop_of_ty::<false, false>(ty, name)?;
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
            true_ty.flags.contains(TypeFlags::NEVER)
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

    fn discriminate_contextual_ty_by_object_members(
        &mut self,
        n: &'cx ast::ObjectLit<'cx>,
        contextual_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        debug_assert!(contextual_ty.kind.is_union());
        let key = DiscriminateContextualTyByObjectLiteral::new(n, contextual_ty);
        if let Some(cached) = self
            .discriminant_context_ty_by_object_literal_cache
            .get(&key)
        {
            return cached;
        }

        let res = self
            .get_matching_union_constituent_for_object_literal(contextual_ty, n)
            .unwrap_or_else(|| {
                enum DiscriminatingItem<'cx> {
                    Node(&'cx ast::ObjectMember<'cx>),
                    Prop(SymbolID),
                }
                #[derive(Debug, Clone, Copy)]
                enum DiscriminatingContext<'cx> {
                    Ident(&'cx ast::Ident),
                    Init(&'cx ast::Expr<'cx>),
                    None,
                }
                let discriminators = n
                    .members
                    .iter()
                    .map(|item| DiscriminatingItem::Node(*item))
                    .chain(
                        self.get_props_of_ty(contextual_ty)
                            .iter()
                            .map(|item| DiscriminatingItem::Prop(*item)),
                    )
                    .filter_map(|item| match item {
                        DiscriminatingItem::Node(item) => match item.kind {
                            ast::ObjectMemberKind::Shorthand(n) => {
                                let name = self.binder.symbol(self.final_res(n.id)).name;
                                if self.is_discriminant_prop(contextual_ty, name) {
                                    Some((DiscriminatingContext::Ident(n.name), name))
                                } else {
                                    None
                                }
                            }
                            ast::ObjectMemberKind::PropAssignment(n) => {
                                let name = self.binder.symbol(self.final_res(n.id)).name;
                                if n.init.kind.is_possibly_discriminant_value()
                                    && self.is_discriminant_prop(contextual_ty, name)
                                {
                                    Some((DiscriminatingContext::Init(n.init), name))
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        },
                        DiscriminatingItem::Prop(item) => {
                            let prop_symbol = self.symbol(item);
                            if !prop_symbol.flags.contains(SymbolFlags::OPTIONAL) {
                                return None;
                            }
                            let s = self.binder.symbol(self.final_res(n.id));
                            let Some(members) = s.members.as_ref() else {
                                return None;
                            };
                            let name = s.name;
                            if !members.0.contains_key(&name)
                                && self.is_discriminant_prop(contextual_ty, name)
                            {
                                Some((DiscriminatingContext::None, name))
                            } else {
                                None
                            }
                        }
                    })
                    .map(|(item, name)| {
                        (
                            move |this: &mut Self| match item {
                                DiscriminatingContext::Ident(n) => {
                                    this.get_context_free_ty_of_ident(n)
                                }
                                DiscriminatingContext::Init(n) => {
                                    this.get_context_free_ty_of_expr(n)
                                }
                                DiscriminatingContext::None => this.undefined_ty,
                            },
                            name,
                        )
                    })
                    .collect::<Vec<_>>();

                self.discriminate_ty_by_discriminable_items(
                    contextual_ty,
                    &discriminators,
                    |this, s, t| {
                        if this.is_type_assignable_to(s, t) {
                            Ternary::TRUE
                        } else {
                            Ternary::FALSE
                        }
                    },
                )
            });

        let prev = self
            .discriminant_context_ty_by_object_literal_cache
            .insert(key, res);
        debug_assert!(prev.is_none());
        res
    }

    pub(super) fn discriminate_ty_by_discriminable_items<F>(
        &mut self,
        target: &'cx ty::Ty<'cx>,
        discriminators: &[(F, SymbolName)],
        related: impl Fn(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>) -> Ternary,
    ) -> &'cx ty::Ty<'cx>
    where
        F: Fn(&mut Self) -> &'cx ty::Ty<'cx>,
    {
        let u = target.kind.expect_union();
        let tys = u.tys;
        let mut include: Vec<Ternary> = tys
            .iter()
            .map(|t| {
                if t.flags.intersects(TypeFlags::PRIMITIVE)
                    || self.get_reduced_ty(t).flags.contains(TypeFlags::NEVER)
                {
                    Ternary::FALSE
                } else {
                    Ternary::TRUE
                }
            })
            .collect();
        for (get_discriminating_ty, property_name) in discriminators {
            let mut matched = false;
            for i in 0..tys.len() {
                if include[i] != Ternary::FALSE {
                    if let Some(target_ty) = self.get_ty_of_prop_of_ty(tys[i], *property_name) {
                        let discriminating_ty = get_discriminating_ty(self);
                        if self.some_type(discriminating_ty, |this, t| {
                            related(this, t, target_ty) != Ternary::FALSE
                        }) {
                            matched = true;
                        } else {
                            include[i] = Ternary::MAYBE;
                        }
                    }
                }
            }
            for i in 0..tys.len() {
                if include[i] == Ternary::MAYBE {
                    include[i] = if matched {
                        Ternary::FALSE
                    } else {
                        Ternary::TRUE
                    };
                }
            }
        }
        let filtered = if include.iter().any(|&t| t == Ternary::FALSE) {
            let filtered_tys: Vec<_> = tys
                .iter()
                .enumerate()
                .filter(|(i, _)| include[*i] != Ternary::FALSE)
                .map(|(_, t)| *t)
                .collect();
            self.get_union_ty::<false>(
                &filtered_tys,
                ty::UnionReduction::None,
                None,
                None,
                None,
                None,
            )
        } else {
            target
        };
        if filtered.flags.contains(TypeFlags::NEVER) {
            target
        } else {
            filtered
        }
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
        if !flags.is_some_and(|flags| {
            flags.contains(ContextFlags::NO_CONSTRAINTS) && instantiated_ty.kind.is_type_variable()
        }) {
            let apparent_ty = self
                .map_ty(
                    instantiated_ty,
                    |this, t| {
                        Some(if t.get_object_flags().contains(ObjectFlags::MAPPED) {
                            t
                        } else {
                            this.get_apparent_ty(t)
                        })
                    },
                    true,
                )
                .unwrap();
            return if apparent_ty.kind.is_union() {
                if let Some(n) = self.p.node(node).as_object_lit() {
                    Some(self.discriminate_contextual_ty_by_object_members(n, apparent_ty))
                } else {
                    // TODO: is_jsx_attribute
                    Some(apparent_ty)
                }
            } else {
                Some(apparent_ty)
            };
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
        if let Some(first_param) = sig.params.first()
            && self.symbol(*first_param).name == SymbolName::Atom(keyword::KW_THIS)
        {
            target_params_count -= 1;
        }
        !self.has_effective_rest_param(sig) && sig.get_param_count(self) < target_params_count
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
            .copied()
            .collect::<Vec<_>>();
        if sigs.len() == 1 {
            Some(sigs[0])
        } else {
            self.get_intersected_sigs(&sigs)
        }
    }

    pub(super) fn compare_ty_params_identical(
        &mut self,
        source: Option<ty::Tys<'cx>>,
        target: Option<ty::Tys<'cx>>,
    ) -> bool {
        let Some(sources) = source else { return true };
        let Some(targets) = target else { return true };
        if sources.len() != targets.len() {
            return false;
        }

        let mapper = self.create_ty_mapper(targets, sources);
        for (source, target) in sources.iter().zip(targets.iter()) {
            if source == target {
                continue;
            }
            let source = self
                .get_constraint_from_ty_param(source)
                .unwrap_or(self.unknown_ty);
            let target = self
                .get_constraint_from_ty_param(target)
                .unwrap_or(self.unknown_ty);
            let target = self.instantiate_ty(target, Some(mapper));
            if !self.is_type_identical_to(source, target) {
                return false;
            }
        }
        true
    }

    pub(super) fn get_parameter_name_at_position(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        pos: usize,
        override_rest_ty: Option<&'cx ty::Ty<'cx>>,
    ) -> SymbolName {
        let param_count = sig.params.len() - if sig.has_rest_param() { 1 } else { 0 };
        if pos < param_count {
            return self.symbol(sig.params[pos]).name;
        }
        let rest_param = sig.params.get(param_count).copied().unwrap_or(Symbol::ERR);
        let rest_ty = override_rest_ty.unwrap_or_else(|| self.get_type_of_symbol(rest_param));
        if let Some(tuple_ty) = rest_ty.as_tuple() {
            let index = pos - param_count;
            todo!()
            // const associatedName = tupleType.labeledElementDeclarations?.[index];
            // const elementFlags = tupleType.elementFlags[index];
            // return getTupleElementLabel(associatedName, index, elementFlags, restParameter);
        } else {
            self.symbol(rest_param).name
        }
    }

    fn get_intersected_sigs(&mut self, sigs: &[&'cx ty::Sig<'cx>]) -> Option<&'cx ty::Sig<'cx>> {
        if self.config.compiler_options().no_implicit_any() {
            let first = sigs.first().copied();
            sigs.iter().skip(1).fold(first, |left, right| {
                if left == Some(right) || left.is_none() {
                    left
                } else if let Some(left) = left
                    && let left_ty_params = self.get_sig_links(left.id).get_ty_params()
                    && let right_ty_params = self.get_sig_links(right.id).get_ty_params()
                    && self.compare_ty_params_identical(left_ty_params, right_ty_params)
                {
                    // combine signatures of intersection members
                    let ty_params = left_ty_params.or(right_ty_params);
                    let param_mapper = if let Some(left_ty_mapper) = left_ty_params
                        && let Some(right_ty_mapper) = right_ty_params
                    {
                        Some(self.create_ty_mapper(right_ty_mapper, left_ty_mapper)
                            as &dyn ty::TyMap<'cx>)
                    } else {
                        None
                    };
                    let mut flags = (left.flags | right.flags)
                        & ty::SigFlags::PROPAGATING_FLAGS
                            .intersection(ty::SigFlags::HAS_REST_PARAMETER.complement());
                    let params = {
                        // combine_intersection_params
                        let left_count = left.get_param_count(self);
                        let right_count = right.get_param_count(self);
                        let longest = if left_count >= right_count {
                            left
                        } else {
                            right
                        };
                        let shorter = if longest == left { right } else { left };
                        let longest_count = if longest == left {
                            left_count
                        } else {
                            right_count
                        };
                        let either_has_effective_rest = self.has_effective_rest_param(left)
                            || self.has_effective_rest_param(right);
                        let needs_extra_rest_element =
                            either_has_effective_rest && !self.has_effective_rest_param(longest);
                        let mut params = Vec::with_capacity(
                            longest_count + (if needs_extra_rest_element { 1 } else { 0 }),
                        );
                        for i in 0..longest_count {
                            let mut longest_param_type =
                                self.try_get_ty_at_pos(longest, i).unwrap();
                            if longest.eq(right) {
                                longest_param_type =
                                    self.instantiate_ty(longest_param_type, param_mapper);
                            }
                            let mut shorter_param_type = self
                                .try_get_ty_at_pos(shorter, i)
                                .unwrap_or(self.unknown_ty);
                            if shorter.eq(right) {
                                shorter_param_type =
                                    self.instantiate_ty(shorter_param_type, param_mapper);
                            }
                            let union_param_type = self.get_union_ty::<false>(
                                &[longest_param_type, shorter_param_type],
                                ty::UnionReduction::Lit,
                                None,
                                None,
                                None,
                                None,
                            );
                            let is_rest_params = either_has_effective_rest
                                && !needs_extra_rest_element
                                && i == (longest_count - 1);
                            let is_optional = i >= self.get_min_arg_count(longest)
                                && i >= self.get_min_arg_count(shorter);
                            let left_name = if i >= left_count {
                                None
                            } else {
                                Some(self.get_parameter_name_at_position(left, i, None))
                            };
                            let right_name = if i >= right_count {
                                None
                            } else {
                                Some(self.get_parameter_name_at_position(right, i, None))
                            };
                            let param_name = if left_name == right_name {
                                left_name
                            } else if left_name.is_none() {
                                right_name
                            } else if right_name.is_none() {
                                left_name
                            } else {
                                None
                            };

                            let flags = SymbolFlags::FUNCTION_SCOPED_VARIABLE
                                | if is_optional && !is_rest_params {
                                    SymbolFlags::OPTIONAL
                                } else {
                                    SymbolFlags::empty()
                                };
                            let name = param_name.unwrap_or(SymbolName::ParamIndex(i as u32));
                            let links = links::SymbolLinks::default()
                                .with_ty(if is_rest_params {
                                    self.create_array_ty(union_param_type, false)
                                } else {
                                    union_param_type
                                })
                                .with_check_flags(if is_rest_params {
                                    CheckFlags::REST_PARAMETER
                                } else if is_optional {
                                    CheckFlags::OPTIONAL_PARAMETER
                                } else {
                                    CheckFlags::empty()
                                });
                            let param_symbol = self.create_transient_symbol(
                                name,
                                flags | SymbolFlags::TRANSIENT,
                                links,
                                None,
                                None,
                                None,
                            );
                            params.push(param_symbol);
                        }
                        params
                    };
                    let last_param = params.last().copied();
                    if let Some(last_param) = last_param
                        && self
                            .get_check_flags(last_param)
                            .contains(CheckFlags::REST_PARAMETER)
                    {
                        flags.insert(ty::SigFlags::HAS_REST_PARAMETER);
                    }
                    let this_param = {
                        // combine_intersection_this_param
                        let left_this_param = self.get_sig_links(left.id).get_this_param();
                        let right_this_param = self.get_sig_links(right.id).get_this_param();
                        match (left_this_param, right_this_param) {
                            (None, None) => None,
                            (None, Some(_)) => right_this_param,
                            (Some(_), None) => left_this_param,
                            (Some(left_this_param), Some(right_this_param)) => {
                                let left_this_ty = self.get_type_of_symbol(left_this_param);
                                let right_this_ty = self.get_type_of_symbol(right_this_param);
                                let right_this_ty =
                                    self.instantiate_ty(right_this_ty, param_mapper);
                                let this_ty = self.get_union_ty::<false>(
                                    &[left_this_ty, right_this_ty],
                                    ty::UnionReduction::Lit,
                                    None,
                                    None,
                                    None,
                                    None,
                                );
                                Some(self.create_transient_symbol_with_ty(left_this_param, this_ty))
                            }
                        }
                    };
                    let min_args_count = left.min_args_count.max(right.min_args_count);
                    let composite_sigs = if left.composite_kind == Some(TypeFlags::INTERSECTION)
                        && let Some(left_composite_sigs) = left.composite_sigs
                    {
                        // TODO: reduce alloc
                        let mut composite_sigs = Vec::with_capacity(left_composite_sigs.len() + 1);
                        composite_sigs.extend_from_slice(left_composite_sigs);
                        composite_sigs.push(*right);
                        self.alloc(composite_sigs)
                    } else {
                        self.alloc(vec![left, *right])
                    };
                    let mapper = param_mapper.map(|mapper| {
                        if left.composite_kind == Some(TypeFlags::INTERSECTION)
                            && left.composite_sigs.is_some()
                            && left.mapper.is_some()
                        {
                            self.combine_ty_mappers(left.mapper, mapper)
                        } else {
                            mapper
                        }
                    });
                    let res = ty::Sig {
                        id: ty::SigID::dummy(),
                        node_id: left.node_id,
                        class_decl: left.class_decl,
                        flags,
                        params: self.alloc(params),
                        min_args_count,
                        ret: None,
                        target: None,
                        mapper,
                        composite_sigs: Some(self.alloc(composite_sigs)),
                        composite_kind: Some(TypeFlags::INTERSECTION),
                    };
                    let mut sig_links = super::links::SigLinks::default();
                    if let Some(ty_params) = ty_params {
                        sig_links.set_ty_params(ty_params)
                    };
                    if let Some(this_param) = this_param {
                        sig_links.set_this_param(this_param)
                    };
                    let sig = self.new_sig(res);
                    let sig_links = self.sig_links.insert(sig.id, sig_links);
                    debug_assert!(sig_links.is_none());
                    Some(sig)
                } else {
                    None
                }
            })
        } else {
            None
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

    pub(super) fn create_union_sig(
        &mut self,
        sig: &ty::Sig<'cx>,
        union_sigs: ty::Sigs<'cx>,
    ) -> &'cx ty::Sig<'cx> {
        let next = ty::Sig {
            id: ty::SigID::dummy(),
            flags: sig.flags & ty::SigFlags::PROPAGATING_FLAGS,
            params: sig.params,
            min_args_count: sig.min_args_count,
            ret: sig.ret,
            node_id: sig.node_id,
            target: None,
            mapper: None,
            class_decl: sig.class_decl,
            composite_sigs: Some(union_sigs),
            composite_kind: Some(ty::TypeFlags::UNION),
        };
        let mut links = super::links::SigLinks::default();
        if let Some(ty_params) = self.get_sig_links(sig.id).get_ty_params() {
            links.set_ty_params(ty_params);
        }
        if let Some(this_param) = self.get_sig_links(sig.id).get_this_param() {
            links.set_this_param(this_param);
        }
        let s = self.new_sig(next);
        let prev = self.sig_links.insert(s.id, links);
        debug_assert!(prev.is_none());
        s
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
                    let sigs = self.alloc(sigs);
                    self.create_union_sig(sigs[0], sigs);
                }
            }
        } else {
            return self.get_contextual_call_sig(ty, id);
        }
        None
    }

    pub(super) fn is_context_sensitive_fn_or_object_literal_method(&self, id: ast::NodeID) -> bool {
        let node = self.p.node(id);
        (node.is_fn_expr_or_arrow_fn_expr() || self.p.is_object_lit_method(id))
            && self.is_context_sensitive_fn_like(id)
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
