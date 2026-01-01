use bolt_ts_atom::Atom;
use bolt_ts_binder::AssignmentKind;
use bolt_ts_binder::FlowFlags;
use bolt_ts_binder::SymbolID;
use bolt_ts_binder::{SymbolFlags, SymbolName};
use bolt_ts_config::Target;
use bolt_ts_span::Span;
use bolt_ts_utils::FxIndexMap;
use bolt_ts_utils::{ensure_sufficient_stack, fx_indexmap_with_capacity};

use crate::check::node_check_flags::NodeCheckFlags;
use crate::ty::CheckFlags;
use crate::ty::TypeFlags;

use super::ObjectFlags;
use super::TyChecker;
use super::ast;
use super::errors;
use super::symbol_info::SymbolInfo;
use super::ty;
use super::ty::AccessFlags;
use super::{CheckMode, InferenceContextId, SymbolLinks, TyLinks};

pub(super) fn get_suggestion_boolean_op(op: &str) -> Option<&str> {
    match op {
        "^" | "^=" => Some("!=="),
        "&" | "&=" => Some("&&"),
        "|" | "|=" => Some("||"),
        _ => None,
    }
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug)]
    pub struct IterationUse: u8 {
        const ALLOWS_SYNC_ITERABLES_FLAG    = 1 << 0;
        const ALLOWS_ASYNC_ITERABLES_FLAG   = 1 << 1;
        const ALLOWS_STRING_INPUT_FLAG      = 1 << 2;
        const FOR_OF_FLAG                   = 1 << 3;
        const YIELD_STAR_FLAG               = 1 << 4;
        const SPREAD_FLAG                   = 1 << 5;
        const DESTRUCTURING_FLAG            = 1 << 6;
        const POSSIBLY_OUT_OF_BOUNDS        = 1 << 7;

        const ELEMENT = Self::ALLOWS_SYNC_ITERABLES_FLAG.bits();
        const SPREAD = Self::ALLOWS_SYNC_ITERABLES_FLAG.bits()
            | Self::SPREAD_FLAG.bits();
        const DESTRUCTURING = Self::ALLOWS_SYNC_ITERABLES_FLAG.bits()
            | Self::DESTRUCTURING_FLAG.bits();
        const FOR_OF = Self::ALLOWS_SYNC_ITERABLES_FLAG.bits()
            | Self::ALLOWS_STRING_INPUT_FLAG.bits()
            | Self::FOR_OF_FLAG.bits();
        const FOR_AWAIT_OF = Self::ALLOWS_SYNC_ITERABLES_FLAG.bits()
            | Self::ALLOWS_ASYNC_ITERABLES_FLAG.bits()
            | Self::ALLOWS_STRING_INPUT_FLAG.bits()
            | Self::FOR_OF_FLAG.bits();
        const YIELD_STAR = Self::ALLOWS_SYNC_ITERABLES_FLAG.bits()
            | Self::YIELD_STAR_FLAG.bits();
        const ASYNC_YIELD_STAR = Self::ALLOWS_SYNC_ITERABLES_FLAG.bits()
            | Self::ALLOWS_ASYNC_ITERABLES_FLAG.bits()
            | Self::YIELD_STAR_FLAG.bits();
        const GENERATOR_RETURN_TYPE = Self::ALLOWS_SYNC_ITERABLES_FLAG.bits();
        const ASYNC_GENERATOR_RETURN_TYPE = Self::ALLOWS_ASYNC_ITERABLES_FLAG.bits();
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_fresh_ty(&self, ty: &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> {
        if ty.flags.intersects(TypeFlags::BOOLEAN_LITERAL) {
            if ty == self.true_ty || ty == self.regular_true_ty {
                Some(self.true_ty)
            } else if ty == self.false_ty || ty == self.regular_false_ty {
                Some(self.false_ty)
            } else {
                unreachable!()
            }
        } else if let Some(links) = ty.fresh_ty_links_id() {
            self.fresh_ty_links_arena[links].get_fresh_ty()
        } else {
            unreachable!()
        }
    }

    fn set_fresh_ty(&mut self, ty: &'cx ty::Ty<'cx>, fresh_ty: &'cx ty::Ty<'cx>) {
        let links = ty.fresh_ty_links_id().unwrap();
        self.fresh_ty_links_arena[links].set_fresh_ty(fresh_ty);
    }

    pub(super) fn get_regular_ty(&self, ty: &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> {
        if ty.flags.intersects(TypeFlags::BOOLEAN_LITERAL) {
            if ty == self.true_ty || ty == self.regular_true_ty {
                Some(self.regular_true_ty)
            } else if ty == self.false_ty || ty == self.regular_false_ty {
                Some(self.regular_false_ty)
            } else {
                unreachable!()
            }
        } else if let Some(links) = ty.fresh_ty_links_id() {
            self.fresh_ty_links_arena[links].get_regular_ty()
        } else {
            unreachable!()
        }
    }

    fn set_regular_ty(&mut self, ty: &'cx ty::Ty<'cx>, regular_ty: &'cx ty::Ty<'cx>) {
        let links = ty.fresh_ty_links_id().unwrap();
        self.fresh_ty_links_arena[links].set_regular_ty(regular_ty);
    }

    pub(super) fn get_fresh_ty_of_literal_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty.flags.intersects(TypeFlags::FRESHABLE) {
            if let Some(fresh_ty) = self.get_fresh_ty(ty) {
                fresh_ty
            } else {
                let links = self.fresh_ty_links_arena.alloc(Default::default());
                let fresh_ty = match ty.kind {
                    ty::TyKind::NumberLit(lit) => {
                        let t = self.alloc(ty::NumberLitTy {
                            val: lit.val,
                            symbol: lit.symbol,
                            links,
                        });
                        self.new_ty(ty::TyKind::NumberLit(t), ty.flags)
                    }
                    ty::TyKind::StringLit(lit) => {
                        let t = self.alloc(ty::StringLitTy {
                            val: lit.val,
                            symbol: lit.symbol,
                            links,
                        });
                        self.new_ty(ty::TyKind::StringLit(t), ty.flags)
                    }
                    ty::TyKind::BigIntLit(lit) => {
                        let t = self.alloc(ty::BigIntLitTy {
                            val: lit.val,
                            neg: lit.neg,
                            links,
                        });
                        self.new_ty(ty::TyKind::BigIntLit(t), ty.flags)
                    }
                    _ => unreachable!(),
                };
                self.fresh_ty_links_arena[links].set_fresh_ty(fresh_ty);
                self.fresh_ty_links_arena[links].set_regular_ty(ty);
                self.set_fresh_ty(ty, fresh_ty);
                assert!(self.get_regular_ty(ty).is_some_and(|t| t == ty));
                fresh_ty
            }
        } else {
            ty
        }
    }

    pub(super) fn check_num_lit(&mut self, val: f64) -> &'cx ty::Ty<'cx> {
        // TODO: check grammar
        let t = self.get_number_literal_type_from_number(val);
        self.get_fresh_ty_of_literal_ty(t)
    }

    #[inline]
    pub(super) fn check_string_lit(&mut self, val: Atom) -> &'cx ty::Ty<'cx> {
        // TODO: hasSkipDirectInferenceFlag
        let t = self.get_string_literal_type_from_string(val);
        self.get_fresh_ty_of_literal_ty(t)
    }

    pub(super) fn check_bigint_lit(&mut self, neg: bool, val: Atom) -> &'cx ty::Ty<'cx> {
        // TODO: check grammar
        let t = self.get_bigint_literal_type(neg, val);
        self.get_fresh_ty_of_literal_ty(t)
    }

    pub(super) fn check_expr(&mut self, expr: &'cx ast::Expr<'cx>) -> &'cx ty::Ty<'cx> {
        use bolt_ts_ast::ExprKind::*;
        let saved_current_node = self.current_node;
        self.current_node = Some(expr.id());
        self.instantiation_count = 0;
        let ty = match expr.kind {
            Bin(bin) => ensure_sufficient_stack(|| self.check_bin_expr(bin)),
            NumLit(lit) => self.check_num_lit(lit.val),
            StringLit(lit) => self.check_string_lit(lit.val),
            NoSubstitutionTemplateLit(lit) => self.check_string_lit(lit.val),
            BigIntLit(lit) => self.check_bigint_lit(lit.val.0, lit.val.1),
            BoolLit(lit) => {
                if lit.val {
                    self.true_ty
                } else {
                    self.false_ty
                }
            }
            NullLit(_) => self.null_widening_ty,
            Ident(ident) => self.check_ident(ident),
            ArrayLit(lit) => self.check_array_lit(lit, false),
            Omit(_) => self.undefined_ty,
            Paren(paren) => self.check_expr(paren.expr),
            Cond(cond) => self.check_cond(cond),
            ObjectLit(lit) => self.check_object_lit(lit),
            Call(call) => self.check_call_like_expr(call),
            New(call) => self.check_call_like_expr(call),
            Fn(f) => self.check_fn_like_expr(f),
            ArrowFn(f) => self.check_fn_like_expr(f),
            Assign(assign) => self.check_assign_expr(assign),
            PrefixUnary(unary) => self.check_prefix_unary_expr(unary),
            PostfixUnary(unary) => self.check_postfix_unary_expr(unary),
            Class(class) => {
                self.check_class_like_decl(class);
                self.get_type_of_symbol(self.get_symbol_of_decl(class.id))
            }
            EleAccess(node) => self.check_ele_access_expr(node),
            PropAccess(node) => self.check_prop_access_expr(node),
            Typeof(n) => {
                self.check_expr(n.expr);
                self.typeof_ty()
            }
            Void(n) => {
                self.check_expr(n.expr);
                self.undefined_ty
            }
            This(n) => self.check_this_expr(n),
            Super(n) => self.check_super_expr(n),
            As(n) => self.check_assertion(n.id, n.expr, n.ty),
            TyAssertion(n) => self.check_assertion(n.id, n.expr, n.ty),
            Satisfies(n) => self.check_expr(n.expr),
            NonNull(n) => self.check_non_null_assertion(n),
            Template(n) => self.check_template_expr(n),
            ExprWithTyArgs(n) => self.check_expr_with_ty_args(n),
            SpreadElement(n) => self.check_spread_element(n),
            RegExpLit(_) => self.global_regexp_ty(),
            TaggedTemplate(n) => self.check_tagged_template_expr(n),
            Delete(n) => self.check_delete_expr(n),
            JsxElem(_) => {
                // TODO:
                self.undefined_ty
            }
            JsxSelfClosingElem(_) => {
                // TODO:
                self.undefined_ty
            }
            JsxFrag(_) => {
                // TODO:
                self.undefined_ty
            }
            Await(_) => {
                // TODO:
                self.undefined_ty
            }
        };
        let ty = self.instantiate_ty_with_single_generic_call_sig(expr.id(), ty);
        self.current_node = saved_current_node;
        ty
    }

    fn check_delete_expr(&mut self, node: &'cx ast::DeleteExpr<'cx>) -> &'cx ty::Ty<'cx> {
        self.check_expr(node.expr);

        let expr = ast::Expr::skip_parens(node.expr);
        if !expr.kind.is_access_expr() {
            let error =
                errors::TheOperandOfADeleteOperatorMustBeAPropertyReference { span: expr.span() };
            self.push_error(Box::new(error));
            return self.boolean_ty();
        }

        let links = self.get_node_links(expr.id());
        let Some(symbol) = links.get_resolved_symbol() else {
            return self.boolean_ty();
        };

        let symbol = self.get_export_symbol_of_value_symbol_if_exported(symbol);
        if self.is_readonly_symbol(symbol) {
            let error =
                errors::TheOperandOfADeleteOperatorCannotBeAReadOnlyProperty { span: expr.span() };
            self.push_error(Box::new(error));
        }

        self.boolean_ty()
    }

    pub(super) fn check_super_expr(&mut self, node: &'cx ast::SuperExpr) -> &'cx ty::Ty<'cx> {
        let is_call_expr = self
            .p
            .node(self.parent(node.id).unwrap())
            .as_call_expr()
            .is_some_and(|call| call.expr.id() == node.id);
        let immediate_container = self
            .node_query(node.id.module())
            .get_super_container(node.id, true);
        let mut container = immediate_container;
        let need_to_capture_lexical_this = false;
        let is_async_function = false;

        if !is_call_expr {
            loop {
                let Some(c) = container else {
                    break;
                };
                if !self.p.node(c).is_arrow_fn_expr() {
                    break;
                };
                container = self.node_query(c.module()).get_super_container(c, true);
            }
        }

        if container.is_none_or(|c| {
            // is legal usage of super expr?
            !(if is_call_expr {
                self.p.node(c).is_class_ctor()
            } else {
                let p = self.parent(c).unwrap();
                let p_node = self.p.node(p);
                if p_node.is_class_like() || p_node.is_object_lit() {
                    let node = self.p.node(c);
                    if node.is_class_method_elem()
                        || node.is_object_method_member()
                        || node.is_method_signature()
                        || node.is_getter_decl()
                        || node.is_setter_decl()
                        || node.is_class_prop_elem()
                    {
                        true
                    } else if node.is_static() {
                        node.is_class_static_block_decl()
                    } else {
                        node.is_prop_signature() || node.is_class_ctor()
                    }
                } else {
                    false
                }
            })
        }) {
            let current = self
                .node_query(node.id.module())
                .find_ancestor(node.id, |n| {
                    if container.is_some_and(|c| c == n.id()) {
                        Some(false)
                    } else if n.is_computed_prop_name() {
                        Some(true)
                    } else {
                        None
                    }
                });
            let error: bolt_ts_middle::Diag = if current
                .is_some_and(|c| self.p.node(c).is_computed_prop_name())
            {
                Box::new(errors::SuperCannotBeReferencedInAComputedPropertyName { span: node.span })
            } else if is_call_expr {
                Box::new(errors::SuperCallsAreNotPermittedOutsideConstructorsOrInNestedFunctionsInsideConstructors { span: node.span })
            } else if container.is_none_or(|c| {
                self.parent(c).is_none_or(|p| {
                    let p = self.p.node(p);
                    !(p.is_class_like() || p.is_object_lit())
                })
            }) {
                Box::new(errors::SuperCanOnlyBeReferencedInMembersOfDerivedClassesOrObjectLiteralExpressions {
                    span: node.span,
                })
            } else {
                Box::new(errors::SuperPropertyAccessIsPermittedOnlyInAConstructorMemberFunctionOrMemberAccessorOfADerivedClass {
                    span: node.span,
                })
            };
            self.push_error(error);

            return self.error_ty;
        }

        let container = container.unwrap();
        let node_check_flags;
        if is_call_expr || self.p.node(container).is_static() {
            node_check_flags = NodeCheckFlags::SUPER_STATIC;
        } else {
            node_check_flags = NodeCheckFlags::SUPER_INSTANCE;
        }

        let _ = self.get_node_links(node.id);
        self.get_mut_node_links(node.id)
            .config_flags(|old| old | node_check_flags);

        let class_like_decl = self.parent(container).unwrap();

        let has_extends = match self.p.node(class_like_decl) {
            ast::Node::ClassDecl(c) => c.extends.is_some(),
            ast::Node::ClassExpr(c) => c.extends.is_some(),
            _ => unreachable!("class like: {:#?}", self.p.node(class_like_decl)),
        };
        if !has_extends {
            let error = errors::SuperCanOnlyBeReferencedInADerivedClass { span: node.span };
            self.push_error(Box::new(error));
            return self.error_ty;
        }

        let class_ty = self.get_declared_ty_of_symbol(self.get_symbol_of_decl(class_like_decl));
        let Some(base_class_ty) = self.get_base_tys(class_ty).first() else {
            return self.error_ty;
        };

        if node_check_flags == NodeCheckFlags::SUPER_STATIC {
            self.get_base_constructor_type_of_class(class_ty)
        } else {
            let this_ty = Self::this_ty(class_ty);
            self.get_ty_with_this_arg(base_class_ty, this_ty, false)
        }
    }

    fn check_tagged_template_expr(
        &mut self,
        node: &'cx ast::TaggedTemplateExpr<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let sig = self.get_resolved_sig(node.id);
        self.get_ret_ty_of_sig(sig)
    }

    fn check_spread_element(&mut self, node: &'cx ast::SpreadElement<'cx>) -> &'cx ty::Ty<'cx> {
        let array_or_iterable_ty = self.check_expr(node.expr);
        self.check_iterated_ty_or_element_ty(
            IterationUse::SPREAD,
            array_or_iterable_ty,
            self.undefined_ty,
            Some(node.expr.id()),
        )
    }

    pub(super) fn check_iterated_ty_or_element_ty(
        &mut self,
        mode: IterationUse,
        input_ty: &'cx ty::Ty<'cx>,
        send_ty: &'cx ty::Ty<'cx>,
        error_node: Option<ast::NodeID>,
    ) -> &'cx ty::Ty<'cx> {
        if self.is_type_any(input_ty) {
            return input_ty;
        }
        self.get_iterated_ty_or_elem_ty(mode, input_ty, send_ty, error_node, true)
            .unwrap_or(self.any_ty)
    }

    fn get_iteration_tys_of_iter(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mode: IterationUse,
        error_node: Option<ast::NodeID>,
    ) -> ty::IterationTys<'cx> {
        if self.is_type_any(ty) {
            return self.any_iteration_tys();
        }

        // TODO: more case:

        // if !ty.flags.intersects(TypeFlags::UNION) {}

        // let cache_key = if mode.intersects(IterationUse::ALLOWS_ASYNC_ITERABLES_FLAG) {
        // } else {
        // };
        self.any_iteration_tys()
    }

    fn get_iterated_ty_or_elem_ty(
        &mut self,
        mode: IterationUse,
        input_ty: &'cx ty::Ty<'cx>,
        send_ty: &'cx ty::Ty<'cx>,
        error_node: Option<ast::NodeID>,
        check_assignability: bool,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let allow_async_iterables = mode.intersects(IterationUse::ALLOWS_ASYNC_ITERABLES_FLAG);
        if input_ty == self.never_ty {
            if let Some(error_node) = error_node {
                // TODO: report
            }
            return None;
        }
        let uplevel_iteration = *self.config.target() >= Target::ES2015;
        let downlevel_iteration = false; // !uplevel_iteration && false;
        let possible_out_of_bounds = self.config.no_unchecked_indexed_access()
            && mode.intersects(IterationUse::POSSIBLY_OUT_OF_BOUNDS);
        if uplevel_iteration || downlevel_iteration || allow_async_iterables {
            let iteration_tys = self.get_iteration_tys_of_iter(
                input_ty,
                mode,
                if uplevel_iteration { error_node } else { None },
            );
            // if check_assignability {}
            // if iteration_tys || uplevel_iteration {
            //     return if possible_out_of_bounds {};
            // }
        }
        None
    }

    fn is_template_literal_contextual_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.flags
            .intersects(TypeFlags::STRING_LITERAL.union(TypeFlags::TEMPLATE_LITERAL))
            || ty.flags.intersects(TypeFlags::INSTANTIABLE_NON_PRIMITIVE) && {
                let t = self
                    .get_base_constraint_of_ty(ty)
                    .unwrap_or(self.unknown_ty);
                t.maybe_type_of_kind(TypeFlags::STRING_LIKE)
            }
    }

    fn check_template_expr(&mut self, node: &'cx ast::TemplateExpr<'cx>) -> &'cx ty::Ty<'cx> {
        let mut texts = Vec::with_capacity(8);
        texts.push(node.head.text);
        let mut tys = Vec::with_capacity(8);
        for span in node.spans {
            let ty = self.check_expr(span.expr);
            texts.push(span.text);
            if self.is_type_assignable_to(ty, self.template_constraint_ty()) {
                tys.push(ty);
            } else {
                tys.push(self.string_ty);
            }
        }
        if self.is_const_context(node.id) || {
            let t = self
                .get_contextual_ty(node.id, None)
                .unwrap_or(self.unknown_ty);
            self.some_type(t, |this, t| this.is_template_literal_contextual_ty(t))
        } {
            self.get_template_lit_ty(&texts, &tys)
        } else {
            self.string_ty
        }
    }

    pub(super) fn get_regular_ty_of_object_literal(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if !ty.is_fresh_object_literal() {
            return ty;
        }
        let Some(a) = ty.kind.as_object_anonymous() else {
            unreachable!()
        };
        let fresh_ty_links = a.fresh_ty_links;
        if let Some(ty) = self.fresh_ty_links_arena[fresh_ty_links].get_regular_ty() {
            return ty;
        }

        let members = self
            .get_props_of_object_ty(ty)
            .iter()
            .map(|&prop| {
                let original = self.get_type_of_symbol(prop);
                let updated = self.get_regular_ty_of_object_literal(original);
                let name = self.symbol(prop).name;
                let prop = if updated == original {
                    prop
                } else {
                    self.create_transient_symbol_with_ty(prop, updated)
                };
                (name, prop)
            })
            .collect();

        let object_flags = ty.get_object_flags() & !ObjectFlags::FRESH_LITERAL;
        let resolved = self.get_ty_links(ty.id).get_structured_members().unwrap();
        assert!(resolved.call_sigs.is_empty());
        assert!(resolved.ctor_sigs.is_empty());
        assert!(resolved.index_infos.is_empty());
        let regular = self.create_anonymous_ty_with_resolved(
            a.symbol,
            object_flags,
            self.alloc(members),
            resolved.call_sigs,
            resolved.ctor_sigs,
            resolved.index_infos,
            None,
        );
        self.fresh_ty_links_arena[fresh_ty_links].set_regular_ty(regular);
        regular
    }

    pub(super) fn get_regular_ty_of_literal_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if ty.flags.intersects(TypeFlags::FRESHABLE) {
            self.get_regular_ty(ty).unwrap()
        } else if ty.kind.is_union() {
            if let Some(t) = self.get_regular_ty(ty) {
                t
            } else {
                let regular_ty = self
                    .map_ty(
                        ty,
                        |this, t| Some(this.get_regular_ty_of_literal_ty(t)),
                        false,
                    )
                    .unwrap();
                self.set_regular_ty(ty, regular_ty);
                regular_ty
            }
        } else {
            ty
        }
    }

    fn check_assertion(
        &mut self,
        node_id: ast::NodeID,
        assert_expr: &'cx ast::Expr<'cx>,
        assert_ty: &'cx ast::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let expr_ty = self.check_expr(assert_expr);
        if assert_ty.is_const_ty_refer() {
            return self.get_regular_ty_of_literal_ty(expr_ty);
        }
        self.check_node_deferred(node_id);
        let ret = self.get_ty_from_type_node(assert_ty);
        if let Some(_old) = self.get_node_links(node_id).get_assertion_expression_ty() {
            // debug_assert_eq!(old, expr_ty); // TODO: remove duplicate.
        } else {
            self.get_mut_node_links(node_id)
                .set_assertion_expression_ty(expr_ty);
        }
        ret
    }

    pub(super) fn check_truthiness_expr(&mut self, expr: &'cx ast::Expr) -> &'cx ty::Ty<'cx> {
        // TODO: check truthiness of ty
        self.check_expr(expr)
    }

    fn is_post_super_flow_node(
        &self,
        mut flow: bolt_ts_binder::FlowID,
        mut no_cache_check: bool,
    ) -> bool {
        loop {
            let flow_node = self.flow_node(flow);
            let flags = flow_node.flags;
            if flags.intersects(FlowFlags::SHARED) {
                if !no_cache_check {
                    // TODO:
                }
                no_cache_check = true;
            }
            if flags.intersects(
                FlowFlags::ASSIGNMENT
                    .union(FlowFlags::CONDITION)
                    .union(FlowFlags::ARRAY_MUTATION)
                    .union(FlowFlags::SWITCH_CLAUSE),
            ) {
                flow = match &flow_node.kind {
                    bolt_ts_binder::FlowNodeKind::Cond(n) => n.antecedent,
                    bolt_ts_binder::FlowNodeKind::Assign(n) => n.antecedent,
                    _ => unreachable!(),
                };
            } else if flags.intersects(FlowFlags::CALL) {
                let bolt_ts_binder::FlowNodeKind::Call(n) = &flow_node.kind else {
                    unreachable!()
                };
                if matches!(n.node.expr.kind, ast::ExprKind::Super(_)) {
                    return true;
                }
                flow = n.antecedent;
            } else if flags.intersects(FlowFlags::BRANCH_LABEL) {
                let bolt_ts_binder::FlowNodeKind::Label(n) = &flow_node.kind else {
                    unreachable!()
                };
                return n.antecedent.as_ref().is_some_and(|list| {
                    list.iter()
                        .all(|item| self.is_post_super_flow_node(*item, false))
                });
            } else if flags.intersects(FlowFlags::LOOP_LABEL) {
                let bolt_ts_binder::FlowNodeKind::Label(n) = &flow_node.kind else {
                    unreachable!()
                };
                flow = n.antecedent.as_ref().unwrap()[0];
            } else if flags.intersects(FlowFlags::REDUCE_LABEL) {
                todo!()
            } else {
                return flags.intersects(FlowFlags::UNREACHABLE);
            }
        }
    }

    fn check_this_before_super(
        &mut self,
        expr: &'cx ast::ThisExpr,
        container: &'cx ast::ClassCtor,
        push_error: impl FnOnce(&mut Self, bolt_ts_span::Span),
    ) {
        let containing_class_decl = self.parent(container.id).unwrap();
        let has_base_ty_node = match self.p.node(containing_class_decl) {
            ast::Node::ClassDecl(c) => c.extends.is_some(),
            ast::Node::ClassExpr(c) => c.extends.is_some(),
            _ => unreachable!(),
        };
        if has_base_ty_node
            && !self.class_decl_extends_null(containing_class_decl)
            && let Some(flow) = self.get_flow_node_of_node(expr.id)
            && !self.is_post_super_flow_node(flow, false)
        {
            push_error(self, expr.span);
        }
    }

    fn check_this_expr(&mut self, expr: &'cx ast::ThisExpr) -> &'cx ty::Ty<'cx> {
        let is_ty_query = self.node_query(expr.id.module()).is_in_type_query(expr.id);
        let mut container_id = self
            .node_query(expr.id.module())
            .get_this_container(expr.id, true, true);
        let mut container = self.p.node(container_id);

        let mut captured_by_arrow_fn = false;
        let this_in_computed_prop_name = false;

        if let Some(ctor) = container.as_class_ctor() {
            self.check_this_before_super(expr, ctor, |this, span| {
                let error =
                    errors::SuperMustBeCalledBeforeAccessingThisInTheConstructorOfADerivedClass {
                        span,
                    };
                this.push_error(Box::new(error));
            });
        }

        loop {
            if container.is_arrow_fn_expr() {
                container_id = self.node_query(container_id.module()).get_this_container(
                    container_id,
                    false,
                    !this_in_computed_prop_name,
                );
                container = self.p.node(container_id);
                captured_by_arrow_fn = true;
            }
            break;
        }

        if this_in_computed_prop_name {
            todo!()
        } else if container.is_module_decl() {
            let error = errors::ThisCannotBeReferencedInAModuleOrNamespaceBody { span: expr.span };
            self.push_error(Box::new(error));
        }

        let ty = self.try_get_this_ty_at(expr.id, true, Some(container_id));

        if self.config.no_implicit_this() {
            let global_this_ty = self.get_type_of_symbol(self.global_this_symbol);
            match ty {
                Some(ty) if ty == global_this_ty && captured_by_arrow_fn => {
                    todo!("error")
                }
                None => {
                    let mut error =
                        errors::ThisImplicitlyHasTypeAnyBecauseItDoesNotHaveATypeAnnotation {
                            span: expr.span,
                            related: None,
                        };
                    if !container.is_program()
                        && let Some(outside_this) =
                            self.try_get_this_ty_at(container_id, true, None)
                        && outside_this != global_this_ty
                    {
                        error.related = Some(errors::AnOuterValueOfThisIsShadowedByThisContainer {
                            span: expr.span,
                        });
                    }
                    self.push_error(Box::new(error));
                }
                _ => {}
            }
        }

        ty.unwrap_or(self.any_ty)
    }

    pub(super) fn check_expr_with_contextual_ty(
        &mut self,
        expr: &'cx ast::Expr,
        ctx_ty: &'cx ty::Ty<'cx>,
        inference: Option<InferenceContextId>,
        check_mode: CheckMode,
    ) -> &'cx ty::Ty<'cx> {
        let node = expr.id();

        let old_check_mode = self.check_mode;
        let check_mode = check_mode
            | CheckMode::CONTEXTUAL
            | if inference.is_some() {
                CheckMode::INFERENTIAL
            } else {
                CheckMode::empty()
            };
        self.check_mode = Some(check_mode);
        self.push_type_context(node, Some(ctx_ty), false);
        self.push_inference_context(node, inference);

        let ty = self.check_expr(expr);

        self.pop_type_context();
        self.pop_inference_context();
        self.check_mode = old_check_mode;

        ty
    }

    pub(super) fn check_expr_with_cache(&mut self, expr: &'cx ast::Expr) -> &'cx ty::Ty<'cx> {
        if self
            .check_mode
            .is_some_and(|check_mode| check_mode != CheckMode::empty())
        {
            self.check_expr(expr)
        } else if let Some(ty) = self.get_node_links(expr.id()).get_resolved_ty() {
            ty
        } else {
            let ty = self.check_expr(expr);
            self.get_mut_node_links(expr.id()).set_resolved_ty(ty);
            ty
        }
    }

    pub(super) fn check_ident_for_mutable_loc(
        &mut self,
        ident: &'cx ast::Ident,
    ) -> &'cx ty::Ty<'cx> {
        let ty = self.check_ident(ident);
        if self.is_const_context(ident.id) {
            self.get_regular_ty_of_literal_ty(ty)
        } else {
            let contextual_ty = self.get_contextual_ty(ident.id, None);
            let contextual_ty = self.instantiate_contextual_ty(contextual_ty, ident.id, None);
            self.get_widened_lit_like_ty_for_contextual_ty(ty, contextual_ty)
        }
    }

    pub(super) fn check_expr_for_mutable_location(
        &mut self,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let id = expr.id();
        let ty = self.check_expr(expr);
        if self.is_const_context(id) {
            self.get_regular_ty_of_literal_ty(ty)
        } else if expr.kind.is_type_assertion() {
            ty
        } else {
            let contextual_ty = self.get_contextual_ty(id, None);
            let contextual_ty = self.instantiate_contextual_ty(contextual_ty, id, None);
            self.get_widened_lit_like_ty_for_contextual_ty(ty, contextual_ty)
        }
    }

    fn check_cond(&mut self, cond: &'cx ast::CondExpr) -> &'cx ty::Ty<'cx> {
        let ty = self.check_expr(cond.cond);
        let ty1 = self.check_expr(cond.when_true);
        let ty2 = self.check_expr(cond.when_false);
        self.get_union_ty(&[ty1, ty2], ty::UnionReduction::Subtype, false, None, None)
    }

    fn check_object_lit(&mut self, node: &'cx ast::ObjectLit<'cx>) -> &'cx ty::Ty<'cx> {
        self.push_cached_contextual_type(node.id);

        let mut object_flags = ObjectFlags::FRESH_LITERAL;
        let mut properties_table = fx_indexmap_with_capacity(node.members.len());
        let mut properties_array = Vec::with_capacity(node.members.len());
        let mut spread = self.empty_object_ty();
        // let mut properties_array = Vec::with_capacity(node.members.len());
        let is_const_context = self.is_const_context(node.id);
        let mut has_computed_string_property = false;
        let mut has_computed_number_property = false;
        let mut has_computed_symbol_property = false;

        let symbol = std::cell::OnceCell::new();
        for member in node.members {
            use bolt_ts_ast::ObjectMemberKind::*;
            match member.kind {
                Shorthand(_) | PropAssignment(_) | Method(_) => {
                    let member_symbol = self.get_symbol_of_decl(member.id());
                    let ty = match member.kind {
                        Shorthand(n) => self.check_ident(n.name),
                        PropAssignment(n) => self.check_object_prop_member(n),
                        Method(n) => self.check_object_method_member(n),
                        _ => unreachable!(),
                    };
                    let name = match member.kind {
                        Shorthand(n) => SymbolName::Atom(n.name.name),
                        PropAssignment(n) => bolt_ts_binder::prop_name(n.name),
                        Method(n) => bolt_ts_binder::prop_name(n.name),
                        _ => unreachable!(),
                    };
                    object_flags |= ty.get_object_flags() & ObjectFlags::PROPAGATING_FLAGS;
                    let member_s = self.binder.symbol(member_symbol);
                    let declarations = member_s.decls.clone();
                    let value_declaration = member_s.value_decl;
                    let prop = self.create_transient_symbol(
                        name,
                        SymbolFlags::PROPERTY.union(SymbolFlags::TRANSIENT)
                            | self.binder.symbol(member_symbol).flags,
                        SymbolLinks::default()
                            .with_target(member_symbol)
                            .with_ty(ty),
                        declarations,
                        value_declaration,
                    );
                    properties_table.insert(name, prop);
                    properties_array.push(member_symbol);
                }
                SpreadAssignment(s) => {
                    if !properties_array.is_empty() {
                        let props = self.alloc(std::mem::take(&mut properties_table));
                        let right = create_object_lit_ty(self, node, object_flags, props);
                        let s = *symbol.get_or_init(|| self.get_symbol_of_decl(node.id));
                        spread = self.get_spread_ty(
                            spread,
                            right,
                            Some(s),
                            object_flags,
                            is_const_context,
                        );
                        properties_array.clear();
                        has_computed_string_property = false;
                        has_computed_number_property = false;
                        has_computed_symbol_property = false;
                    }
                    let ty = {
                        let old = self.check_mode;
                        self.check_mode = self
                            .check_mode
                            .map(|mode| mode.intersection(CheckMode::INFERENTIAL));
                        let ty = self.check_expr(s.expr);
                        self.check_mode = old;
                        self.get_reduced_ty(ty)
                    };
                    if self.is_valid_spread_ty(ty) {
                        let merged_ty = self
                            .try_merge_union_of_object_ty_and_empty_object(ty, is_const_context);
                        let s = *symbol.get_or_init(|| self.get_symbol_of_decl(node.id));
                        spread = self.get_spread_ty(
                            spread,
                            merged_ty,
                            Some(s),
                            object_flags,
                            is_const_context,
                        );
                    } else {
                        // TODO: error
                        spread = self.error_ty;
                    }
                }
                _ => {
                    debug_assert!(matches!(member.kind, Setter(_) | Getter(_)));
                    // TODO: deferred check

                    let name = match member.kind {
                        Setter(n) => bolt_ts_binder::prop_name(n.name),
                        Getter(n) => bolt_ts_binder::prop_name(n.name),
                        _ => unreachable!(),
                    };
                    let member_symbol = self.get_symbol_of_decl(member.id());
                    properties_table.insert(name, member_symbol);
                    properties_array.push(member_symbol);
                }
            }
        }
        self.pop_type_context();

        if self.is_error(spread) {
            return self.error_ty;
        }

        if spread != self.empty_object_ty() {
            if !properties_array.is_empty() {
                let props = self.alloc(std::mem::take(&mut properties_table));
                let right = create_object_lit_ty(self, node, object_flags, props);
                let s = *symbol.get_or_init(|| self.get_symbol_of_decl(node.id));
                spread = self.get_spread_ty(spread, right, Some(s), object_flags, is_const_context);
                properties_array.clear();
                has_computed_string_property = false;
                has_computed_number_property = false;
            }
            let properties_table = self.alloc(properties_table);
            return self
                .map_ty(
                    spread,
                    |this, t| {
                        if t == this.empty_object_ty() {
                            Some(create_object_lit_ty(
                                this,
                                node,
                                object_flags,
                                properties_table,
                            ))
                        } else {
                            Some(t)
                        }
                    },
                    false,
                )
                .unwrap();
        }

        fn create_object_lit_ty<'cx>(
            this: &mut TyChecker<'cx>,
            node: &'cx ast::ObjectLit<'cx>,
            object_flags: ObjectFlags,
            properties_table: &'cx FxIndexMap<SymbolName, SymbolID>,
        ) -> &'cx ty::Ty<'cx> {
            let ty = this.create_anonymous_ty(
                Some(this.final_res(node.id)),
                object_flags
                    | (ObjectFlags::OBJECT_LITERAL
                        .union(ObjectFlags::CONTAINS_OBJECT_OR_ARRAY_LITERAL)),
                None,
            );

            let props = this.get_props_from_members(properties_table);
            this.ty_links.insert(
                ty.id,
                TyLinks::default().with_structured_members(this.alloc(ty::StructuredMembers {
                    members: properties_table,
                    call_sigs: Default::default(),
                    ctor_sigs: Default::default(),
                    index_infos: Default::default(),
                    props,
                })),
            );
            ty
        }

        let properties_table = self.alloc(properties_table);
        create_object_lit_ty(self, node, object_flags, properties_table)
    }

    fn is_empty_object_ty_or_spreads_into_empty_object(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        self.is_empty_object_ty(ty)
            || ty.flags.intersects(
                TypeFlags::NULL
                    | TypeFlags::UNDEFINED
                    | TypeFlags::BOOLEAN_LIKE
                    | TypeFlags::NUMBER_LIKE
                    | TypeFlags::BIG_INT_LIKE
                    | TypeFlags::STRING_LIKE
                    | TypeFlags::ENUM_LIKE
                    | TypeFlags::NON_PRIMITIVE
                    | TypeFlags::INDEX,
            )
    }

    pub(super) fn try_merge_union_of_object_ty_and_empty_object(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        readonly: bool,
    ) -> &'cx ty::Ty<'cx> {
        let Some(u) = ty.kind.as_union() else {
            return ty;
        };
        if u.tys
            .iter()
            .all(|ty| self.is_empty_object_ty_or_spreads_into_empty_object(ty))
        {
            return u
                .tys
                .iter()
                .find(|ty| !self.is_empty_object_ty(ty))
                .copied()
                .unwrap_or(self.empty_object_ty());
        }
        let Some(first_ty) = u
            .tys
            .iter()
            .find(|ty| !self.is_empty_object_ty_or_spreads_into_empty_object(ty))
            .copied()
        else {
            return ty;
        };
        if u.tys
            .iter()
            .any(|&ty| ty != first_ty && !self.is_empty_object_ty_or_spreads_into_empty_object(ty))
        {
            return ty;
        }

        // get anonymous partial ty
        let props = self.get_props_of_ty(ty);
        let mut members = fx_indexmap_with_capacity(props.len());
        for prop in props {
            // TODO: exclude private and projected
            let s = self.symbol(*prop);
            let prop_flags = s.flags;
            let name = s.name;
            let is_setonly_accessor = prop_flags.intersects(SymbolFlags::SET_ACCESSOR)
                && !prop_flags.intersects(SymbolFlags::GET_ACCESSOR);
            const FLAGS: SymbolFlags = SymbolFlags::PROPERTY
                .union(SymbolFlags::OPTIONAL)
                .union(SymbolFlags::TRANSIENT);
            let decls = s.decls.clone();
            let ty = if is_setonly_accessor {
                self.undefined_ty
            } else {
                let ty = self.get_type_of_symbol(*prop);
                self.add_optionality(ty, true, false)
            };
            // TODO: is_late_check_flags
            let check_flags = if readonly {
                CheckFlags::READONLY
            } else {
                CheckFlags::empty()
            };
            let name_ty = self.get_symbol_links(*prop).get_name_ty();
            let links = SymbolLinks::default()
                .with_ty(ty)
                .with_check_flags(check_flags);
            let links = if let Some(name_ty) = name_ty {
                links.with_name_ty(name_ty)
            } else {
                links
            };
            let result = self.create_transient_symbol(name, FLAGS, links, decls, None);
            let prev = members.insert(name, result);
            assert!(prev.is_none());
        }
        let index_infos = self.get_index_infos_of_ty(ty);

        self.create_anonymous_ty_with_resolved(
            ty.symbol(),
            ObjectFlags::OBJECT_LITERAL.union(ObjectFlags::CONTAINS_OBJECT_OR_ARRAY_LITERAL),
            self.alloc(members),
            self.empty_array(),
            self.empty_array(),
            index_infos,
            None,
        )
    }

    fn check_assign_expr(&mut self, assign: &'cx ast::AssignExpr<'cx>) -> &'cx ty::Ty<'cx> {
        if let Eq = assign.op {
            let right = self.check_expr(assign.right);
            return self.check_destructing_assign(assign, right, false);
        };
        let l = self.check_expr(assign.left);
        let r = self.check_expr(assign.right);

        // if ty == self.any_ty() {
        //     let error = errors::CannotAssignToNameBecauseItIsATy {
        //         name: self.atoms.get(assign.binding.name).to_string(),
        //         ty: l.kind.to_string(self.binder,self.atoms),
        //         span: assign.span,
        //     };
        //     self.push_error(assign.span.module, Box::new(error));
        // }
        use bolt_ts_ast::AssignOp::*;
        match assign.op {
            Eq => unreachable!(),
            AddEq => self
                .check_binary_like_expr_for_add(l, r)
                .unwrap_or(self.any_ty),
            SubEq => self.undefined_ty,
            MulEq => self.undefined_ty,
            DivEq => self.undefined_ty,
            ModEq => self.undefined_ty,
            ShlEq => self.undefined_ty,
            ShrEq => self.undefined_ty,
            UShrEq => self.undefined_ty,
            BitOrEq | BitAndEq | BitXorEq => self.check_bin_expr_for_normal(
                assign.span,
                l,
                assign.left.span(),
                r,
                assign.right.span(),
                assign.op.as_str(),
            ),
            LogicalAndEq => todo!(),
            LogicalOrEq => todo!(),
        }
    }

    fn check_prefix_unary_expr(
        &mut self,
        expr: &'cx ast::PrefixUnaryExpr<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let op_ty = self.check_expr(expr.expr);
        if op_ty == self.silent_never_ty {
            return op_ty;
        }

        match expr.expr.kind {
            ast::ExprKind::NumLit(lit) => match expr.op {
                ast::PrefixUnaryOp::Minus => {
                    let ty = self.get_number_literal_type_from_number(-lit.val);
                    return self.get_fresh_ty_of_literal_ty(ty);
                }
                ast::PrefixUnaryOp::Plus => {
                    let ty = self.get_number_literal_type_from_number(lit.val);
                    return self.get_fresh_ty_of_literal_ty(ty);
                }
                _ => (),
            },
            ast::ExprKind::BigIntLit(lit) => match expr.op {
                ast::PrefixUnaryOp::Minus => {
                    let neg = !lit.val.0;
                    let ty = self.get_bigint_literal_type(neg, lit.val.1);
                    return self.get_fresh_ty_of_literal_ty(ty);
                }
                ast::PrefixUnaryOp::Plus => {
                    let neg = lit.val.0;
                    let ty = self.get_bigint_literal_type(neg, lit.val.1);
                    return self.get_fresh_ty_of_literal_ty(ty);
                }
                _ => (),
            },
            _ => (),
        }
        if let ast::ExprKind::NumLit(lit) = expr.expr.kind {}

        match expr.op {
            ast::PrefixUnaryOp::Plus => {
                if let ty::TyKind::NumberLit(n) = op_ty.kind {
                    op_ty
                } else {
                    self.number_ty
                }
            }
            ast::PrefixUnaryOp::Minus => {
                if let ty::TyKind::NumberLit(n) = op_ty.kind {
                    self.get_number_literal_type_from_number(-n.val.val())
                } else {
                    self.number_ty
                }
            }
            ast::PrefixUnaryOp::PlusPlus | ast::PrefixUnaryOp::MinusMinus => {
                let ok = self.check_arithmetic_op_ty(op_ty, false, |_| {});
                if ok {
                    self.check_reference_expr(
                        expr.expr,
                        |this| {
                            let error =
                                errors::TheOperandOfAnIncrementOrDecrementOperatorMustBeAVariableOrAPropertyAccess {
                                    span: expr.span,
                                    is_incr: expr.op == ast::PrefixUnaryOp::PlusPlus,
                                };
                            this.push_error(Box::new(error));
                        },
                        |this| {
                            let error =
                            errors::TheOperandOfAnIncrementOrDecrementOperatorMayNotBeAnOptionalPropertyAccess {
                                span: expr.span,
                                is_incr: expr.op == ast::PrefixUnaryOp::PlusPlus,
                            };
                        this.push_error(Box::new(error));
                        },
                    );
                }

                match expr.op {
                    ast::PrefixUnaryOp::PlusPlus => {
                        if let ty::TyKind::NumberLit(n) = op_ty.kind {
                            self.get_number_literal_type_from_number(n.val.val() + 1.)
                        } else {
                            self.number_ty
                        }
                    }
                    ast::PrefixUnaryOp::MinusMinus => {
                        if let ty::TyKind::NumberLit(n) = op_ty.kind {
                            self.get_number_literal_type_from_number(n.val.val() - 1.)
                        } else {
                            self.number_ty
                        }
                    }
                    _ => unreachable!(),
                }
            }
            ast::PrefixUnaryOp::Tilde => self.number_ty,
            ast::PrefixUnaryOp::Excl => self.boolean_ty(),
        }
    }

    fn check_reference_expr(
        &mut self,
        op: &'cx ast::Expr<'cx>,
        push_invalid_reference_error: impl FnOnce(&mut Self),
        push_invalid_optional_chain_error: impl FnOnce(&mut Self),
    ) -> bool {
        let n = ast::Expr::skip_outer_expr(op);
        if !matches!(
            n.kind,
            ast::ExprKind::Ident(_) | ast::ExprKind::PropAccess(_) | ast::ExprKind::EleAccess(_)
        ) {
            push_invalid_reference_error(self);
            false
        } else if self
            .p
            .node_flags(n.id())
            .intersects(ast::NodeFlags::OPTIONAL_CHAIN)
        {
            push_invalid_optional_chain_error(self);
            false
        } else {
            true
        }
    }

    pub(super) fn check_arithmetic_op_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        is_await_valid: bool,
        push_error: impl FnOnce(&mut Self),
    ) -> bool {
        if !self.is_type_assignable_to(ty, self.number_or_bigint_ty()) {
            // let awaited_ty = is_await_valid.then(|| self)
            push_error(self);
            false
        } else {
            true
        }
    }

    fn check_postfix_unary_expr(
        &mut self,
        expr: &'cx ast::PostfixUnaryExpr<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let op_ty = self.check_expr(expr.expr);
        let ok = self.check_arithmetic_op_ty(op_ty, false, |this| {
            let error = errors::AnArithmeticOperandMustBeOfTypeAnyNumberBigintOrAnEnumType {
                span: expr.span,
            };
            this.push_error(Box::new(error));
        });
        if ok {
            self.check_reference_expr(expr.expr, |this| {
                let error =
                    errors::TheOperandOfAnIncrementOrDecrementOperatorMustBeAVariableOrAPropertyAccess {
                        span: expr.span,
                        is_incr: expr.op == ast::PostfixUnaryOp::PlusPlus,
                    };
                this.push_error(Box::new(error));
            },
            |this| {
                let error =
                errors::TheOperandOfAnIncrementOrDecrementOperatorMayNotBeAnOptionalPropertyAccess {
                    span: expr.span,
                    is_incr: expr.op == ast::PostfixUnaryOp::PlusPlus,
                };
                this.push_error(Box::new(error));
            });
        }
        op_ty
    }

    fn check_bin_expr_for_normal(
        &mut self,
        expr_span: Span,
        left_ty: &'cx ty::Ty<'cx>,
        left_span: Span,
        right_ty: &'cx ty::Ty<'cx>,
        right_span: Span,
        op: &str,
    ) -> &'cx ty::Ty<'cx> {
        assert!(matches!(op, "^" | "^=" | "&" | "&=" | "|" | "|="));
        if left_ty.flags.intersects(TypeFlags::BOOLEAN_LIKE)
            && right_ty.flags.intersects(TypeFlags::BOOLEAN_LIKE)
            && let Some(sugg) = get_suggestion_boolean_op(op)
        {
            let error = errors::TheOp1IsNotAllowedForBooleanTypesConsiderUsingOp2Instead {
                span: expr_span,
                op1: op.to_string(),
                op2: sugg.to_string(),
            };
            self.push_error(Box::new(error));
            return self.number_ty;
        }

        let left = self.check_arithmetic_op_ty(left_ty, false, |this| {
            let error =
                errors::TheSideOfAnArithmeticOperationMustBeOfTypeAnyNumberBigintOrAnEnumType {
                    span: left_span,
                    left_or_right: errors::LeftOrRight::Left,
                };
            this.push_error(Box::new(error));
        });
        let right = self.check_arithmetic_op_ty(right_ty, false, |this| {
            let error =
                errors::TheSideOfAnArithmeticOperationMustBeOfTypeAnyNumberBigintOrAnEnumType {
                    span: right_span,
                    left_or_right: errors::LeftOrRight::Right,
                };
            this.push_error(Box::new(error));
        });

        self.number_ty
    }

    fn check_ele_access_expr(&mut self, node: &'cx ast::EleAccessExpr<'cx>) -> &'cx ty::Ty<'cx> {
        let expr_ty = self.check_expr(node.expr);
        let assign_kind = self
            .node_query(node.id.module())
            .get_assignment_target_kind(node.id);
        let assign_kind_is_none = assign_kind == AssignmentKind::None;
        let object_ty = if !assign_kind_is_none
            || self
                .node_query(node.id.module())
                .is_method_access_for_call(node.id)
        {
            self.get_widened_literal_ty(expr_ty)
        } else {
            expr_ty
        };

        let index_ty = self.check_expr(node.arg);

        if object_ty == self.error_ty {
            return self.error_ty;
        }

        let index_ty = if self.is_for_in_variable_for_numeric_prop_names(node.arg) {
            self.number_ty
        } else {
            index_ty
        };

        let access_flags = if assign_kind_is_none {
            AccessFlags::EXPRESSION_POSITION
        } else {
            AccessFlags::WRITING
                | if self.is_generic_object_ty(object_ty) && object_ty.kind.is_this_ty_param() {
                    AccessFlags::NO_INDEX_SIGNATURES
                } else {
                    AccessFlags::empty()
                }
                | if assign_kind == AssignmentKind::Compound {
                    AccessFlags::EXPRESSION_POSITION
                } else {
                    AccessFlags::empty()
                }
        };

        self.get_indexed_access_ty(object_ty, index_ty, Some(access_flags), Some(node.id))
    }

    pub fn check_computed_prop_name(
        &mut self,
        node: &'cx ast::ComputedPropName<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(ty) = self.get_node_links(node.id).get_resolved_ty() {
            return ty;
        };
        let ty = self.check_expr(node.expr);
        self.get_mut_node_links(node.id).set_resolved_ty(ty);
        ty
    }
}
