use bolt_ts_span::Span;

use crate::ensure_sufficient_stack;
use crate::parser::AssignmentKind;
use crate::ty::TypeFlags;

use super::ObjectFlags;
use super::TyChecker;
use super::ast;
use super::bind::{SymbolFlags, SymbolName};
use super::errors;
use super::ty;
use super::ty::AccessFlags;
use super::{CheckMode, InferenceContextId, SymbolLinks, TyLinks};

fn get_suggestion_boolean_op(op: &str) -> Option<&str> {
    match op {
        "^" | "^=" => Some("!=="),
        "&" | "&=" => Some("&&"),
        "|" | "|=" => Some("||"),
        _ => None,
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_fresh_ty_of_literal_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty.flags.intersects(TypeFlags::FRESHABLE) {
            if let Some(fresh_ty) = self.get_ty_links(ty.id).get_fresh_ty() {
                fresh_ty
            } else {
                let fresh_ty = match ty.kind {
                    ty::TyKind::NumberLit(lit) => {
                        let t = self.alloc(ty::NumberLitTy { val: lit.val });
                        self.new_ty(ty::TyKind::NumberLit(t), ty.flags)
                    }
                    ty::TyKind::StringLit(lit) => {
                        let t = self.alloc(ty::StringLitTy { val: lit.val });
                        self.new_ty(ty::TyKind::StringLit(t), ty.flags)
                    }
                    _ => unreachable!(),
                };
                let prev = self.ty_links.insert(
                    fresh_ty.id,
                    TyLinks::default()
                        .with_fresh_ty(fresh_ty)
                        .with_regular_ty(ty),
                );
                assert!(prev.is_none());
                self.get_mut_ty_links(ty.id).set_fresh_ty(fresh_ty);
                assert!(self.get_ty_links(ty.id).get_regular_ty().is_some());
                fresh_ty
            }
        } else {
            ty
        }
    }

    pub(super) fn check_expr(&mut self, expr: &'cx ast::Expr<'cx>) -> &'cx ty::Ty<'cx> {
        use bolt_ts_ast::ExprKind::*;
        let ty = match expr.kind {
            Bin(bin) => ensure_sufficient_stack(|| self.check_bin_expr(bin)),
            NumLit(lit) => {
                let t = self.get_number_literal_type(lit.val);
                self.get_fresh_ty_of_literal_ty(t)
            }
            StringLit(lit) => {
                let t = self.get_string_literal_type(lit.val);
                self.get_fresh_ty_of_literal_ty(t)
            }
            BoolLit(lit) => {
                if lit.val {
                    self.true_ty
                } else {
                    self.false_ty
                }
            }
            NullLit(_) => self.null_ty,
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
                self.check_class_decl_like(class);
                self.undefined_ty
            }
            PropAccess(node) => self.check_prop_access_expr(node),
            Typeof(n) => {
                self.check_expr(n.expr);
                self.typeof_ty()
            }
            Void(n) => {
                self.check_expr(n.expr);
                self.undefined_ty
            }
            EleAccess(node) => self.check_ele_access(node),
            This(n) => self.check_this_expr(n),
            Super(_) => self.undefined_ty,
            As(n) => self.check_assertion(n.expr, n.ty),
            Satisfies(n) => self.check_expr(n.expr),
            NonNull(n) => self.check_expr(n.expr),
            Template(n) => self.check_template_expr(n),
        };

        self.instantiate_ty_with_single_generic_call_sig(expr.id(), ty)
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
        if self.p.is_const_context(node.id) {
            // TODO:
        }
        self.string_ty
    }

    pub(super) fn get_regular_ty_of_literal_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if ty.flags.intersects(TypeFlags::FRESHABLE) {
            self.ty_links[&ty.id].expect_regular_ty()
        } else if let Some(u) = ty.kind.as_union() {
            if let Some(t) = self.get_ty_links(ty.id).get_regular_ty() {
                t
            } else {
                let regular_ty = self
                    .map_ty(
                        ty,
                        |this, t| Some(this.get_regular_ty_of_literal_ty(t)),
                        false,
                    )
                    .unwrap();
                self.get_mut_ty_links(ty.id).set_regular_ty(regular_ty);
                regular_ty
            }
        } else {
            ty
        }
    }

    fn check_assertion(
        &mut self,
        assert_expr: &'cx ast::Expr<'cx>,
        assert_ty: &'cx ast::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let expr_ty = self.check_expr(assert_expr);
        if assert_ty.is_const_ty_refer() {
            self.get_regular_ty_of_literal_ty(expr_ty)
        } else {
            self.get_ty_from_type_node(assert_ty)
        }
    }

    pub(super) fn check_truthiness_expr(&mut self, expr: &'cx ast::Expr) -> &'cx ty::Ty<'cx> {
        let ty = self.check_expr(expr);
        // TODO: check truthiness of ty
        ty
    }

    fn check_this_expr(&mut self, expr: &'cx ast::ThisExpr) -> &'cx ty::Ty<'cx> {
        let is_ty_query = self.p.is_in_type_query(expr.id);
        let mut container_id = self.p.get_this_container(expr.id, true, true);
        let mut container = self.p.node(container_id);

        let mut captured_by_arrow_fn = false;
        let this_in_computed_prop_name = false;

        if container.is_class_ctor() {
            // TODO: `check_this_before_super`
        }

        loop {
            if container.is_arrow_fn_expr() {
                container_id =
                    self.p
                        .get_this_container(container_id, false, !this_in_computed_prop_name);
                container = self.p.node(container_id);
                captured_by_arrow_fn = true;
            }
            break;
        }

        let ty = self.try_get_this_ty_at(expr, true, Some(container_id));

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
        if self.check_mode.is_some() {
            self.check_expr(expr)
        } else if let Some(ty) = self.get_node_links(expr.id()).get_resolved_ty() {
            ty
        } else {
            let ty = self.check_expr(expr);
            self.get_mut_node_links(expr.id()).set_resolved_ty(ty);
            ty
        }
    }

    pub(super) fn check_expr_for_mutable_location(
        &mut self,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let id = expr.id();
        let ty = self.check_expr(expr);
        if self.p.is_const_context(id) {
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
        self.get_union_ty(&[ty1, ty2], ty::UnionReduction::Subtype)
    }

    fn check_object_lit(&mut self, node: &'cx ast::ObjectLit<'cx>) -> &'cx ty::Ty<'cx> {
        let mut object_flags = ObjectFlags::FRESH_LITERAL;
        let members = node
            .members
            .iter()
            .map(|member| {
                let member_symbol = self.get_symbol_of_decl(member.id());
                use bolt_ts_ast::ObjectMemberKind::*;
                let ty = match member.kind {
                    Shorthand(n) => self.check_ident(n.name),
                    Prop(n) => self.check_object_prop_member(n),
                    Method(n) => self.check_object_method_member(n),
                };
                let name = match member.kind {
                    Shorthand(n) => SymbolName::Ele(n.name.name),
                    Prop(n) => crate::bind::prop_name(n.name),
                    Method(n) => crate::bind::prop_name(n.name),
                };
                object_flags |= ty.get_object_flags() & ObjectFlags::PROPAGATING_FLAGS;
                let prop = self.create_transient_symbol(
                    name,
                    SymbolFlags::PROPERTY | self.binder.symbol(member_symbol).flags,
                    Some(member_symbol),
                    SymbolLinks::default()
                        .with_target(member_symbol)
                        .with_ty(ty),
                );
                (name, prop)
            })
            .collect();
        let ty = self.create_anonymous_ty(
            self.final_res(node.id),
            object_flags
                | ObjectFlags::OBJECT_LITERAL
                | ObjectFlags::CONTAINS_OBJECT_OR_ARRAY_LITERAL,
        );
        let props = self.get_props_from_members(&members);
        let members = self.alloc(members);
        self.ty_links.insert(
            ty.id,
            TyLinks::default().with_structured_members(self.alloc(ty::StructuredMembers {
                members,
                base_tys: Default::default(),
                base_ctor_ty: Default::default(),
                call_sigs: Default::default(),
                ctor_sigs: Default::default(),
                index_infos: Default::default(),
                props,
            })),
        );
        ty
    }

    fn check_assign_expr(&mut self, assign: &'cx ast::AssignExpr<'cx>) -> &'cx ty::Ty<'cx> {
        if let Eq = assign.op {
            let right = self.check_expr(assign.right);
            return self.check_destructing_assign(assign, right, false);
        };
        let l = self.check_expr(assign.left);
        let r = self.check_expr(assign.right);
        use bolt_ts_ast::AssignOp::*;
        let ty = match assign.op {
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
        };
        // if ty == self.any_ty() {
        //     let error = errors::CannotAssignToNameBecauseItIsATy {
        //         name: self.atoms.get(assign.binding.name).to_string(),
        //         ty: l.kind.to_string(self.binder,self.atoms),
        //         span: assign.span,
        //     };
        //     self.push_error(assign.span.module, Box::new(error));
        // }
        ty
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
                    let ty = self.get_number_literal_type(-lit.val);
                    return self.get_fresh_ty_of_literal_ty(ty);
                }
                ast::PrefixUnaryOp::Plus => {
                    let ty = self.get_number_literal_type(lit.val);
                    return self.get_fresh_ty_of_literal_ty(ty);
                }
                _ => (),
            },
            _ => (),
        }

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
                    self.get_number_literal_type(-n.val)
                } else {
                    self.number_ty
                }
            }
            ast::PrefixUnaryOp::PlusPlus => {
                if let ty::TyKind::NumberLit(n) = op_ty.kind {
                    self.get_number_literal_type(n.val + 1.)
                } else {
                    self.number_ty
                }
            }
            ast::PrefixUnaryOp::MinusMinus => {
                if let ty::TyKind::NumberLit(n) = op_ty.kind {
                    self.get_number_literal_type(n.val - 1.)
                } else {
                    self.number_ty
                }
            }
            ast::PrefixUnaryOp::Tilde => self.number_ty,
            ast::PrefixUnaryOp::Excl => self.boolean_ty(),
        }
    }

    fn check_postfix_unary_expr(
        &mut self,
        expr: &'cx ast::PostfixUnaryExpr<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let op_ty = self.check_expr(expr.expr);
        // self.check_arithmetic_op_ty(op_ty, push_error);
        op_ty
    }

    fn check_arithmetic_op_ty(
        &mut self,
        t: &'cx ty::Ty<'cx>,
        push_error: impl FnOnce(&mut Self),
    ) -> &'cx ty::Ty<'cx> {
        if !self.is_type_assignable_to(t, self.number_ty) {
            push_error(self)
        }
        t
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
        {
            if let Some(sugg) = get_suggestion_boolean_op(op) {
                let error = errors::TheOp1IsNotAllowedForBooleanTypesConsiderUsingOp2Instead {
                    span: expr_span,
                    op1: op.to_string(),
                    op2: sugg.to_string(),
                };
                self.push_error(Box::new(error));
                return self.number_ty;
            }
        }

        let left = self.check_arithmetic_op_ty(left_ty, |this| {
            let error =
                errors::TheSideOfAnArithmeticOperationMustBeOfTypeAnyNumberBigintOrAnEnumType {
                    span: left_span,
                    left_or_right: errors::LeftOrRight::Left,
                };
            this.push_error(Box::new(error));
        });
        let right = self.check_arithmetic_op_ty(right_ty, |this| {
            let error =
                errors::TheSideOfAnArithmeticOperationMustBeOfTypeAnyNumberBigintOrAnEnumType {
                    span: right_span,
                    left_or_right: errors::LeftOrRight::Right,
                };
            this.push_error(Box::new(error));
        });

        self.number_ty
    }

    fn check_ele_access(&mut self, node: &'cx ast::EleAccessExpr<'cx>) -> &'cx ty::Ty<'cx> {
        let expr_ty = self.check_expr(node.expr);
        let assign_kind = self.p.get_assignment_kind(node.id);
        let assign_kind_is_none = assign_kind == AssignmentKind::None;
        let object_ty = if !assign_kind_is_none || self.p.is_method_access_for_call(node.id) {
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
                | if self.is_generic_object(object_ty) && object_ty.kind.is_this_ty_param() {
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
}
