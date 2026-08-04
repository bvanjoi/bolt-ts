use bolt_ts_ast::keyword;
use bolt_ts_ast::keyword::is_prim_value_name;
use bolt_ts_ast::r#trait::node_id_of_binding;
use bolt_ts_ast_visitor::VisitorResult;
use bolt_ts_binder::SymbolFlags;
use bolt_ts_binder::SymbolID;

use super::CheckMode;
use super::TyChecker;
use super::ast;
use super::check_call_like::CallLikeExpr;
use super::flow::flow_loop_ctx_len;
use super::links::SigLinks;
use super::ty;
use super::ty::CheckFlags;
use super::ty::SigID;
use super::ty::SigKind;
use super::ty::TypeFlags;
use super::ty::{Sig, SigFlags};
use super::type_predicate::TyPred;

impl<'cx> TyChecker<'cx> {
    pub(super) fn new_sig(&mut self, sig: Sig<'cx>) -> &'cx Sig<'cx> {
        debug_assert!(sig.id == SigID::dummy(), "TODO: hidden id");
        let sig = sig.with_id(self.sigs.len() as u32);
        let s = self.alloc(sig);
        self.sigs.push(s);
        s
    }

    fn create_optional_call_sig(
        &mut self,
        sig: &'cx Sig<'cx>,
        call_chain_flags: SigFlags,
    ) -> &'cx Sig<'cx> {
        let new = ty::Sig {
            id: SigID::dummy(),
            flags: sig.flags | call_chain_flags,
            params: sig.params,
            min_args_count: sig.min_args_count,
            ret: sig.ret,
            node_id: sig.node_id,
            target: sig.target,
            mapper: sig.mapper,
            class_decl: sig.class_decl,
            composite_sigs: sig.composite_sigs,
            composite_kind: sig.composite_kind,
        };
        let mut links = SigLinks::default();
        if let Some(ty_params) = self.get_sig_links(sig.id).get_ty_params() {
            links.set_ty_params(ty_params);
        }
        if let Some(this_param) = self.get_sig_links(sig.id).get_this_param() {
            links.set_this_param(this_param);
        }
        let new = self.new_sig(new);
        let prev = self.sig_links.insert(new.id, links);
        debug_assert!(prev.is_none());
        new
    }

    pub(super) fn get_optional_call_sig(
        &mut self,
        sig: &'cx Sig<'cx>,
        call_chain_flags: SigFlags,
    ) -> &'cx Sig<'cx> {
        if sig.flags.intersection(SigFlags::CALL_CHAIN_FLAGS) == call_chain_flags {
            return sig;
        }
        if call_chain_flags.contains(SigFlags::IS_INNER_CALL_CHAIN) {
            if let Some(cached) = self.get_sig_links(sig.id).get_inner_optional_call_sig() {
                return cached;
            };
            let new = self.create_optional_call_sig(sig, call_chain_flags);
            self.get_mut_sig_links(sig.id)
                .set_inner_optional_call_sig(new);
            new
        } else {
            debug_assert!(call_chain_flags.contains(SigFlags::IS_OUTER_CALL_CHAIN),);
            if let Some(cached) = self.get_sig_links(sig.id).get_outer_optional_call_sig() {
                return cached;
            };
            let new = self.create_optional_call_sig(sig, call_chain_flags);
            self.get_mut_sig_links(sig.id)
                .set_outer_optional_call_sig(new);
            new
        }
    }

    fn get_ty_params_from_decl(&mut self, decl: ast::NodeID) -> Option<ty::Tys<'cx>> {
        let mut result = vec![];
        let ty_params = self.get_effective_ty_param_decls(decl);
        self.append_ty_params(&mut result, ty_params);
        if !ty_params.is_empty() {
            Some(self.alloc(result))
        } else if self.p.node(decl).is_fn_decl() {
            self.get_sig_of_ty_tag(decl)
                .and_then(|sig| self.get_sig_links(sig.id).get_ty_params())
        } else {
            None
        }
    }

    pub(crate) fn get_sig_from_decl(&mut self, id: ast::NodeID) -> &'cx Sig<'cx> {
        if let Some(sig) = self.get_node_links(id).get_resolved_sig() {
            return sig;
        }
        let decl = self.p.node(id);
        let host_decl = decl; // TODO: sig in js doc
        let class_ty = if host_decl.is_class_ctor() {
            let class_decl = self.parent(id).unwrap();
            let class_symbol = self.get_symbol_of_declaration(class_decl);
            Some(self.get_declared_ty_of_symbol(class_symbol))
        } else {
            None
        };
        let ty_params = if let Some(class_ty) = class_ty {
            let r = class_ty.kind.expect_object_reference();
            let i = r.target.kind.expect_object_interface();
            i.local_ty_params
        } else {
            self.get_ty_params_from_decl(id)
        };
        let (sig, this_param) = get_sig_from_decl(self, decl);
        let sig = self.new_sig(sig);
        let mut links = super::links::SigLinks::default();
        if let Some(ty_params) = ty_params {
            links.set_ty_params(ty_params);
        }
        if let Some(this_param) = this_param {
            links.set_this_param(this_param);
        }
        let prev = self.sig_links.insert(sig.id, links);
        debug_assert!(prev.is_none());
        self.get_mut_node_links(id).set_resolved_sig(sig);
        sig
    }

    pub(super) fn get_sigs_of_symbol(&mut self, id: SymbolID) -> ty::Sigs<'cx> {
        let s = self.symbol(id);
        let Some(decls) = &s.decls else {
            return self.empty_array();
        };
        let decls = decls
            .iter()
            .enumerate()
            .filter_map(|(i, &decl)| {
                let n = self.p.node(decl);
                if !n.is_fn_like() {
                    None
                } else if i > 0 && n.fn_body().is_some() {
                    None
                } else {
                    Some(decl)
                }
            })
            .collect::<Vec<_>>();
        let sigs = decls
            .into_iter()
            .map(|decl| self.get_sig_from_decl(decl))
            .collect::<Vec<_>>();
        self.alloc(sigs)
    }

    pub(crate) fn get_signatures_of_type(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        kind: SigKind,
    ) -> ty::Sigs<'cx> {
        let ty = self.get_reduced_apparent_ty(ty);
        self.get_signatures_of_structured_type(ty, kind)
    }

    fn get_signatures_of_structured_type(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        kind: SigKind,
    ) -> ty::Sigs<'cx> {
        if ty.kind.is_structured() {
            self.resolve_structured_type_members(ty);
            self.signatures_of_type(ty, kind)
        } else {
            self.empty_array()
        }
    }

    pub(super) fn get_sig_of_ty_tag(&self, id: ast::NodeID) -> Option<&'cx Sig<'cx>> {
        let n = self.p.node(id);
        // TODO: js
        if n.is_fn_decl_like() {
            return None;
        }
        None
    }

    pub(super) fn get_single_sig<const ALLOW_MEMBERS: bool>(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        kind: SigKind,
    ) -> Option<&'cx Sig<'cx>> {
        if !ty.flags.contains(TypeFlags::OBJECT) {
            return None;
        }
        self.resolve_structured_type_members(ty);
        if !ALLOW_MEMBERS
            || (self.properties_of_object_type(ty).is_empty()
                && self.index_infos_of_ty(ty).is_empty())
        {
            let call_sigs = self.signatures_of_structured_type(ty, SigKind::Call);
            let ctor_sigs = self.signatures_of_structured_type(ty, SigKind::Constructor);
            if kind == SigKind::Call && call_sigs.len() == 1 && ctor_sigs.is_empty() {
                Some(call_sigs[0])
            } else if kind == SigKind::Constructor && ctor_sigs.len() == 1 && call_sigs.is_empty() {
                Some(ctor_sigs[0])
            } else {
                None
            }
        } else {
            None
        }
    }

    #[inline]
    pub(super) fn get_single_call_sig(&mut self, ty: &'cx ty::Ty<'cx>) -> Option<&'cx Sig<'cx>> {
        self.get_single_sig::<false>(ty, SigKind::Call)
    }

    pub(super) fn get_single_call_or_ctor_sig(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx Sig<'cx>> {
        self.get_single_call_sig(ty)
            .or_else(|| self.get_single_sig::<false>(ty, SigKind::Constructor))
    }

    pub(super) fn get_base_sig(&mut self, sig: &'cx Sig<'cx>) -> &'cx Sig<'cx> {
        if let Some(ty_params) = self.get_sig_links(sig.id).get_ty_params() {
            // TODO: baseSignatureCache
            let ty_eraser = self.create_ty_eraser(ty_params);
            let targets = {
                let tys = ty_params
                    .iter()
                    .map(|tp| {
                        self.get_constraint_of_ty_param(tp)
                            .unwrap_or(self.unknown_ty)
                    })
                    .collect::<Vec<_>>();
                self.alloc(tys)
            };
            let base_constraint_mapper = self.create_ty_mapper(ty_params, targets);
            let base_constraints = ty_params
                .iter()
                .map(|ty| self.instantiate_ty_worker(ty, base_constraint_mapper))
                .collect::<Vec<_>>();
            let mut base_constraints: ty::Tys<'cx> = self.alloc(base_constraints);
            for _ in 0..ty_params.len() - 1 {
                base_constraints = self.instantiate_tys(base_constraints, base_constraint_mapper)
            }
            base_constraints = self.instantiate_tys(base_constraints, ty_eraser);
            let mapper = self.create_ty_mapper(ty_params, base_constraints);
            self.instantiate_sig::<true>(sig, mapper)
        } else {
            sig
        }
    }

    fn create_erased_sig(&mut self, sig: &'cx Sig<'cx>, ty_params: ty::Tys<'cx>) -> &'cx Sig<'cx> {
        let mapper = self.create_ty_eraser(ty_params);
        self.instantiate_sig::<true>(sig, mapper)
    }

    pub(super) fn get_erased_sig(&mut self, sig: &'cx Sig<'cx>) -> &'cx Sig<'cx> {
        if let Some(ty_params) = self.get_sig_links(sig.id).get_ty_params() {
            // TODO: cache
            self.create_erased_sig(sig, ty_params)
        } else {
            sig
        }
    }

    pub(super) fn get_resolved_signature(
        &mut self,
        node: ast::NodeID,
        check_mode: Option<CheckMode>,
    ) -> &'cx ty::Sig<'cx> {
        let resolving_sig = self.resolving_sig();
        let cached = self.get_node_links(node).get_resolved_sig();
        if let Some(cached) = cached
            && cached != resolving_sig
        {
            // TODO: candidates_out_array
            return cached;
        }
        let saved_resolution_start = self.resolution_start;
        if cached.is_none() {
            self.resolution_start = self.resolution_tys.len() as i32;
            self.get_mut_node_links(node)
                .set_resolved_sig(resolving_sig);
        } else {
            self.get_mut_node_links(node)
                .override_resolved_sig(resolving_sig);
        }

        let check_mode = check_mode.unwrap_or(CheckMode::empty());
        let sig = match self.p.node(node) {
            ast::Node::CallExpr(call) => call.resolve_sig(self, check_mode),
            ast::Node::NewExpr(new) => new.resolve_sig(self, check_mode),
            ast::Node::TaggedTemplateExpr(expr) => expr.resolve_sig(self, check_mode),
            _ => unreachable!(),
        };

        self.resolution_start = saved_resolution_start;
        if sig != resolving_sig {
            if self.flow_loop_start == flow_loop_ctx_len(self) {
                self.get_mut_node_links(node).override_resolved_sig(sig);
            } else if let Some(cached) = cached {
                self.get_mut_node_links(node).override_resolved_sig(cached);
            } else {
                self.get_mut_node_links(node).clear_resolved_sig();
            }
        }

        sig
    }

    fn is_declaration_with_explicit_ty_annotation(&self, node: ast::NodeID) -> bool {
        use ast::Node::*;
        match self.p.node(node) {
            VarDecl(ast::VarDecl { ty, .. })
            | ClassPropElem(ast::ClassPropElem { ty, .. })
            | PropSignature(ast::PropSignature { ty, .. })
            | ParamDecl(ast::ParamDecl { ty, .. }) => {
                // TODO: is_in_js_file
                ty.is_some()
            }
            _ => false,
        }
    }

    fn get_explicit_ty_of_symbol(&mut self, symbol: SymbolID) -> Option<&'cx ty::Ty<'cx>> {
        let symbol = self.resolve_symbol::<false>(symbol);
        let s = self.symbol(symbol);
        let s_value_decl = s.value_decl;
        if s.flags.intersects(
            SymbolFlags::FUNCTION
                .union(SymbolFlags::METHOD)
                .union(SymbolFlags::CLASS)
                .union(SymbolFlags::VALUE_MODULE),
        ) {
            Some(self.get_type_of_symbol(symbol))
        } else if s
            .flags
            .intersects(SymbolFlags::VARIABLE.union(SymbolFlags::PROPERTY))
        {
            if self.get_check_flags(symbol).contains(CheckFlags::MAPPED)
                && let Some(original) = self.get_symbol_links(symbol).get_synthetic_origin()
                && self.get_explicit_ty_of_symbol(original).is_some()
            {
                return Some(self.get_type_of_symbol(symbol));
            }

            let decl = s_value_decl?;
            if self.is_declaration_with_explicit_ty_annotation(decl) {
                return Some(self.get_type_of_symbol(symbol));
            } else if self.p.node(decl).is_var_decl()
                && let Some(parent_parent) = self.parent(decl).and_then(|_nn| self.parent(decl))
                && self.p.node(parent_parent).is_for_of_stmt()
            {
                todo!()
            }
            None
        } else {
            None
        }
    }

    fn get_ty_of_dotted_name(&mut self, n: &'cx ast::Expr<'cx>) -> Option<&'cx ty::Ty<'cx>> {
        if self
            .p
            .node_flags(n.id())
            .contains(ast::NodeFlags::IN_WITH_STATEMENT)
        {
            return None;
        }
        use ast::ExprKind::*;
        match n.kind {
            Ident(n) if !is_prim_value_name(n.name) => {
                let symbol = self.final_res(n.id);
                let symbol = self.get_export_symbol_of_value_symbol_if_exported(symbol);
                self.get_explicit_ty_of_symbol(symbol)
            }
            This(_) => {
                // TODO:
                None
            }
            Super(_) => {
                // TODO:
                None
            }
            PropAccess(_) => {
                // TODO:
                None
            }
            Paren(n) => self.get_ty_of_dotted_name(n.expr),
            _ => None,
        }
    }

    pub(super) fn get_effects_sig(&mut self, node: ast::NodeID) -> Option<&'cx Sig<'cx>> {
        let sig = if let Some(sig) = self.get_node_links(node).get_effects_sig() {
            sig
        } else {
            let mut func_ty = None;
            let n = self.p.node(node);
            let expr = match n {
                ast::Node::CallExpr(call) => call.expr,
                // TODO: instanceof
                _ => unreachable!(),
            };
            if let Some(bin) = n.as_bin_expr() {
                let _right_tyy = self.check_non_null_expr(bin.right);
                // func_ty = Some()
                todo!()
            } else if let parent = self.parent(node).unwrap()
                && let Some(_stmt) = self.p.node(parent).as_expr_stmt()
            {
                func_ty = self.get_ty_of_dotted_name(expr);
            } else if !matches!(expr.kind, ast::ExprKind::Super(_)) {
                if self.node_query(node.module()).is_optional_chain(node) {
                    todo!()
                } else {
                    func_ty = Some(self.check_non_null_expr(expr));
                }
            };
            let sigs = if let Some(func_ty) = func_ty {
                let apparent_ty = self.get_apparent_ty(func_ty);
                self.get_signatures_of_type(apparent_ty, SigKind::Call)
            } else {
                self.get_signatures_of_type(self.unknown_ty, SigKind::Call)
            };
            let sig = if sigs.len() == 1 && self.get_sig_links(sigs[0].id).get_ty_params().is_none()
            {
                if self.has_ty_pred_or_never_ret_ty(sigs[0]) {
                    sigs[0]
                } else {
                    self.unknown_sig()
                }
            } else if sigs.iter().any(|sig| self.has_ty_pred_or_never_ret_ty(sig)) {
                let sig = self.get_resolved_signature(node, None);
                if self.has_ty_pred_or_never_ret_ty(sig) {
                    sig
                } else {
                    self.unknown_sig()
                }
            } else {
                self.unknown_sig()
            };
            self.get_mut_node_links(node).set_effects_sig(sig);
            sig
        };
        if sig == self.unknown_sig() {
            None
        } else {
            Some(sig)
        }
    }

    fn has_ty_pred_or_never_ret_ty(&mut self, sig: &'cx Sig<'cx>) -> bool {
        self.get_ty_predicate_of_sig(sig).is_some()
            || sig.node_id.is_some_and(|decl| {
                self.get_ret_ty_from_anno(decl)
                    .unwrap_or(self.unknown_ty)
                    .flags
                    .intersects(TypeFlags::NEVER)
            })
    }

    fn instantiate_ty_pred(
        &mut self,
        pred: &'cx TyPred<'cx>,
        mapper: Option<&'cx dyn ty::TyMap<'cx>>,
    ) -> &'cx TyPred<'cx> {
        use super::type_predicate::TyPredKind::*;
        match pred.kind {
            Ident(p) => {
                let ty = self.instantiate_ty(p.ty, mapper);
                self.create_ident_ty_pred(p.param_name, p.param_index, ty)
            }
            AssertsThis(p) => {
                let ty = p.ty.map(|ty| self.instantiate_ty(ty, mapper));
                let kind = AssertsThis(super::type_predicate::AssertsThisTyPred { ty });
                self.alloc(TyPred { kind })
            }
            This(p) => {
                let ty = self.instantiate_ty(p.ty, mapper);
                let kind = This(super::type_predicate::ThisTyPred { ty });
                self.alloc(TyPred { kind })
            }
            AssertsIdent(n) => {
                let ty = n.ty.map(|ty| self.instantiate_ty(ty, mapper));
                let kind = AssertsIdent(super::type_predicate::AssertsIdentTyPred { ty, ..n });
                self.alloc(TyPred { kind })
            }
        }
    }

    pub(super) fn get_ty_predicate_of_sig(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
    ) -> Option<&'cx TyPred<'cx>> {
        let pred = if let Some(pred) = self.get_sig_links(sig.id).get_resolved_ty_pred() {
            pred
        } else if let Some(target) = sig.target {
            let pred = if let Some(target_ty_pred) = self.get_ty_predicate_of_sig(target) {
                assert!(sig.mapper.is_some());

                self.instantiate_ty_pred(target_ty_pred, sig.mapper)
            } else {
                self.no_ty_pred()
            };
            self.get_mut_sig_links(sig.id).set_resolved_ty_pred(pred);
            pred
        } else if let Some(composite_sigs) = sig.composite_sigs {
            let kind = sig.composite_kind.unwrap();
            let is_intersection = kind == TypeFlags::INTERSECTION;
            debug_assert!(is_intersection || kind == TypeFlags::UNION);
            let pred = self
                .get_union_or_intersection_ty_pred(composite_sigs, is_intersection)
                .unwrap_or(self.no_ty_pred());
            self.get_mut_sig_links(sig.id).set_resolved_ty_pred(pred);
            pred
        } else {
            let ty = sig
                .node_id
                .and_then(|node_id| self.get_effective_ret_type_node(node_id));
            if let Some(ty) = ty {
                if let ast::TyKind::Pred(p) = ty.kind {
                    self.create_ty_pred_from_ty_pred_node(p, sig)
                } else {
                    self.no_ty_pred()
                }
            } else if let Some(decl) = sig.node_id
                && let decl_node = self.p.node(decl)
                && decl_node.is_fn_decl_like()
                && self
                    .get_sig_links(sig.id)
                    .get_resolved_ret_ty()
                    .is_none_or(|ret_ty| ret_ty.flags.contains(TypeFlags::BOOLEAN))
                && !sig.params.is_empty()
            {
                // get_type_predicate_from_body
                if matches!(
                    decl_node,
                    ast::Node::ClassCtor(_) | ast::Node::GetterDecl(_) | ast::Node::SetterDecl(_)
                ) {
                    self.no_ty_pred()
                } else if decl_node.fn_flags() != ast::FnFlags::empty() {
                    self.no_ty_pred()
                } else {
                    let Some(fn_body) = decl_node.fn_body() else {
                        unreachable!()
                    };

                    let single_return;
                    let mut bailed_early = false;
                    // let mut has_implicit_return = false;
                    match fn_body {
                        ast::ArrowFnExprBody::Expr(expr) => {
                            single_return = Some(expr);
                        }
                        ast::ArrowFnExprBody::Block(block_stmt) => {
                            // has_implicit_return = self.fn_has_implicit_return(decl);
                            struct CollectReturnStmtVisitor<'cx> {
                                single_return: Option<&'cx ast::Expr<'cx>>,
                                bailed_early: bool,
                            }
                            impl<'cx> RetStmtVisitor<'cx> for CollectReturnStmtVisitor<'cx> {
                                type Result = bolt_ts_ast_visitor::ControlFlow;

                                fn visit_ret_stmt(
                                    &mut self,
                                    _: &mut TyChecker<'_>,
                                    stmt: &'cx ast::RetStmt<'cx>,
                                ) -> Self::Result {
                                    if self.single_return.is_some() {
                                        self.bailed_early = true;
                                        return bolt_ts_ast_visitor::ControlFlow::Break;
                                    }
                                    match stmt.expr {
                                        Some(expr) => {
                                            self.single_return = Some(expr);
                                            bolt_ts_ast_visitor::ControlFlow::Continue
                                        }
                                        None => {
                                            self.bailed_early = true;
                                            bolt_ts_ast_visitor::ControlFlow::Break
                                        }
                                    }
                                }
                            }

                            let mut visitor = CollectReturnStmtVisitor {
                                single_return: None,
                                bailed_early: false,
                            };
                            for_each_return_statement(self, block_stmt.id, &mut visitor);
                            single_return = visitor.single_return;
                            bailed_early = visitor.bailed_early;
                        }
                    }
                    if bailed_early
                    /* || has_implicit_return */
                    {
                        self.no_ty_pred()
                    } else {
                        match single_return {
                            Some(single_return) => self
                                .check_if_expression_refines_any_parameter(decl, single_return)
                                .unwrap_or(self.no_ty_pred()),
                            None => self.no_ty_pred(),
                        }
                    }
                }
            } else {
                self.no_ty_pred()
            }
        };
        if std::ptr::eq(pred, self.no_ty_pred()) {
            None
        } else {
            Some(pred)
        }
    }

    fn check_if_expression_refines_any_parameter(
        &mut self,
        decl: ast::NodeID,
        expr: &'cx ast::Expr<'cx>,
    ) -> Option<&'cx TyPred<'cx>> {
        let expr = ast::Expr::skip_parens(expr);
        let ret_ty = self.check_expression_cached(expr, None);
        if !ret_ty.flags.contains(TypeFlags::BOOLEAN) {
            return None;
        }

        let n = self.p.node(decl);
        let params = n.params()?;
        for (i, param) in params.iter().enumerate() {
            let param_symbol = self.final_res(param.id);
            let init_ty = self.get_type_of_symbol(param_symbol);

            let ast::BindingKind::Ident(name) = param.name.kind else {
                continue;
            };
            if init_ty.flags.contains(TypeFlags::BOOLEAN) || self.is_symbol_assigned(param_symbol) {
                continue;
            }
            if let Some(true_ty) =
                self.check_if_expression_refines_parameter(decl, expr, param, init_ty)
            {
                return Some(self.create_ident_ty_pred(name.name, i as u32, true_ty));
            }
        }
        None
    }

    fn check_if_expression_refines_parameter(
        &mut self,
        func: ast::NodeID,
        expr: &'cx ast::Expr<'cx>,
        param: &'cx ast::ParamDecl<'cx>,
        init_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let antecedent = if let Some(flow) = self.get_flow_node_of_node(expr.id()) {
            Some(flow)
        } else if let Some(parent) = self.parent(expr.id())
            && let ast::Node::RetStmt(n) = self.p.node(parent)
            && let Some(flow) = self.get_flow_node_of_node(n.id)
        {
            Some(flow)
        } else {
            None
        };

        let shared_flow_start = self.shared_flow_info.len();
        let mut key = std::cell::OnceCell::new();
        let true_ty = self.get_ty_at_flow_cond_worker::<true>(
            expr.id(),
            antecedent,
            param.id,
            shared_flow_start,
            init_ty,
            init_ty,
            Some(func),
            &mut key,
        );
        let true_ty = self.get_ty_from_flow_ty(true_ty);
        if true_ty == init_ty {
            return None;
        }

        let mut key = std::cell::OnceCell::new();
        let false_ty = self.get_ty_at_flow_cond_worker::<false>(
            expr.id(),
            antecedent,
            param.id,
            shared_flow_start,
            init_ty,
            true_ty,
            Some(func),
            &mut key,
        );
        let false_ty = self.get_ty_from_flow_ty(false_ty);
        let false_subtype = self.get_reduced_ty(false_ty);
        false_subtype
            .flags
            .contains(TypeFlags::NEVER)
            .then_some(true_ty)
    }
}

fn get_sig_from_decl<'cx>(
    checker: &TyChecker<'cx>,
    node: ast::Node<'cx>,
) -> (Sig<'cx>, Option<SymbolID>) {
    debug_assert!(
        node.is_fn_decl()
            || node.is_fn_expr()
            || node.is_arrow_fn_expr()
            || node.is_class_ctor()
            || node.is_ctor_sig_decl()
            || node.is_class_method_elem()
            || node.is_method_signature()
            || node.is_object_method_member()
            || node.is_call_sig_decl()
            || node.is_fn_ty()
            || node.is_ctor_ty()
            || node.is_getter_decl()
            || node.is_setter_decl(),
        "node: {node:#?}",
    );
    let params_of_node = if node.is_getter_decl() {
        Default::default()
    } else {
        node.params().unwrap()
    };
    let mut this_param = None;
    let has_rest_param = ast::has_rest_param(params_of_node);
    let mut flags = SigFlags::empty();
    let mut min_args_count = 0;
    let mut params = Vec::with_capacity(params_of_node.len());
    for (idx, param) in params_of_node.iter().enumerate() {
        let id = node_id_of_binding(*param);
        let symbol = checker.final_res(id);
        if idx == 0
            && checker
                .symbol(symbol)
                .name
                .as_atom()
                .is_some_and(|atom| atom == keyword::KW_THIS)
        {
            this_param = Some(symbol);
        } else {
            params.push(symbol);
        }

        if param.ty.is_some_and(|ty| ty.kind.is_lit()) {
            flags |= SigFlags::HAS_LITERAL_TYPES;
        }

        let is_opt = param.question.is_some() || param.dotdotdot.is_some() || param.init.is_some();
        if !is_opt {
            min_args_count = params.len();
        } else {
            assert!(
                min_args_count < params.len(),
                "required parameters cannot follow an optional parameter."
            )
        }
    }
    if has_rest_param {
        flags.insert(SigFlags::HAS_REST_PARAMETER);
    }
    match node {
        ast::Node::CtorTy(n)
            if n.modifiers
                .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::ABSTRACT)) =>
        {
            flags |= SigFlags::ABSTRACT;
        }
        ast::Node::ClassCtor(n)
            if n.modifiers
                .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::ABSTRACT)) =>
        {
            flags |= SigFlags::ABSTRACT;
        }
        _ => {}
    }
    let params: &[SymbolID] = checker.alloc(params);
    let ret = match node {
        ast::Node::FnDecl(decl) => decl.ty.map(|ty| ty.id()),
        ast::Node::FnExpr(_) => None,
        ast::Node::ArrowFnExpr(_) => None,
        ast::Node::ClassCtor(c) => {
            let class_id = checker.parent(c.id).unwrap();
            Some(class_id)
        }
        ast::Node::CtorSigDecl(c) => c.ty.map(|ty| ty.id()),
        ast::Node::ClassMethodElem(f) => f.ty.map(|ty| ty.id()),
        ast::Node::MethodSignature(f) => f.ty.map(|ty| ty.id()),
        ast::Node::CallSigDecl(f) => f.ty.map(|ty| ty.id()),
        ast::Node::FnTy(f) => Some(f.ty.id()),
        ast::Node::CtorTy(f) => Some(f.ty.id()),
        ast::Node::GetterDecl(f) => f.ty.map(|ty| ty.id()),
        ast::Node::ObjectMethodMember(f) => f.ty.map(|ty| ty.id()),
        ast::Node::SetterDecl(_) => None,
        _ => unreachable!(),
    };
    (
        ty::Sig {
            flags,
            params,
            min_args_count: min_args_count as u32,
            ret,
            node_id: Some(node.id()),
            target: None,
            mapper: None,
            id: SigID::dummy(),
            class_decl: None,
            composite_sigs: None,
            composite_kind: None,
        },
        this_param,
    )
}

trait RetStmtVisitor<'cx> {
    type Result: VisitorResult;

    fn visit_ret_stmt(
        &mut self,
        checker: &mut TyChecker<'cx>,
        stmt: &'cx ast::RetStmt<'cx>,
    ) -> Self::Result;
}

fn for_each_return_statement<'cx, V: RetStmtVisitor<'cx>>(
    checker: &mut TyChecker<'cx>,
    node: ast::NodeID,
    v: &mut V,
) -> V::Result {
    match checker.p.node(node) {
        ast::Node::RetStmt(n) => v.visit_ret_stmt(checker, n),
        ast::Node::CaseBlock(n) => {
            for clause in n.clauses {
                let result = for_each_return_statement(checker, clause.id(), v);
                if result.branch() == bolt_ts_ast_visitor::ControlFlow::Break {
                    return result;
                }
            }
            V::Result::output()
        }
        ast::Node::BlockStmt(n) => {
            for stmt in n.stmts {
                let result = for_each_return_statement(checker, stmt.id(), v);
                if result.branch() == bolt_ts_ast_visitor::ControlFlow::Break {
                    return result;
                }
            }
            V::Result::output()
        }
        ast::Node::IfStmt(n) => {
            let result = for_each_return_statement(checker, n.then.id(), v);
            if result.branch() == bolt_ts_ast_visitor::ControlFlow::Break {
                return result;
            }
            if let Some(else_then) = n.else_then {
                let result = for_each_return_statement(checker, else_then.id(), v);
                if result.branch() == bolt_ts_ast_visitor::ControlFlow::Break {
                    return result;
                }
            }
            V::Result::output()
        }
        ast::Node::DoWhileStmt(ast::DoWhileStmt { stmt, .. })
        | ast::Node::WhileStmt(ast::WhileStmt { stmt, .. }) => {
            let result = for_each_return_statement(checker, stmt.id(), v);
            if result.branch() == bolt_ts_ast_visitor::ControlFlow::Break {
                return result;
            }
            V::Result::output()
        }
        ast::Node::ForStmt(ast::ForStmt { body, .. })
        | ast::Node::ForInStmt(ast::ForInStmt { body, .. })
        | ast::Node::ForOfStmt(ast::ForOfStmt { body, .. }) => {
            let result = for_each_return_statement(checker, body.id(), v);
            if result.branch() == bolt_ts_ast_visitor::ControlFlow::Break {
                return result;
            }
            V::Result::output()
        }
        // TODO: with
        ast::Node::SwitchStmt(n) => {
            let result = for_each_return_statement(checker, n.case_block.id, v);
            if result.branch() == bolt_ts_ast_visitor::ControlFlow::Break {
                return result;
            }
            V::Result::output()
        }
        ast::Node::CaseClause(n) => {
            for stmt in n.stmts {
                let result = for_each_return_statement(checker, stmt.id(), v);
                if result.branch() == bolt_ts_ast_visitor::ControlFlow::Break {
                    return result;
                }
            }
            V::Result::output()
        }
        ast::Node::DefaultClause(n) => {
            for stmt in n.stmts {
                let result = for_each_return_statement(checker, stmt.id(), v);
                if result.branch() == bolt_ts_ast_visitor::ControlFlow::Break {
                    return result;
                }
            }
            V::Result::output()
        }
        ast::Node::LabeledStmt(n) => for_each_return_statement(checker, n.stmt.id(), v),
        ast::Node::TryStmt(n) => {
            let result = for_each_return_statement(checker, n.try_block.id, v);
            if result.branch() == bolt_ts_ast_visitor::ControlFlow::Break {
                return result;
            }
            if let Some(catch_clause) = n.catch_clause {
                let result = for_each_return_statement(checker, catch_clause.id, v);
                if result.branch() == bolt_ts_ast_visitor::ControlFlow::Break {
                    return result;
                }
            }
            if let Some(finally_block) = n.finally_block {
                let result = for_each_return_statement(checker, finally_block.id, v);
                if result.branch() == bolt_ts_ast_visitor::ControlFlow::Break {
                    return result;
                }
            }
            V::Result::output()
        }
        ast::Node::CatchClause(n) => for_each_return_statement(checker, n.block.id, v),
        _ => V::Result::output(),
    }
}
