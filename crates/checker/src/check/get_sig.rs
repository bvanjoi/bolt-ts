use bolt_ts_ast::keyword;
use bolt_ts_ast::pprint_ident;
use bolt_ts_ast::r#trait::node_id_of_binding;
use bolt_ts_binder::SymbolFlags;
use bolt_ts_binder::SymbolID;

use super::TyChecker;
use super::ast;
use super::check_call_like::CallLikeExpr;
use super::symbol_info::SymbolInfo;
use super::ty;
use super::ty::CheckFlags;
use super::ty::SigID;
use super::ty::SigKind;
use super::ty::TypeFlags;
use super::ty::{Sig, SigFlags};
use super::type_predicate::TyPred;

impl<'cx> TyChecker<'cx> {
    pub(super) fn new_sig(&mut self, sig: Sig<'cx>) -> &'cx Sig<'cx> {
        assert!(sig.id == SigID::dummy(), "TODO: hidden id");
        let sig = sig.with_id(self.sigs.len());
        let s = self.alloc(sig);
        self.sigs.push(s);
        s
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

    pub(super) fn get_sig_from_decl(&mut self, id: ast::NodeID) -> &'cx Sig<'cx> {
        if let Some(sig) = self.get_node_links(id).get_resolved_sig() {
            return sig;
        }
        let decl = self.p.node(id);
        let host_decl = decl; // TODO: sig in js doc
        let class_ty = if host_decl.is_class_ctor() {
            let class_decl = self.parent(id).unwrap();
            let class_symbol = self.get_symbol_of_decl(class_decl);
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
        let sig = get_sig_from_decl(self, decl);
        let sig = self.new_sig(sig);
        if let Some(ty_params) = ty_params {
            let links = super::links::SigLinks::default().with_ty_params(ty_params);
            let prev = self.sig_links.insert(sig.id, links);
            debug_assert!(prev.is_none());
        }
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

    pub(super) fn get_sig_of_ty_tag(&mut self, id: ast::NodeID) -> Option<&'cx Sig<'cx>> {
        None
    }

    pub(super) fn get_single_sig(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        kind: SigKind,
        allow_members: bool,
    ) -> Option<&'cx Sig<'cx>> {
        if !ty.kind.is_object() {
            return None;
        }
        self.resolve_structured_type_members(ty);
        if !allow_members
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
        self.get_single_sig(ty, SigKind::Call, false)
    }

    pub(super) fn get_single_call_or_ctor_sig(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx Sig<'cx>> {
        self.get_single_call_sig(ty)
            .or_else(|| self.get_single_sig(ty, SigKind::Constructor, false))
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
                .map(|ty| self.instantiate_ty(ty, Some(base_constraint_mapper)))
                .collect::<Vec<_>>();
            let mut base_constraints: ty::Tys<'cx> = self.alloc(base_constraints);
            for _ in 0..ty_params.len() - 1 {
                base_constraints = self.instantiate_tys(base_constraints, base_constraint_mapper)
            }
            base_constraints = self.instantiate_tys(base_constraints, ty_eraser);
            let mapper = self.create_ty_mapper(ty_params, base_constraints);
            self.instantiate_sig(sig, mapper, true)
        } else {
            sig
        }
    }

    fn create_erased_sig(&mut self, sig: &'cx Sig<'cx>, ty_params: ty::Tys<'cx>) -> &'cx Sig<'cx> {
        let mapper = self.create_ty_eraser(ty_params);
        self.instantiate_sig(sig, mapper, true)
    }

    pub(super) fn get_erased_sig(&mut self, sig: &'cx Sig<'cx>) -> &'cx Sig<'cx> {
        if let Some(ty_params) = self.get_sig_links(sig.id).get_ty_params() {
            // TODO: cache
            self.create_erased_sig(sig, ty_params)
        } else {
            sig
        }
    }

    pub(super) fn get_resolved_sig(&mut self, node: ast::NodeID) -> &'cx ty::Sig<'cx> {
        let resolving_sig = self.resolving_sig();
        if let Some(cached) = self.get_node_links(node).get_resolved_sig()
            && cached != resolving_sig
        {
            return cached;
        }

        self.get_mut_node_links(node)
            .set_resolved_sig(resolving_sig);

        let sig = match self.p.node(node) {
            ast::Node::CallExpr(call) => call.resolve_sig(self),
            ast::Node::TaggedTemplateExpr(expr) => expr.resolve_sig(self),
            _ => unreachable!(),
        };

        self.get_mut_node_links(node).override_resolved_sig(sig);
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
        let symbol = self.resolve_symbol(symbol, false);
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

            let Some(decl) = s_value_decl else {
                return None;
            };
            if self.is_declaration_with_explicit_ty_annotation(decl) {
                return Some(self.get_type_of_symbol(symbol));
            } else if self.p.node(decl).is_var_decl()
                && let Some(parent_parent) = self.parent(decl).and_then(|n| self.parent(decl))
                && self.p.node(parent_parent).is_for_of_stmt()
            {
                // TODO:
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
            Ident(n) => {
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
                let right_ty = self.check_expr(bin.right);
                // func_ty = Some()
                todo!()
            } else if let parent = self.parent(node).unwrap()
                && let Some(stmt) = self.p.node(parent).as_expr_stmt()
            {
                func_ty = self.get_ty_of_dotted_name(stmt.expr);
            } else if !matches!(expr.kind, ast::ExprKind::Super(_)) {
                // TODO: is_optional_chain
                func_ty = Some(self.check_non_null_expr(expr));
            };
            let sigs = if let Some(func_ty) = func_ty {
                let apparent_ty = self.get_apparent_ty(func_ty);
                self.get_signatures_of_type(apparent_ty, SigKind::Call)
            } else {
                self.get_signatures_of_type(self.unknown_ty, SigKind::Call)
            };
            let candidate =
                if sigs.len() == 1 && self.get_sig_links(sigs[0].id).get_ty_params().is_none() {
                    Some(sigs[0])
                } else if sigs.iter().any(|sig| self.has_ty_pred_or_never_ret_ty(sig)) {
                    // TODO: get_resolved_sig(node)
                    Some(self.get_resolved_sig(node))
                } else {
                    None
                };
            let sig = candidate
                .filter(|sig| self.has_ty_pred_or_never_ret_ty(sig))
                .unwrap_or(self.unknown_sig());
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
        } else {
            // TODO: composite sigs
            let ty = sig
                .node_id
                .and_then(|node_id| self.get_effective_ret_type_node(node_id));
            let pred = if let Some(ty) = ty {
                if let ast::TyKind::Pred(p) = ty.kind {
                    self.create_ty_pred_from_ty_pred_node(p, sig)
                } else {
                    self.no_ty_pred()
                }
            } else if let Some(decl) = sig.node_id {
                // TODO:
                self.no_ty_pred()
            } else {
                self.no_ty_pred()
            };
            self.get_mut_sig_links(sig.id).set_resolved_ty_pred(pred);
            pred
        };
        if std::ptr::eq(pred, self.no_ty_pred()) {
            None
        } else {
            Some(pred)
        }
    }
}

fn get_sig_from_decl<'cx>(checker: &TyChecker<'cx>, node: ast::Node<'cx>) -> Sig<'cx> {
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
    Sig {
        flags,
        this_param,
        params,
        min_args_count,
        ret,
        node_id: Some(node.id()),
        target: None,
        mapper: None,
        id: SigID::dummy(),
        class_decl: None,
    }
}
