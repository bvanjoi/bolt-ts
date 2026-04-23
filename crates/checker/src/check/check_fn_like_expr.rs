use super::NodeCheckFlags;
use super::ty;
use super::ty::TypeFlags;
use super::{CheckMode, TyChecker};

use bolt_ts_ast as ast;
use bolt_ts_ast::r#trait;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LanguageFeatures {
    Classes,
    ForOf,
    Generators,
    Iteration,
    SpreadElements,
    RestElements,
    TaggedTemplates,
    DestructuringAssignment,
    BindingPatterns,
    ArrowFunctions,
    BlockScopedVariables,
    ObjectAssign,
    RegularExpressionFlagsUnicode,
    RegularExpressionFlagsSticky,
    Exponentiation,
    AsyncFunctions,
    ForAwaitOf,
    AsyncGenerators,
    AsyncIteration,
    ObjectSpreadRest,
    RegularExpressionFlagsDotAll,
    BindinglessCatch,
    BigInt,
    NullishCoalesce,
    OptionalChaining,
    LogicalAssignment,
    TopLevelAwait,
    ClassFields,
    PrivateNamesAndClassStaticBlocks,
    RegularExpressionFlagsHasIndices,
    ShebangComments,
    RegularExpressionFlagsUnicodeSets,
    UsingAndAwaitUsing,
    ClassAndClassElementDecorators,
}

// const LANGUAGE_FEATURE_MINIMUM_TARGET: std::sync::LazyLock<
//     rustc_hash::FxHashMap<LanguageFeatures, bolt_ts_config::Target>,
// > = std::sync::LazyLock::new(|| {
//     rustc_hash::FxHashMap::from_iter(
//         [
//             (LanguageFeatures::Classes, bolt_ts_config::Target::ES2015),
//             (LanguageFeatures::ForOf, bolt_ts_config::Target::ES2015),
//             (LanguageFeatures::Generators, bolt_ts_config::Target::ES2015),
//             (LanguageFeatures::Iteration, bolt_ts_config::Target::ES2015),
//             (
//                 LanguageFeatures::SpreadElements,
//                 bolt_ts_config::Target::ES2015,
//             ),
//             (
//                 LanguageFeatures::RestElements,
//                 bolt_ts_config::Target::ES2015,
//             ),
//             (
//                 LanguageFeatures::TaggedTemplates,
//                 bolt_ts_config::Target::ES2015,
//             ),
//             (
//                 LanguageFeatures::DestructuringAssignment,
//                 bolt_ts_config::Target::ES2015,
//             ),
//             (
//                 LanguageFeatures::BindingPatterns,
//                 bolt_ts_config::Target::ES2015,
//             ),
//             (
//                 LanguageFeatures::ArrowFunctions,
//                 bolt_ts_config::Target::ES2015,
//             ),
//             (
//                 LanguageFeatures::BlockScopedVariables,
//                 bolt_ts_config::Target::ES2015,
//             ),
//             (
//                 LanguageFeatures::ObjectAssign,
//                 bolt_ts_config::Target::ES2015,
//             ),
//             (
//                 LanguageFeatures::RegularExpressionFlagsUnicode,
//                 bolt_ts_config::Target::ES2015,
//             ),
//             (
//                 LanguageFeatures::RegularExpressionFlagsSticky,
//                 bolt_ts_config::Target::ES2015,
//             ),
//             (
//                 LanguageFeatures::Exponentiation,
//                 bolt_ts_config::Target::ES2016,
//             ),
//             (
//                 LanguageFeatures::AsyncFunctions,
//                 bolt_ts_config::Target::ES2017,
//             ),
//             (LanguageFeatures::ForAwaitOf, bolt_ts_config::Target::ES2018),
//             (
//                 LanguageFeatures::AsyncGenerators,
//                 bolt_ts_config::Target::ES2018,
//             ),
//             (
//                 LanguageFeatures::AsyncIteration,
//                 bolt_ts_config::Target::ES2018,
//             ),
//             (
//                 LanguageFeatures::ObjectSpreadRest,
//                 bolt_ts_config::Target::ES2018,
//             ),
//             (
//                 LanguageFeatures::RegularExpressionFlagsDotAll,
//                 bolt_ts_config::Target::ES2018,
//             ),
//             (
//                 LanguageFeatures::BindinglessCatch,
//                 bolt_ts_config::Target::ES2019,
//             ),
//             (LanguageFeatures::BigInt, bolt_ts_config::Target::ES2020),
//             (
//                 LanguageFeatures::NullishCoalesce,
//                 bolt_ts_config::Target::ES2020,
//             ),
//             (
//                 LanguageFeatures::OptionalChaining,
//                 bolt_ts_config::Target::ES2020,
//             ),
//             (
//                 LanguageFeatures::LogicalAssignment,
//                 bolt_ts_config::Target::ES2021,
//             ),
//             (
//                 LanguageFeatures::TopLevelAwait,
//                 bolt_ts_config::Target::ES2022,
//             ),
//             (
//                 LanguageFeatures::ClassFields,
//                 bolt_ts_config::Target::ES2022,
//             ),
//             (
//                 LanguageFeatures::PrivateNamesAndClassStaticBlocks,
//                 bolt_ts_config::Target::ES2022,
//             ),
//             (
//                 LanguageFeatures::RegularExpressionFlagsHasIndices,
//                 bolt_ts_config::Target::ES2022,
//             ),
//             (
//                 LanguageFeatures::ShebangComments,
//                 bolt_ts_config::Target::ES2023,
//             ),
//             (
//                 LanguageFeatures::RegularExpressionFlagsUnicodeSets,
//                 bolt_ts_config::Target::ES2024,
//             ),
//             (
//                 LanguageFeatures::UsingAndAwaitUsing,
//                 bolt_ts_config::Target::ESNext,
//             ),
//             (
//                 LanguageFeatures::ClassAndClassElementDecorators,
//                 bolt_ts_config::Target::ESNext,
//             ),
//         ]
//         .into_iter(),
//     )
// });

impl<'cx> TyChecker<'cx> {
    fn contextually_check_fn_expr_or_object_method_member(&mut self, id: ast::NodeID) {
        let flags = |this: &mut Self| this.get_node_links(id).flags();

        if !flags(self).contains(NodeCheckFlags::CONTEXT_CHECKED) {
            let contextual_sig = self.get_contextual_sig(id);

            if !flags(self).contains(NodeCheckFlags::CONTEXT_CHECKED) {
                self.get_mut_node_links(id)
                    .config_flags(|flags| flags | NodeCheckFlags::CONTEXT_CHECKED);
                let symbol = self.get_symbol_of_decl(id);
                let ty = self.get_type_of_symbol(symbol);
                let sigs = self.get_signatures_of_type(ty, ty::SigKind::Call);
                let Some(sig) = sigs.first() else { return };
                if self.is_context_sensitive(id) {
                    if let Some(contextual_sig) = contextual_sig {
                        let inference = self.get_inference_context(id);
                        let mut instantiated_contextual_sig = None;
                        if let Some(check_mode) = self.check_mode
                            && check_mode.intersects(CheckMode::INFERENTIAL)
                        {
                            let inference = inference.unwrap().inference.unwrap();
                            self.infer_from_annotated_params_and_return(
                                sig,
                                contextual_sig,
                                inference,
                            );
                            let rest_ty = contextual_sig.get_effective_rest_ty(self);
                            if let Some(rest_ty) = rest_ty
                                && rest_ty.flags.contains(TypeFlags::TYPE_PARAMETER)
                            {
                                let mapper = self.inference(inference).non_fixing_mapper;
                                instantiated_contextual_sig =
                                    Some(self.instantiate_sig(contextual_sig, mapper, false));
                            }
                        }
                        let instantiated_contextual_sig =
                            if let Some(sig) = instantiated_contextual_sig {
                                sig
                            } else if let Some(inference) = inference
                                && let Some(i) = inference.inference
                            {
                                let mapper = self.inference(i).mapper;
                                self.instantiate_sig(contextual_sig, mapper, false)
                            } else {
                                contextual_sig
                            };
                        self.assign_contextual_param_tys(sig, instantiated_contextual_sig);
                    } else {
                        self.assign_non_contextual_param_tys(sig);
                    }
                }

                if contextual_sig.is_some()
                    && self.get_ret_ty_from_anno(id).is_none()
                    && self.get_sig_links(sig.id).get_resolved_ret_ty().is_none()
                {
                    let ret_ty = self.get_ret_ty_from_body(id);
                    self.get_mut_sig_links(sig.id).set_resolved_ret_ty(ret_ty);
                }

                self.check_sig_decl(id);
            }
        }
    }

    pub(super) fn check_sig_decl(&mut self, id: ast::NodeID) {
        let n = self.p.node(id);
        let ty_params = self.get_effective_ty_param_decls(id);
        self.check_ty_params(ty_params);

        if let Some(return_ty) = n.ret_ty() {
            self.check_ty(return_ty);
        }

        // check signature declaration
        let ret_ty_node = self.get_effective_ret_type_node(id);
        let ret_ty_error_location = ret_ty_node;
        if self.config.compiler_options().no_implicit_any() && ret_ty_node.is_none() {
            match n {
                ast::Node::CtorSigDecl(_) => {
                    todo!()
                }
                ast::Node::CallSigDecl(_) => {
                    todo!()
                }
                _ => {}
            }
        }

        if let Some(ret_ty_node) = ret_ty_node
            && let Some(ret_ty_error_location) = ret_ty_error_location
        {
            let fn_flags = n.fn_flags();
            if fn_flags.intersection(ast::FnFlags::INVALID.union(ast::FnFlags::GENERATOR))
                == ast::FnFlags::GENERATOR
            {
                let ret_ty = self.get_ty_from_type_node(ret_ty_node);
                // TODO:
            } else if fn_flags.intersection(ast::FnFlags::ASYNC_GENERATOR) == ast::FnFlags::ASYNC {
                self.check_async_fn_ret_ty(id, ret_ty_node, ret_ty_error_location);
            }
        }
    }

    fn check_async_fn_ret_ty(
        &mut self,
        id: ast::NodeID,
        ret_ty_node: &'cx ast::Ty<'cx>,
        ret_ty_error_location: &'cx ast::Ty<'cx>,
    ) {
        let ret_ty = self.get_ty_from_type_node(ret_ty_node);
        if self.config.compiler_options().target() >= &bolt_ts_config::Target::ES2015 {
            if self.is_error(ret_ty) {
                return;
            }
            // let global_promise_ty = self.get_global_promise_ty();
            // if global_promise_ty != self.empty_generic_ty()
            //     && !self.is_reference_to_ty(ret_ty, global_promise_ty)
            // {
            //     todo!("error handler")
            // }
        } else {
            // TODO: mark_linked_reference
            if self.is_error(ret_ty) {
                return;
            }
            // TODO:
        }

        self.check_awaited_ty(ret_ty, false, ret_ty_error_location.id(), |_| {});
    }

    pub(super) fn check_awaited_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        with_alias: bool,
        error_node: ast::NodeID,
        push_error: impl FnOnce(&mut Self),
    ) -> &'cx ty::Ty<'cx> {
        let awaited_ty = if with_alias {
            self.get_awaited_ty(ty)
        } else {
            self.get_awaited_ty_no_alias(ty)
        };
        awaited_ty.unwrap_or(self.error_ty)
    }

    pub(super) fn check_fn_like_expr_or_object_method_member(
        &mut self,
        node: ast::NodeID,
    ) -> &'cx ty::Ty<'cx> {
        self.check_node_deferred(node);

        if let Some(mode) = self.check_mode
            && mode.contains(CheckMode::SKIP_CONTEXT_SENSITIVE)
            && self.is_context_sensitive(node)
        {
            return if self.get_effective_ret_type_node(node).is_none()
                && !self.has_context_sensitive_params(node)
                && let Some(contextual_sig) = self.get_contextual_sig(node)
                && let ret_ty_of_sig = self.get_ret_ty_of_sig(contextual_sig)
                && self.could_contain_ty_var(ret_ty_of_sig)
            {
                if let Some(context_free_ty) = self.get_node_links(node).get_context_free_ty() {
                    return context_free_ty;
                };
                let ret_ty = self.get_ret_ty_from_body(node);
                let ret_only_sig = self.new_sig(ty::Sig {
                    id: ty::SigID::dummy(),
                    params: self.empty_array(),
                    ret: None,
                    flags: ty::SigFlags::IS_NON_INFERRABLE,
                    this_param: None,
                    target: None,
                    mapper: None,
                    class_decl: None,
                    min_args_count: 0,
                    node_id: None,
                    composite_sigs: None,
                    composite_kind: None,
                });
                let prev = self.sig_links.insert(
                    ret_only_sig.id,
                    super::SigLinks::default().with_resolved_ret_ty(ret_ty),
                );
                debug_assert!(prev.is_none());
                let symbol = self.get_symbol_of_decl(node);
                let call_sigs = self.alloc([ret_only_sig]);
                let ret_only_ty = self.create_anonymous_ty_with_resolved(
                    Some(symbol),
                    ty::ObjectFlags::NON_INFERRABLE_TYPE,
                    self.alloc(Default::default()),
                    call_sigs,
                    self.empty_array(),
                    self.empty_array(),
                    None,
                );
                self.get_mut_node_links(node)
                    .set_context_free_ty(ret_only_ty);
                ret_only_ty
            } else {
                self.any_fn_ty()
            };
        }

        self.contextually_check_fn_expr_or_object_method_member(node);

        let symbol = self.get_symbol_of_decl(node);
        self.get_type_of_symbol(symbol)
    }

    pub(super) fn check_fn_like_expr(
        &mut self,
        expr: &impl r#trait::FnExprLike<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        self.check_fn_like_expr_or_object_method_member(expr.id())
    }

    pub(super) fn check_fn_like_expr_deferred(&mut self, expr: &impl r#trait::FnExprLike<'cx>) {
        self.check_fn_like_expr_or_object_method_member_deferred(
            expr,
            r#trait::FnExprLike::body(expr),
        );
    }

    pub(super) fn check_fn_like_expr_or_object_method_member_deferred(
        &mut self,
        func: &impl r#trait::FnLike<'cx>,
        body: ast::ArrowFnExprBody<'cx>,
    ) {
        let ret_ty = self.get_ret_ty_from_anno(func.id());
        self.check_all_code_paths_in_non_void_fn_ret_or_throw(func, ret_ty);

        use bolt_ts_ast::ArrowFnExprBody::*;
        match body {
            Block(block) => self.check_block(block),
            Expr(expr) => {
                let expr_ty = self.check_expr(expr);
                if let Some(return_or_promised_ty) = ret_ty.and_then(|t| {
                    let fn_flags = self.p.node(func.id()).fn_flags();
                    self.unwrap_ret_ty(t, fn_flags)
                }) {
                    self.check_ret_expr::<false>(
                        func.id(),
                        return_or_promised_ty,
                        Some(expr),
                        expr_ty,
                    );
                }
            }
        }
    }
}
