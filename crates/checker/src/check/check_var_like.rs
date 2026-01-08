use crate::check::check_expr::IterationUse;

use super::TyChecker;
use super::errors;
use super::symbol_info::SymbolInfo;

use bolt_ts_ast as ast;
use bolt_ts_ast::r#trait;
use bolt_ts_binder::SymbolFlags;

impl<'cx> TyChecker<'cx> {
    fn are_declaration_flags_identical(&self, left: ast::NodeID, right: ast::NodeID) -> bool {
        let l = self.p.node(left);
        let r = self.p.node(right);
        if l.is_param_decl() && r.is_var_decl() {
            true
        } else if l.is_var_decl() && r.is_param_decl() {
            true
        } else if l.has_question() != r.has_question() {
            false
        } else {
            use ast::ModifierKind;
            const FLAGS: enumflags2::BitFlags<ast::ModifierKind> = enumflags2::make_bitflags!(
                ModifierKind::{Private | Protected | Async | Abstract | Readonly | Static}
            );
            match (l.modifiers(), r.modifiers()) {
                (None, None) => true,
                (None, Some(r)) => !r.flags.intersects(FLAGS),
                (Some(l), None) => !l.flags.intersects(FLAGS),
                (Some(l), Some(r)) => l.flags.intersects(FLAGS) == r.flags.intersects(FLAGS),
            }
        }
    }

    fn check_non_pat_var_like_decl(
        &mut self,
        name_id: ast::NodeID,
        decl_id: ast::NodeID,
        decl: &'cx impl r#trait::VarLike<'cx>,
    ) {
        if !self.p.node(name_id).is_object_binding_elem()
            && let Some(ty) = decl.decl_ty()
        {
            self.check_ty(ty);
        }

        let symbol = self.get_symbol_of_decl(decl_id);
        let ty = self.get_type_of_symbol(symbol);
        let s = self.binder.symbol(symbol);
        if decl_id == s.value_decl.unwrap() {
            if let Some(init) = decl.init() {
                let init_ty = self.check_expr_cached(init);
                debug_assert!(
                    decl.decl_ty()
                        .is_none_or(|_| self.node_links[&init.id()].get_resolved_ty().is_some())
                );
                if ty != init_ty {
                    self.check_type_assignable_to_and_optionally_elaborate(
                        init_ty,
                        ty,
                        Some(name_id),
                        Some(init.id()),
                    );
                }
            }

            // TODO: ensure check once for the symbol
            if let Some(decls) = self.binder.symbol(symbol).decls.as_ref()
                && decls.len() > 1
                && decls.iter().any(|&d| {
                    d != decl_id
                        && self.p.node(d).is_variable_like()
                        && !self.are_declaration_flags_identical(d, decl_id)
                })
            {
                let error = errors::AllDeclarationsOfXMustHaveIdenticalModifiers {
                    span: self.p.node(name_id).span(),
                    symbol: self.binder.symbol(symbol).name.to_string(&self.atoms),
                };
                self.push_error(Box::new(error));
            }
        } else {
            let is_assignment = s.flags.intersects(SymbolFlags::ASSIGNMENT);
            let decl_ty = self.get_widened_ty_for_var_like_decl(decl);
            if !self.is_error(ty)
                && !self.is_error(decl_ty)
                && !self.is_type_identical_to(ty, decl_ty)
                && !is_assignment
            {
                let name = self.p.node(name_id).ident_name().unwrap();
                let error = errors::SubsequentVariableDeclarationsMustHaveTheSameTypeVariableMustBeOfTypeXButHereHasTypeY {
                    span: self.p.node(name_id).span(),
                    var: self.atoms.get(name.name).to_string(),
                    ty1: ty.to_string(self),
                    ty2: decl_ty.to_string(self),
                };
                self.push_error(Box::new(error));
            }
        }
    }

    pub(super) fn check_var_like_decl(&mut self, decl: &'cx impl r#trait::VarLike<'cx>) {
        use bolt_ts_ast::r#trait::VarLikeName::*;
        let id = decl.id();
        let name = decl.name();
        match name {
            Ident(name) => self.check_non_pat_var_like_decl(name.id, id, decl),
            StringLit { raw, .. } => self.check_non_pat_var_like_decl(raw.id, id, decl),
            NumLit(num) => self.check_non_pat_var_like_decl(num.id, id, decl),
            Computed(_) => {}
            ArrayPat(_) | ObjectPat(_) => {
                match name {
                    ArrayPat(n) => {
                        for elem in n.elems {
                            match elem.kind {
                                ast::ArrayBindingElemKind::Omit(_) => {}
                                ast::ArrayBindingElemKind::Binding(binding) => {
                                    self.check_var_like_decl(binding);
                                }
                            }
                        }
                    }
                    ObjectPat(n) => {
                        for elem in n.elems {
                            self.check_var_like_decl(*elem);
                        }
                    }
                    _ => unreachable!(),
                }

                // TODO: `is_in_ambient_context` then `return`
                let need_check_initializer = self.p.node(id).has_only_expr_init()
                    && decl.init().is_some()
                    && self
                        .parent(id)
                        .and_then(|id| self.parent(id))
                        .is_some_and(|p| !self.p.node(p).is_for_in_stmt());
                let need_check_widened_ty = match name {
                    ArrayPat(n) => !n
                        .elems
                        .iter()
                        .any(|ele| !matches!(ele.kind, ast::ArrayBindingElemKind::Omit(_))),
                    ObjectPat(n) => n.elems.is_empty(),
                    _ => unreachable!(),
                };
                if need_check_initializer || need_check_widened_ty {
                    let widened_ty = self.get_widened_ty_for_var_like_decl(decl);
                    if need_check_initializer {
                        let initializer_ty = self.check_expr_cached(decl.init().unwrap());
                        if self.config.strict_null_checks() && need_check_widened_ty {
                            self.check_non_null_non_void_ty(initializer_ty, id);
                        } else {
                            // checkTypeAssignableToAndOptionallyElaborate(
                            //     initializerType,
                            //     getWidenedTypeForVariableLikeDeclaration(node),
                            //     node,
                            //     node.initializer,
                            // );
                        }
                    }
                    if need_check_widened_ty {
                        if let ArrayPat(_) = name {
                            self.check_iterated_ty_or_element_ty(
                                IterationUse::DESTRUCTURING,
                                widened_ty,
                                self.undefined_ty,
                                Some(id),
                            );
                        } else if self.config.strict_null_checks() {
                            // TODO:
                            // self.check_non_null_non_void_ty(widened_ty, id);
                        }
                    }
                }
            }
        }

        if decl.init().is_some()
            && decl.is_param()
            && let Some(f) = self.node_query(id.module()).get_containing_fn(id)
            && self.p.node(f).fn_body().is_none()
        {
            let error =
                errors::AParameterInitializerIsOnlyAllowedInAFunctionOrConstructorImplementation {
                    span: self.p.node(id).span(),
                };
            self.push_error(Box::new(error));
        }
    }
}
