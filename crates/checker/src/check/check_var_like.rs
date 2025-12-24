use crate::check::errors;
use crate::check::symbol_info::SymbolInfo;

use super::TyChecker;

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
                let init_ty = self.check_expr_with_cache(init);
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
        match decl.name() {
            Ident(name) => self.check_non_pat_var_like_decl(name.id, decl.id(), decl),
            StringLit { raw, .. } => self.check_non_pat_var_like_decl(raw.id, decl.id(), decl),
            NumLit(num) => self.check_non_pat_var_like_decl(num.id, decl.id(), decl),
            ArrayPat(_) | ObjectPat(_) | Computed(_) => {
                // todo
            }
        }

        if decl.init().is_some()
            && decl.is_param()
            && let id = decl.id()
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
