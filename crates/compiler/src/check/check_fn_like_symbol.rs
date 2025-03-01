use bolt_ts_ast as ast;
use bolt_ts_ast::ArrowFnExprBody;
use bolt_ts_span::Span;

use crate::bind::{SymbolFnKind, SymbolID};
use crate::{keyword, ty};

use super::TyChecker;
use super::check_type_related_to::TypeRelatedChecker;
use super::relation::{RelationKind, SigCheckMode};
use super::{Ternary, errors};

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_fn_like_symbol(&mut self, symbol: SymbolID) {
        let f = &self.binder.symbol(symbol).expect_fn();
        assert!(!f.decls.is_empty());
        assert_ne!(f.kind, SymbolFnKind::FnExpr);

        let mut has_overloads = false;
        let mut body_declaration = None;

        let mut last_seen_non_ambient_decl = None;
        for decl in &f.decls {
            let node = self.p.node(*decl);
            let is_ambient_context = node.node_flags().intersects(ast::NodeFlags::AMBIENT);
            let is_ambient_context_or_interface = self.p.parent(*decl).is_some_and(|parent| {
                let p = self.p.node(parent);
                p.is_interface_decl() || p.is_object_lit_ty()
            }) || is_ambient_context;

            if !is_ambient_context_or_interface {
                last_seen_non_ambient_decl = Some(*decl);
            }

            if node.is_fn_decl()
                || node.is_method_signature()
                || node.is_class_method_ele()
                || node.is_class_ctor()
            {
                if let Some(body) = node.fn_body() {
                    assert!(matches!(body, ArrowFnExprBody::Block(_)));
                    if body_declaration.is_none() {
                        body_declaration = Some(*decl);
                    }
                } else {
                    has_overloads = true;
                }
            }
        }

        if let Some(last_seen_non_ambient_decl) = last_seen_non_ambient_decl {
            let n = self.p.node(last_seen_non_ambient_decl);
            if n.fn_body().is_none()
                && !n.has_syntactic_modifier(ast::ModifierKind::Abstract.into())
            {
                if f.kind == SymbolFnKind::Ctor {
                    let node = self.p.node(f.decls[0]).expect_class_ctor();
                    let lo = node.span.lo;
                    let hi = lo + keyword::KW_CONSTRUCTOR_STR.len() as u32;
                    let span = Span::new(lo, hi, node.span.module);
                    let error = errors::ConstructorImplementationIsMissing { span };
                    self.push_error(Box::new(error));
                } else {
                    let span = self.p.node(f.decls[0]).ident_name().unwrap().span;
                    let error = errors::FunctionImplementationIsMissingOrNotImmediatelyFollowingTheDeclaration { span };
                    self.push_error(Box::new(error));
                }
            }
        }

        if has_overloads {
            if let Some(body_declaration) = body_declaration {
                let sigs = self.get_sigs_of_symbol(symbol);
                let body_sig = self.get_sig_from_decl(body_declaration);
                for sig in sigs {
                    if !self.is_implementation_compatible_with_overload(body_sig, sig) {
                        let error_node = sig.def_id();
                        let error = errors::ThisOverloadSignatureIsNotCompatibleWithItsImplementationSignature {
                            span: self.p.node(error_node).ident_name().unwrap().span,
                        };
                        self.push_error(Box::new(error));
                    }
                }
            }
        }
    }

    fn is_implementation_compatible_with_overload(
        &mut self,
        implementation: &'cx ty::Sig<'cx>,
        overload: &'cx ty::Sig<'cx>,
    ) -> bool {
        let erased_source = self.get_erased_sig(implementation);
        let erased_target = self.get_erased_sig(overload);

        let source_ret_ty = self.get_ret_ty_of_sig(erased_source);
        let target_ret_ty = self.get_ret_ty_of_sig(erased_target);
        if target_ret_ty == self.void_ty
            || self.is_type_related_to(target_ret_ty, source_ret_ty, RelationKind::Assignable)
            || self.is_type_related_to(source_ret_ty, target_ret_ty, RelationKind::Assignable)
        {
            self.is_sig_assignable_to(erased_source, erased_target, true)
        } else {
            false
        }
    }

    fn is_sig_assignable_to(
        &mut self,
        source: &'cx ty::Sig<'cx>,
        target: &'cx ty::Sig<'cx>,
        ignore_ret_ty: bool,
    ) -> bool {
        let check_mode = if ignore_ret_ty {
            SigCheckMode::IGNORE_RETURN_TYPES
        } else {
            SigCheckMode::empty()
        };
        let mut checker = TypeRelatedChecker::new(self, RelationKind::Assignable, None);
        checker.compare_sig_related(
            source,
            target,
            check_mode,
            false,
            |this, source, target, report_error| {
                this.c.check_type_assignable_to(source, target, None)
            },
        ) != Ternary::FALSE
    }
}
