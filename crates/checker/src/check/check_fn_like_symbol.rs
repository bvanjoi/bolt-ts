use bolt_ts_ast as ast;
use bolt_ts_ast::ArrowFnExprBody;
use bolt_ts_ast::ModifierKind;
use bolt_ts_ast::keyword;
use bolt_ts_binder::{SymbolFlags, SymbolID};
use bolt_ts_span::Span;

use super::TyChecker;
use super::check_type_related_to::TypeRelatedChecker;
use super::relation::{RelationKind, SigCheckMode};
use super::symbol_info::SymbolInfo;
use super::ty;
use super::{Ternary, errors};

const FLAGS_TO_CHECK: enumflags2::BitFlags<ast::ModifierKind> =
    enumflags2::make_bitflags!(ModifierKind::{Export | Ambient | Private | Protected | Abstract});

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_fn_like_symbol(&mut self, symbol: SymbolID) {
        let s = self.binder.symbol(symbol);
        let decls = s.decls.as_ref().unwrap();
        assert!(!decls.is_empty());

        let mut has_overloads = false;
        let mut body_declaration = None;
        let mut some_node_flags = enumflags2::BitFlags::empty();
        let mut all_node_flags = FLAGS_TO_CHECK;
        let is_ctor = s.flags.intersects(SymbolFlags::CONSTRUCTOR);

        let mut last_seen_non_ambient_decl = None;
        let mut multiple_constructor_implement = false;
        let mut duplicate_function_declaration = false;
        let mut fn_decls = vec![];

        for decl in decls {
            let node = self.p.node(*decl);
            let is_ambient_context = self.p.node_flags(*decl).intersects(ast::NodeFlags::AMBIENT);
            let is_ambient_context_or_interface = self.parent(*decl).is_some_and(|parent| {
                let p = self.p.node(parent);
                p.is_interface_decl() || p.is_object_lit_ty()
            }) || is_ambient_context;

            if node.is_fn_decl()
                || node.is_method_signature()
                || node.is_class_method_elem()
                || node.is_class_ctor()
            {
                fn_decls.push(*decl);
                let current_node_flags =
                    self.get_effective_declaration_flags(*decl, FLAGS_TO_CHECK);
                some_node_flags |= current_node_flags;
                all_node_flags &= current_node_flags;
                if let Some(body) = node.fn_body() {
                    debug_assert!(matches!(body, ArrowFnExprBody::Block(_)));
                    if body_declaration.is_none() {
                        body_declaration = Some(*decl);
                    } else if is_ctor {
                        multiple_constructor_implement = true;
                    } else {
                        duplicate_function_declaration = true;
                    }
                } else {
                    has_overloads = true;
                }

                if !is_ambient_context_or_interface {
                    last_seen_non_ambient_decl = Some(*decl);
                }
            }
        }

        if multiple_constructor_implement {
            let errors = fn_decls
                .iter()
                .map(|&d| self.p.node(d).as_class_ctor().unwrap().name_span)
                .map(|span| errors::MultipleConstructorImplementationsAreNotAllowed { span })
                .collect::<Vec<_>>();
            for error in errors {
                self.diags.push(bolt_ts_errors::Diag {
                    inner: Box::new(error),
                });
            }
        }

        if duplicate_function_declaration {
            let errors = fn_decls
                .iter()
                .map(|&d| {
                    let decl = self.p.node(d);
                    errors::DuplicateFunctionImplementation { span: decl.span() }
                })
                .collect::<Vec<_>>();
            for error in errors {
                self.diags.push(bolt_ts_errors::Diag {
                    inner: Box::new(error),
                });
            }
        }

        if let Some(last_seen_non_ambient_decl) = last_seen_non_ambient_decl
            && let n = self.p.node(last_seen_non_ambient_decl)
            && n.fn_body().is_none()
            && !n.has_syntactic_modifier(ast::ModifierKind::Abstract.into())
        {
            if s.flags.intersects(SymbolFlags::CONSTRUCTOR) {
                let node = self.p.node(decls[0]).expect_class_ctor();
                let lo = node.span.lo();
                let hi = lo + keyword::KW_CONSTRUCTOR_STR.len() as u32;
                let span = Span::new(lo, hi, node.span.module());
                let error = errors::ConstructorImplementationIsMissing { span };
                self.diags.push(bolt_ts_errors::Diag {
                    inner: Box::new(error),
                });
            } else {
                let n = self.p.node(decls[0]);
                let span = n.name().unwrap().span();
                let error = errors::FunctionImplementationIsMissingOrNotImmediatelyFollowingTheDeclaration { span };
                self.diags.push(bolt_ts_errors::Diag {
                    inner: Box::new(error),
                });
            }
        }

        if has_overloads {
            self.check_flags_agreement_between_overloads(
                symbol,
                body_declaration,
                some_node_flags,
                all_node_flags,
            );
            if let Some(body_declaration) = body_declaration {
                let sigs = self.get_sigs_of_symbol(symbol);
                let body_sig = self.get_sig_from_decl(body_declaration);
                if let Some(first_error_sig) = sigs
                    .iter()
                    .find(|&sig| !self.is_implementation_compatible_with_overload(body_sig, sig))
                {
                    let error_node = first_error_sig.def_id();
                    let error = errors::ThisOverloadSignatureIsNotCompatibleWithItsImplementationSignature {
                            span: self.p.node(error_node).ident_name().unwrap().span,
                        };
                    self.push_error(Box::new(error));
                }
            }
        }
    }

    fn get_canonical_overload(
        &self,
        overloads: &[ast::NodeID],
        implementation: Option<ast::NodeID>,
    ) -> ast::NodeID {
        if let Some(implementation) = implementation
            && self.parent(implementation) == self.parent(overloads[0])
        {
            implementation
        } else {
            overloads[0]
        }
    }

    fn check_flags_agreement_between_overloads(
        &mut self,
        symbol: SymbolID,
        implementation: Option<ast::NodeID>,
        some_overload_flags: enumflags2::BitFlags<ModifierKind>,
        allow_overload_flags: enumflags2::BitFlags<ModifierKind>,
    ) {
        let s = self.binder.symbol(symbol);
        let decls = s.decls.as_ref().unwrap();
        assert!(!decls.is_empty());
        if !(some_overload_flags ^ allow_overload_flags).is_empty() {
            let n = self.get_canonical_overload(decls, implementation);
            let cannoical_flags = self.get_effective_declaration_flags(n, FLAGS_TO_CHECK);
            // TODO: overloads_in_different_files

            for o in decls.clone() {
                let flags = self.get_effective_declaration_flags(o, FLAGS_TO_CHECK);
                let deviation_in_file = flags ^ cannoical_flags;
                if deviation_in_file.contains(ast::ModifierKind::Export) {
                } else if deviation_in_file.contains(ast::ModifierKind::Ambient) {
                    let span = self
                        .node_query(o.module())
                        .get_name_of_decl(o)
                        .unwrap()
                        .span();
                    let error = errors::OverloadSignaturesMustAllBeAmbientOrNonAmbient { span };
                    self.push_error(Box::new(error));
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
                if this.c.check_type_assignable_to(source, target, None) {
                    Ternary::TRUE
                } else {
                    Ternary::FALSE
                }
            },
        ) != Ternary::FALSE
    }
}
