use bolt_ts_ast as ast;
use bolt_ts_ast::keyword;
use bolt_ts_binder_errors as errors;
use bolt_ts_config::Target;

use super::BinderState;

impl<'cx, 'atoms, 'parser> BinderState<'cx, 'atoms, 'parser> {
    pub(super) fn check_strict_mode_eval_or_arguments(
        &mut self,
        context_node: ast::NodeID,
        n: &ast::Ident,
    ) {
        debug_assert!(self.in_strict_mode);
        if matches!(n.name, keyword::IDENT_ARGUMENTS | keyword::IDENT_EVAL) {
            if self
                .node_query()
                .get_containing_class(context_node)
                .is_some()
            {
                let error = errors::CodeContainedInAClassIsEvaluatedInJavaScriptSStrictModeWhichDoesNotAllowThisUseOf0ForMoreInformationSeeHttpsColonSlashSlashdeveloperMozillaOrgSlashenUsSlashdocsSlashWebSlashJavaScriptSlashReferenceSlashStrictMode {
                    span: n.span,
                    name: self.atoms.get(n.name).to_string(),
                };
                self.push_error(Box::new(error));
            } else if self.p.external_module_indicator.is_some() {
                let error = errors::InvalidUseOfXModulesAreAutomaticallyInStrictMode {
                    name: self.atoms.get(n.name).to_string(),
                    span: n.span,
                };
                self.push_error(Box::new(error));
            } else {
                let error = errors::InvalidUseOfXInStrictMode {
                    name: self.atoms.get(n.name).to_string(),
                    span: n.span,
                };
                self.push_error(Box::new(error));
            }
        }
    }

    pub(super) fn check_strict_mode_function_declaration(&mut self, n: &'cx ast::FnDecl<'cx>) {
        debug_assert!(self.in_strict_mode);
        if *self.compiler_options.compiler_options().target() >= Target::ES2015 {
            return;
        }
        let c = self.block_scope_container.unwrap();
        let c = self.p.node(c);
        use ast::Node::*;
        if matches!(c, Program(_) | BlockModuleDecl(_) | NestedModuleDecl(_))
            || c.is_fn_like_or_class_static_block_decl()
        {
            return;
        }
        if self.node_query().get_containing_class(n.id).is_some() {
            let error = errors::FunctionDeclarationsAreNotAllowedInsideBlocksInStrictModeWhenTargetingEs5ClassDefinitionsAreAutomaticallyInStrictMode {
              span: n.name.unwrap().span
            };
            self.push_error(Box::new(error));
        } else if self.p.external_module_indicator.is_some() {
            let error = errors::FunctionDeclarationsAreNotAllowedInsideBlocksInStrictModeWhenTargetingEs5ModulesAreAutomaticallyInStrictMode {
              span: n.name.unwrap().span
            };
            self.push_error(Box::new(error));
        } else {
            let error =
                errors::FunctionDeclarationsAreNotAllowedInsideBlocksInStrictModeWhenTargetingEs5 {
                    span: n.name.unwrap().span,
                };
            self.push_error(Box::new(error));
        }
    }
}
