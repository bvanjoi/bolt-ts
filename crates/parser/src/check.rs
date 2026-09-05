use super::ParserState;
use super::ast;
use super::errors;
use super::parsing_ctx::ParseContext;

impl<'cx, const VARIANT: u8> ParserState<'cx, '_, VARIANT> {
    pub(super) fn check_export_default_error(&mut self, span: bolt_ts_span::Span) {
        if self.parse_context.contains(ParseContext::MODULE_BLOCK)
            && !self.node_context_flags.contains(ast::NodeFlags::AMBIENT)
        {
            let error = errors::ADefaultExportCanOnlyBeUsedInAnEcmascriptStyleModule { span };
            self.push_error(Box::new(error));
        } else if !self.parse_context.contains(ParseContext::TOP_LEVEL) {
            let error =
                errors::ADefaultExportMustBeAtTheTopLevelOfAFileOrModuleDeclaration { span };
            self.push_error(Box::new(error));
        }
    }

    pub(super) fn check_export_assignment_error(&mut self, span: bolt_ts_span::Span) {
        self.check_module_element_context(|this| {
            let error =
                errors::AnExportAssignmentMustBeAtTheTopLevelOfAFileOrModuleDeclaration { span };
            this.push_error(Box::new(error));
        });
    }

    pub(super) fn check_module_declaration_error(&mut self, span: bolt_ts_span::Span) {
        self.check_module_element_context(|this| {
            let error =
                errors::ANamespaceDeclarationIsOnlyAllowedAtTheTopLevelOfANamespaceOrModule {
                    span,
                };
            this.push_error(Box::new(error));
        });
    }

    pub(super) fn check_export_declaration_error(&mut self, span: bolt_ts_span::Span) {
        self.check_module_element_context(|this| {
            let error =
                errors::AnExportDeclarationCanOnlyBeUsedAtTheTopLevelOfANamespaceOrModule { span };
            this.push_error(Box::new(error));
        });
    }

    fn is_in_module_or_namespace(&self) -> bool {
        self.parse_context
            .intersects(ParseContext::TOP_LEVEL.union(ParseContext::MODULE_BLOCK))
    }

    pub(super) fn check_module_element_context(&mut self, push_error: impl FnOnce(&mut Self)) {
        if self.is_in_module_or_namespace() {
            return;
        }

        push_error(self);
    }

    pub(super) fn check_allow_block_declaration(&mut self, push_error: impl FnOnce(&mut Self)) {
        if self
            .parse_context
            .contains(ParseContext::DISALLOW_BLOCK_DECLARATION)
        {
            push_error(self);
        }
    }
}
