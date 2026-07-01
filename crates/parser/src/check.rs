use super::ParserState;
use super::ast;
use super::errors;
use super::parsing_ctx::ParseContext;

impl<'cx> ParserState<'cx, '_> {
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
        if self
            .parse_context
            .intersects(ParseContext::TOP_LEVEL.union(ParseContext::MODULE_BLOCK))
        {
            return;
        }

        let error =
            errors::AnExportAssignmentMustBeAtTheTopLevelOfAFileOrModuleDeclaration { span };
        self.push_error(Box::new(error));
    }

    pub(super) fn check_module_declaration_error(&mut self, span: bolt_ts_span::Span) {
        if self
            .parse_context
            .intersects(ParseContext::TOP_LEVEL.union(ParseContext::MODULE_BLOCK))
        {
            return;
        }

        let error =
            errors::ANamespaceDeclarationIsOnlyAllowedAtTheTopLevelOfANamespaceOrModule { span };
        self.push_error(Box::new(error));
    }

    pub(super) fn check_export_declaration_error(&mut self, span: bolt_ts_span::Span) {
        if self
            .parse_context
            .intersects(ParseContext::TOP_LEVEL.union(ParseContext::MODULE_BLOCK))
        {
            return;
        }

        let error =
            errors::AnExportDeclarationCanOnlyBeUsedAtTheTopLevelOfANamespaceOrModule { span };
        self.push_error(Box::new(error));
    }
}
