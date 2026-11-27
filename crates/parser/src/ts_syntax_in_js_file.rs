use bolt_ts_ast::TokenKind;
use bolt_ts_span::Span;

use super::ParserState;
use super::const_variant::is_js_like_variant;
use super::{ast, errors};

impl<'cx, const VARIANT: u8> ParserState<'cx, '_, VARIANT> {
    fn mark_syntax_error_in_javascript(&mut self, f: impl FnOnce(&mut Self)) {
        if !is_js_like_variant(VARIANT) {
            return;
        }
        std::hint::cold_path();
        f(self);
    }

    pub(super) fn mark_error_for_invalid_token_in_javascript(
        &mut self,
        t: Option<(Span, TokenKind)>,
    ) {
        self.mark_syntax_error_in_javascript(|this| {
            if let Some((span, modifier)) = t {
                let error = errors::TheXModifierCanOnlyBeUsedInTypeScriptFiles {
                    span,
                    modifier: modifier.as_str(),
                };
                this.push_error(Box::new(error));
            }
        });
    }

    pub(super) fn mark_error_for_invalid_modifier_in_javascript(
        &mut self,
        t: Option<(Span, ast::ModifierKind)>,
    ) {
        self.mark_syntax_error_in_javascript(|this| {
            if let Some((span, modifier)) = t
                && !matches!(modifier, ast::ModifierKind::Accessor)
            {
                let error = errors::TheXModifierCanOnlyBeUsedInTypeScriptFiles {
                    span,
                    modifier: modifier.as_str(),
                };
                this.push_error(Box::new(error));
            }
        });
    }

    pub(super) fn mark_error_for_return_type_in_javascript(&mut self, ty: &'cx ast::Ty<'cx>) {
        self.mark_syntax_error_in_javascript(|this| {
            let error = errors::TypeAnnotationsCanOnlyBeUsedInTypeScriptFiles { span: ty.span() };
            this.push_error(Box::new(error));
        });
    }
}
