use bolt_ts_ast::TokenKind;
use bolt_ts_ast::{self as ast};
use bolt_ts_ast_factory::ASTFactory;
use bolt_ts_span::Span;

use super::parsing_ctx::ParseContext;
use super::{PResult, ParserState, errors};

#[derive(Copy, Clone)]
pub(super) struct ParseBreak;
#[derive(Copy, Clone)]
pub(super) struct ParseContinue;

pub(super) trait ParseBreakOrContinue<'cx, 'p> {
    type Node;
    const IS_CONTINUE: bool;
    fn expect_token(&self) -> TokenKind;
    fn finish(
        &self,
        state: &mut ParserState<'cx, 'p>,
        span: Span,
        label: Option<&'cx ast::Ident>,
    ) -> Self::Node;
}

impl<'cx, 'p> ParseBreakOrContinue<'cx, 'p> for ParseBreak {
    type Node = &'cx ast::BreakStmt<'cx>;
    const IS_CONTINUE: bool = false;
    fn expect_token(&self) -> TokenKind {
        TokenKind::Break
    }
    fn finish(
        &self,
        state: &mut ParserState<'cx, 'p>,
        span: Span,
        label: Option<&'cx ast::Ident>,
    ) -> Self::Node {
        state.create_break_statement(span, label)
    }
}

impl<'cx, 'p> ParseBreakOrContinue<'cx, 'p> for ParseContinue {
    type Node = &'cx ast::ContinueStmt<'cx>;
    const IS_CONTINUE: bool = true;
    fn expect_token(&self) -> TokenKind {
        TokenKind::Continue
    }
    fn finish(
        &self,
        state: &mut ParserState<'cx, 'p>,
        span: Span,
        label: Option<&'cx ast::Ident>,
    ) -> Self::Node {
        state.create_continue_statement(span, label)
    }
}

impl<'cx, 'p> ParserState<'cx, 'p> {
    pub(super) fn parse_break_or_continue<Node, P: ParseBreakOrContinue<'cx, 'p, Node = Node>>(
        &mut self,
        kind: &P,
    ) -> PResult<Node> {
        let start = self.token.start();

        self.expect(kind.expect_token());

        let label = if self.can_parse_semi() {
            None
        } else {
            let label = self.create_ident(true, None);
            if !self.labels.contains(&label.name) {
                self.push_error(Box::new(errors::JumpTargetCannotCrossFunctionBoundary {
                    span: label.span,
                }));
            }
            Some(label)
        };

        self.parse_semi();

        let span = self.new_span(start);

        match P::IS_CONTINUE {
            true if !self.parse_context.contains(ParseContext::ALLOW_CONTINUE) => {
                self.push_error(Box::new(
                    errors::AContinueOrBreakStatementCanOnlyBeUsedWithinAnEnclosingIterationStatement {
                        span,
                        is_continue: true
                    },
                ));
            }
            false if !self.parse_context.contains(ParseContext::ALLOW_BREAK) => {
                self.push_error(Box::new(
                    errors::AContinueOrBreakStatementCanOnlyBeUsedWithinAnEnclosingIterationStatement {
                        span,
                        is_continue: false
                    },
            ));
            }
            _ => {}
        }

        Ok(kind.finish(self, span, label))
    }
}
