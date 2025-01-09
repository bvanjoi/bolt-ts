use bolt_ts_span::Span;

use crate::ast;

use super::{token::TokenKind, PResult, ParserState};

#[derive(Copy, Clone)]
pub(super) struct ParseBreak;
#[derive(Copy, Clone)]
pub(super) struct ParseContinue;

pub(super) trait ParseBreakOrContinue<'cx, 'p> {
    type Node;
    fn expect_token(&self) -> TokenKind;
    fn finish(
        &self,
        state: &mut ParserState<'cx, 'p>,
        id: ast::NodeID,
        span: Span,
        label: Option<&'cx ast::Ident>,
    ) -> Self::Node;
}

impl<'cx, 'p> ParseBreakOrContinue<'cx, 'p> for ParseBreak {
    type Node = &'cx ast::BreakStmt<'cx>;
    fn expect_token(&self) -> TokenKind {
        TokenKind::Break
    }
    fn finish(
        &self,
        state: &mut ParserState<'cx, 'p>,
        id: ast::NodeID,
        span: Span,
        label: Option<&'cx ast::Ident>,
    ) -> Self::Node {
        let stmt = state.alloc(ast::BreakStmt { id, span, label });
        state.insert_map(stmt.id, ast::Node::BreakStmt(stmt));
        stmt
    }
}

impl<'cx, 'p> ParseBreakOrContinue<'cx, 'p> for ParseContinue {
    type Node = &'cx ast::ContinueStmt<'cx>;
    fn expect_token(&self) -> TokenKind {
        TokenKind::Continue
    }
    fn finish(
        &self,
        state: &mut ParserState<'cx, 'p>,
        id: ast::NodeID,
        span: Span,
        label: Option<&'cx ast::Ident>,
    ) -> Self::Node {
        let stmt = state.alloc(ast::ContinueStmt { id, span, label });
        state.insert_map(stmt.id, ast::Node::ContinueStmt(stmt));
        stmt
    }
}

impl<'cx, 'p> ParserState<'cx, 'p> {
    pub(super) fn parse_break_or_continue<Node>(
        &mut self,
        kind: &impl ParseBreakOrContinue<'cx, 'p, Node = Node>,
    ) -> PResult<Node> {
        let id = self.next_node_id();
        let start = self.token.start();

        self.expect(kind.expect_token())?;

        let label = if self.can_parse_semi() {
            None
        } else {
            Some(self.create_ident(true, None))
        };
        self.parse_semi();
        let span = self.new_span(start);
        Ok(kind.finish(self, id, span, label))
    }
}
