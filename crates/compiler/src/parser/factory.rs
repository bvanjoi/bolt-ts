use bolt_ts_ast::{self as ast, TokenKind};
use bolt_ts_atom::AtomId;
use bolt_ts_span::Span;

use super::ParserState;

impl<'cx> ParserState<'cx, '_> {
    pub fn create_numeric_literal(&mut self, val: f64, span: Span) -> &'cx ast::NumLit {
        let id = self.next_node_id();
        let n = self.alloc(ast::NumLit { id, val, span });
        self.nodes.insert(id, ast::Node::NumLit(n));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        n
    }

    pub fn create_jsx_text(
        &mut self,
        text: AtomId,
        span: Span,
        contains_only_trivia_whitespace: bool,
    ) -> &'cx ast::JsxText {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxText {
            id,
            text,
            span,
            contains_only_trivia_whitespace,
        });
        self.nodes.insert(id, ast::Node::JsxText(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_jsx_jsx_closing_fragment(&mut self, span: Span) -> &'cx ast::JsxClosingFrag {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxClosingFrag { id, span });
        self.nodes.insert(id, ast::Node::JsxClosingFrag(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_jsx_named_attr(
        &mut self,
        name: ast::JsxAttrName<'cx>,
        init: Option<ast::JsxAttrValue<'cx>>,
        span: Span,
    ) -> &'cx ast::JsxNamedAttr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxNamedAttr {
            id,
            span,
            name,
            init,
        });
        self.nodes.insert(id, ast::Node::JsxNamedAttr(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_jsx_spread_attr(
        &mut self,
        expr: &'cx ast::Expr,
        span: Span,
    ) -> &'cx ast::JsxSpreadAttr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxSpreadAttr { id, expr, span });
        self.nodes.insert(id, ast::Node::JsxSpreadAttr(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_jsx_ns_name(
        &mut self,
        ns: &'cx ast::Ident,
        name: &'cx ast::Ident,
        span: Span,
    ) -> &'cx ast::JsxNsName<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxNsName { id, span, ns, name });
        self.nodes.insert(id, ast::Node::JsxNsName(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_this_expr(&mut self, span: Span) -> &'cx ast::ThisExpr {
        let id = self.next_node_id();
        let this = self.alloc(ast::ThisExpr { id, span });
        self.nodes.insert(this.id, ast::Node::ThisExpr(this));
        self.node_flags_map.insert(this.id, ast::NodeFlags::empty());
        this
    }

    pub fn create_jsx_expr(
        &mut self,
        dotdotdot_token: Option<Span>,
        expr: Option<&'cx ast::Expr<'cx>>,
        span: Span,
    ) -> &'cx ast::JsxExpr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxExpr {
            id,
            span,
            dotdotdot_token,
            expr,
        });
        self.nodes.insert(id, ast::Node::JsxExpr(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_jsx_attrs(&mut self, attrs: &'cx [ast::JsxAttr<'cx>]) -> ast::JsxAttrs<'cx> {
        attrs
    }
}
