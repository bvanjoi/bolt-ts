use bolt_ts_ast::{self as ast};
use bolt_ts_atom::AtomId;
use bolt_ts_span::Span;

use super::{
    ParserState,
    paren_rule::{NoParenRule, ParenRuleTrait},
};

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

    fn create_base_prop_access_expr(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        question_dot: Option<Span>,
        name: &'cx ast::Ident,
    ) -> &'cx ast::PropAccessExpr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::PropAccessExpr {
            id,
            span,
            expr,
            question_dot,
            name,
        });
        self.nodes.insert(id, ast::Node::PropAccessExpr(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_prop_access_expr(
        &mut self,
        start: u32,
        expr: &'cx ast::Expr<'cx>,
        name: &'cx ast::Ident,
    ) -> &'cx ast::PropAccessExpr<'cx> {
        let expr = NoParenRule.paren_left_side_of_access(expr, false);
        let span = self.new_span(start);
        self.create_base_prop_access_expr(span, expr, None, name)
    }

    pub fn create_prop_access_chain(
        &mut self,
        start: u32,
        expr: &'cx ast::Expr<'cx>,
        question_dot: Option<Span>,
        name: &'cx ast::Ident,
    ) -> &'cx ast::PropAccessExpr<'cx> {
        let expr = NoParenRule.paren_left_side_of_access(expr, true);
        let span = self.new_span(start);
        self.create_base_prop_access_expr(span, expr, question_dot, name)
    }

    pub fn create_jsx_opening_frag(&mut self, span: Span) -> &'cx ast::JsxOpeningFrag {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxOpeningFrag { id, span });
        self.nodes.insert(id, ast::Node::JsxOpeningFrag(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_jsx_opening_ele(
        &mut self,
        span: Span,
        tag_name: ast::JsxTagName<'cx>,
        ty_args: Option<&'cx ast::Tys<'cx>>,
        attrs: ast::JsxAttrs<'cx>,
    ) -> &'cx ast::JsxOpeningEle<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxOpeningEle {
            id,
            span,
            tag_name,
            ty_args,
            attrs,
        });
        self.nodes.insert(id, ast::Node::JsxOpeningEle(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_jsx_closing_ele(
        &mut self,
        span: Span,
        tag_name: ast::JsxTagName<'cx>,
    ) -> &'cx ast::JsxClosingEle<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxClosingEle { id, span, tag_name });
        self.nodes.insert(id, ast::Node::JsxClosingEle(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_jsx_self_closing_ele(
        &mut self,
        span: Span,
        tag_name: ast::JsxTagName<'cx>,
        ty_args: Option<&'cx ast::Tys<'cx>>,
        attrs: ast::JsxAttrs<'cx>,
    ) -> &'cx ast::JsxSelfClosingEle<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxSelfClosingEle {
            id,
            span,
            tag_name,
            ty_args,
            attrs,
        });
        self.nodes.insert(id, ast::Node::JsxSelfClosingEle(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_jsx_frag(
        &mut self,
        span: Span,
        opening: &'cx ast::JsxOpeningFrag,
        children: &'cx [ast::JsxChild<'cx>],
        closing: &'cx ast::JsxClosingFrag,
    ) -> &'cx ast::JsxFrag<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxFrag {
            id,
            span,
            opening_ele: opening,
            children,
            closing_ele: closing,
        });
        self.nodes.insert(id, ast::Node::JsxFrag(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_jsx_ele(
        &mut self,
        span: Span,
        opening: &'cx ast::JsxOpeningEle<'cx>,
        children: &'cx [ast::JsxChild<'cx>],
        closing: &'cx ast::JsxClosingEle<'cx>,
    ) -> &'cx ast::JsxEle<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxEle {
            id,
            span,
            opening_ele: opening,
            children,
            closing_ele: closing,
        });
        self.nodes.insert(id, ast::Node::JsxEle(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }
}
