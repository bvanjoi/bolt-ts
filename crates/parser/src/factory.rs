use bolt_ts_ast::{self as ast};
use bolt_ts_atom::AtomId;
use bolt_ts_span::Span;

use super::{
    ParserState, errors,
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
        let flags = if question_dot.is_some() {
            ast::NodeFlags::OPTIONAL_CHAIN
        } else {
            ast::NodeFlags::empty()
        };
        self.node_flags_map.insert(id, flags);
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
    ) -> &'cx ast::JsxOpeningElem<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxOpeningElem {
            id,
            span,
            tag_name,
            ty_args,
            attrs,
        });
        self.nodes.insert(id, ast::Node::JsxOpeningElem(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_jsx_closing_ele(
        &mut self,
        span: Span,
        tag_name: ast::JsxTagName<'cx>,
    ) -> &'cx ast::JsxClosingElem<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxClosingElem { id, span, tag_name });
        self.nodes.insert(id, ast::Node::JsxClosingElem(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_jsx_self_closing_ele(
        &mut self,
        span: Span,
        tag_name: ast::JsxTagName<'cx>,
        ty_args: Option<&'cx ast::Tys<'cx>>,
        attrs: ast::JsxAttrs<'cx>,
    ) -> &'cx ast::JsxSelfClosingElem<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxSelfClosingElem {
            id,
            span,
            tag_name,
            ty_args,
            attrs,
        });
        self.nodes.insert(id, ast::Node::JsxSelfClosingElem(node));
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
            opening_frag: opening,
            children,
            closing_frag: closing,
        });
        self.nodes.insert(id, ast::Node::JsxFrag(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_jsx_ele(
        &mut self,
        span: Span,
        opening: &'cx ast::JsxOpeningElem<'cx>,
        children: &'cx [ast::JsxChild<'cx>],
        closing: &'cx ast::JsxClosingElem<'cx>,
    ) -> &'cx ast::JsxElem<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxElem {
            id,
            span,
            opening_elem: opening,
            children,
            closing_elem: closing,
        });
        self.nodes.insert(id, ast::Node::JsxElem(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_tagged_template_expr(
        &mut self,
        start: u32,
        tag: &'cx ast::Expr<'cx>,
        ty_args: Option<&'cx ast::Tys<'cx>>,
        tpl: &'cx ast::Expr<'cx>,
        question_dot: Option<Span>,
    ) -> &'cx ast::TaggedTemplateExpr<'cx> {
        let id = self.next_node_id();
        let tagged_template = self.alloc(ast::TaggedTemplateExpr {
            id,
            span: self.new_span(start),
            tag,
            tpl,
            ty_args,
        });
        self.nodes
            .insert(id, ast::Node::TaggedTemplateExpr(tagged_template));
        let flags = if question_dot.is_some()
            || self
                .node_flags_map
                .get(tag.id())
                .contains(ast::NodeFlags::OPTIONAL_CHAIN)
        {
            self.push_error(Box::new(
                errors::TaggedTemplateExpressionsAreNotPermittedInAnOptionalChain {
                    span: tpl.span(),
                },
            ));

            ast::NodeFlags::OPTIONAL_CHAIN
        } else {
            ast::NodeFlags::empty()
        };
        self.node_flags_map.insert(id, flags);
        tagged_template
    }

    pub fn create_binary_expr(
        &mut self,
        start: u32,
        left: &'cx ast::Expr<'cx>,
        op: ast::BinOp,
        right: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::BinExpr<'cx> {
        let id = self.next_node_id();
        let span = self.new_span(start);
        let node = self.alloc(ast::BinExpr {
            id,
            span,
            left,
            op,
            right,
        });
        self.nodes.insert(id, ast::Node::BinExpr(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_class_ctor(
        &mut self,
        start: u32,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ret: Option<&'cx ast::Ty<'cx>>,
        body: Option<&'cx ast::BlockStmt<'cx>>,
    ) -> &'cx ast::ClassCtor<'cx> {
        let id = self.next_node_id();
        let span = self.new_span(start);
        let ctor = self.alloc(ast::ClassCtor {
            id,
            span,
            ty_params,
            params,
            ret,
            body,
        });
        self.nodes.insert(id, ast::Node::ClassCtor(ctor));
        self.node_flags_map.insert(id, self.context_flags);
        ctor
    }

    pub fn create_class_prop_elem(
        &mut self,
        start: u32,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: &'cx ast::PropName<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
        init: Option<&'cx ast::Expr<'cx>>,
        excl: Option<ast::Token>,
    ) -> &'cx bolt_ts_ast::ClassPropElem<'cx> {
        let id = self.next_node_id();
        let prop = self.alloc(ast::ClassPropElem {
            id,
            span: self.new_span(start),
            modifiers,
            name,
            ty,
            init,
            question: None,
            excl: excl.map(|e| e.span),
        });
        self.nodes.insert(id, ast::Node::ClassPropElem(prop));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        prop
    }

    pub fn create_computed_prop_name(
        &mut self,
        start: u32,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::ComputedPropName<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ComputedPropName {
            id,
            span: self.new_span(start),
            expr,
        });
        self.nodes.insert(id, ast::Node::ComputedPropName(node));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        node
    }

    pub fn create_class_static_block_decl(
        &mut self,
        start: u32,
        body: &'cx ast::BlockStmt<'cx>,
    ) -> &'cx ast::ClassStaticBlock<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ClassStaticBlock {
            id,
            span: self.new_span(start),
            body,
        });
        self.nodes.insert(id, ast::Node::ClassStaticBlock(node));
        self.node_flags_map
            .insert(id, ast::NodeFlags::CLASS_STATIC_BLOCK);
        node
    }
}
