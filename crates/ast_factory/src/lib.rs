use bolt_ts_ast as ast;
use bolt_ts_ast::NodeID;
use bolt_ts_ast::r#trait::NoParenRule;
use bolt_ts_ast::r#trait::ParenRuleTrait;
use bolt_ts_atom::Atom;
use bolt_ts_span::Span;

pub trait ASTFactory<'cx> {
    fn next_node_id(&mut self) -> NodeID;
    fn insert_node(&mut self, node_id: NodeID, node: ast::Node<'cx>);
    fn insert_node_flags(&mut self, node_id: NodeID, flags: ast::NodeFlags);
    fn alloc<T>(&self, val: T) -> &'cx T;
    fn node_context_flags(&self) -> ast::NodeFlags;
    fn set_external_module_indicator(&mut self, node_id: NodeID);
    fn set_external_module_indicator_if_has_export_modifier(
        &mut self,
        node: NodeID,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) {
        if modifiers.is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::EXPORT)) {
            self.set_external_module_indicator(node);
        }
    }

    #[inline(always)]
    fn create_void_expression(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::VoidExpr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::VoidExpr { id, span, expr });
        self.insert_node(id, ast::Node::VoidExpr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_numeric_literal(&mut self, val: f64, span: Span) -> &'cx ast::NumLit {
        let id = self.next_node_id();
        let node = self.alloc(ast::NumLit { id, val, span });
        self.insert_node(id, ast::Node::NumLit(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_jsx_text(
        &mut self,
        text: Atom,
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
        self.insert_node(id, ast::Node::JsxText(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_jsx_closing_fragment(&mut self, span: Span) -> &'cx ast::JsxClosingFrag {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxClosingFrag { id, span });
        self.insert_node(id, ast::Node::JsxClosingFrag(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_jsx_named_attribute(
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
        self.insert_node(id, ast::Node::JsxNamedAttr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_jsx_spread_attribute(
        &mut self,
        expr: &'cx ast::Expr,
        span: Span,
    ) -> &'cx ast::JsxSpreadAttr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxSpreadAttr { id, expr, span });
        self.insert_node(id, ast::Node::JsxSpreadAttr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_jsx_namespace_name(
        &mut self,
        ns: &'cx ast::Ident,
        name: &'cx ast::Ident,
        span: Span,
    ) -> &'cx ast::JsxNsName<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxNsName { id, span, ns, name });
        self.insert_node(id, ast::Node::JsxNsName(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_this_expression(&mut self, span: Span) -> &'cx ast::ThisExpr {
        let id = self.next_node_id();
        let this = self.alloc(ast::ThisExpr { id, span });
        self.insert_node(this.id, ast::Node::ThisExpr(this));
        self.insert_node_flags(this.id, self.node_context_flags());
        this
    }

    #[inline(always)]
    fn create_jsx_expression(
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
        self.insert_node(id, ast::Node::JsxExpr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_base_property_access_expression(
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
        self.insert_node(id, ast::Node::PropAccessExpr(node));
        let flags = if question_dot.is_some() {
            ast::NodeFlags::OPTIONAL_CHAIN
        } else {
            self.node_context_flags()
        };
        self.insert_node_flags(id, flags);
        node
    }

    #[inline(always)]
    fn create_property_access_expression(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        name: &'cx ast::Ident,
    ) -> &'cx ast::PropAccessExpr<'cx> {
        let expr = NoParenRule.paren_left_side_of_access(expr, false);
        self.create_base_property_access_expression(span, expr, None, name)
    }

    #[inline(always)]
    fn create_property_access_chain(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        question_dot: Option<Span>,
        name: &'cx ast::Ident,
    ) -> &'cx ast::PropAccessExpr<'cx> {
        let expr = NoParenRule.paren_left_side_of_access(expr, true);
        self.create_base_property_access_expression(span, expr, question_dot, name)
    }

    #[inline(always)]
    fn create_jsx_opening_fragment(&mut self, span: Span) -> &'cx ast::JsxOpeningFrag {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxOpeningFrag { id, span });
        self.insert_node(id, ast::Node::JsxOpeningFrag(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_jsx_opening_element(
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
        self.insert_node(id, ast::Node::JsxOpeningElem(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_jsx_closing_element(
        &mut self,
        span: Span,
        tag_name: ast::JsxTagName<'cx>,
    ) -> &'cx ast::JsxClosingElem<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxClosingElem { id, span, tag_name });
        self.insert_node(id, ast::Node::JsxClosingElem(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_jsx_self_closing_element(
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
        self.insert_node(id, ast::Node::JsxSelfClosingElem(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_jsx_fragment(
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
        self.insert_node(id, ast::Node::JsxFrag(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_jsx_element(
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
        self.insert_node(id, ast::Node::JsxElem(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_tagged_template_expression(
        &mut self,
        span: Span,
        tag: &'cx ast::Expr<'cx>,
        ty_args: Option<&'cx ast::Tys<'cx>>,
        tpl: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::TaggedTemplateExpr<'cx> {
        let id = self.next_node_id();
        let tagged_template = self.alloc(ast::TaggedTemplateExpr {
            id,
            span,
            tag,
            tpl,
            ty_args,
        });
        self.insert_node(id, ast::Node::TaggedTemplateExpr(tagged_template));
        self.insert_node_flags(id, self.node_context_flags());
        tagged_template
    }

    #[inline(always)]
    fn create_binary_expression(
        &mut self,
        span: Span,
        left: &'cx ast::Expr<'cx>,
        op: ast::BinOp,
        right: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::BinExpr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::BinExpr {
            id,
            span,
            left,
            op,
            right,
        });
        self.insert_node(id, ast::Node::BinExpr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    #[allow(clippy::too_many_arguments)]
    fn create_class_constructor(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name_span: Span,
        params: ast::ParamsDecl<'cx>,
        ret: Option<&'cx ast::Ty<'cx>>,
        body: Option<&'cx ast::BlockStmt<'cx>>,
    ) -> &'cx ast::ClassCtor<'cx> {
        let id = self.next_node_id();
        let ctor = self.alloc(ast::ClassCtor {
            id,
            span,
            modifiers,
            name_span,
            params,
            ret,
            body,
        });
        self.insert_node(id, ast::Node::ClassCtor(ctor));
        self.insert_node_flags(id, self.node_context_flags());
        ctor
    }

    #[inline(always)]
    #[allow(clippy::too_many_arguments)]
    fn create_class_property_element(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: &'cx ast::PropName<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
        init: Option<&'cx ast::Expr<'cx>>,
        excl: Option<ast::Token>,
        question: Option<Span>,
    ) -> &'cx bolt_ts_ast::ClassPropElem<'cx> {
        let id = self.next_node_id();
        let prop = self.alloc(ast::ClassPropElem {
            id,
            span,
            modifiers,
            name,
            ty,
            init,
            question,
            excl: excl.map(|e| e.span),
        });
        self.insert_node(id, ast::Node::ClassPropElem(prop));
        let mut node_flags = self.node_context_flags();
        if modifiers.is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT)) {
            node_flags |= ast::NodeFlags::AMBIENT;
        }
        self.insert_node_flags(id, node_flags);
        prop
    }

    #[inline(always)]
    fn create_computed_prop_name(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::ComputedPropName<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ComputedPropName { id, span, expr });
        self.insert_node(id, ast::Node::ComputedPropName(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_class_static_block_declaration(
        &mut self,
        span: Span,
        body: &'cx ast::BlockStmt<'cx>,
    ) -> &'cx ast::ClassStaticBlockDecl<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ClassStaticBlockDecl { id, span, body });
        self.insert_node(id, ast::Node::ClassStaticBlockDecl(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_method_signature(
        &mut self,
        span: Span,
        name: &'cx ast::PropName<'cx>,
        question: Option<Span>,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
    ) -> &'cx ast::MethodSignature<'cx> {
        let id = self.next_node_id();
        let sig = self.alloc(ast::MethodSignature {
            id,
            span,
            name,
            question,
            ty_params,
            params,
            ty,
        });
        self.insert_node(id, ast::Node::MethodSignature(sig));
        self.insert_node_flags(id, self.node_context_flags());
        sig
    }

    #[inline(always)]
    #[allow(clippy::too_many_arguments)]
    fn create_object_property_assignment(
        &mut self,
        span: Span,
        name: &'cx ast::PropName<'cx>,
        init: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::ObjectPropAssignment<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ObjectPropAssignment {
            id,
            span,
            name,
            init,
        });
        self.insert_node(id, ast::Node::ObjectPropAssignment(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    #[allow(clippy::too_many_arguments)]
    fn create_object_method_member(
        &mut self,
        span: Span,
        asterisk: Option<Span>,
        name: &'cx ast::PropName<'cx>,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
        body: &'cx ast::BlockStmt<'cx>,
    ) -> &'cx ast::ObjectMethodMember<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ObjectMethodMember {
            id,
            span,
            asterisk,
            name,
            ty_params,
            params,
            ty,
            body,
        });
        self.insert_node(id, ast::Node::ObjectMethodMember(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    #[allow(clippy::too_many_arguments)]
    fn create_class_method_element(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        asterisk: Option<Span>,
        name: &'cx ast::PropName<'cx>,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
        body: Option<&'cx ast::BlockStmt<'cx>>,
    ) -> &'cx ast::ClassMethodElem<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ClassMethodElem {
            id,
            span,
            modifiers,
            asterisk,
            name,
            ty_params,
            params,
            ty,
            body,
        });
        self.insert_node(id, ast::Node::ClassMethodElem(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_return_statement(
        &mut self,
        span: Span,
        expr: Option<&'cx ast::Expr<'cx>>,
    ) -> &'cx ast::RetStmt<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::RetStmt { id, span, expr });
        self.insert_node(id, ast::Node::RetStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_switch_statement(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        case_block: &'cx ast::CaseBlock<'cx>,
    ) -> &'cx ast::SwitchStmt<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::SwitchStmt {
            id,
            span,
            expr,
            case_block,
        });
        self.insert_node(id, ast::Node::SwitchStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_case_clause(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        stmts: ast::Stmts<'cx>,
    ) -> &'cx ast::CaseClause<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::CaseClause {
            id,
            span,
            expr,
            stmts,
        });
        self.insert_node(id, ast::Node::CaseClause(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_default_clause(
        &mut self,
        span: Span,
        stmts: ast::Stmts<'cx>,
    ) -> &'cx ast::DefaultClause<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::DefaultClause { id, span, stmts });
        self.insert_node(id, ast::Node::DefaultClause(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_case_block(
        &mut self,
        span: Span,
        clauses: &'cx [ast::CaseOrDefaultClause<'cx>],
    ) -> &'cx ast::CaseBlock<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::CaseBlock { id, span, clauses });
        self.insert_node(id, ast::Node::CaseBlock(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_variable_declaration(
        &mut self,
        span: Span,
        name: &'cx ast::Binding<'cx>,
        excl: Option<Span>,
        ty: Option<&'cx ast::Ty<'cx>>,
        init: Option<&'cx ast::Expr<'cx>>,
        flags: ast::NodeFlags,
    ) -> &'cx ast::VarDecl<'cx> {
        debug_assert!(
            flags == ast::NodeFlags::LET || flags == ast::NodeFlags::CONST || flags.is_empty()
        );
        let id = self.next_node_id();
        let node = self.alloc(ast::VarDecl {
            id,
            span,
            name,
            excl,
            ty,
            init,
        });
        self.insert_node(id, ast::Node::VarDecl(node));
        self.insert_node_flags(id, flags | self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_variable_statement(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        declaration_list: &'cx [&'cx ast::VarDecl<'cx>],
    ) -> &'cx ast::VarStmt<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::VarStmt {
            id,
            span,
            modifiers,
            list: declaration_list,
        });
        self.insert_node(id, ast::Node::VarStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        self.set_external_module_indicator_if_has_export_modifier(node.id, modifiers);
        node
    }

    #[inline(always)]
    fn create_for_of_statement(
        &mut self,
        span: Span,
        r#await: Option<Span>,
        init: ast::ForInitKind<'cx>,
        expr: &'cx ast::Expr<'cx>,
        body: &'cx ast::Stmt<'cx>,
    ) -> &'cx ast::ForOfStmt<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ForOfStmt {
            id,
            span,
            r#await,
            init,
            expr,
            body,
        });
        self.insert_node(id, ast::Node::ForOfStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_for_statement(
        &mut self,
        span: Span,
        init: Option<ast::ForInitKind<'cx>>,
        cond: Option<&'cx ast::Expr<'cx>>,
        incr: Option<&'cx ast::Expr<'cx>>,
        body: &'cx ast::Stmt<'cx>,
    ) -> &'cx ast::ForStmt<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ForStmt {
            id,
            span,
            init,
            body,
            cond,
            incr,
        });
        self.insert_node(id, ast::Node::ForStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_for_in_statement(
        &mut self,
        span: Span,
        init: ast::ForInitKind<'cx>,
        expr: &'cx ast::Expr<'cx>,
        body: &'cx ast::Stmt<'cx>,
    ) -> &'cx ast::ForInStmt<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ForInStmt {
            id,
            span,
            init,
            expr,
            body,
        });
        self.insert_node(id, ast::Node::ForInStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_throw_statement(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::ThrowStmt<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ThrowStmt { id, span, expr });
        self.insert_node(id, ast::Node::ThrowStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_delete_expression(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::DeleteExpr<'cx> {
        let id = self.next_node_id();
        let n = self.alloc(ast::DeleteExpr { id, span, expr });
        self.insert_node(id, ast::Node::DeleteExpr(n));
        self.insert_node_flags(id, self.node_context_flags());
        n
    }

    #[inline(always)]
    fn create_index_signature_declaration(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: &'cx ast::Binding<'cx>,
        name_ty: &'cx ast::Ty<'cx>,
        ty: &'cx ast::Ty<'cx>,
    ) -> &'cx ast::IndexSigDecl<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::IndexSigDecl {
            id,
            span,
            modifiers,
            key: name,
            key_ty: name_ty,
            ty,
        });
        self.insert_node(id, ast::Node::IndexSigDecl(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_labeled_statement(
        &mut self,
        span: Span,
        label: &'cx ast::Ident,
        stmt: &'cx ast::Stmt<'cx>,
    ) -> &'cx ast::LabeledStmt<'cx> {
        let id = self.next_node_id();
        let stmt = self.alloc(ast::LabeledStmt {
            id,
            span,
            label,
            stmt,
        });
        self.insert_node(id, ast::Node::LabeledStmt(stmt));
        self.insert_node_flags(id, self.node_context_flags());
        stmt
    }

    #[inline(always)]
    fn create_await_expression(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::AwaitExpr<'cx> {
        let id = self.next_node_id();
        let stmt = self.alloc(ast::AwaitExpr { id, span, expr });
        self.insert_node(id, ast::Node::AwaitExpr(stmt));
        self.insert_node_flags(id, self.node_context_flags());
        stmt
    }

    #[inline(always)]
    fn create_import_declaration(
        &mut self,
        span: Span,
        clause: Option<&'cx ast::ImportClause<'cx>>,
        module: &'cx ast::StringLit,
    ) -> &'cx ast::ImportDecl<'cx> {
        let id = self.next_node_id();
        let import = self.alloc(ast::ImportDecl {
            id,
            span,
            clause,
            module,
        });
        self.set_external_module_indicator(import.id);
        self.insert_node(id, ast::Node::ImportDecl(import));
        self.insert_node_flags(id, self.node_context_flags());
        import
    }

    #[inline(always)]
    fn create_modifier(&mut self, span: Span, kind: ast::ModifierKind) -> &'cx ast::Modifier {
        let id = self.next_node_id();
        let m = self.alloc(ast::Modifier::new(id, span, kind));
        self.insert_node(id, ast::Node::Modifier(m));
        self.insert_node_flags(id, self.node_context_flags());
        m
    }

    #[inline(always)]
    fn create_arrow_function_expression(
        &mut self,
        span: Span,
        modifier: Option<&'cx ast::Modifier>,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
        body: ast::ArrowFnExprBody<'cx>,
    ) -> &'cx ast::ArrowFnExpr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ArrowFnExpr {
            id,
            span,
            async_modifier: modifier,
            ty_params,
            params,
            ty,
            body,
        });
        self.insert_node(id, ast::Node::ArrowFnExpr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_yield_expression(
        &mut self,
        span: Span,
        asterisk: Option<Span>,
        expr: Option<&'cx ast::Expr<'cx>>,
    ) -> &'cx ast::YieldExpr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::YieldExpr {
            id,
            span,
            asterisk,
            expr,
        });
        self.insert_node(id, ast::Node::YieldExpr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_omit_expression(&mut self, span: Span) -> &'cx ast::OmitExpr {
        let id = self.next_node_id();
        let node = self.alloc(ast::OmitExpr { id, span });
        self.insert_node(id, ast::Node::OmitExpr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_call_expression(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        ty_args: Option<&'cx ast::Tys<'cx>>,
        args: ast::Exprs<'cx>,
    ) -> &'cx ast::CallExpr<'cx> {
        let id = self.next_node_id();
        let call = self.alloc(ast::CallExpr {
            id,
            span,
            ty_args,
            question: None,
            expr,
            args,
        });
        self.insert_node(id, ast::Node::CallExpr(call));
        self.insert_node_flags(id, self.node_context_flags());
        call
    }

    #[inline(always)]
    fn create_call_expression_chain(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        ty_args: Option<&'cx ast::Tys<'cx>>,
        question: Option<Span>,
        args: ast::Exprs<'cx>,
    ) -> &'cx ast::CallExpr<'cx> {
        let id = self.next_node_id();
        let call = self.alloc(ast::CallExpr {
            id,
            span,
            ty_args,
            question,
            expr,
            args,
        });
        self.insert_node(id, ast::Node::CallExpr(call));
        let flags = self.node_context_flags() | ast::NodeFlags::OPTIONAL_CHAIN;
        self.insert_node_flags(id, flags);
        call
    }

    #[inline(always)]
    fn create_element_access_expression(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        arg: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::EleAccessExpr<'cx> {
        let id = self.next_node_id();
        let expr = self.alloc(ast::EleAccessExpr {
            id,
            span,
            expr,
            question: None,
            arg,
        });
        self.insert_node(id, ast::Node::EleAccessExpr(expr));
        self.insert_node_flags(id, self.node_context_flags());
        expr
    }

    #[inline(always)]
    fn create_element_access_expression_chain(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        question: Option<Span>,
        arg: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::EleAccessExpr<'cx> {
        let id = self.next_node_id();
        let expr = self.alloc(ast::EleAccessExpr {
            id,
            span,
            expr,
            question,
            arg,
        });
        self.insert_node(id, ast::Node::EleAccessExpr(expr));
        self.insert_node_flags(
            id,
            self.node_context_flags() | ast::NodeFlags::OPTIONAL_CHAIN,
        );
        expr
    }

    #[inline(always)]
    fn create_export_assignment<const IS_EXPORT_EQUALS: bool>(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::ExportAssign<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ExportAssign {
            id,
            span,
            expr,
            is_export_equals: IS_EXPORT_EQUALS,
        });
        self.set_external_module_indicator(id);
        self.insert_node(id, ast::Node::ExportAssign(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_interface_declaration(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: &'cx ast::Ident,
        ty_params: Option<ast::TyParams<'cx>>,
        extends: Option<&'cx ast::InterfaceExtendsClause<'cx>>,
        members: ast::ObjectTyMembers<'cx>,
    ) -> &'cx ast::InterfaceDecl<'cx> {
        let id = self.next_node_id();
        let decl = self.alloc(ast::InterfaceDecl {
            id,
            span,
            modifiers,
            name,
            ty_params,
            extends,
            members,
        });
        self.set_external_module_indicator_if_has_export_modifier(id, modifiers);
        self.insert_node(id, ast::Node::InterfaceDecl(decl));
        self.insert_node_flags(id, self.node_context_flags());
        decl
    }

    #[inline(always)]
    fn create_type_parameter(
        &mut self,
        span: Span,
        const_modifier: Option<Span>,
        name: &'cx ast::Ident,
        constraint: Option<&'cx ast::Ty<'cx>>,
        default: Option<&'cx ast::Ty<'cx>>,
    ) -> &'cx ast::TyParam<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::TyParam {
            id,
            span,
            const_modifier,
            name,
            constraint,
            default,
        });
        self.insert_node(id, ast::Node::TyParam(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_type_predicate(
        &mut self,
        span: Span,
        asserts: Option<Span>,
        name: ast::PredTyName<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
    ) -> &'cx ast::PredTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::PredTy {
            id,
            span,
            asserts,
            name,
            ty,
        });
        self.insert_node(id, ast::Node::PredTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    fn create_import_equals_declaration(
        &mut self,
        span: Span,
        export_modifier: Option<&'cx ast::Modifier>,
        name: &'cx ast::Ident,
        is_type_only: bool,
        module_reference: ast::ModuleReferenceKind<'cx>,
    ) -> &'cx ast::ImportEqualsDecl<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ImportEqualsDecl {
            id,
            span,
            export_modifier,
            is_type_only,
            name,
            module_reference,
        });
        self.insert_node(id, ast::Node::ImportEqualsDecl(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    fn create_external_module_reference(
        &mut self,
        span: Span,
        module_spec: &'cx ast::StringLit,
    ) -> &'cx ast::ExternalModuleReference<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ExternalModuleReference::new(id, span, module_spec));
        self.insert_node(id, ast::Node::ExternalModuleReference(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    fn create_class_semi_element(&mut self, span: Span) -> &'cx ast::ClassSemiElem {
        let id = self.next_node_id();
        let node = self.alloc(ast::ClassSemiElem { id, span });
        self.insert_node(id, ast::Node::ClassSemiElem(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    fn create_import_shorthand_specifier(
        &mut self,
        span: Span,
        name: &'cx ast::Ident,
    ) -> &'cx ast::ImportShorthandSpec<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ImportShorthandSpec { id, span, name });
        self.insert_node(id, ast::Node::ImportShorthandSpec(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    fn create_export_shorthand_specifier(
        &mut self,
        span: Span,
        name: &'cx ast::Ident,
    ) -> &'cx ast::ExportShorthandSpec<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ExportShorthandSpec { id, span, name });
        self.insert_node(id, ast::Node::ExportShorthandSpec(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    fn create_enum_declaration(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: &'cx ast::Ident,
        members: ast::EnumMembers<'cx>,
    ) -> &'cx ast::EnumDecl<'cx> {
        let id = self.next_node_id();
        let decl = self.alloc(ast::EnumDecl {
            id,
            span,
            modifiers,
            name,
            members,
        });
        self.set_external_module_indicator(id);
        self.insert_node(id, ast::Node::EnumDecl(decl));
        self.insert_node_flags(id, self.node_context_flags());
        decl
    }

    fn create_enum_member(
        &mut self,
        span: Span,
        name: ast::EnumMemberNameKind<'cx>,
        init: Option<&'cx ast::Expr<'cx>>,
    ) -> &'cx ast::EnumMember<'cx> {
        let id = self.next_node_id();
        let member = self.alloc(ast::EnumMember {
            id,
            span,
            name,
            init,
        });
        self.insert_node(id, ast::Node::EnumMember(member));
        self.insert_node_flags(id, self.node_context_flags());
        member
    }

    fn create_conditional_expression(
        &mut self,
        span: Span,
        cond: &'cx ast::Expr<'cx>,
        when_true: &'cx ast::Expr<'cx>,
        when_false: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::CondExpr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::CondExpr {
            id,
            span,
            cond,
            when_true,
            when_false,
        });
        self.insert_node(id, ast::Node::CondExpr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    fn create_reference_type(
        &mut self,
        span: Span,
        name: &'cx ast::EntityName<'cx>,
        type_arguments: Option<&'cx ast::Tys<'cx>>,
    ) -> &'cx ast::ReferTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ReferTy {
            id,
            span,
            name,
            ty_args: type_arguments,
        });
        self.insert_node(id, ast::Node::ReferTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    fn create_setter_declaration(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: &'cx ast::PropName<'cx>,
        params: ast::ParamsDecl<'cx>,
        body: Option<&'cx ast::BlockStmt<'cx>>,
    ) -> &'cx ast::SetterDecl<'cx> {
        debug_assert!(params.len() <= 2);
        let id = self.next_node_id();
        let node = self.alloc(ast::SetterDecl {
            id,
            modifiers,
            span,
            name,
            params,
            body,
        });
        self.insert_node(id, ast::Node::SetterDecl(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    fn create_getter_declaration(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: &'cx ast::PropName<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
        body: Option<&'cx ast::BlockStmt<'cx>>,
    ) -> &'cx ast::GetterDecl<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::GetterDecl {
            id,
            modifiers,
            span,
            name,
            ty,
            body,
        });
        self.insert_node(id, ast::Node::GetterDecl(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_nested_module_declaration(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: &'cx ast::Ident,
        block: ast::NestedModuleBlock<'cx>,
        has_export_decl: bool,
    ) -> &'cx ast::NestedModuleDecl<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::NestedModuleDecl {
            id,
            span,
            modifiers,
            name,
            block,
        });
        let flags = self.node_context_flags();
        // set_export_context_flags
        let flags = if flags.contains(ast::NodeFlags::AMBIENT) && !has_export_decl {
            flags | ast::NodeFlags::EXPORT_CONTEXT
        } else {
            flags & ast::NodeFlags::EXPORT_CONTEXT.complement()
        };
        self.set_external_module_indicator_if_has_export_modifier(id, modifiers);
        self.insert_node(id, ast::Node::NestedModuleDecl(node));
        self.insert_node_flags(id, flags);
        node
    }

    #[inline(always)]
    fn create_block_module_declaration<const IS_GLOBAL_ARGUMENT: bool>(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: ast::ModuleName<'cx>,
        block: Option<&'cx ast::ModuleBlock<'cx>>,
        has_export_decl: bool,
    ) -> &'cx ast::BlockModuleDecl<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::BlockModuleDecl {
            id,
            span,
            modifiers,
            is_global_argument: IS_GLOBAL_ARGUMENT,
            name,
            block,
        });
        let flags = self.node_context_flags();
        // set_export_context_flags
        let flags =
            if flags.contains(ast::NodeFlags::AMBIENT) && block.is_some() && !has_export_decl {
                flags | ast::NodeFlags::EXPORT_CONTEXT
            } else {
                flags & ast::NodeFlags::EXPORT_CONTEXT.complement()
            };
        self.set_external_module_indicator_if_has_export_modifier(id, modifiers);
        self.insert_node(id, ast::Node::BlockModuleDecl(node));
        self.insert_node_flags(id, flags);
        node
    }

    fn create_type_assertion_expression(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        ty: &'cx ast::Ty<'cx>,
    ) -> &'cx ast::TyAssertion<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::TyAssertion { id, span, expr, ty });
        self.insert_node(id, ast::Node::TyAssertionExpr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    fn create_empty_statement(&mut self, span: Span) -> &'cx ast::EmptyStmt {
        let id = self.next_node_id();
        let node = self.alloc(ast::EmptyStmt { id, span });
        self.insert_node(id, ast::Node::EmptyStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    fn create_assignment_expression(
        &mut self,
        span: Span,
        left: &'cx ast::Expr<'cx>,
        op: ast::AssignOp,
        right: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::AssignExpr<'cx> {
        let id = self.next_node_id();
        let expr = self.alloc(ast::AssignExpr {
            id,
            left,
            op,
            right,
            span,
        });
        self.insert_node(id, ast::Node::AssignExpr(expr));
        self.insert_node_flags(id, self.node_context_flags());
        expr
    }

    #[inline(always)]
    fn create_expression_statement(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::ExprStmt<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ExprStmt { id, span, expr });
        self.insert_node(id, ast::Node::ExprStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_postfix_unary_expression(
        &mut self,
        span: Span,
        op: ast::PostfixUnaryOp,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::PostfixUnaryExpr<'cx> {
        let id = self.next_node_id();
        let expr = self.alloc(ast::PostfixUnaryExpr { id, span, op, expr });
        self.insert_node(id, ast::Node::PostfixUnaryExpr(expr));
        self.insert_node_flags(id, self.node_context_flags());
        expr
    }

    #[inline(always)]
    fn create_import_expression(&mut self, span: Span) -> &'cx ast::ImportExpression {
        let id = self.next_node_id();
        let expr = self.alloc(ast::ImportExpression { id, span });
        self.insert_node(id, ast::Node::ImportExpression(expr));
        self.insert_node_flags(id, self.node_context_flags());
        expr
    }

    #[inline(always)]
    fn create_import_type<const IS_TYPEOF: bool>(
        &mut self,
        span: Span,
        argument: &'cx ast::Ty<'cx>,
        qualifier: Option<&'cx ast::EntityName<'cx>>,
        type_arguments: Option<&'cx ast::Tys<'cx>>,
    ) -> &'cx ast::ImportType<'cx> {
        let id = self.next_node_id();
        let ty = self.alloc(ast::ImportType {
            id,
            span,
            is_typeof: IS_TYPEOF,
            argument,
            qualifier,
            type_arguments,
        });
        self.insert_node(id, ast::Node::ImportType(ty));
        self.insert_node_flags(id, self.node_context_flags());
        ty
    }

    #[inline(always)]
    fn create_new_expression(
        &mut self,
        span: Span,
        expression: &'cx ast::Expr<'cx>,
        type_arguments: Option<&'cx ast::Tys<'cx>>,
        arguments: Option<ast::Exprs<'cx>>,
    ) -> &'cx ast::NewExpr<'cx> {
        let id = self.next_node_id();
        let ty = self.alloc(ast::NewExpr {
            id,
            span,
            expr: expression,
            ty_args: type_arguments,
            args: arguments,
        });
        self.insert_node(id, ast::Node::NewExpr(ty));
        self.insert_node_flags(id, self.node_context_flags());
        ty
    }

    #[inline(always)]
    fn create_object_shorthand_property_assignment(
        &mut self,
        span: Span,
        name: &'cx ast::Ident,
        equal_token: Option<Span>,
        object_assignment_initializer: Option<&'cx ast::Expr<'cx>>,
    ) -> &'cx ast::ObjectShorthandMember<'cx> {
        let id = self.next_node_id();
        let prop = self.alloc(ast::ObjectShorthandMember {
            id,
            span,
            name,
            equal_token,
            object_assignment_initializer,
        });
        self.insert_node(id, ast::Node::ObjectShorthandMember(prop));
        self.insert_node_flags(id, self.node_context_flags());
        prop
    }

    #[inline(always)]
    fn create_parameter_declaration(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        dotdotdot: Option<Span>,
        name: &'cx ast::Binding<'cx>,
        question: Option<Span>,
        ty: Option<&'cx ast::Ty<'cx>>,
        init: Option<&'cx ast::Expr<'cx>>,
    ) -> &'cx ast::ParamDecl<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ParamDecl {
            id,
            span,
            modifiers,
            dotdotdot,
            name,
            question,
            ty,
            init,
        });
        self.insert_node(id, ast::Node::ParamDecl(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_non_null_expression(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::NonNullExpr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::NonNullExpr { id, span, expr });
        self.insert_node(id, ast::Node::NonNullExpr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_new_meta_property(
        &mut self,
        span: Span,
        name: &'cx ast::Ident,
    ) -> &'cx ast::NewMetaProperty<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::NewMetaProperty { id, span, name });
        self.insert_node(id, ast::Node::NewMetaProperty(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_constructor_type(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: &'cx ast::Ty<'cx>,
    ) -> &'cx ast::CtorTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::CtorTy {
            id,
            span,
            modifiers,
            ty_params,
            params,
            ty,
        });
        self.insert_node(id, ast::Node::CtorTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_function_type(
        &mut self,
        span: Span,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: &'cx ast::Ty<'cx>,
    ) -> &'cx ast::FnTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::FnTy {
            id,
            span,
            ty_params,
            params,
            ty,
        });
        self.insert_node(id, ast::Node::FnTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_private_identifier(&mut self, span: Span, name: Atom) -> &'cx ast::PrivateIdent {
        let id = self.next_node_id();
        let node = self.alloc(ast::PrivateIdent { id, span, name });
        self.insert_node(id, ast::Node::PrivateIdent(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_expression_with_type_arguments(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        ty_args: Option<&'cx ast::Tys<'cx>>,
    ) -> &'cx ast::ExprWithTyArgs<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ExprWithTyArgs {
            id,
            span,
            expr,
            ty_args,
        });
        self.insert_node(id, ast::Node::ExprWithTyArgs(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_rest_type(&mut self, span: Span, ty: &'cx ast::Ty<'cx>) -> &'cx ast::RestTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::RestTy { id, span, ty });
        self.insert_node(id, ast::Node::RestTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_conditional_type(
        &mut self,
        span: Span,
        check_ty: &'cx ast::Ty<'cx>,
        extends_ty: &'cx ast::Ty<'cx>,
        true_ty: &'cx ast::Ty<'cx>,
        false_ty: &'cx ast::Ty<'cx>,
    ) -> &'cx ast::CondTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::CondTy {
            id,
            span,
            check_ty,
            extends_ty,
            true_ty,
            false_ty,
        });
        self.insert_node(id, ast::Node::CondTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_spread_assignment(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::SpreadAssignment<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::SpreadAssignment { id, span, expr });
        self.insert_node(id, ast::Node::SpreadAssignment(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_parentheses_expression(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::ParenExpr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ParenExpr { id, span, expr });
        self.insert_node(id, ast::Node::ParenExpr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    // Ident, literal, program
    #[inline(always)]
    fn create_identifier(&mut self, name: Atom, span: Span) -> &'cx ast::Ident {
        let id = self.next_node_id();
        let ident = self.alloc(ast::Ident { id, name, span });
        self.insert_node(id, ast::Node::Ident(ident));
        self.insert_node_flags(id, self.node_context_flags());
        ident
    }

    #[inline(always)]
    fn create_program(&mut self, span: Span, stmts: ast::Stmts<'cx>) -> &'cx ast::Program<'cx> {
        let id = self.next_node_id();
        let program = ast::Program::new(id, span, stmts);
        let program = self.alloc(program);
        self.insert_node(id, ast::Node::Program(program));
        self.insert_node_flags(id, self.node_context_flags());
        program
    }

    #[inline(always)]
    fn create_block_statement(
        &mut self,
        span: Span,
        stmts: ast::Stmts<'cx>,
    ) -> &'cx ast::BlockStmt<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::BlockStmt { id, span, stmts });
        self.insert_node(id, ast::Node::BlockStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    // Function and class declarations
    #[inline(always)]
    fn create_function_declaration(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        asterisk: Option<Span>,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
        body: Option<&'cx ast::BlockStmt<'cx>>,
    ) -> &'cx ast::FnDecl<'cx> {
        let id = self.next_node_id();
        let decl = self.alloc(ast::FnDecl {
            id,
            span,
            modifiers,
            asterisk,
            name,
            ty_params,
            params,
            ty,
            body,
        });
        self.insert_node_flags(id, self.node_context_flags());
        self.set_external_module_indicator_if_has_export_modifier(id, modifiers);
        self.insert_node(id, ast::Node::FnDecl(decl));
        decl
    }

    #[inline(always)]
    fn create_function_expression(
        &mut self,
        span: Span,
        async_modifier: Option<&'cx ast::Modifier>,
        asterisk: Option<Span>,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
        body: &'cx ast::BlockStmt<'cx>,
    ) -> &'cx ast::FnExpr<'cx> {
        let id = self.next_node_id();
        let expr = self.alloc(ast::FnExpr {
            id,
            span,
            async_modifier,
            asterisk,
            name,
            ty_params,
            params,
            ty,
            body,
        });
        self.insert_node(id, ast::Node::FnExpr(expr));
        self.insert_node_flags(id, self.node_context_flags());
        expr
    }

    #[inline(always)]
    fn create_class_declaration(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        extends: Option<&'cx ast::ClassExtendsClause<'cx>>,
        implements: Option<&'cx ast::ClassImplementsClause<'cx>>,
        elems: &'cx ast::ClassElems<'cx>,
    ) -> &'cx ast::ClassDecl<'cx> {
        let id = self.next_node_id();
        let decl = self.alloc(ast::ClassDecl {
            id,
            span,
            modifiers,
            name,
            ty_params,
            extends,
            implements,
            elems,
        });
        self.set_external_module_indicator_if_has_export_modifier(id, modifiers);
        self.insert_node_flags(id, self.node_context_flags());
        self.insert_node(id, ast::Node::ClassDecl(decl));
        decl
    }

    #[inline(always)]
    fn create_class_expression(
        &mut self,
        span: Span,
        name: Option<&'cx ast::Ident>,
        ty_params: Option<ast::TyParams<'cx>>,
        extends: Option<&'cx ast::ClassExtendsClause<'cx>>,
        implements: Option<&'cx ast::ClassImplementsClause<'cx>>,
        elems: &'cx ast::ClassElems<'cx>,
    ) -> &'cx ast::ClassExpr<'cx> {
        let id = self.next_node_id();
        let expr = self.alloc(ast::ClassExpr {
            id,
            span,
            name,
            ty_params,
            extends,
            implements,
            elems,
        });
        self.insert_node(id, ast::Node::ClassExpr(expr));
        self.insert_node_flags(id, self.node_context_flags());
        expr
    }

    #[inline(always)]
    fn create_class_extends_clause(
        &mut self,
        span: Span,
        expr_with_ty_args: &'cx ast::ExprWithTyArgs<'cx>,
    ) -> &'cx ast::ClassExtendsClause<'cx> {
        let id = self.next_node_id();
        let clause = self.alloc(ast::ClassExtendsClause {
            id,
            span,
            expr_with_ty_args,
        });
        self.insert_node(id, ast::Node::ClassExtendsClause(clause));
        self.insert_node_flags(id, self.node_context_flags());
        clause
    }

    #[inline(always)]
    fn create_union_type(
        &mut self,
        span: Span,
        tys: &'cx [&'cx ast::Ty<'cx>],
    ) -> &'cx ast::UnionTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::UnionTy { id, span, tys });
        self.insert_node(id, ast::Node::UnionTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_intersection_type(
        &mut self,
        span: Span,
        tys: &'cx [&'cx ast::Ty<'cx>],
    ) -> &'cx ast::IntersectionTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::IntersectionTy { id, span, tys });
        self.insert_node(id, ast::Node::IntersectionTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_infer_type(
        &mut self,
        span: Span,
        ty_param: &'cx ast::TyParam<'cx>,
    ) -> &'cx ast::InferTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::InferTy { id, span, ty_param });
        self.insert_node(id, ast::Node::InferTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_type_operator_type(
        &mut self,
        span: Span,
        op: ast::TyOpKind,
        ty: &'cx ast::Ty<'cx>,
    ) -> &'cx ast::TypeOp<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::TypeOp { id, span, op, ty });
        self.insert_node(id, ast::Node::TyOp(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_nullable_type(
        &mut self,
        span: Span,
        ty: &'cx ast::Ty<'cx>,
    ) -> &'cx ast::NullableTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::NullableTy { id, span, ty });
        self.insert_node(id, ast::Node::NullableTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_indexed_access_type(
        &mut self,
        span: Span,
        ty: &'cx ast::Ty<'cx>,
        index_ty: &'cx ast::Ty<'cx>,
    ) -> &'cx ast::IndexedAccessTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::IndexedAccessTy {
            id,
            span,
            ty,
            index_ty,
        });
        self.insert_node(id, ast::Node::IndexedAccessTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_array_type(&mut self, span: Span, ele: &'cx ast::Ty<'cx>) -> &'cx ast::ArrayTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ArrayTy { id, span, ele });
        self.insert_node(id, ast::Node::ArrayTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_qualified_name(
        &mut self,
        span: Span,
        left: &'cx ast::EntityName<'cx>,
        right: &'cx ast::Ident,
    ) -> &'cx ast::QualifiedName<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::QualifiedName {
            id,
            span,
            left,
            right,
        });
        self.insert_node(id, ast::Node::QualifiedName(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_typeof_type(
        &mut self,
        span: Span,
        name: &'cx ast::EntityName<'cx>,
        ty_args: Option<&'cx ast::Tys<'cx>>,
    ) -> &'cx ast::TypeofTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::TypeofTy {
            id,
            span,
            name,
            ty_args,
        });
        self.insert_node(id, ast::Node::TypeofTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_this_type(&mut self, span: Span) -> &'cx ast::ThisTy {
        let id = self.next_node_id();
        let node = self.alloc(ast::ThisTy { id, span });
        self.insert_node(id, ast::Node::ThisTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_template_literal_type(
        &mut self,
        span: Span,
        head: &'cx ast::TemplateHead,
        spans: &'cx [&'cx ast::TemplateSpanTy<'cx>],
    ) -> &'cx ast::TemplateLitTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::TemplateLitTy {
            id,
            span,
            head,
            spans,
        });
        self.insert_node(id, ast::Node::TemplateLitTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_template_span_type(
        &mut self,
        span: Span,
        ty: &'cx ast::Ty<'cx>,
        text: Atom,
        is_tail: bool,
    ) -> &'cx ast::TemplateSpanTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::TemplateSpanTy {
            id,
            span,
            ty,
            text,
            is_tail,
        });
        self.insert_node(id, ast::Node::TemplateSpanTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_mapped_type(
        &mut self,
        span: Span,
        readonly_token: Option<ast::Token>,
        ty_param: &'cx ast::TyParam<'cx>,
        name_ty: Option<&'cx ast::Ty<'cx>>,
        question_token: Option<ast::Token>,
        ty: Option<&'cx ast::Ty<'cx>>,
    ) -> &'cx ast::MappedTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::MappedTy {
            id,
            span,
            readonly_token,
            ty_param,
            name_ty,
            question_token,
            ty,
        });
        self.insert_node(id, ast::Node::MappedTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_tuple_type(
        &mut self,
        span: Span,
        tys: &'cx [&'cx ast::Ty<'cx>],
    ) -> &'cx ast::TupleTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::TupleTy { id, span, tys });
        self.insert_node(id, ast::Node::TupleTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_named_tuple_type(
        &mut self,
        span: Span,
        dotdotdot: Option<Span>,
        name: &'cx ast::Ident,
        question: Option<Span>,
        ty: &'cx ast::Ty<'cx>,
    ) -> &'cx ast::NamedTupleTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::NamedTupleTy {
            id,
            span,
            dotdotdot,
            name,
            question,
            ty,
        });
        self.insert_node(id, ast::Node::NamedTupleTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_object_literal_type(
        &mut self,
        span: Span,
        members: ast::ObjectTyMembers<'cx>,
    ) -> &'cx ast::ObjectLitTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ObjectLitTy { id, span, members });
        self.insert_node(id, ast::Node::ObjectLitTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_parenthesized_type(
        &mut self,
        span: Span,
        ty: &'cx ast::Ty<'cx>,
    ) -> &'cx ast::ParenTy<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ParenTy { id, span, ty });
        self.insert_node(id, ast::Node::ParenTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_property_signature(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: &'cx ast::PropName<'cx>,
        question: Option<Span>,
        ty: Option<&'cx ast::Ty<'cx>>,
    ) -> &'cx ast::PropSignature<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::PropSignature {
            id,
            span,
            modifiers,
            name,
            question,
            ty,
        });
        self.insert_node(id, ast::Node::PropSignature(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_call_signature_declaration(
        &mut self,
        span: Span,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
    ) -> &'cx ast::CallSigDecl<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::CallSigDecl {
            id,
            span,
            ty_params,
            params,
            ty,
        });
        self.insert_node(id, ast::Node::CallSigDecl(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_constructor_signature_declaration(
        &mut self,
        span: Span,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
    ) -> &'cx ast::CtorSigDecl<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::CtorSigDecl {
            id,
            span,
            ty_params,
            params,
            ty,
        });
        self.insert_node(id, ast::Node::CtorSigDecl(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_intrinsic_type(&mut self, span: Span) -> &'cx ast::IntrinsicTy {
        let id = self.next_node_id();
        let node = self.alloc(ast::IntrinsicTy { id, span });
        self.insert_node(id, ast::Node::IntrinsicTy(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    // Statement nodes
    #[inline(always)]
    fn create_debugger_statement(&mut self, span: Span) -> &'cx ast::DebuggerStmt {
        let id = self.next_node_id();
        let node = self.alloc(ast::DebuggerStmt { id, span });
        self.insert_node(id, ast::Node::DebuggerStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_do_while_statement(
        &mut self,
        span: Span,
        stmt: &'cx ast::Stmt<'cx>,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::DoWhileStmt<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::DoWhileStmt {
            id,
            span,
            stmt,
            expr,
        });
        self.insert_node(id, ast::Node::DoWhileStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_while_statement(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        stmt: &'cx ast::Stmt<'cx>,
    ) -> &'cx ast::WhileStmt<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::WhileStmt {
            id,
            span,
            expr,
            stmt,
        });
        self.insert_node(id, ast::Node::WhileStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_catch_clause(
        &mut self,
        span: Span,
        var: Option<&'cx ast::VarDecl<'cx>>,
        block: &'cx ast::BlockStmt<'cx>,
    ) -> &'cx ast::CatchClause<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::CatchClause {
            id,
            span,
            var,
            block,
        });
        self.insert_node(id, ast::Node::CatchClause(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_try_statement(
        &mut self,
        span: Span,
        try_block: &'cx ast::BlockStmt<'cx>,
        catch_clause: Option<&'cx ast::CatchClause<'cx>>,
        finally_block: Option<&'cx ast::BlockStmt<'cx>>,
    ) -> &'cx ast::TryStmt<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::TryStmt {
            id,
            span,
            try_block,
            catch_clause,
            finally_block,
        });
        self.insert_node(id, ast::Node::TryStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_type_alias_declaration(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        name: &'cx ast::Ident,
        ty_params: Option<ast::TyParams<'cx>>,
        ty: &'cx ast::Ty<'cx>,
    ) -> &'cx ast::TypeAliasDecl<'cx> {
        let id = self.next_node_id();
        let decl = self.alloc(ast::TypeAliasDecl {
            id,
            span,
            modifiers,
            name,
            ty_params,
            ty,
        });
        self.insert_node_flags(id, self.node_context_flags());
        self.set_external_module_indicator_if_has_export_modifier(id, modifiers);
        self.insert_node(id, ast::Node::TypeAliasDecl(decl));
        decl
    }

    #[inline(always)]
    fn create_export_declaration(
        &mut self,
        span: Span,
        clause: &'cx ast::ExportClause<'cx>,
    ) -> &'cx ast::ExportDecl<'cx> {
        let id = self.next_node_id();
        let decl = self.alloc(ast::ExportDecl { id, span, clause });
        self.insert_node(id, ast::Node::ExportDecl(decl));
        self.insert_node_flags(id, self.node_context_flags());
        self.set_external_module_indicator(decl.id);
        decl
    }

    #[inline(always)]
    fn create_named_exports_declaration(
        &mut self,
        span: Span,
        list: &'cx [&'cx ast::ExportSpec<'cx>],
        module: Option<&'cx ast::StringLit>,
    ) -> &'cx ast::SpecsExport<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::SpecsExport {
            id,
            span,
            list,
            module,
        });
        self.insert_node(id, ast::Node::SpecsExport(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_global_export_declaration(
        &mut self,
        span: Span,
        module: &'cx ast::StringLit,
    ) -> &'cx ast::GlobExport<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::GlobExport { id, span, module });
        self.insert_node(id, ast::Node::GlobExport(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_namespace_export_declaration(
        &mut self,
        span: Span,
        name: &'cx ast::ModuleExportName<'cx>,
        module: &'cx ast::StringLit,
    ) -> &'cx ast::NsExport<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::NsExport {
            id,
            span,
            name,
            module,
        });
        self.insert_node(id, ast::Node::NsExport(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_import_clause(
        &mut self,
        span: Span,
        is_type_only: bool,
        name: Option<&'cx ast::Ident>,
        kind: Option<ast::ImportClauseKind<'cx>>,
    ) -> &'cx ast::ImportClause<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ImportClause {
            id,
            span,
            is_type_only,
            name,
            kind,
        });
        self.insert_node(id, ast::Node::ImportClause(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_namespace_import(
        &mut self,
        span: Span,
        name: &'cx ast::Ident,
    ) -> &'cx ast::NsImport<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::NsImport { id, span, name });
        self.insert_node(id, ast::Node::NsImport(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_interface_extends_clause(
        &mut self,
        span: Span,
        list: &'cx [&'cx ast::ReferTy<'cx>],
    ) -> &'cx ast::InterfaceExtendsClause<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::InterfaceExtendsClause { id, span, list });
        self.insert_node(id, ast::Node::InterfaceExtendsClause(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_class_implements_clause(
        &mut self,
        span: Span,
        list: &'cx [&'cx ast::ReferTy<'cx>],
    ) -> &'cx ast::ClassImplementsClause<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ClassImplementsClause { id, span, list });
        self.insert_node(id, ast::Node::ClassImplementsClause(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_object_binding_element(
        &mut self,
        span: Span,
        dotdotdot: Option<Span>,
        name: &'cx ast::ObjectBindingName<'cx>,
        init: Option<&'cx ast::Expr<'cx>>,
    ) -> &'cx ast::ObjectBindingElem<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ObjectBindingElem {
            id,
            span,
            dotdotdot,
            name,
            init,
        });
        self.insert_node(id, ast::Node::ObjectBindingElem(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_object_binding_pattern(
        &mut self,
        span: Span,
        elems: &'cx [&'cx ast::ObjectBindingElem<'cx>],
    ) -> &'cx ast::ObjectPat<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ObjectPat { id, span, elems });
        self.insert_node(id, ast::Node::ObjectPat(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_array_binding_pattern(
        &mut self,
        span: Span,
        elems: &'cx [&'cx ast::ArrayBindingElem<'cx>],
    ) -> &'cx ast::ArrayPat<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ArrayPat { id, span, elems });
        self.insert_node(id, ast::Node::ArrayPat(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_array_binding(
        &mut self,
        span: Span,
        dotdotdot: Option<Span>,
        name: &'cx ast::Binding<'cx>,
        init: Option<&'cx ast::Expr<'cx>>,
    ) -> &'cx ast::ArrayBinding<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ArrayBinding {
            id,
            span,
            dotdotdot,
            name,
            init,
        });
        self.insert_node(id, ast::Node::ArrayBinding(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_module_block(
        &mut self,
        span: Span,
        stmts: ast::Stmts<'cx>,
    ) -> &'cx ast::ModuleBlock<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ModuleBlock { id, span, stmts });
        self.insert_node(id, ast::Node::ModuleBlock(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_if_statement(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        then: &'cx ast::Stmt<'cx>,
        else_then: Option<&'cx ast::Stmt<'cx>>,
    ) -> &'cx ast::IfStmt<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::IfStmt {
            id,
            span,
            expr,
            then,
            else_then,
        });
        self.insert_node(id, ast::Node::IfStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_break_statement(
        &mut self,
        span: Span,
        label: Option<&'cx ast::Ident>,
    ) -> &'cx ast::BreakStmt<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::BreakStmt { id, span, label });
        self.insert_node(id, ast::Node::BreakStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_continue_statement(
        &mut self,
        span: Span,
        label: Option<&'cx ast::Ident>,
    ) -> &'cx ast::ContinueStmt<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ContinueStmt { id, span, label });
        self.insert_node(id, ast::Node::ContinueStmt(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_import_named_specifier(
        &mut self,
        span: Span,
        prop_name: &'cx ast::ModuleExportName<'cx>,
        name: &'cx ast::Ident,
    ) -> &'cx ast::ImportNamedSpec<'cx> {
        let id = self.next_node_id();
        let spec = self.alloc(ast::ImportNamedSpec {
            id,
            span,
            prop_name,
            name,
        });
        self.insert_node(id, ast::Node::ImportNamedSpec(spec));
        self.insert_node_flags(id, self.node_context_flags());
        spec
    }

    #[inline(always)]
    fn create_export_named_specifier(
        &mut self,
        span: Span,
        prop_name: &'cx ast::ModuleExportName<'cx>,
        name: &'cx ast::ModuleExportName<'cx>,
    ) -> &'cx ast::ExportNamedSpec<'cx> {
        let id = self.next_node_id();
        let spec = self.alloc(ast::ExportNamedSpec {
            id,
            span,
            prop_name,
            name,
        });
        self.insert_node(id, ast::Node::ExportNamedSpec(spec));
        self.insert_node_flags(id, self.node_context_flags());
        spec
    }

    // Expression nodes
    #[inline(always)]
    fn create_prefix_unary_expression(
        &mut self,
        span: Span,
        op: ast::PrefixUnaryOp,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::PrefixUnaryExpr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::PrefixUnaryExpr { id, span, op, expr });
        self.insert_node(id, ast::Node::PrefixUnaryExpr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_typeof_expression(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::TypeofExpr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::TypeofExpr { id, span, expr });
        self.insert_node(id, ast::Node::TypeofExpr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_satisfies_expression(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        ty: &'cx ast::Ty<'cx>,
    ) -> &'cx ast::SatisfiesExpr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::SatisfiesExpr { id, span, expr, ty });
        self.insert_node(id, ast::Node::SatisfiesExpr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_as_expression(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        ty: &'cx ast::Ty<'cx>,
    ) -> &'cx ast::AsExpr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::AsExpr { id, span, expr, ty });
        self.insert_node(id, ast::Node::AsExpr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_array_literal(
        &mut self,
        span: Span,
        elems: &'cx [&'cx ast::Expr<'cx>],
    ) -> &'cx ast::ArrayLit<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ArrayLit { id, span, elems });
        self.insert_node(id, ast::Node::ArrayLit(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_spread_element(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::SpreadElement<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::SpreadElement { id, span, expr });
        self.insert_node(id, ast::Node::SpreadElement(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_super_expression(&mut self, span: Span) -> &'cx ast::SuperExpr {
        let id = self.next_node_id();
        let node = self.alloc(ast::SuperExpr { id, span });
        self.insert_node(id, ast::Node::SuperExpr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_template_expression(
        &mut self,
        span: Span,
        head: &'cx ast::TemplateHead,
        spans: &'cx [&'cx ast::TemplateSpan<'cx>],
    ) -> &'cx ast::TemplateExpr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::TemplateExpr {
            id,
            span,
            head,
            spans,
        });
        self.insert_node(id, ast::Node::TemplateExpr(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_template_head(&mut self, span: Span, text: Atom) -> &'cx ast::TemplateHead {
        let id = self.next_node_id();
        let node = self.alloc(ast::TemplateHead { id, span, text });
        self.insert_node(id, ast::Node::TemplateHead(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_template_span(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        text: Atom,
        is_tail: bool,
    ) -> &'cx ast::TemplateSpan<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::TemplateSpan {
            id,
            span,
            expr,
            text,
            is_tail,
        });
        self.insert_node(id, ast::Node::TemplateSpan(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_object_literal(
        &mut self,
        span: Span,
        members: &'cx [&'cx ast::ObjectMember<'cx>],
    ) -> &'cx ast::ObjectLit<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ObjectLit { id, span, members });
        self.insert_node(id, ast::Node::ObjectLit(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_regular_expression_literal(&mut self, span: Span, val: Atom) -> &'cx ast::RegExpLit {
        let id = self.next_node_id();
        let node = self.alloc(ast::RegExpLit { id, span, val });
        self.insert_node(id, ast::Node::RegExpLit(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_boolean_literal(&mut self, val: bool, span: Span) -> &'cx ast::Lit<bool> {
        let id = self.next_node_id();
        let node = self.alloc(ast::Lit { id, val, span });
        self.insert_node(id, ast::Node::BoolLit(node));
        self.insert_node_flags(id, self.node_context_flags());
        node
    }
}
