use bolt_ts_ast as ast;
use bolt_ts_ast::NodeID;
use bolt_ts_ast::r#trait::NoParenRule;
use bolt_ts_ast::r#trait::ParenRuleTrait;
use bolt_ts_atom::Atom;
use bolt_ts_span::Span;

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug)]
    pub struct VarDeclarationContext: u8 {
        const FOR = 1 << 0;
        const CONST = 1 << 1;
        const AMBIENT = 1 << 2;
        const LET = 1 << 3;
    }
}

impl VarDeclarationContext {
    pub const fn init_should_exit(&self) -> bool {
        self.contains(VarDeclarationContext::CONST)
            && !self.contains(VarDeclarationContext::AMBIENT)
            && !self.contains(VarDeclarationContext::FOR)
    }
}

pub trait ASTFactory<'cx> {
    fn next_node_id(&mut self) -> NodeID;
    fn insert_node(&mut self, node_id: NodeID, node: ast::Node<'cx>);
    fn insert_node_flags(&mut self, node_id: NodeID, flags: ast::NodeFlags);
    fn alloc<T>(&self, val: T) -> &'cx T;
    fn node_context_flags(&self) -> ast::NodeFlags;
    fn set_external_module_indicator(&mut self, node_id: NodeID);

    #[inline(always)]
    fn create_numeric_literal(&mut self, val: f64, span: Span) -> &'cx ast::NumLit {
        let id = self.next_node_id();
        let node = self.alloc(ast::NumLit { id, val, span });
        self.insert_node(id, ast::Node::NumLit(node));
        self.insert_node_flags(id, ast::NodeFlags::empty());
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_jsx_jsx_closing_fragment(&mut self, span: Span) -> &'cx ast::JsxClosingFrag {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxClosingFrag { id, span });
        self.insert_node(id, ast::Node::JsxClosingFrag(node));
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_jsx_named_attr(
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_jsx_spread_attr(
        &mut self,
        expr: &'cx ast::Expr,
        span: Span,
    ) -> &'cx ast::JsxSpreadAttr<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxSpreadAttr { id, expr, span });
        self.insert_node(id, ast::Node::JsxSpreadAttr(node));
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_jsx_ns_name(
        &mut self,
        ns: &'cx ast::Ident,
        name: &'cx ast::Ident,
        span: Span,
    ) -> &'cx ast::JsxNsName<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxNsName { id, span, ns, name });
        self.insert_node(id, ast::Node::JsxNsName(node));
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_this_expr(&mut self, span: Span) -> &'cx ast::ThisExpr {
        let id = self.next_node_id();
        let this = self.alloc(ast::ThisExpr { id, span });
        self.insert_node(this.id, ast::Node::ThisExpr(this));
        self.insert_node_flags(this.id, ast::NodeFlags::empty());
        this
    }

    #[inline(always)]
    fn create_jsx_expr(
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_jsx_attrs(&mut self, attrs: &'cx [ast::JsxAttr<'cx>]) -> ast::JsxAttrs<'cx> {
        attrs
    }

    #[inline(always)]
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
        self.insert_node(id, ast::Node::PropAccessExpr(node));
        let flags = if question_dot.is_some() {
            ast::NodeFlags::OPTIONAL_CHAIN
        } else {
            ast::NodeFlags::empty()
        };
        self.insert_node_flags(id, flags);
        node
    }

    #[inline(always)]
    fn create_prop_access_expr(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        name: &'cx ast::Ident,
    ) -> &'cx ast::PropAccessExpr<'cx> {
        let expr = NoParenRule.paren_left_side_of_access(expr, false);
        self.create_base_prop_access_expr(span, expr, None, name)
    }

    #[inline(always)]
    fn create_prop_access_chain(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
        question_dot: Option<Span>,
        name: &'cx ast::Ident,
    ) -> &'cx ast::PropAccessExpr<'cx> {
        let expr = NoParenRule.paren_left_side_of_access(expr, true);
        self.create_base_prop_access_expr(span, expr, question_dot, name)
    }

    #[inline(always)]
    fn create_jsx_opening_frag(&mut self, span: Span) -> &'cx ast::JsxOpeningFrag {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxOpeningFrag { id, span });
        self.insert_node(id, ast::Node::JsxOpeningFrag(node));
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_jsx_opening_ele(
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_jsx_closing_ele(
        &mut self,
        span: Span,
        tag_name: ast::JsxTagName<'cx>,
    ) -> &'cx ast::JsxClosingElem<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::JsxClosingElem { id, span, tag_name });
        self.insert_node(id, ast::Node::JsxClosingElem(node));
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_jsx_self_closing_ele(
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_jsx_frag(
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_jsx_ele(
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_tagged_template_expr(
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
        tagged_template
    }

    #[inline(always)]
    fn create_binary_expr(
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_class_ctor(
        &mut self,
        span: Span,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
        ty_params: Option<ast::TyParams<'cx>>,
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
            ty_params,
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
    fn create_class_prop_elem(
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_class_static_block_decl(
        &mut self,
        span: Span,
        body: &'cx ast::BlockStmt<'cx>,
    ) -> &'cx ast::ClassStaticBlockDecl<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::ClassStaticBlockDecl { id, span, body });
        self.insert_node(id, ast::Node::ClassStaticBlockDecl(node));
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
        sig
    }

    #[inline(always)]
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_class_method_elem(
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
    fn create_ret_stmt(
        &mut self,
        span: Span,
        expr: Option<&'cx ast::Expr<'cx>>,
    ) -> &'cx ast::RetStmt<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::RetStmt { id, span, expr });
        self.insert_node(id, ast::Node::RetStmt(node));
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_switch_stmt(
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_var_decl(
        &mut self,
        span: Span,
        name: &'cx ast::Binding<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
        init: Option<&'cx ast::Expr<'cx>>,
        ctx: VarDeclarationContext,
    ) -> &'cx ast::VarDecl<'cx> {
        let id = self.next_node_id();
        let node = self.alloc(ast::VarDecl {
            id,
            span,
            name,
            ty,
            init,
        });
        self.insert_node(id, ast::Node::VarDecl(node));
        let flags = match ctx {
            c if c.contains(VarDeclarationContext::CONST) => ast::NodeFlags::CONST,
            c if c.contains(VarDeclarationContext::LET) => ast::NodeFlags::LET,
            _ => ast::NodeFlags::empty(),
        } | self.node_context_flags();
        self.insert_node_flags(id, flags);
        node
    }

    #[inline(always)]
    fn create_for_of_stmt(
        &mut self,
        span: Span,
        r#await: Option<Span>,
        init: ast::ForInitKind<'cx>,
        expr: &'cx ast::Expr<'cx>,
        body: &'cx ast::Stmt<'cx>,
        flags: ast::NodeFlags,
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
        self.insert_node_flags(id, flags | self.node_context_flags());
        node
    }

    #[inline(always)]
    fn create_for_in_stmt(
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
    fn create_throw_stmt(
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
    fn create_delete_expr(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::DeleteExpr<'cx> {
        let id = self.next_node_id();
        let n = self.alloc(ast::DeleteExpr { id, span, expr });
        self.insert_node(id, ast::Node::DeleteExpr(n));
        self.insert_node_flags(id, ast::NodeFlags::empty());
        n
    }

    #[inline(always)]
    fn create_index_sig_decl(
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
        node
    }

    #[inline(always)]
    fn create_labeled_stmt(
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
        stmt
    }

    #[inline(always)]
    fn create_await_expr(
        &mut self,
        span: Span,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ast::AwaitExpr<'cx> {
        let id = self.next_node_id();
        let stmt = self.alloc(ast::AwaitExpr { id, span, expr });
        self.insert_node(id, ast::Node::AwaitExpr(stmt));
        self.insert_node_flags(id, ast::NodeFlags::empty());
        stmt
    }

    #[inline(always)]
    fn create_binding(&mut self, kind: ast::BindingKind<'cx>) -> &'cx ast::Binding<'cx> {
        let binding = self.alloc(ast::Binding {
            span: kind.span(),
            kind,
        });
        binding
    }

    #[inline(always)]
    fn create_import_decl(
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
        self.insert_node_flags(id, ast::NodeFlags::empty());
        import
    }

    #[inline]
    fn create_modifier(&mut self, span: Span, kind: ast::ModifierKind) -> &'cx ast::Modifier {
        let id = self.next_node_id();
        let m = self.alloc(ast::Modifier { id, span, kind });
        self.insert_node(id, ast::Node::Modifier(m));
        m
    }

    #[inline]
    fn create_modifiers(
        &mut self,
        span: Span,
        modifiers: &'cx [&'cx ast::Modifier],
        flags: enumflags2::BitFlags<ast::ModifierKind>,
    ) -> &'cx ast::Modifiers<'cx> {
        self.alloc(ast::Modifiers {
            span,
            flags,
            list: modifiers,
        })
    }

    #[inline]
    fn create_arrow_fn_expr(
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

    #[inline]
    fn create_yield_expr(
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
}
