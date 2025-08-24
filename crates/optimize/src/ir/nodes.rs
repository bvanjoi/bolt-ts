use bolt_ts_arena::la_arena;
use bolt_ts_ast::{self as ast, keyword};
use bolt_ts_atom::Atom;
use bolt_ts_checker::ty::TyID;
use bolt_ts_ecma_logical::js_double_to_boolean;
use bolt_ts_span::Span;

use crate::ir::GraphID;

macro_rules! decl_nodes {
    ( $( $node_field: ident: $node_name:ident ),* ) => {
        ::paste::paste! {
            $(
                #[derive(Default)]
                struct [<$node_name Arena>](la_arena::Arena<$node_name>);
                #[derive(Debug, Clone, Copy)]
                pub struct [<$node_name ID>](la_arena::Idx<$node_name>);
            )*

            #[derive(Default)]
            pub struct Nodes {
                $(
                   [<$node_field _nodes>]: [<$node_name Arena>],
                )*
            }

            impl Nodes {
                $(
                    pub fn [<get_ $node_field>](&self, id: &[<$node_name ID>]) -> &$node_name {
                        &self.[<$node_field _nodes>].0[id.0]
                    }
                    pub fn [<get_mut_ $node_field>](&mut self, id: &[<$node_name ID>]) -> &mut $node_name {
                        &mut self.[<$node_field _nodes>].0[id.0]
                    }
                )*
            }

        }
    };
}

decl_nodes!(
    var_stmt: VarStmt,
    if_stmt: IfStmt,
    ret_stmt: RetStmt,
    block_stmt: BlockStmt,
    throw_stmt: ThrowStmt,
    for_stmt: ForStmt,
    module_block: ModuleBlock,
    fn_decl: FnDecl,
    for_in_stmt: ForInStmt,
    for_of_stmt: ForOfStmt,
    while_stmt: WhileStmt,
    do_stmt: DoStmt,
    empty_stmt: EmptyStmt,
    break_stmt: BreakStmt,
    continue_stmt: ContinueStmt,
    try_stmt: TryStmt,
    catch_clause: CatchClause,
    labeled_stmt: LabeledStmt,
    expr_stmt: ExprStmt,
    class_decl: ClassDecl,
    module_decl: ModuleDecl,
    class_ctor: ClassCtor,
    class_prop_elem: ClassPropElem,
    class_method_elem: ClassMethodElem,
    class_static_block: ClassStaticBlock,
    getter_decl: GetterDecl,
    setter_decl: SetterDecl,
    enum_decl: EnumDecl,
    import_decl: ImportDecl,
    export_decl: ExportDecl,
    export_assign: ExportAssign,

    bin_expr: BinExpr,
    omit_expr: OmitExpr,
    paren_expr: ParenExpr,
    cond_expr: CondExpr,
    call_expr: CallExpr,
    fn_expr: FnExpr,
    class_expr: ClassExpr,
    new_expr: NewExpr,
    assign_expr: AssignExpr,
    arrow_fn_expr: ArrowFnExpr,
    prefix_unary_expr: PrefixUnaryExpr,
    postfix_unary_expr: PostfixUnaryExpr,
    prop_access_expr: PropAccessExpr,
    ele_access_expr: EleAccessExpr,
    this_expr: ThisExpr,
    typeof_expr: TypeofExpr,
    void_expr: VoidExpr,
    super_expr: SuperExpr,
    template_expr: TemplateExpr,
    tagged_template_expr: TaggedTemplateExpr,
    num_lit: NumLit,
    bigint_lit: BigIntLit,
    bool_lit: BoolLit,
    regexp_lit: RegExpLit,
    null_lit: NullLit,
    string_lit: StringLit,
    array_lit: ArrayLit,
    object_lit: ObjectLit,
    ident: Ident,
    computed_prop_name: ComputedPropName,

    param_decl: ParamDecl,
    modifier: Modifier,
    var_decl: VarDecl,
    class_extends_clause: ClassExtendsClause,
    enum_member: EnumMember,
    ns_export: NsExport,
    shorthand_spec: ShorthandSpec,
    glob_export: GlobExport,
    specs_export: SpecsExport,
    export_named_spec: ExportNamedSpec,
    ns_import: NsImport,
    import_named_spec: ImportNamedSpec,
    import_clause: ImportClause,
    object_pat: ObjectPat,
    object_binding_elem: ObjectBindingElem,
    array_pat: ArrayPat,
    array_binding: ArrayBinding,
    object_shorthand_member: ObjectShorthandMember,
    object_prop_member: ObjectPropMember,
    object_method_member: ObjectMethodMember,
    spread_assignment: SpreadAssignment,
    spread_element: SpreadElement,
    template_head: TemplateHead,
    template_span: TemplateSpan,

    jsx_elem: JsxElem,
    jsx_opening_elem: JsxOpeningElem,
    jsx_closing_elem: JsxClosingElem,
    jsx_spread_attr: JsxSpreadAttr,
    jsx_named_attr: JsxNamedAttr,
    jsx_ns_name: JsxNsName,
    jsx_frag: JsxFrag,
    jsx_opening_frag: JsxOpeningFrag,
    jsx_closing_frag: JsxClosingFrag,
    jsx_self_closing_elem: JsxSelfClosingElem,
    jsx_text: JsxText,
    jsx_expr: JsxExpr
);

#[inline(always)]
fn usize_into_idx<T>(len: usize) -> la_arena::Idx<T> {
    let raw = la_arena::RawIdx::from_u32(len as u32);
    la_arena::Idx::from_raw(raw)
}

impl Nodes {
    pub fn alloc_jsx_elem(
        &mut self,
        span: Span,
        opening_elem: JsxOpeningElemID,
        children: Vec<JsxChild>,
        closing_elem: JsxClosingElemID,
    ) -> JsxElemID {
        let idx = JsxElemID(usize_into_idx(self.jsx_elem_nodes.0.len()));
        let id = self.jsx_elem_nodes.0.alloc(JsxElem {
            id: idx,
            span,
            opening_elem,
            children,
            closing_elem,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_jsx_frag(
        &mut self,
        span: Span,
        opening_frag: JsxOpeningFragID,
        children: Vec<JsxChild>,
        closing_frag: JsxClosingFragID,
    ) -> JsxFragID {
        let idx = JsxFragID(usize_into_idx(self.jsx_frag_nodes.0.len()));
        let id = self.jsx_frag_nodes.0.alloc(JsxFrag {
            id: idx,
            span,
            opening_frag,
            children,
            closing_frag,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_jsx_opening_frag(&mut self, span: Span) -> JsxOpeningFragID {
        let idx = JsxOpeningFragID(usize_into_idx(self.jsx_opening_frag_nodes.0.len()));
        let id = self
            .jsx_opening_frag_nodes
            .0
            .alloc(JsxOpeningFrag { id: idx, span });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_jsx_closing_frag(&mut self, span: Span) -> JsxClosingFragID {
        let idx = JsxClosingFragID(usize_into_idx(self.jsx_closing_frag_nodes.0.len()));
        let id = self
            .jsx_closing_frag_nodes
            .0
            .alloc(JsxClosingFrag { id: idx, span });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_jsx_self_closing_elem(
        &mut self,
        span: Span,
        tag_name: JsxTagName,
        attrs: Vec<JsxAttr>,
    ) -> JsxSelfClosingElemID {
        let idx = JsxSelfClosingElemID(usize_into_idx(self.jsx_self_closing_elem_nodes.0.len()));
        let id = self
            .jsx_self_closing_elem_nodes
            .0
            .alloc(JsxSelfClosingElem {
                id: idx,
                span,
                tag_name,
                attrs,
            });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_jsx_expr(
        &mut self,
        span: Span,
        dotdotdot: Option<Span>,
        expr: Option<Expr>,
    ) -> JsxExprID {
        let idx = JsxExprID(usize_into_idx(self.jsx_expr_nodes.0.len()));
        let id = self.jsx_expr_nodes.0.alloc(JsxExpr {
            id: idx,
            span,
            dotdotdot,
            expr,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_jsx_text(
        &mut self,
        span: Span,
        text: Atom,
        contains_only_trivia_whitespace: bool,
    ) -> JsxTextID {
        let idx = JsxTextID(usize_into_idx(self.jsx_text_nodes.0.len()));
        let id = self.jsx_text_nodes.0.alloc(JsxText {
            id: idx,
            span,
            text,
            contains_only_trivia_whitespace,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_jsx_closing_elem(&mut self, span: Span, tag_name: JsxTagName) -> JsxClosingElemID {
        let idx = JsxClosingElemID(usize_into_idx(self.jsx_closing_elem_nodes.0.len()));
        let id = self.jsx_closing_elem_nodes.0.alloc(JsxClosingElem {
            id: idx,
            span,
            tag_name,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_jsx_opening_elem(
        &mut self,
        span: Span,
        tag_name: JsxTagName,
        attrs: Vec<JsxAttr>,
    ) -> JsxOpeningElemID {
        let idx = JsxOpeningElemID(usize_into_idx(self.jsx_opening_elem_nodes.0.len()));
        let id = self.jsx_opening_elem_nodes.0.alloc(JsxOpeningElem {
            id: idx,
            span,
            tag_name,
            attrs,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_jsx_named_attr(
        &mut self,
        span: Span,
        name: JsxAttrName,
        init: Option<JsxAttrValue>,
    ) -> JsxNamedAttrID {
        let idx = JsxNamedAttrID(usize_into_idx(self.jsx_named_attr_nodes.0.len()));
        let id = self.jsx_named_attr_nodes.0.alloc(JsxNamedAttr {
            id: idx,
            span,
            name,
            init,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_jsx_spread_attr(&mut self, span: Span, expr: Expr) -> JsxSpreadAttrID {
        let idx = JsxSpreadAttrID(usize_into_idx(self.jsx_spread_attr_nodes.0.len()));
        let id = self.jsx_spread_attr_nodes.0.alloc(JsxSpreadAttr {
            id: idx,
            span,
            expr,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_jsx_ns_name(&mut self, span: Span, ns: IdentID, name: IdentID) -> JsxNsNameID {
        let idx = JsxNsNameID(usize_into_idx(self.jsx_ns_name_nodes.0.len()));
        let id = self.jsx_ns_name_nodes.0.alloc(JsxNsName {
            id: idx,
            span,
            ns,
            name,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_spread_element(&mut self, span: Span, expr: Expr) -> SpreadElementID {
        let idx = SpreadElementID(usize_into_idx(self.spread_element_nodes.0.len()));
        let id = self.spread_element_nodes.0.alloc(SpreadElement {
            id: idx,
            span,
            expr,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_tagged_template_expr(
        &mut self,
        span: Span,
        tag: Expr,
        tpl: Expr,
    ) -> TaggedTemplateExprID {
        let idx = TaggedTemplateExprID(usize_into_idx(self.tagged_template_expr_nodes.0.len()));
        let id = self.tagged_template_expr_nodes.0.alloc(TaggedTemplateExpr {
            id: idx,
            span,
            tag,
            tpl,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_template_expr(
        &mut self,
        span: Span,
        head: TemplateHeadID,
        spans: Vec<TemplateSpanID>,
    ) -> TemplateExprID {
        let idx = TemplateExprID(usize_into_idx(self.template_expr_nodes.0.len()));
        let id = self.template_expr_nodes.0.alloc(TemplateExpr {
            id: idx,
            span,
            head,
            spans,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_template_span(
        &mut self,
        span: Span,
        expr: Expr,
        text: Atom,
        is_tail: bool,
    ) -> TemplateSpanID {
        let idx = TemplateSpanID(usize_into_idx(self.template_span_nodes.0.len()));
        let id = self.template_span_nodes.0.alloc(TemplateSpan {
            id: idx,
            span,
            expr,
            text,
            is_tail,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_template_head(&mut self, span: Span, text: Atom) -> TemplateHeadID {
        let idx = TemplateHeadID(usize_into_idx(self.template_head_nodes.0.len()));
        let id = self.template_head_nodes.0.alloc(TemplateHead {
            id: idx,
            span,
            text,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_void_expr(&mut self, span: Span, expr: Expr) -> VoidExprID {
        let idx = VoidExprID(usize_into_idx(self.void_expr_nodes.0.len()));
        let id = self.void_expr_nodes.0.alloc(VoidExpr {
            id: idx,
            span,
            expr,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_typeof_expr(&mut self, span: Span, expr: Expr) -> TypeofExprID {
        let idx = TypeofExprID(usize_into_idx(self.typeof_expr_nodes.0.len()));
        let id = self.typeof_expr_nodes.0.alloc(TypeofExpr {
            id: idx,
            span,
            expr,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_super_expr(&mut self, span: Span) -> SuperExprID {
        let idx = SuperExprID(usize_into_idx(self.super_expr_nodes.0.len()));
        let id = self.super_expr_nodes.0.alloc(SuperExpr { id: idx, span });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_ele_access_expr(&mut self, span: Span, expr: Expr, arg: Expr) -> EleAccessExprID {
        let idx = EleAccessExprID(usize_into_idx(self.ele_access_expr_nodes.0.len()));
        let id = self.ele_access_expr_nodes.0.alloc(EleAccessExpr {
            id: idx,
            span,
            expr,
            arg,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_prop_access_expr(
        &mut self,
        span: Span,
        expr: Expr,
        question_dot: Option<Span>,
        name: IdentID,
    ) -> PropAccessExprID {
        let idx = PropAccessExprID(usize_into_idx(self.prop_access_expr_nodes.0.len()));
        let id = self.prop_access_expr_nodes.0.alloc(PropAccessExpr {
            id: idx,
            span,
            expr,
            question_dot,
            name,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_postfix_unary_expr(
        &mut self,
        span: Span,
        op: ast::PostfixUnaryOp,
        expr: Expr,
    ) -> PostfixUnaryExprID {
        let idx = PostfixUnaryExprID(usize_into_idx(self.postfix_unary_expr_nodes.0.len()));
        let id = self.postfix_unary_expr_nodes.0.alloc(PostfixUnaryExpr {
            id: idx,
            span,
            op,
            expr,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_prefix_unary_expr(
        &mut self,
        span: Span,
        op: ast::PrefixUnaryOp,
        expr: Expr,
    ) -> PrefixUnaryExprID {
        let idx = PrefixUnaryExprID(usize_into_idx(self.prefix_unary_expr_nodes.0.len()));
        let id = self.prefix_unary_expr_nodes.0.alloc(PrefixUnaryExpr {
            id: idx,
            span,
            op,
            expr,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_arrow_fn_expr(
        &mut self,
        span: Span,
        params: Vec<ParamDeclID>,
        body: GraphID,
    ) -> ArrowFnExprID {
        let idx = ArrowFnExprID(usize_into_idx(self.arrow_fn_expr_nodes.0.len()));
        let id = self.arrow_fn_expr_nodes.0.alloc(ArrowFnExpr {
            id: idx,
            span,
            params,
            body,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_new_expr(&mut self, span: Span, expr: Expr, args: Vec<Expr>) -> NewExprID {
        let idx = NewExprID(usize_into_idx(self.new_expr_nodes.0.len()));
        let id = self.new_expr_nodes.0.alloc(NewExpr {
            id: idx,
            span,
            expr,
            args,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_class_expr(
        &mut self,
        span: Span,
        name: Option<IdentID>,
        extends: Option<ClassExtendsClauseID>,
        elems: Vec<ClassElem>,
    ) -> ClassExprID {
        let idx = ClassExprID(usize_into_idx(self.class_expr_nodes.0.len()));
        let id = self.class_expr_nodes.0.alloc(ClassExpr {
            id: idx,
            span,
            name,
            extends,
            elems,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_fn_expr(
        &mut self,
        span: Span,
        name: Option<IdentID>,
        params: Vec<ParamDeclID>,
        body: GraphID,
    ) -> FnExprID {
        let idx = FnExprID(usize_into_idx(self.fn_expr_nodes.0.len()));
        let id = self.fn_expr_nodes.0.alloc(FnExpr {
            id: idx,
            span,
            name,
            params,
            body,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_call_expr(&mut self, span: Span, callee: Expr, args: Vec<Expr>) -> CallExprID {
        let idx = CallExprID(usize_into_idx(self.call_expr_nodes.0.len()));
        let id = self.call_expr_nodes.0.alloc(CallExpr {
            id: idx,
            span,
            callee,
            args,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_object_lit(&mut self, span: Span, members: Vec<ObjectLitMember>) -> ObjectLitID {
        let idx = ObjectLitID(usize_into_idx(self.object_lit_nodes.0.len()));
        let id = self.object_lit_nodes.0.alloc(ObjectLit {
            id: idx,
            span,
            members,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_spread_assignment(&mut self, span: Span, expr: Expr) -> SpreadAssignmentID {
        let idx = SpreadAssignmentID(usize_into_idx(self.spread_assignment_nodes.0.len()));
        let id = self.spread_assignment_nodes.0.alloc(SpreadAssignment {
            id: idx,
            span,
            expr,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_object_method_member(
        &mut self,
        span: Span,
        name: PropName,
        params: Vec<ParamDeclID>,
        body: BlockStmtID,
    ) -> ObjectMethodMemberID {
        let idx = ObjectMethodMemberID(usize_into_idx(self.object_method_member_nodes.0.len()));
        let id = self.object_method_member_nodes.0.alloc(ObjectMethodMember {
            id: idx,
            span,
            name,
            params,
            body,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_object_prop_member(
        &mut self,
        span: Span,
        name: PropName,
        init: Expr,
    ) -> ObjectPropMemberID {
        let idx = ObjectPropMemberID(usize_into_idx(self.object_prop_member_nodes.0.len()));
        let id = self.object_prop_member_nodes.0.alloc(ObjectPropMember {
            id: idx,
            span,
            name,
            init,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_object_shorthand_member(
        &mut self,
        span: Span,
        name: IdentID,
    ) -> ObjectShorthandMemberID {
        let idx =
            ObjectShorthandMemberID(usize_into_idx(self.object_shorthand_member_nodes.0.len()));
        let id = self
            .object_shorthand_member_nodes
            .0
            .alloc(ObjectShorthandMember {
                id: idx,
                span,
                name,
            });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_array_lit(&mut self, span: Span, elems: Vec<Expr>) -> ArrayLitID {
        let idx = ArrayLitID(usize_into_idx(self.array_lit_nodes.0.len()));
        let id = self.array_lit_nodes.0.alloc(ArrayLit {
            id: idx,
            span,
            elems,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_bool_lit(&mut self, span: Span, val: bool) -> BoolLitID {
        let idx = BoolLitID(usize_into_idx(self.bool_lit_nodes.0.len()));
        let id = self.bool_lit_nodes.0.alloc(BoolLit { id: idx, span, val });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_this_expr(&mut self, span: Span) -> ThisExprID {
        let idx = ThisExprID(usize_into_idx(self.this_expr_nodes.0.len()));
        let id = self.this_expr_nodes.0.alloc(ThisExpr { id: idx, span });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_bin_expr(
        &mut self,
        span: Span,
        left: Expr,
        op: ast::BinOp,
        right: Expr,
    ) -> BinExprID {
        let idx = BinExprID(usize_into_idx(self.bin_expr_nodes.0.len()));
        let id = self.bin_expr_nodes.0.alloc(BinExpr {
            id: idx,
            span,
            left,
            op,
            right,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_labeled_stmt(&mut self, span: Span, label: IdentID, body: Stmt) -> LabeledStmtID {
        let idx = LabeledStmtID(usize_into_idx(self.labeled_stmt_nodes.0.len()));
        let id = self.labeled_stmt_nodes.0.alloc(LabeledStmt {
            id: idx,
            span,
            label,
            body,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_empty_stmt(&mut self, span: Span) -> EmptyStmtID {
        let idx = EmptyStmtID(usize_into_idx(self.empty_stmt_nodes.0.len()));
        let id = self.empty_stmt_nodes.0.alloc(EmptyStmt { id: idx, span });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_do_stmt(&mut self, span: Span, stmt: Stmt, expr: Expr) -> DoStmtID {
        let idx = DoStmtID(usize_into_idx(self.do_stmt_nodes.0.len()));
        let id = self.do_stmt_nodes.0.alloc(DoStmt {
            id: idx,
            span,
            stmt,
            expr,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_while_stmt(&mut self, span: Span, expr: Expr, body: Stmt) -> WhileStmtID {
        let idx = WhileStmtID(usize_into_idx(self.while_stmt_nodes.0.len()));
        let id = self.while_stmt_nodes.0.alloc(WhileStmt {
            id: idx,
            span,
            expr,
            body,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_try_stmt(
        &mut self,
        span: Span,
        try_block: BlockStmtID,
        catch_clause: Option<CatchClauseID>,
        finally_block: Option<BlockStmtID>,
    ) -> TryStmtID {
        let idx = TryStmtID(usize_into_idx(self.try_stmt_nodes.0.len()));
        let id = self.try_stmt_nodes.0.alloc(TryStmt {
            id: idx,
            span,
            try_block,
            catch_clause,
            finally_block,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_catch_clause(
        &mut self,
        span: Span,
        var: Option<VarDeclID>,
        block: BlockStmtID,
    ) -> CatchClauseID {
        let idx = CatchClauseID(usize_into_idx(self.catch_clause_nodes.0.len()));
        let id = self.catch_clause_nodes.0.alloc(CatchClause {
            id: idx,
            span,
            var,
            block,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_export_assign(
        &mut self,
        span: Span,
        modifiers: Option<Modifiers>,
        expr: Expr,
    ) -> ExportAssignID {
        let idx = ExportAssignID(usize_into_idx(self.export_assign_nodes.0.len()));
        let id = self.export_assign_nodes.0.alloc(ExportAssign {
            id: idx,
            span,
            modifiers,
            expr,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_export_decl(&mut self, span: Span, clause: ExportClause) -> ExportDeclID {
        let idx = ExportDeclID(usize_into_idx(self.export_decl_nodes.0.len()));
        let id = self.export_decl_nodes.0.alloc(ExportDecl {
            id: idx,
            span,
            clause,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_specs_export(
        &mut self,
        span: Span,
        list: Vec<ExportSpec>,
        module: Option<StringLitID>,
    ) -> SpecsExportID {
        let idx = SpecsExportID(usize_into_idx(self.specs_export_nodes.0.len()));
        let id = self.specs_export_nodes.0.alloc(SpecsExport {
            id: idx,
            span,
            list,
            module,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_export_named_spec(
        &mut self,
        span: Span,
        prop_name: ModuleExportName,
        name: ModuleExportName,
    ) -> ExportNamedSpecID {
        let idx = ExportNamedSpecID(usize_into_idx(self.export_named_spec_nodes.0.len()));
        let id = self.export_named_spec_nodes.0.alloc(ExportNamedSpec {
            id: idx,
            span,
            prop_name,
            name,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_ns_export(
        &mut self,
        span: Span,
        name: ModuleExportName,
        module: StringLitID,
    ) -> NsExportID {
        let idx = NsExportID(usize_into_idx(self.ns_export_nodes.0.len()));
        let id = self.ns_export_nodes.0.alloc(NsExport {
            id: idx,
            span,
            name,
            module,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_glob_export(&mut self, span: Span, name: StringLitID) -> GlobExportID {
        let idx = GlobExportID(usize_into_idx(self.glob_export_nodes.0.len()));
        let id = self.glob_export_nodes.0.alloc(GlobExport {
            id: idx,
            span,
            name,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_import_decl(
        &mut self,
        span: Span,
        clause: ImportClauseID,
        module: StringLitID,
    ) -> ImportDeclID {
        let idx = ImportDeclID(usize_into_idx(self.import_decl_nodes.0.len()));
        let id = self.import_decl_nodes.0.alloc(ImportDecl {
            id: idx,
            span,
            clause,
            module,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_import_clause(
        &mut self,
        span: Span,
        name: Option<IdentID>,
        kind: Option<ImportClauseKind>,
    ) -> ImportClauseID {
        let idx = ImportClauseID(usize_into_idx(self.import_clause_nodes.0.len()));
        let id = self.import_clause_nodes.0.alloc(ImportClause {
            id: idx,
            span,
            name,
            kind,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_import_named_spec(
        &mut self,
        span: Span,
        prop_name: ModuleExportName,
        name: IdentID,
    ) -> ImportNamedSpecID {
        let idx = ImportNamedSpecID(usize_into_idx(self.import_named_spec_nodes.0.len()));
        let id = self.import_named_spec_nodes.0.alloc(ImportNamedSpec {
            id: idx,
            span,
            prop_name,
            name,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_shorthand_spec(&mut self, span: Span, name: IdentID) -> ShorthandSpecID {
        let idx = ShorthandSpecID(usize_into_idx(self.shorthand_spec_nodes.0.len()));
        let id = self.shorthand_spec_nodes.0.alloc(ShorthandSpec {
            id: idx,
            span,
            name,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_ns_import(&mut self, span: Span, name: IdentID) -> NsImportID {
        let idx = NsImportID(usize_into_idx(self.ns_import_nodes.0.len()));
        let id = self.ns_import_nodes.0.alloc(NsImport {
            id: idx,
            span,
            name,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_enum_decl(
        &mut self,
        span: Span,
        modifiers: Option<Modifiers>,
        name: IdentID,
        members: Vec<EnumMemberID>,
    ) -> EnumDeclID {
        let idx = EnumDeclID(usize_into_idx(self.enum_decl_nodes.0.len()));
        let id = self.enum_decl_nodes.0.alloc(EnumDecl {
            id: idx,
            span,
            modifiers,
            name,
            members,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_enum_member(
        &mut self,
        span: Span,
        name: PropName,
        init: Option<Expr>,
    ) -> EnumMemberID {
        let idx = EnumMemberID(usize_into_idx(self.enum_member_nodes.0.len()));
        let id = self.enum_member_nodes.0.alloc(EnumMember {
            id: idx,
            span,
            name,
            init,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_throw_stmt(&mut self, span: Span, expr: Expr) -> ThrowStmtID {
        let idx = ThrowStmtID(usize_into_idx(self.throw_stmt_nodes.0.len()));
        let id = self.throw_stmt_nodes.0.alloc(ThrowStmt {
            id: idx,
            span,
            expr,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_module_decl(
        &mut self,
        span: Span,
        modifiers: Option<Modifiers>,
        name: ModuleName,
        block: ModuleBlockID,
    ) -> ModuleDeclID {
        let idx = ModuleDeclID(usize_into_idx(self.module_decl_nodes.0.len()));
        let id = self.module_decl_nodes.0.alloc(ModuleDecl {
            id: idx,
            span,
            modifiers,
            name,
            block,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_module_block(&mut self, span: Span, stmts: Vec<Stmt>) -> ModuleBlockID {
        let idx = ModuleBlockID(usize_into_idx(self.module_block_nodes.0.len()));
        let id = self.module_block_nodes.0.alloc(ModuleBlock {
            id: idx,
            span,
            stmts,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_expr_stmt(&mut self, span: Span, expr: Expr) -> ExprStmtID {
        let idx = ExprStmtID(usize_into_idx(self.expr_stmt_nodes.0.len()));
        let id = self.expr_stmt_nodes.0.alloc(ExprStmt {
            id: idx,
            span,
            expr,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_class_static_block(
        &mut self,
        span: Span,
        body: BlockStmtID,
    ) -> ClassStaticBlockID {
        let idx = ClassStaticBlockID(usize_into_idx(self.class_static_block_nodes.0.len()));
        let id = self.class_static_block_nodes.0.alloc(ClassStaticBlock {
            id: idx,
            span,
            body,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_setter_decl(
        &mut self,
        span: Span,
        modifiers: Option<Modifiers>,
        name: PropName,
        params: Vec<ParamDeclID>,
        body: BlockStmtID,
    ) -> SetterDeclID {
        let idx = SetterDeclID(usize_into_idx(self.setter_decl_nodes.0.len()));
        let id = self.setter_decl_nodes.0.alloc(SetterDecl {
            id: idx,
            span,
            modifiers,
            name,
            params,
            body,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_getter_decl(
        &mut self,
        span: Span,
        modifiers: Option<Modifiers>,
        name: PropName,
        body: BlockStmtID,
    ) -> GetterDeclID {
        let idx = GetterDeclID(usize_into_idx(self.getter_decl_nodes.0.len()));
        let id = self.getter_decl_nodes.0.alloc(GetterDecl {
            id: idx,
            span,
            modifiers,
            name,
            body,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_class_method_elem(
        &mut self,
        span: Span,
        modifiers: Option<Modifiers>,
        name: PropName,
        params: Vec<ParamDeclID>,
        body: BlockStmtID,
    ) -> ClassMethodElemID {
        let idx = ClassMethodElemID(usize_into_idx(self.class_method_elem_nodes.0.len()));
        let id = self.class_method_elem_nodes.0.alloc(ClassMethodElem {
            id: idx,
            span,
            modifiers,
            name,
            params,
            body,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_class_prop_elem(
        &mut self,
        span: Span,
        modifiers: Option<Modifiers>,
        name: PropName,
        init: Option<Expr>,
    ) -> ClassPropElemID {
        let idx = ClassPropElemID(usize_into_idx(self.class_prop_elem_nodes.0.len()));
        let id = self.class_prop_elem_nodes.0.alloc(ClassPropElem {
            id: idx,
            span,
            modifiers,
            name,
            init,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_class_extends_clause(&mut self, span: Span, expr: Expr) -> ClassExtendsClauseID {
        let idx = ClassExtendsClauseID(usize_into_idx(self.class_extends_clause_nodes.0.len()));
        let id = self.class_extends_clause_nodes.0.alloc(ClassExtendsClause {
            id: idx,
            span,
            expr,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_class_decl(
        &mut self,
        span: Span,
        modifiers: Option<Modifiers>,
        name: IdentID,
        extends: Option<ClassExtendsClauseID>,
        elems: Vec<ClassElem>,
    ) -> ClassDeclID {
        let idx = ClassDeclID(usize_into_idx(self.class_decl_nodes.0.len()));
        let id = self.class_decl_nodes.0.alloc(ClassDecl {
            id: idx,
            span,
            modifiers,
            name: Some(name),
            extends,
            elems,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_class_ctor(
        &mut self,
        span: Span,
        params: Vec<ParamDeclID>,
        body: BlockStmtID,
    ) -> ClassCtorID {
        let idx = ClassCtorID(usize_into_idx(self.class_ctor_nodes.0.len()));
        let id = self.class_ctor_nodes.0.alloc(ClassCtor {
            id: idx,
            span,
            params,
            body,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_fn_decl(
        &mut self,
        span: Span,
        modifiers: Option<Modifiers>,
        name: IdentID,
        params: Vec<ParamDeclID>,
        body: GraphID,
    ) -> FnDeclID {
        let idx = FnDeclID(usize_into_idx(self.fn_decl_nodes.0.len()));
        let id = self.fn_decl_nodes.0.alloc(FnDecl {
            id: idx,
            span,
            modifiers,
            name,
            params,
            body,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_param_decl(
        &mut self,
        span: Span,
        modifiers: Option<Modifiers>,
        dotdotdot: Option<Span>,
        name: Binding,
        question: Option<Span>,
        init: Option<Expr>,
    ) -> ParamDeclID {
        let idx = ParamDeclID(usize_into_idx(self.param_decl_nodes.0.len()));
        let id = self.param_decl_nodes.0.alloc(ParamDecl {
            id: idx,
            span,
            modifiers,
            dotdotdot,
            name,
            question,
            init,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_modifier(&mut self, span: Span, kind: ast::ModifierKind) -> ModifierID {
        let idx = ModifierID(usize_into_idx(self.modifier_nodes.0.len()));
        let id = self.modifier_nodes.0.alloc(Modifier {
            id: idx,
            span,
            kind,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_block_stmt(&mut self, span: Span, stmts: Vec<Stmt>) -> BlockStmtID {
        let idx = BlockStmtID(usize_into_idx(self.block_stmt_nodes.0.len()));
        let id = self.block_stmt_nodes.0.alloc(BlockStmt {
            id: idx,
            span,
            stmts,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_ret_stmt(&mut self, span: Span, expr: Option<Expr>) -> RetStmtID {
        let idx = RetStmtID(usize_into_idx(self.ret_stmt_nodes.0.len()));
        let id = self.ret_stmt_nodes.0.alloc(RetStmt {
            id: idx,
            span,
            expr,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_continue_stmt(&mut self, span: Span, label: Option<IdentID>) -> ContinueStmtID {
        let idx = ContinueStmtID(usize_into_idx(self.continue_stmt_nodes.0.len()));
        let id = self.continue_stmt_nodes.0.alloc(ContinueStmt {
            id: idx,
            span,
            label,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_break_stmt(&mut self, span: Span, label: Option<IdentID>) -> BreakStmtID {
        let idx = BreakStmtID(usize_into_idx(self.break_stmt_nodes.0.len()));
        let id = self.break_stmt_nodes.0.alloc(BreakStmt {
            id: idx,
            span,
            label,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_for_in_stmt(
        &mut self,
        span: Span,
        init: ForInit,
        expr: Expr,
        body: Stmt,
    ) -> ForInStmtID {
        let idx = ForInStmtID(usize_into_idx(self.for_in_stmt_nodes.0.len()));
        let id = self.for_in_stmt_nodes.0.alloc(ForInStmt {
            id: idx,
            span,
            init,
            expr,
            body,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_for_of_stmt(
        &mut self,
        span: Span,
        r#await: Option<Span>,
        init: ForInit,
        expr: Expr,
        body: Stmt,
    ) -> ForOfStmtID {
        let idx = ForOfStmtID(usize_into_idx(self.for_of_stmt_nodes.0.len()));
        let id = self.for_of_stmt_nodes.0.alloc(ForOfStmt {
            id: idx,
            span,
            r#await,
            init,
            expr,
            body,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_for_stmt(
        &mut self,
        span: Span,
        init: Option<ForInit>,
        cond: Option<Expr>,
        incr: Option<Expr>,
        body: Stmt,
    ) -> ForStmtID {
        let idx = ForStmtID(usize_into_idx(self.for_stmt_nodes.0.len()));
        let id = self.for_stmt_nodes.0.alloc(ForStmt {
            id: idx,
            span,
            init,
            cond,
            incr,
            body,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_if_stmt(
        &mut self,
        span: Span,
        expr: Expr,
        then: Stmt,
        else_then: Option<Stmt>,
    ) -> IfStmtID {
        let idx = IfStmtID(usize_into_idx(self.if_stmt_nodes.0.len()));
        let id = self.if_stmt_nodes.0.alloc(IfStmt {
            id: idx,
            span,
            expr,
            then,
            else_then,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_var_stmt(
        &mut self,
        span: Span,
        modifiers: Option<Modifiers>,
        decls: Vec<VarDeclID>,
    ) -> VarStmtID {
        let idx = VarStmtID(usize_into_idx(self.var_stmt_nodes.0.len()));
        let id = self.var_stmt_nodes.0.alloc(VarStmt {
            id: idx,
            span,
            modifiers,
            decls,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_var_decl(&mut self, span: Span, name: Binding, init: Option<Expr>) -> VarDeclID {
        let idx = VarDeclID(usize_into_idx(self.var_decl_nodes.0.len()));
        let id = self.var_decl_nodes.0.alloc(VarDecl {
            id: idx,
            span,
            name,
            init,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_ident(&mut self, ty: TyID, span: Span, name: Atom) -> IdentID {
        let idx = IdentID(usize_into_idx(self.ident_nodes.0.len()));
        let id = self.ident_nodes.0.alloc(Ident {
            id: idx,
            ty,
            span,
            name,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_object_pat(&mut self, span: Span, elems: Vec<ObjectBindingElemID>) -> ObjectPatID {
        let idx = ObjectPatID(usize_into_idx(self.object_pat_nodes.0.len()));
        let id = self.object_pat_nodes.0.alloc(ObjectPat {
            id: idx,
            span,
            elems,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_string_lit(&mut self, span: Span, val: Atom, is_template: bool) -> StringLitID {
        let idx = StringLitID(usize_into_idx(self.string_lit_nodes.0.len()));
        let id = self.string_lit_nodes.0.alloc(StringLit {
            id: idx,
            span,
            val,
            is_template,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_num_lit(&mut self, span: Span, val: f64) -> NumLitID {
        let idx = NumLitID(usize_into_idx(self.num_lit_nodes.0.len()));
        let id = self.num_lit_nodes.0.alloc(NumLit { id: idx, span, val });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_bigint_lit(&mut self, span: Span, pos: bool, val: Atom) -> BigIntLitID {
        let idx = BigIntLitID(usize_into_idx(self.bigint_lit_nodes.0.len()));
        let id = self.bigint_lit_nodes.0.alloc(BigIntLit {
            id: idx,
            span,
            val: (pos, val),
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_null_lit(&mut self, span: Span) -> NullLitID {
        let idx = NullLitID(usize_into_idx(self.null_lit_nodes.0.len()));
        let id = self.null_lit_nodes.0.alloc(NullLit { id: idx, span });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_regexp_lit(&mut self, span: Span, val: Atom) -> RegExpLitID {
        let idx = RegExpLitID(usize_into_idx(self.regexp_lit_nodes.0.len()));
        let id = self
            .regexp_lit_nodes
            .0
            .alloc(RegExpLit { id: idx, span, val });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_computed_prop_name(&mut self, span: Span, expr: Expr) -> ComputedPropNameID {
        let idx = ComputedPropNameID(usize_into_idx(self.computed_prop_name_nodes.0.len()));
        let id = self.computed_prop_name_nodes.0.alloc(ComputedPropName {
            id: idx,
            span,
            expr,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_object_binding_elem(
        &mut self,
        span: Span,
        name: ObjectBindingName,
        init: Option<Expr>,
    ) -> ObjectBindingElemID {
        let idx = ObjectBindingElemID(usize_into_idx(self.object_binding_elem_nodes.0.len()));
        let id = self.object_binding_elem_nodes.0.alloc(ObjectBindingElem {
            id: idx,
            span,
            dotdotdot: None, // This will be set later if needed
            name,
            init,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_array_pat(&mut self, span: Span, elems: Vec<ArrayBindingElem>) -> ArrayPatID {
        let idx = ArrayPatID(usize_into_idx(self.array_pat_nodes.0.len()));
        let id = self.array_pat_nodes.0.alloc(ArrayPat {
            id: idx,
            span,
            elems,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_array_binding(
        &mut self,
        span: Span,
        dotdotdot: Option<Span>,
        name: Binding,
        init: Option<Expr>,
    ) -> ArrayBindingID {
        let idx = ArrayBindingID(usize_into_idx(self.array_binding_nodes.0.len()));
        let id = self.array_binding_nodes.0.alloc(ArrayBinding {
            id: idx,
            span,
            dotdotdot,
            name,
            init,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_omit_expr(&mut self, span: Span) -> OmitExprID {
        let idx = OmitExprID(usize_into_idx(self.omit_expr_nodes.0.len()));
        let id = self.omit_expr_nodes.0.alloc(OmitExpr { id: idx, span });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_paren_expr(&mut self, span: Span, expr: Expr) -> ParenExprID {
        let idx = ParenExprID(usize_into_idx(self.paren_expr_nodes.0.len()));
        let id = self.paren_expr_nodes.0.alloc(ParenExpr {
            id: idx,
            span,
            expr,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_assign_expr(
        &mut self,
        span: Span,
        left: Expr,
        op: ast::AssignOp,
        right: Expr,
    ) -> AssignExprID {
        let idx = AssignExprID(usize_into_idx(self.assign_expr_nodes.0.len()));
        let id = self.assign_expr_nodes.0.alloc(AssignExpr {
            id: idx,
            span,
            left,
            op,
            right,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }

    pub fn alloc_cond_expr(
        &mut self,
        span: Span,
        cond: Expr,
        when_true: Expr,
        when_false: Expr,
    ) -> CondExprID {
        let idx = CondExprID(usize_into_idx(self.cond_expr_nodes.0.len()));
        let id = self.cond_expr_nodes.0.alloc(CondExpr {
            id: idx,
            span,
            cond,
            when_true,
            when_false,
        });
        debug_assert_eq!(id, idx.0);
        idx
    }
}

#[derive(Debug)]
pub struct JsxExpr {
    id: JsxExprID,
    span: Span,
    dotdotdot: Option<Span>,
    expr: Option<Expr>,
}

impl JsxExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn dotdotdot(&self) -> Option<Span> {
        self.dotdotdot
    }

    pub fn expr(&self) -> Option<Expr> {
        self.expr
    }
}

#[derive(Debug)]
pub struct JsxText {
    id: JsxTextID,
    span: Span,
    text: Atom,
    contains_only_trivia_whitespace: bool,
}

impl JsxText {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn text(&self) -> Atom {
        self.text
    }

    pub fn contains_only_trivia_whitespace(&self) -> bool {
        self.contains_only_trivia_whitespace
    }
}

#[derive(Debug)]
pub struct JsxSelfClosingElem {
    id: JsxSelfClosingElemID,
    span: Span,
    tag_name: JsxTagName,
    attrs: Vec<JsxAttr>,
}

impl JsxSelfClosingElem {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn tag_name(&self) -> JsxTagName {
        self.tag_name
    }

    pub fn attrs(&self) -> &[JsxAttr] {
        &self.attrs
    }
}

#[derive(Debug)]
pub struct JsxClosingFrag {
    id: JsxClosingFragID,
    span: Span,
}

#[derive(Debug)]
pub struct JsxOpeningFrag {
    id: JsxOpeningFragID,
    span: Span,
}

#[derive(Debug)]
pub struct JsxFrag {
    id: JsxFragID,
    span: Span,
    opening_frag: JsxOpeningFragID,
    children: Vec<JsxChild>,
    closing_frag: JsxClosingFragID,
}

impl JsxFrag {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn children(&self) -> &[JsxChild] {
        &self.children
    }

    pub fn opening_frag(&self) -> JsxOpeningFragID {
        self.opening_frag
    }

    pub fn closing_frag(&self) -> JsxClosingFragID {
        self.closing_frag
    }
}

#[derive(Debug)]
pub struct JsxNsName {
    id: JsxNsNameID,
    span: Span,
    ns: IdentID,
    name: IdentID,
}

impl JsxNsName {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn ns(&self) -> IdentID {
        self.ns
    }

    pub fn name(&self) -> IdentID {
        self.name
    }
}

#[derive(Debug)]
pub struct JsxNamedAttr {
    id: JsxNamedAttrID,
    span: Span,
    name: JsxAttrName,
    init: Option<JsxAttrValue>,
}

impl JsxNamedAttr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> JsxAttrName {
        self.name
    }

    pub fn init(&self) -> Option<JsxAttrValue> {
        self.init
    }
}

#[derive(Debug, Clone, Copy)]
pub enum JsxAttrValue {
    StringLit(StringLitID),
    Expr(JsxExprID),
    Ele(JsxElemID),
    SelfClosingEle(JsxSelfClosingElemID),
    Frag(JsxFragID),
}

#[derive(Debug, Clone, Copy)]
pub enum JsxAttrName {
    Ident(IdentID),
    Ns(JsxNsNameID),
}

#[derive(Debug)]
pub struct JsxSpreadAttr {
    id: JsxSpreadAttrID,
    span: Span,
    expr: Expr,
}

impl JsxSpreadAttr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }
}

#[derive(Debug, Clone, Copy)]
pub enum JsxTagName {
    Ident(IdentID),
    This(ThisExprID),
    Ns(JsxNsNameID),
    PropAccess(PropAccessExprID),
}

#[derive(Debug, Clone, Copy)]
pub enum JsxChild {
    Text(JsxTextID),
    Expr(JsxExprID),
    Elem(JsxElemID),
    SelfClosingEle(JsxSelfClosingElemID),
    Frag(JsxFragID),
}

pub struct JsxClosingElem {
    id: JsxClosingElemID,
    span: Span,
    tag_name: JsxTagName,
}

impl JsxClosingElem {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn tag_name(&self) -> JsxTagName {
        self.tag_name
    }
}

#[derive(Debug, Clone, Copy)]
pub enum JsxAttr {
    Spread(JsxSpreadAttrID),
    Named(JsxNamedAttrID),
}

#[derive(Debug)]
pub struct JsxOpeningElem {
    id: JsxOpeningElemID,
    span: Span,
    tag_name: JsxTagName,
    attrs: Vec<JsxAttr>,
}

impl JsxOpeningElem {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn tag_name(&self) -> JsxTagName {
        self.tag_name
    }

    pub fn attrs(&self) -> &[JsxAttr] {
        &self.attrs
    }
}

#[derive(Debug)]
pub struct JsxElem {
    id: JsxElemID,
    span: Span,
    opening_elem: JsxOpeningElemID,
    children: Vec<JsxChild>,
    closing_elem: JsxClosingElemID,
}

impl JsxElem {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn children(&self) -> &[JsxChild] {
        &self.children
    }

    pub fn opening_elem(&self) -> JsxOpeningElemID {
        self.opening_elem
    }

    pub fn closing_elem(&self) -> JsxClosingElemID {
        self.closing_elem
    }
}

#[derive(Debug)]
pub struct ComputedPropName {
    id: ComputedPropNameID,
    span: Span,
    expr: Expr,
}

impl ComputedPropName {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }
}

#[derive(Debug)]
pub struct SuperExpr {
    id: SuperExprID,
    span: Span,
}

#[derive(Debug)]
pub struct VoidExpr {
    id: VoidExprID,
    span: Span,
    expr: Expr,
}

impl VoidExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }
}

#[derive(Debug)]
pub struct TypeofExpr {
    id: TypeofExprID,
    span: Span,
    expr: Expr,
}

impl TypeofExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }
}

#[derive(Debug)]
pub struct ThisExpr {
    id: ThisExprID,
    span: Span,
}

#[derive(Debug)]
pub struct EleAccessExpr {
    id: EleAccessExprID,
    span: Span,
    expr: Expr,
    arg: Expr,
}

impl EleAccessExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }

    pub fn arg(&self) -> Expr {
        self.arg
    }
}

#[derive(Debug)]
pub struct PropAccessExpr {
    id: PropAccessExprID,
    span: Span,
    expr: Expr,
    question_dot: Option<Span>,
    name: IdentID,
}

impl PropAccessExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }

    pub fn question_dot(&self) -> Option<Span> {
        self.question_dot
    }

    pub fn name(&self) -> IdentID {
        self.name
    }
}

#[derive(Debug)]
pub struct PostfixUnaryExpr {
    id: PostfixUnaryExprID,
    span: Span,
    expr: Expr,
    op: ast::PostfixUnaryOp,
}

impl PostfixUnaryExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn op(&self) -> ast::PostfixUnaryOp {
        self.op
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }
}

#[derive(Debug)]
pub struct PrefixUnaryExpr {
    id: PrefixUnaryExprID,
    span: Span,
    op: ast::PrefixUnaryOp,
    expr: Expr,
}

impl PrefixUnaryExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn op(&self) -> ast::PrefixUnaryOp {
        self.op
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }
}

#[derive(Debug)]
pub struct TaggedTemplateExpr {
    id: TaggedTemplateExprID,
    span: Span,
    tag: Expr,
    tpl: Expr,
}

impl TaggedTemplateExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn tag(&self) -> Expr {
        self.tag
    }

    pub fn tpl(&self) -> Expr {
        self.tpl
    }
}

#[derive(Debug)]
pub struct TemplateExpr {
    id: TemplateExprID,
    span: Span,
    head: TemplateHeadID,
    spans: Vec<TemplateSpanID>,
}

impl TemplateExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn head(&self) -> TemplateHeadID {
        self.head
    }

    pub fn spans(&self) -> &[TemplateSpanID] {
        &self.spans
    }
}

#[derive(Debug)]
pub struct TemplateHead {
    id: TemplateHeadID,
    span: Span,
    text: Atom,
}

impl TemplateHead {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn text(&self) -> Atom {
        self.text
    }
}

#[derive(Debug)]
pub struct TemplateSpan {
    id: TemplateSpanID,
    span: Span,
    expr: Expr,
    text: Atom,
    is_tail: bool,
}

impl TemplateSpan {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }

    pub fn text(&self) -> Atom {
        self.text
    }
}

#[derive(Debug)]
pub struct SpreadElement {
    id: SpreadElementID,
    span: Span,
    expr: Expr,
}

impl SpreadElement {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }
}

#[derive(Debug)]
pub struct ArrowFnExpr {
    id: ArrowFnExprID,
    span: Span,
    params: Vec<ParamDeclID>,
    body: GraphID,
}

impl ArrowFnExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn params(&self) -> &[ParamDeclID] {
        &self.params
    }

    pub fn body(&self) -> GraphID {
        self.body
    }
}

#[derive(Debug)]
pub struct AssignExpr {
    id: AssignExprID,
    span: Span,
    left: Expr,
    op: ast::AssignOp,
    right: Expr,
}

impl AssignExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn left(&self) -> Expr {
        self.left
    }

    pub fn op(&self) -> ast::AssignOp {
        self.op
    }

    pub fn right(&self) -> Expr {
        self.right
    }
}

#[derive(Debug)]
pub struct NewExpr {
    id: NewExprID,
    span: Span,
    expr: Expr,
    args: Vec<Expr>,
}

impl NewExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }

    pub fn args(&self) -> &[Expr] {
        &self.args
    }
}

pub struct ClassExpr {
    id: ClassExprID,
    span: Span,
    name: Option<IdentID>,
    extends: Option<ClassExtendsClauseID>,
    elems: Vec<ClassElem>,
}

impl ClassExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> Option<IdentID> {
        self.name
    }

    pub fn extends(&self) -> Option<ClassExtendsClauseID> {
        self.extends
    }

    pub fn elems(&self) -> &[ClassElem] {
        &self.elems
    }
}

#[derive(Debug)]
pub struct FnExpr {
    id: FnExprID,
    span: Span,
    name: Option<IdentID>,
    params: Vec<ParamDeclID>,
    body: GraphID,
}

impl FnExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> Option<IdentID> {
        self.name
    }

    pub fn params(&self) -> &[ParamDeclID] {
        &self.params
    }

    pub fn body(&self) -> GraphID {
        self.body
    }
}

#[derive(Debug)]
pub struct CallExpr {
    id: CallExprID,
    span: Span,
    callee: Expr,
    args: Vec<Expr>,
}

impl CallExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn callee(&self) -> Expr {
        self.callee
    }

    pub fn args(&self) -> &[Expr] {
        &self.args
    }
}

#[derive(Debug)]
pub struct SpreadAssignment {
    id: SpreadAssignmentID,
    span: Span,
    expr: Expr,
}

impl SpreadAssignment {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }
}

#[derive(Debug)]
pub struct ObjectMethodMember {
    id: ObjectMethodMemberID,
    span: Span,
    name: PropName,
    params: Vec<ParamDeclID>,
    body: BlockStmtID,
}

impl ObjectMethodMember {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> PropName {
        self.name
    }

    pub fn params(&self) -> &[ParamDeclID] {
        &self.params
    }

    pub fn body(&self) -> BlockStmtID {
        self.body
    }
}

#[derive(Debug)]
pub struct ObjectPropMember {
    id: ObjectPropMemberID,
    span: Span,
    name: PropName,
    init: Expr,
}

impl ObjectPropMember {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> PropName {
        self.name
    }

    pub fn init(&self) -> Expr {
        self.init
    }
}

#[derive(Debug)]
pub struct ObjectShorthandMember {
    id: ObjectShorthandMemberID,
    span: Span,
    name: IdentID,
}

impl ObjectShorthandMember {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> IdentID {
        self.name
    }
}

#[derive(Debug)]
pub struct CondExpr {
    id: CondExprID,
    span: Span,
    cond: Expr,
    when_true: Expr,
    when_false: Expr,
}

impl CondExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn cond(&self) -> Expr {
        self.cond
    }

    pub fn when_true(&self) -> Expr {
        self.when_true
    }

    pub fn when_false(&self) -> Expr {
        self.when_false
    }
}

#[derive(Debug)]
pub struct ArrayLit {
    id: ArrayLitID,
    span: Span,
    elems: Vec<Expr>,
}

impl ArrayLit {
    pub fn span(&self) -> Span {
        self.span
    }
    pub fn elems(&self) -> &[Expr] {
        &self.elems
    }
}

#[derive(Debug)]
pub struct ObjectLit {
    id: ObjectLitID,
    span: Span,
    members: Vec<ObjectLitMember>,
}

impl ObjectLit {
    pub fn span(&self) -> Span {
        self.span
    }
    pub fn members(&self) -> &[ObjectLitMember] {
        &self.members
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ObjectLitMember {
    Method(ObjectMethodMemberID),
    Prop(ObjectPropMemberID),
    Shorthand(ObjectShorthandMemberID),
    SpreadAssignment(SpreadAssignmentID),
    Getter(GetterDeclID),
    Setter(SetterDeclID),
}

#[derive(Debug)]
pub struct BinExpr {
    id: BinExprID,
    span: Span,
    left: Expr,
    op: ast::BinOp,
    right: Expr,
}

impl BinExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn left(&self) -> Expr {
        self.left
    }

    pub fn op(&self) -> ast::BinOp {
        self.op
    }

    pub fn right(&self) -> Expr {
        self.right
    }
}

#[derive(Debug)]
pub struct OmitExpr {
    id: OmitExprID,
    span: Span,
}

#[derive(Debug)]
pub struct ParenExpr {
    id: ParenExprID,
    span: Span,
    expr: Expr,
}

impl ParenExpr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }
}

#[derive(Debug)]
pub struct ObjectPat {
    id: ObjectPatID,
    span: Span,
    elems: Vec<ObjectBindingElemID>,
}

impl ObjectPat {
    pub fn span(&self) -> Span {
        self.span
    }
    pub fn elems(&self) -> &[ObjectBindingElemID] {
        &self.elems
    }
}

#[derive(Debug)]
pub struct ObjectBindingElem {
    id: ObjectBindingElemID,
    span: Span,
    dotdotdot: Option<Span>,
    name: ObjectBindingName,
    init: Option<Expr>,
}

impl ObjectBindingElem {
    pub fn span(&self) -> Span {
        self.span
    }
    pub fn dotdotdot(&self) -> Option<Span> {
        self.dotdotdot
    }
    pub fn name(&self) -> ObjectBindingName {
        self.name
    }
    pub fn init(&self) -> Option<Expr> {
        self.init
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ObjectBindingName {
    Shorthand(IdentID),
    Prop { prop_name: PropName, name: Binding },
}

#[derive(Debug)]
pub struct ArrayPat {
    id: ArrayPatID,
    span: Span,
    elems: Vec<ArrayBindingElem>,
}
impl ArrayPat {
    pub fn span(&self) -> Span {
        self.span
    }
    pub fn elems(&self) -> &[ArrayBindingElem] {
        &self.elems
    }
}

#[derive(Debug)]
pub enum ArrayBindingElem {
    Omit(OmitExprID),
    Binding(ArrayBindingID),
}

pub struct ArrayBinding {
    id: ArrayBindingID,
    span: Span,
    dotdotdot: Option<Span>,
    name: Binding,
    init: Option<Expr>,
}

impl ArrayBinding {
    pub fn dotdotdot(&self) -> Option<Span> {
        self.dotdotdot
    }
    pub fn name(&self) -> Binding {
        self.name
    }
    pub fn init(&self) -> Option<Expr> {
        self.init
    }
}

#[derive(Debug)]
pub struct LabeledStmt {
    id: LabeledStmtID,
    span: Span,
    label: IdentID,
    body: Stmt,
}

impl LabeledStmt {
    pub fn span(&self) -> Span {
        self.span
    }
    pub fn label(&self) -> IdentID {
        self.label
    }
    pub fn body(&self) -> Stmt {
        self.body
    }
}

#[derive(Debug)]
pub struct ExprStmt {
    id: ExprStmtID,
    span: Span,
    expr: Expr,
}
impl ExprStmt {
    pub fn span(&self) -> Span {
        self.span
    }
    pub fn expr(&self) -> Expr {
        self.expr
    }
}

#[derive(Debug)]
pub struct EmptyStmt {
    id: EmptyStmtID,
    span: Span,
}

#[derive(Debug)]
pub struct TryStmt {
    id: TryStmtID,
    span: Span,
    try_block: BlockStmtID,
    catch_clause: Option<CatchClauseID>,
    finally_block: Option<BlockStmtID>,
}

impl TryStmt {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn try_block(&self) -> BlockStmtID {
        self.try_block
    }

    pub fn catch_clause(&self) -> Option<CatchClauseID> {
        self.catch_clause
    }

    pub fn finally_block(&self) -> Option<BlockStmtID> {
        self.finally_block
    }
}

#[derive(Debug)]
pub struct CatchClause {
    id: CatchClauseID,
    span: Span,
    var: Option<VarDeclID>,
    block: BlockStmtID,
}

impl CatchClause {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn var(&self) -> Option<VarDeclID> {
        self.var
    }

    pub fn block(&self) -> BlockStmtID {
        self.block
    }
}

#[derive(Debug)]
pub struct ContinueStmt {
    id: ContinueStmtID,
    span: Span,
    label: Option<IdentID>,
}

impl ContinueStmt {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn label(&self) -> Option<IdentID> {
        self.label
    }
}

#[derive(Debug)]
pub struct BreakStmt {
    id: BreakStmtID,
    span: Span,
    label: Option<IdentID>,
}

impl BreakStmt {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn label(&self) -> Option<IdentID> {
        self.label
    }
}

#[derive(Debug)]
pub struct DoStmt {
    id: DoStmtID,
    span: Span,
    stmt: Stmt,
    expr: Expr,
}

impl DoStmt {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn stmt(&self) -> Stmt {
        self.stmt
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }
}

#[derive(Debug)]
pub struct WhileStmt {
    id: WhileStmtID,
    span: Span,
    expr: Expr,
    body: Stmt,
}

impl WhileStmt {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }

    pub fn body(&self) -> Stmt {
        self.body
    }
}

#[derive(Debug)]
pub struct ForInStmt {
    id: ForInStmtID,
    span: Span,
    init: ForInit,
    expr: Expr,
    body: Stmt,
}

impl ForInStmt {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn init(&self) -> &ForInit {
        &self.init
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }

    pub fn body(&self) -> Stmt {
        self.body
    }
}

#[derive(Debug)]
pub struct ForOfStmt {
    id: ForOfStmtID,
    span: Span,
    r#await: Option<Span>,
    init: ForInit,
    expr: Expr,
    body: Stmt,
}

impl ForOfStmt {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn r#await(&self) -> Option<Span> {
        self.r#await
    }

    pub fn init(&self) -> &ForInit {
        &self.init
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }

    pub fn body(&self) -> Stmt {
        self.body
    }
}

#[derive(Debug)]
pub struct ForStmt {
    id: ForStmtID,
    span: Span,
    init: Option<ForInit>,
    cond: Option<Expr>,
    incr: Option<Expr>,
    body: Stmt,
}

impl ForStmt {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn init(&self) -> Option<&ForInit> {
        self.init.as_ref()
    }

    pub fn cond(&self) -> Option<Expr> {
        self.cond
    }

    pub fn incr(&self) -> Option<Expr> {
        self.incr
    }

    pub fn body(&self) -> Stmt {
        self.body
    }
}

#[derive(Debug)]
pub enum ForInit {
    Var(Vec<VarDeclID>),
    Expr(Expr),
}

#[derive(Debug)]
pub struct ExportAssign {
    id: ExportAssignID,
    span: Span,
    modifiers: Option<Modifiers>,
    expr: Expr,
}

impl ExportAssign {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }
}

#[derive(Debug)]
pub struct ExportDecl {
    id: ExportDeclID,
    span: Span,
    clause: ExportClause,
}

impl ExportDecl {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn clause(&self) -> ExportClause {
        self.clause
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExportClause {
    Glob(GlobExportID),
    Ns(NsExportID),
    Specs(SpecsExportID),
}

#[derive(Debug)]
pub struct ImportDecl {
    id: ImportDeclID,
    span: Span,
    clause: ImportClauseID,
    module: StringLitID,
}

impl ImportDecl {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn clause(&self) -> ImportClauseID {
        self.clause
    }

    pub fn module(&self) -> StringLitID {
        self.module
    }
}

#[derive(Debug)]
pub struct ImportClause {
    id: ImportClauseID,
    span: Span,
    name: Option<IdentID>,
    kind: Option<ImportClauseKind>,
}

impl ImportClause {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> Option<IdentID> {
        self.name
    }

    pub fn kind(&self) -> Option<&ImportClauseKind> {
        self.kind.as_ref()
    }
}

#[derive(Debug)]
pub enum ImportClauseKind {
    Ns(NsImportID),
    Specs(Vec<ImportSpec>),
}

#[derive(Debug, Clone, Copy)]
pub enum ImportSpec {
    Named(ImportNamedSpecID),
    Shorthand(ShorthandSpecID),
}

#[derive(Debug)]
pub struct ImportNamedSpec {
    id: ImportNamedSpecID,
    span: Span,
    prop_name: ModuleExportName,
    name: IdentID,
}

impl ImportNamedSpec {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn prop_name(&self) -> ModuleExportName {
        self.prop_name
    }

    pub fn name(&self) -> IdentID {
        self.name
    }
}

#[derive(Debug)]
pub struct ExportNamedSpec {
    id: ExportNamedSpecID,
    span: Span,
    prop_name: ModuleExportName,
    name: ModuleExportName,
}

impl ExportNamedSpec {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn prop_name(&self) -> ModuleExportName {
        self.prop_name
    }

    pub fn name(&self) -> ModuleExportName {
        self.name
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExportSpec {
    Shorthand(ShorthandSpecID),
    Named(ExportNamedSpecID),
}

#[derive(Debug)]
pub struct SpecsExport {
    id: SpecsExportID,
    span: Span,
    list: Vec<ExportSpec>,
    module: Option<StringLitID>,
}

impl SpecsExport {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn list(&self) -> &[ExportSpec] {
        &self.list
    }

    pub fn module(&self) -> Option<StringLitID> {
        self.module
    }
}

#[derive(Debug)]
pub struct GlobExport {
    id: GlobExportID,
    span: Span,
    name: StringLitID,
}

impl GlobExport {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> StringLitID {
        self.name
    }
}

#[derive(Debug)]
pub struct NsExport {
    id: NsExportID,
    span: Span,
    name: ModuleExportName,
    module: StringLitID,
}

impl NsExport {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> ModuleExportName {
        self.name
    }

    pub fn module(&self) -> StringLitID {
        self.module
    }
}

#[derive(Debug)]
pub struct NsImport {
    id: NsImportID,
    span: Span,
    name: IdentID,
}

impl NsImport {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> IdentID {
        self.name
    }
}

#[derive(Debug)]
pub struct ShorthandSpec {
    id: ShorthandSpecID,
    span: Span,
    name: IdentID,
}

impl ShorthandSpec {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> IdentID {
        self.name
    }
}

#[derive(Debug)]
pub struct EnumMember {
    id: EnumMemberID,
    span: Span,
    name: PropName,
    init: Option<Expr>,
}

impl EnumMember {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> PropName {
        self.name
    }

    pub fn init(&self) -> Option<Expr> {
        self.init
    }
}

#[derive(Debug)]
pub struct EnumDecl {
    id: EnumDeclID,
    span: Span,
    modifiers: Option<Modifiers>,
    name: IdentID,
    members: Vec<EnumMemberID>,
}

impl EnumDecl {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn modifiers(&self) -> Option<&Modifiers> {
        self.modifiers.as_ref()
    }

    pub fn name(&self) -> IdentID {
        self.name
    }

    pub fn members(&self) -> &[EnumMemberID] {
        &self.members
    }
}

#[derive(Debug)]
pub struct ThrowStmt {
    id: ThrowStmtID,
    span: Span,
    expr: Expr,
}
impl ThrowStmt {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn expr(&self) -> Expr {
        self.expr
    }
}

#[derive(Debug)]
pub struct SetterDecl {
    id: SetterDeclID,
    span: Span,
    modifiers: Option<Modifiers>,
    name: PropName,
    params: Vec<ParamDeclID>,
    body: BlockStmtID,
}

impl SetterDecl {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn modifiers(&self) -> Option<&Modifiers> {
        self.modifiers.as_ref()
    }

    pub fn name(&self) -> PropName {
        self.name
    }

    pub fn params(&self) -> &[ParamDeclID] {
        &self.params
    }

    pub fn body(&self) -> BlockStmtID {
        self.body
    }
}

#[derive(Debug)]
pub struct GetterDecl {
    id: GetterDeclID,
    span: Span,
    modifiers: Option<Modifiers>,
    name: PropName,
    body: BlockStmtID,
}

impl GetterDecl {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn modifiers(&self) -> Option<&Modifiers> {
        self.modifiers.as_ref()
    }

    pub fn name(&self) -> PropName {
        self.name
    }

    pub fn body(&self) -> BlockStmtID {
        self.body
    }
}

#[derive(Debug)]
pub struct ClassStaticBlock {
    id: ClassStaticBlockID,
    span: Span,
    body: BlockStmtID,
}

impl ClassStaticBlock {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn body(&self) -> BlockStmtID {
        self.body
    }
}

#[derive(Debug)]
pub struct ClassMethodElem {
    id: ClassMethodElemID,
    span: Span,
    modifiers: Option<Modifiers>,
    name: PropName,
    params: Vec<ParamDeclID>,
    body: BlockStmtID,
}

impl ClassMethodElem {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn modifiers(&self) -> Option<&Modifiers> {
        self.modifiers.as_ref()
    }

    pub fn name(&self) -> PropName {
        self.name
    }

    pub fn params(&self) -> &[ParamDeclID] {
        &self.params
    }

    pub fn body(&self) -> BlockStmtID {
        self.body
    }
}

#[derive(Debug)]
pub struct ClassPropElem {
    id: ClassPropElemID,
    span: Span,
    modifiers: Option<Modifiers>,
    name: PropName,
    init: Option<Expr>,
}

impl ClassPropElem {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn modifiers(&self) -> Option<&Modifiers> {
        self.modifiers.as_ref()
    }

    pub fn name(&self) -> PropName {
        self.name
    }

    pub fn init(&self) -> Option<Expr> {
        self.init
    }
}

#[derive(Debug)]
pub struct ClassCtor {
    id: ClassCtorID,
    span: Span,
    params: Vec<ParamDeclID>,
    body: BlockStmtID,
}

impl ClassCtor {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn params(&self) -> &[ParamDeclID] {
        &self.params
    }

    pub fn body(&self) -> BlockStmtID {
        self.body
    }
}

#[derive(Debug)]
pub struct NullLit {
    id: NullLitID,
    span: Span,
}
impl NullLit {
    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug)]
pub struct NumLit {
    id: NumLitID,
    span: Span,
    val: f64,
}
impl NumLit {
    #[inline(always)]
    pub fn span(&self) -> Span {
        self.span
    }
    #[inline(always)]
    pub fn val(&self) -> f64 {
        self.val
    }
}

#[derive(Debug)]
pub struct BigIntLit {
    id: BigIntLitID,
    span: Span,
    val: (bool, Atom),
}

impl BigIntLit {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn val(&self) -> (bool, Atom) {
        self.val
    }
}

#[derive(Debug)]
pub struct BoolLit {
    id: BoolLitID,
    span: Span,
    val: bool,
}

impl BoolLit {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn val(&self) -> bool {
        self.val
    }
}

#[derive(Debug)]
pub struct RegExpLit {
    id: RegExpLitID,
    span: Span,
    val: Atom,
}

impl RegExpLit {
    pub fn span(&self) -> Span {
        self.span
    }
    pub fn val(&self) -> Atom {
        self.val
    }
}

#[derive(Debug)]
pub struct StringLit {
    id: StringLitID,
    span: Span,
    val: Atom,
    is_template: bool,
}
impl StringLit {
    pub fn span(&self) -> Span {
        self.span
    }
    pub fn val(&self) -> Atom {
        self.val
    }
    pub fn is_template(&self) -> bool {
        self.is_template
    }
}

#[derive(Debug)]
pub struct ModuleDecl {
    id: ModuleDeclID,
    span: Span,
    modifiers: Option<Modifiers>,
    name: ModuleName,
    block: ModuleBlockID,
}

impl ModuleDecl {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn modifiers(&self) -> Option<&Modifiers> {
        self.modifiers.as_ref()
    }

    pub fn name(&self) -> ModuleName {
        self.name
    }

    pub fn block(&self) -> ModuleBlockID {
        self.block
    }
}

pub struct ModuleBlock {
    id: ModuleBlockID,
    span: Span,
    stmts: Vec<Stmt>,
}

impl ModuleBlock {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn stmts(&self) -> &[Stmt] {
        &self.stmts
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ModuleName {
    Ident(IdentID),
    StringLit(StringLitID),
}

#[derive(Debug)]
pub struct ClassExtendsClause {
    id: ClassExtendsClauseID,
    span: Span,
    expr: Expr,
}
impl ClassExtendsClause {
    pub fn expr(&self) -> Expr {
        self.expr
    }
}

#[derive(Debug)]
pub struct ClassDecl {
    id: ClassDeclID,
    span: Span,
    modifiers: Option<Modifiers>,
    /// None means for `export default class {}`
    name: Option<IdentID>,
    extends: Option<ClassExtendsClauseID>,
    elems: Vec<ClassElem>,
}
impl ClassDecl {
    pub fn name(&self) -> Option<IdentID> {
        self.name
    }

    pub fn modifiers(&self) -> Option<&Modifiers> {
        self.modifiers.as_ref()
    }

    pub fn extends(&self) -> Option<ClassExtendsClauseID> {
        self.extends
    }

    pub fn elems(&self) -> &[ClassElem] {
        &self.elems
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ClassElem {
    PropElem(ClassPropElemID),
    MethodElem(ClassMethodElemID),
    StaticBlock(ClassStaticBlockID),
    Ctor(ClassCtorID),
    Getter(GetterDeclID),
    Setter(SetterDeclID),
}

#[derive(Debug)]
pub struct RetStmt {
    id: RetStmtID,
    span: Span,
    expr: Option<Expr>,
}
impl RetStmt {
    pub fn expr(&self) -> Option<Expr> {
        self.expr
    }
}

#[derive(Debug)]
pub struct IfStmt {
    id: IfStmtID,
    span: Span,
    expr: Expr,
    then: Stmt,
    else_then: Option<Stmt>,
}

impl IfStmt {
    pub fn expr(&self) -> Expr {
        self.expr
    }

    pub fn then(&self) -> Stmt {
        self.then
    }

    pub fn else_then(&self) -> Option<Stmt> {
        self.else_then
    }
}

#[derive(Debug)]
pub struct BlockStmt {
    id: BlockStmtID,
    span: Span,
    stmts: Vec<Stmt>,
}

impl BlockStmt {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn stmts(&self) -> &[Stmt] {
        &self.stmts
    }
}

#[derive(Debug)]
pub struct FnDecl {
    id: FnDeclID,
    span: Span,
    modifiers: Option<Modifiers>,
    name: IdentID,
    params: Vec<ParamDeclID>,
    body: GraphID,
}

impl FnDecl {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn modifiers(&self) -> Option<&Modifiers> {
        self.modifiers.as_ref()
    }

    pub fn name(&self) -> IdentID {
        self.name
    }

    pub fn params(&self) -> &[ParamDeclID] {
        &self.params
    }

    pub fn body(&self) -> GraphID {
        self.body
    }
}

#[derive(Debug)]
pub struct Modifier {
    id: ModifierID,
    span: Span,
    kind: ast::ModifierKind,
}

#[derive(Debug)]
pub struct Modifiers {
    flags: enumflags2::BitFlags<ast::ModifierKind>,
    list: Vec<ModifierID>,
}

impl Modifiers {
    pub fn new(flags: enumflags2::BitFlags<ast::ModifierKind>, list: Vec<ModifierID>) -> Self {
        Self { flags, list }
    }

    pub fn flags(&self) -> enumflags2::BitFlags<ast::ModifierKind> {
        self.flags
    }

    pub fn list(&self) -> &[ModifierID] {
        &self.list
    }
}

#[derive(Debug)]
pub struct ParamDecl {
    id: ParamDeclID,
    span: Span,
    modifiers: Option<Modifiers>,
    dotdotdot: Option<Span>,
    name: Binding,
    question: Option<Span>,
    // ty: Option<Ty>,
    init: Option<Expr>,
}

impl ParamDecl {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn modifiers(&self) -> Option<&Modifiers> {
        self.modifiers.as_ref()
    }

    pub fn dotdotdot(&self) -> Option<Span> {
        self.dotdotdot
    }

    pub fn name(&self) -> Binding {
        self.name
    }

    pub fn question(&self) -> Option<Span> {
        self.question
    }

    pub fn init(&self) -> Option<Expr> {
        self.init
    }
}

#[derive(Debug)]
pub struct VarStmt {
    id: VarStmtID,
    span: Span,
    modifiers: Option<Modifiers>,
    decls: Vec<VarDeclID>,
}

impl VarStmt {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn modifiers(&self) -> Option<&Modifiers> {
        self.modifiers.as_ref()
    }

    pub fn decls(&self) -> &[VarDeclID] {
        &self.decls
    }
}

#[derive(Debug)]
pub struct VarDecl {
    id: VarDeclID,
    span: Span,
    name: Binding,
    init: Option<Expr>,
}

impl VarDecl {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> Binding {
        self.name
    }

    pub fn init(&self) -> Option<Expr> {
        self.init
    }
}

#[derive(Debug)]
pub struct Ident {
    id: IdentID,
    ty: TyID,
    span: Span,
    name: Atom,
}

impl Ident {
    #[inline(always)]
    pub fn span(&self) -> Span {
        self.span
    }

    #[inline(always)]
    pub fn name(&self) -> Atom {
        self.name
    }

    #[inline(always)]
    pub fn ty(&self) -> TyID {
        self.ty
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Stmt {
    Var(VarStmtID),
    If(IfStmtID),
    For(ForStmtID),
    ForOf(ForOfStmtID),
    ForIn(ForInStmtID),
    Break(BreakStmtID),
    Continue(ContinueStmtID),
    Ret(RetStmtID),
    Block(BlockStmtID),
    Throw(ThrowStmtID),
    Expr(ExprStmtID),
    Fn(FnDeclID),
    Class(ClassDeclID),
    Module(ModuleDeclID),
    Enum(EnumDeclID),
    Import(ImportDeclID),
    Export(ExportDeclID),
    ExportAssign(ExportAssignID),
    Labeled(LabeledStmtID),
    Try(TryStmtID),
    Do(DoStmtID),
    While(WhileStmtID),
    Empty(EmptyStmtID),
}

#[derive(Debug, Clone, Copy)]
pub enum Expr {
    Assign(AssignExprID),
    Bin(BinExprID),
    Omit(OmitExprID),
    Paren(ParenExprID),
    This(ThisExprID),
    Ident(IdentID),
    BoolLit(BoolLitID),
    NullLit(NullLitID),
    NumLit(NumLitID),
    BigIntLit(BigIntLitID),
    RegExpLit(RegExpLitID),
    StringLit(StringLitID),
    ArrayLit(ArrayLitID),
    ObjectLit(ObjectLitID),
    Void(VoidExprID),
    Typeof(TypeofExprID),
    Super(SuperExprID),
    EleAccess(EleAccessExprID),
    PropAccess(PropAccessExprID),
    PostfixUnary(PostfixUnaryExprID),
    PrefixUnary(PrefixUnaryExprID),
    TaggedTemplate(TaggedTemplateExprID),
    Template(TemplateExprID),
    SpreadElem(SpreadElementID),
    ArrowFn(ArrowFnExprID),
    New(NewExprID),
    Class(ClassExprID),
    Fn(FnExprID),
    Call(CallExprID),
    Cond(CondExprID),
    JsxElem(JsxElemID),
    JsxSelfClosingElem(JsxSelfClosingElemID),
    JsxFrag(JsxFragID),
}

#[derive(Debug, Clone, Copy)]
pub enum Binding {
    Ident(IdentID),
    ObjectPat(ObjectPatID),
    ArrayPat(ArrayPatID),
}

#[derive(Debug, Clone, Copy)]
pub enum PropName {
    Ident(IdentID),
    StringLit(StringLitID),
    NumLit(NumLitID),
    Computed(ComputedPropNameID),
}

#[derive(Debug, Clone, Copy)]
pub enum ModuleExportName {
    Ident(IdentID),
    StringLit(StringLitID),
}

impl Nodes {
    pub fn expr_as_literal_to_boolean(&self, expr: Expr) -> Option<bool> {
        match expr {
            Expr::BoolLit(n) => Some(self.get_bool_lit(&n).val),
            Expr::NumLit(n) => Some(js_double_to_boolean(self.get_num_lit(&n).val())),
            Expr::StringLit(n) => Some(self.get_string_lit(&n).val() != keyword::IDENT_EMPTY),
            Expr::NullLit(_) => Some(false),
            // TODO: bigint and undefined
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Node {
    IfStmt(IfStmtID),
    ForStmt(ForStmtID),
    ForOfStmt(ForOfStmtID),
    ForInStmt(ForInStmtID),
    BreakStmt(BreakStmtID),
    ContinueStmt(ContinueStmtID),
    RetStmt(RetStmtID),
    BlockStmt(BlockStmtID),
    ThrowStmt(ThrowStmtID),
    ExprStmt(ExprStmtID),
    LabeledStmt(LabeledStmtID),
    TryStmt(TryStmtID),
    DoStmt(DoStmtID),
    WhileStmt(WhileStmtID),
    EmptyStmt(EmptyStmtID),
    FnDecl(FnDeclID),
    ClassDecl(ClassDeclID),
    ModuleDecl(ModuleDeclID),
    EnumDecl(EnumDeclID),
    ImportDecl(ImportDeclID),
    ExportDecl(ExportDeclID),
    ExportAssign(ExportAssignID),

    AssignExpr(AssignExprID),
    BinExpr(BinExprID),
    OmitExpr(OmitExprID),
    ParenExpr(ParenExprID),
    ThisExpr(ThisExprID),
    Ident(IdentID),
    BoolLit(BoolLitID),
    NullLit(NullLitID),
    NumLit(NumLitID),
    BigIntLit(BigIntLitID),
    RegExpLit(RegExpLitID),
    StringLit(StringLitID),
    ArrayLit(ArrayLitID),
    ObjectLit(ObjectLitID),
    VoidExpr(VoidExprID),
    TypeofExpr(TypeofExprID),
    SuperExpr(SuperExprID),
    EleAccessExpr(EleAccessExprID),
    PropAccessExpr(PropAccessExprID),
    PostfixUnaryExpr(PostfixUnaryExprID),
    PrefixUnaryExpr(PrefixUnaryExprID),
    TaggedTemplateExpr(TaggedTemplateExprID),
    TemplateExpr(TemplateExprID),
    SpreadElem(SpreadElementID),
    ArrowFnExpr(ArrowFnExprID),
    NewExpr(NewExprID),
    ClassExpr(ClassExprID),
    FnExpr(FnExprID),
    CallExpr(CallExprID),
    CondExpr(CondExprID),
    JsxElem(JsxElemID),
    JsxSelfClosingElem(JsxSelfClosingElemID),
    JsxFrag(JsxFragID),
}
