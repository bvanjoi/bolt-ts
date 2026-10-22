use bolt_ts_ast as ast;
use bolt_ts_ty::TypeFlags;
use bolt_ts_utils::fx_hashset_with_capacity;

use super::NodeCheckFlags;
use super::TyChecker;
use super::errors;
use super::relation::RelationKind;

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_node_deferred(&mut self, node: ast::NodeID) {
        let root = node.into_root();
        let flags = self.get_node_links(root).flags();
        if !flags.contains(NodeCheckFlags::TYPE_CHECKED) {
            self.deferred_nodes[node.module().as_usize()].insert(node);
        }
    }

    pub fn check_deferred_nodes(&mut self, module_id: bolt_ts_span::ModuleID) {
        let mut checked = fx_hashset_with_capacity(self.deferred_nodes[module_id.as_usize()].len());
        self.deferred_nodes[module_id.as_usize()].reverse();
        while let Some(node) = self.deferred_nodes[module_id.as_usize()].pop() {
            if checked.insert(node.index_as_u32()) {
                self.check_deferred_node(node);
            }
        }
        debug_assert!(self.deferred_nodes[module_id.as_usize()].is_empty());
    }

    fn is_property_initialized_in_constructor(
        &self,
        property: ast::NodeID,
        ctor: &'cx ast::ClassCtor<'cx>,
    ) -> bool {
        // ensure class constructor had been checked
        self.property_initializer_in_class_constructor_map
            .has(ctor, property)
    }

    /// `check_property_initializer` should be called after `check_class_elements` to
    /// ensure the constructor is checked.
    fn check_property_initializer_deferred(&mut self, class_id: ast::NodeID) {
        if self.config.compiler_options().strict_null_checks()
            && self
                .config
                .compiler_options()
                .strict_property_initialization()
            && !self
                .p
                .node_flags(class_id)
                .contains(ast::NodeFlags::AMBIENT)
        {
            let elements = match self.p.node(class_id) {
                ast::Node::ClassDecl(n) => n.elems.list,
                ast::Node::ClassExpr(n) => n.elems.list,
                _ => unreachable!(),
            };
            let ctor = ast::r#trait::find_constructor_declaration(&elements);
            for elem in elements {
                if elem
                    .kind
                    .modifiers()
                    .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT))
                {
                    continue;
                }
                if !elem.kind.is_static() && elem.kind.is_kind_without_init() {
                    let ast::ClassElemKind::Prop(prop) = elem.kind else {
                        unreachable!()
                    };
                    let prop_name = prop.name;
                    if matches!(
                        prop.name.kind,
                        ast::PropNameKind::Ident(_)
                            | ast::PropNameKind::PrivateIdent(_)
                            | ast::PropNameKind::Computed(_)
                    ) {
                        let symbol = self.get_symbol_of_declaration(prop.id);
                        let prop_ty = self.get_type_of_symbol(symbol);
                        if !(prop_ty.flags.intersects(TypeFlags::ANY_OR_UNKNOWN)
                            || prop_ty.contains_undefined_ty())
                            && ctor.is_none_or(|ctor| {
                                !self.is_property_initialized_in_constructor(prop.id, ctor)
                            })
                        {
                            let error = errors::PropertyXHasNoInitializerAndIsNotDefinitelyAssignedInTheConstructor {
                                    span: prop_name.span(),
                                    property: prop_name.kind.to_string(&self.atoms),
                                };
                            self.push_error(Box::new(error));
                        }
                    }
                }
            }
        }
    }

    pub fn check_deferred_class_nodes_for_property_initializer(
        &mut self,
        module_id: bolt_ts_span::ModuleID,
    ) {
        let mut checked = fx_hashset_with_capacity(
            self.deferred_class_nodes_for_property_initializer_check[module_id.as_usize()].len(),
        );
        self.deferred_class_nodes_for_property_initializer_check[module_id.as_usize()].reverse();
        while let Some(node) =
            self.deferred_class_nodes_for_property_initializer_check[module_id.as_usize()].pop()
        {
            if checked.insert(node.index_as_u32()) {
                self.check_property_initializer_deferred(node);
            }
        }
        debug_assert!(
            self.deferred_class_nodes_for_property_initializer_check[module_id.as_usize()]
                .is_empty()
        );
    }

    fn check_assertion_deferred(
        &mut self,
        node_id: ast::NodeID,
        span: bolt_ts_span::Span,
        assert_ty: &'cx ast::Ty<'cx>,
    ) {
        let Some(expr_ty) = self.get_node_links(node_id).get_assertion_expression_ty() else {
            unreachable!()
        };
        let expr_ty = self.get_base_ty_of_literal_ty(expr_ty);
        let expr_ty = self.get_regular_ty_of_object_literal(expr_ty);
        let target_ty = self.get_ty_from_type_node(assert_ty);
        if !self.is_error(target_ty)
            && let widened_ty = self.get_widened_ty(expr_ty)
            && !self.is_type_related_to(target_ty, widened_ty, RelationKind::Comparable)
        {
            self.check_type_comparable_to(expr_ty, target_ty, Some(node_id), Some(|this: &mut Self| {
                let error = errors::ConversionOfType0ToType1MayBeAMistakeBecauseNeitherTypeSufficientlyOverlapsWithTheOtherIfThisWasIntentionalConvertTheExpressionToUnknownFirst {
                    span,
                    source_ty: this.print_ty(widened_ty, None).to_string(),
                    target_ty: this.print_ty(target_ty, None).to_string(),
                };
                this.push_error(Box::new(error));
            }));
        }
    }

    fn check_deferred_node(&mut self, node: ast::NodeID) {
        let saved_current_node = self.current_node;
        self.current_node = Some(node);
        self.instantiation_count = 0;

        use bolt_ts_ast::Node::*;
        match self.p.node(node) {
            FnExpr(expr) => self.check_fn_like_expr_deferred(expr),
            ArrowFnExpr(expr) => self.check_fn_like_expr_deferred(expr),
            ObjectMethodMember(expr) => {
                let body = ast::ArrowFnExprBody::Block(expr.body);
                self.check_fn_like_expr_or_object_method_member_deferred(expr, body)
            }
            TyAssertionExpr(n) => {
                self.check_assertion_deferred(n.id, n.span, n.ty);
            }
            AsExpr(n) => {
                self.check_assertion_deferred(n.id, n.span, n.ty);
            }
            CallExpr(n) => {
                self.resolve_untyped_call(n);
            }
            NewExpr(n) => {
                self.resolve_untyped_call(n);
            }
            VoidExpr(n) => {
                self.check_expression::<false>(n.expr, None);
            }
            TaggedTemplateExpr(n) => match n.tpl {
                ast::TemplateExpressionKind::NoSubstitutionTemplateLit(_) => {}
                ast::TemplateExpressionKind::TemplateExpr(n) => {
                    self.check_template_expr(n);
                }
            },
            GetterDecl(n) => self.check_getter_decl(n),
            SetterDecl(n) => self.check_setter_decl(n),
            ClassExpr(n) => self.check_class_expression_deferred(n),
            _ => unreachable!("{:#?}", self.p.node(node)),
        }

        self.current_node = saved_current_node;
    }

    fn check_class_expression_deferred(&mut self, n: &'cx ast::ClassExpr<'cx>) {
        self.check_class_elements(n.elems);
        self.mark_check_property_initializer_deferred(n);
        self.register_potentially_unused_class_expression(n);
    }
}
