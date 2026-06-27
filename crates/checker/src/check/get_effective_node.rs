use super::flow::flow_loop_ctx_len;
use super::ty::{self, TypeFlags};
use super::{TyChecker, create_ty::IntersectionFlags};

use bolt_ts_ast as ast;
use bolt_ts_ast::r#trait;
use bolt_ts_binder::SymbolName;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_effective_base_type_node(
        &self,
        id: ast::NodeID,
    ) -> Option<&'cx ast::ClassExtendsClause<'cx>> {
        match self.p.node(id) {
            ast::Node::ClassDecl(c) => c.extends,
            ast::Node::ClassExpr(c) => c.extends,
            _ => None,
        }
    }

    pub(super) fn get_effective_ty_param_decls(&self, id: ast::NodeID) -> ast::TyParams<'cx> {
        let node = self.p.node(id);
        node.ty_params().unwrap_or_default()
    }

    #[inline]
    pub(super) fn get_effective_ret_type_node(&self, id: ast::NodeID) -> Option<&'cx ast::Ty<'cx>> {
        self.p.node(id).ret_ty()
    }

    pub(super) fn get_effective_check_node(&self, id: ast::NodeID) -> ast::NodeID {
        // self.p.skip_outer_expr(id)
        id
    }

    pub(super) fn get_effective_constraint_of_intersection(
        &mut self,
        tys: &[&'cx ty::Ty<'cx>],
        target_is_union: bool,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let mut constraints = Vec::with_capacity(16);
        let mut has_disjoint_domain_ty = false;
        for t in tys {
            if t.flags.intersects(TypeFlags::INSTANTIABLE) {
                let mut constraint = self.get_constraint_of_ty(t);
                while let Some(c) = constraint {
                    if c.flags.intersects(
                        TypeFlags::TYPE_PARAMETER
                            .union(TypeFlags::INDEX)
                            .union(TypeFlags::CONDITIONAL),
                    ) {
                        constraint = self.get_constraint_of_ty(c)
                    } else {
                        break;
                    }
                }
                if let Some(c) = constraint {
                    constraints.push(c);
                    if target_is_union {
                        constraints.push(t);
                    }
                }
            } else if t.flags.intersects(TypeFlags::DISJOINT_DOMAINS)
                || self.is_empty_anonymous_object_ty(t)
            {
                has_disjoint_domain_ty = true;
            }
        }

        if !constraints.is_empty() {
            if has_disjoint_domain_ty {
                for t in tys {
                    if t.flags.intersects(TypeFlags::DISJOINT_DOMAINS)
                        || self.is_empty_anonymous_object_ty(t)
                    {
                        constraints.push(t);
                    }
                }
            }
            if target_is_union || has_disjoint_domain_ty {
                let i = self.get_intersection_ty(
                    &constraints,
                    IntersectionFlags::NoConstraintReduction,
                    None,
                    None,
                );
                return Some(self.get_normalized_ty::<false>(i));
            }
        }

        None
    }

    pub(super) fn get_effective_constraint_of_ty_param(
        &self,
        node: &ast::TyParam<'cx>,
    ) -> Option<&'cx ast::Ty<'cx>> {
        // TODO: js
        node.constraint
    }

    pub(super) fn get_effective_ty_args(
        &mut self,
        id: ast::NodeID,
        ty_params: ty::Tys<'cx>,
    ) -> Option<ty::Tys<'cx>> {
        let node = self.p.node(id);
        let ty_args = if let Some(ty_args) = node.ty_args() {
            let ty_args = ty_args
                .list
                .iter()
                .map(|ty_arg| self.get_ty_from_type_node(ty_arg))
                .collect::<Vec<_>>();
            let ty_args: ty::Tys<'cx> = self.alloc(ty_args);
            Some(ty_args)
        } else {
            None
        };
        let min_ty_argument_count = self.get_min_ty_arg_count_of_ty_params(ty_params);
        self.fill_missing_ty_args(ty_args, Some(ty_params), min_ty_argument_count)
    }

    pub(super) fn get_effective_call_arguments(
        &mut self,
        expr: &impl r#trait::CallLike<'cx>,
    ) -> EffectiveCallArguments<'cx> {
        let id = expr.id();
        let node = self.p.node(id);
        // if node.is_jsx_opening_fragment() {
        //     // TODO:
        //     return self.empty_array();
        // }
        match node {
            ast::Node::TaggedTemplateExpr(_) => {
                // TODO:
                EffectiveCallArguments::Borrowed(expr.args())
            }
            _ => {
                let args = expr.args();
                if let Some(spared_index) = self.get_spread_arg_index(args) {
                    let mut effective_args = args[0..spared_index]
                        .iter()
                        .map(|arg| EffectiveCallArgument::Expression(arg))
                        .collect::<Vec<_>>();
                    for i in spared_index..args.len() {
                        let arg = args[i];
                        let spread_ty = if let ast::ExprKind::SpreadElement(n) = &arg.kind {
                            Some(if flow_loop_ctx_len(self) != 0 {
                                self.check_expression(n.expr, None)
                            } else {
                                self.check_expression_cached(n.expr, None)
                            })
                        } else {
                            None
                        };
                        if let Some(spread_ty) = spread_ty
                            && let Some(t) = spread_ty.as_tuple()
                        {
                            let tys = self.get_element_tys(spread_ty);
                            for (j, ty) in tys.iter().enumerate() {
                                let flags = t.element_flags[j];
                                effective_args.push(EffectiveCallArgument::Synthetic(
                                    SyntheticExpression {
                                        span: arg.span(),
                                        parent: arg.id(),
                                        is_spread: flags.contains(ty::ElementFlags::VARIABLE),
                                        ty: if flags.contains(ty::ElementFlags::REST) {
                                            self.create_array_ty_worker::<false>(ty)
                                        } else {
                                            ty
                                        },
                                        tuple_name_source: None, // TODO: t.labeled_element_declarations
                                    },
                                ));
                            }
                        } else {
                            effective_args.push(EffectiveCallArgument::Expression(arg));
                        }
                    }
                    EffectiveCallArguments::Owned(effective_args)
                } else {
                    EffectiveCallArguments::Borrowed(args)
                }
            }
        }
    }

    pub(super) fn get_effective_declaration_flags(
        &self,
        n: ast::NodeID,
        flags_to_check: ast::ModifierFlags,
    ) -> ast::ModifierFlags {
        let mut modifier_flags = self.node_query(n.module()).get_combined_modifier_flags(n);
        let Some(p) = self.parent(n) else {
            return Default::default();
        };
        let parent_node = self.p.node(p);
        if !parent_node.is_interface_decl()
            && !parent_node.is_class_decl()
            && !parent_node.is_class_expr()
            && self.p.node_flags(n).contains(ast::NodeFlags::AMBIENT)
        {
            if let Some(container) = self.node_query(n.module()).get_enclosing_container(n)
                && let container_flags = self.p.node_flags(container)
                && container_flags.contains(ast::NodeFlags::EXPORT_CONTEXT)
                && !modifier_flags.contains(ast::ModifierFlags::AMBIENT)
                && !(parent_node.is_module_block()
                    && self.parent(p).is_some_and(|pp| {
                        self.p.node(pp).is_module_declaration()
                            && self
                                .p
                                .node_flags(pp)
                                .contains(ast::NodeFlags::GLOBAL_AUGMENTATION)
                    }))
            {
                modifier_flags |= ast::ModifierFlags::EXPORT;
            }

            modifier_flags |= ast::ModifierFlags::AMBIENT;
        }

        modifier_flags & flags_to_check
    }

    pub(super) fn get_effective_prop_name_for_prop_name_node(
        &mut self,
        node: &ast::PropNameKind<'cx>,
    ) -> Option<SymbolName> {
        if let Some(name) = bolt_ts_binder::prop_name_opt(node) {
            Some(name)
        } else if let ast::PropNameKind::Computed(node) = node {
            let ty = self.get_ty_of_expr(node.expr);
            self.try_get_name_from_ty(ty)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct SyntheticExpression<'cx> {
    span: bolt_ts_span::Span,
    parent: ast::NodeID,
    is_spread: bool,
    ty: &'cx ty::Ty<'cx>,
    tuple_name_source: Option<ast::NodeID>,
}

impl<'cx> SyntheticExpression<'cx> {
    pub fn span(&self) -> bolt_ts_span::Span {
        self.span
    }

    pub fn ty(&self) -> &'cx ty::Ty<'cx> {
        self.ty
    }

    pub fn parent(&self) -> ast::NodeID {
        self.parent
    }

    pub fn is_spread(&self) -> bool {
        self.is_spread
    }
}

#[derive(Debug, Clone)]
pub enum EffectiveCallArgument<'cx> {
    Expression(&'cx ast::Expr<'cx>),
    Synthetic(SyntheticExpression<'cx>),
}

impl EffectiveCallArgument<'_> {
    pub fn span(&self) -> bolt_ts_span::Span {
        match self {
            EffectiveCallArgument::Expression(n) => n.span(),
            EffectiveCallArgument::Synthetic(n) => n.span,
        }
    }

    pub fn is_spread_argument(&self) -> bool {
        match self {
            EffectiveCallArgument::Expression(n) => {
                matches!(n.kind, ast::ExprKind::SpreadElement(_))
            }
            EffectiveCallArgument::Synthetic(n) => n.is_spread,
        }
    }
}

pub enum EffectiveCallArguments<'cx> {
    Borrowed(ast::Exprs<'cx>),
    Owned(Vec<EffectiveCallArgument<'cx>>),
}

impl<'cx> EffectiveCallArguments<'cx> {
    pub fn len(&self) -> usize {
        match self {
            EffectiveCallArguments::Borrowed(args) => args.len(),
            EffectiveCallArguments::Owned(args) => args.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get_spared_argument_index(&self) -> Option<usize> {
        match self {
            EffectiveCallArguments::Borrowed(args) => args
                .iter()
                .position(|arg| matches!(arg.kind, ast::ExprKind::SpreadElement(_))),
            EffectiveCallArguments::Owned(args) => args.iter().position(|arg| match arg {
                EffectiveCallArgument::Expression(n) => {
                    matches!(n.kind, ast::ExprKind::SpreadElement(_))
                }
                EffectiveCallArgument::Synthetic(n) => n.is_spread,
            }),
        }
    }

    pub fn get(&self, index: usize) -> Option<std::borrow::Cow<'_, EffectiveCallArgument<'cx>>> {
        if index < self.len() {
            Some(self.index(index))
        } else {
            None
        }
    }

    pub fn index(&self, index: usize) -> std::borrow::Cow<'_, EffectiveCallArgument<'cx>> {
        debug_assert!(index < self.len());
        match self {
            EffectiveCallArguments::Borrowed(args) => {
                std::borrow::Cow::Owned(EffectiveCallArgument::Expression(args[index]))
            }
            EffectiveCallArguments::Owned(args) => std::borrow::Cow::Borrowed(&args[index]),
        }
    }

    pub fn last(&self) -> std::borrow::Cow<'_, EffectiveCallArgument<'cx>> {
        match self {
            EffectiveCallArguments::Borrowed(args) => {
                debug_assert!(!args.is_empty());
                std::borrow::Cow::Owned(EffectiveCallArgument::Expression(args.last().unwrap()))
            }
            EffectiveCallArguments::Owned(args) => {
                debug_assert!(!args.is_empty());
                std::borrow::Cow::Borrowed(args.last().unwrap())
            }
        }
    }
}
