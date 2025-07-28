use super::BinderState;
use super::FlowFlags;
use super::container_flags::ContainerFlags;

use bolt_ts_ast as ast;

use crate::bind::SymbolTable;

impl BinderState<'_, '_, '_> {
    fn create_locals_for_container(&mut self, container: ast::NodeID) {
        assert!(self.p.node(container).has_locals());
        let prev = self.locals.insert(container, SymbolTable::default());
        assert!(prev.is_none());
    }

    fn delete_locals_for_container(&mut self, container: ast::NodeID) {
        assert!(self.p.node(container).has_locals());
        self.locals.remove(&container);
    }

    fn add_to_container_chain(&mut self, next: ast::NodeID) {
        assert!(self.p.node(next).has_locals());
        if let Some(last_container) = self.last_container {
            // same as `last_container.next_container = next`
            let prev = self.container_chain.insert(last_container, next);
            assert!(prev.is_none());
        }
        self.last_container = Some(next);
    }

    pub(super) fn bind_container(&mut self, node: ast::NodeID, container_flags: ContainerFlags) {
        let save_container = self.container;
        let save_this_parent_container = self.this_parent_container;
        let save_block_scope_container = self.block_scope_container;
        let save_in_return_position = self.in_return_position;

        let n = self.p.node(node);
        if n.as_arrow_fn_expr()
            .is_some_and(|n| !matches!(n.body, ast::ArrowFnExprBody::Block(_)))
        {
            self.in_return_position = true;
        }

        if container_flags.intersects(ContainerFlags::IS_CONTAINER) {
            if !n.is_arrow_fn_expr() {
                self.this_parent_container = self.container;
            }
            self.block_scope_container = Some(node);
            self.container = self.block_scope_container;
            if container_flags.intersects(ContainerFlags::HAS_LOCALS) {
                self.create_locals_for_container(node);
                self.add_to_container_chain(node);
            }
        } else if container_flags.intersects(ContainerFlags::IS_BLOCK_SCOPED_CONTAINER) {
            self.block_scope_container = Some(node);
            if container_flags.intersects(ContainerFlags::HAS_LOCALS) {
                self.delete_locals_for_container(node);
            }
        }

        if container_flags.intersects(ContainerFlags::IS_CONTROL_FLOW_CONTAINER) {
            let save_current_flow = self.current_flow;
            let save_break_target = self.current_break_target;
            let save_continue_target = self.current_continue_target;
            let save_return_target = self.current_return_target;
            let save_exception_target = self.current_exception_target;
            // TODO: active_label_list
            let save_has_explicit_return = self.has_explicit_return;
            let is_immediately_invoked = (container_flags
                .intersects(ContainerFlags::IS_FUNCTION_EXPRESSION)
                && !n.has_syntactic_modifier(ast::ModifierKind::Async.into())
                && !n.is_fn_like_and_has_asterisk()
                && self
                    .node_query()
                    .get_immediately_invoked_fn_expr(node)
                    .is_some())
                || self.p.node(node).is_class_static_block_decl();

            if !is_immediately_invoked {
                let flow_node = container_flags.intersects(ContainerFlags::IS_FUNCTION_EXPRESSION | ContainerFlags::IS_OBJECT_LITERAL_OR_CLASS_EXPRESSION_METHOD_OR_ACCESSOR).then_some(node);
                self.current_flow = Some(self.flow_nodes.create_start(flow_node));
            }
            self.current_return_target = if is_immediately_invoked || n.is_class_ctor() {
                Some(self.flow_nodes.create_branch_label())
            } else {
                None
            };
            self.current_exception_target = None;
            self.current_break_target = None;
            self.current_continue_target = None;
            self.has_explicit_return = false;

            self.bind_children(node);

            self.p.node_flags_map.update(node, |flags| {
                *flags &= ast::NodeFlags::REACHABILITY_AND_EMIT_FLAGS.complement();
            });

            if !self
                .flow_nodes
                .get_flow_node(self.current_flow.unwrap())
                .flags
                .contains(FlowFlags::UNREACHABLE)
                && container_flags.contains(ContainerFlags::IS_FUNCTION_LIKE)
                && n.fn_body().is_some()
            {
                self.p.node_flags_map.update(node, |flags| {
                    *flags |= ast::NodeFlags::HAS_IMPLICIT_RETURN;
                    if self.has_explicit_return {
                        *flags |= ast::NodeFlags::HAS_EXPLICIT_RETURN;
                    }
                });
                self.flow_in_nodes
                    .insert_end_flow_node(node, self.current_flow.unwrap());
            }

            if let Some(current_return_target) = self.current_return_target {
                self.flow_nodes
                    .add_antecedent(current_return_target, self.current_flow.unwrap());
                self.current_flow = Some(self.finish_flow_label(current_return_target));
            }

            if !is_immediately_invoked {
                self.current_flow = save_current_flow;
            }
            self.current_break_target = save_break_target;
            self.current_continue_target = save_continue_target;
            self.current_return_target = save_return_target;
            self.current_exception_target = save_exception_target;
            // TODO: active_label_list
            self.has_explicit_return = save_has_explicit_return;
        } else if container_flags.intersects(ContainerFlags::IS_INTERFACE) {
            self.seen_this_keyword = false;
            self.bind_children(node);
            assert!(!n.is_ident());
            self.p.node_flags_map.update(node, |flags| {
                if self.seen_this_keyword {
                    *flags |= ast::NodeFlags::CONTAINS_THIS;
                } else {
                    *flags &= !ast::NodeFlags::CONTAINS_THIS;
                }
            });
        } else {
            self.bind_children(node);
        }

        self.in_return_position = save_in_return_position;
        self.container = save_container;
        self.this_parent_container = save_this_parent_container;
        self.block_scope_container = save_block_scope_container;
    }
}
