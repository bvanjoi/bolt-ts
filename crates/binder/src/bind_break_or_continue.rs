use super::BinderState;
use super::FlowID;

pub(super) trait BreakOrContinue<'cx> {
    fn label(&self) -> Option<&'cx bolt_ts_ast::Ident>;
    fn is_break(&self) -> bool;
}
impl<'cx> BreakOrContinue<'cx> for bolt_ts_ast::BreakStmt<'cx> {
    fn label(&self) -> Option<&'cx bolt_ts_ast::Ident> {
        self.label
    }
    fn is_break(&self) -> bool {
        true
    }
}
impl<'cx> BreakOrContinue<'cx> for bolt_ts_ast::ContinueStmt<'cx> {
    fn label(&self) -> Option<&'cx bolt_ts_ast::Ident> {
        self.label
    }
    fn is_break(&self) -> bool {
        false
    }
}

impl<'cx> BinderState<'cx, '_, '_> {
    pub(super) fn bind_break_or_continue_flow(
        &mut self,
        node: &impl BreakOrContinue<'cx>,
        break_target: Option<FlowID>,
        continue_target: Option<FlowID>,
    ) {
        let flow_label = if node.is_break() {
            break_target
        } else {
            continue_target
        };
        if let Some(flow_label) = flow_label {
            self.flow_nodes
                .add_antecedent(flow_label, self.current_flow.unwrap());
            self.current_flow = Some(self.unreachable_flow_node);
            self.has_flow_effects = true;
        }
    }

    pub(super) fn bind_break_or_continue_stmt(&mut self, node: &impl BreakOrContinue<'cx>) {
        if let Some(label) = node.label() {
            self.bind(label.id);
        } else {
            self.bind_break_or_continue_flow(
                node,
                self.current_break_target,
                self.current_continue_target,
            );
        }
    }
}
