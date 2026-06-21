use super::BinderState;
use super::FlowID;

pub(super) trait BreakOrContinue<'cx> {
    const IS_BREAK: bool;
    fn label(&self) -> Option<&'cx bolt_ts_ast::Ident>;
}
impl<'cx> BreakOrContinue<'cx> for bolt_ts_ast::BreakStmt<'cx> {
    const IS_BREAK: bool = true;
    fn label(&self) -> Option<&'cx bolt_ts_ast::Ident> {
        self.label
    }
}
impl<'cx> BreakOrContinue<'cx> for bolt_ts_ast::ContinueStmt<'cx> {
    const IS_BREAK: bool = false;
    fn label(&self) -> Option<&'cx bolt_ts_ast::Ident> {
        self.label
    }
}

impl<'cx> BinderState<'cx, '_, '_> {
    pub(super) fn bind_break_or_continue_flow<N: BreakOrContinue<'cx>>(
        &mut self,
        break_target: Option<FlowID>,
        continue_target: Option<FlowID>,
    ) {
        let flow_label = if N::IS_BREAK {
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

    pub(super) fn bind_break_or_continue_stmt<N: BreakOrContinue<'cx>>(&mut self, node: &N) {
        if let Some(label) = node.label() {
            self.bind(label.id);
        } else {
            self.bind_break_or_continue_flow::<N>(
                self.current_break_target,
                self.current_continue_target,
            );
        }
    }
}
