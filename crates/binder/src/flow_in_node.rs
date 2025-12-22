use super::FlowID;

#[derive(Debug, Clone, Copy)]
pub enum FlowInNode {
    Noop,
    FnLike(FnLikeFlowInNode),
}

#[derive(Debug, Clone, Copy)]
pub struct FnLikeFlowInNode {
    pub end_flow_node: Option<FlowID>,
    pub return_flow_node: Option<FlowID>,
}

#[derive(Default)]
pub struct FlowInNodes {
    data: Vec<FlowInNode>,
}

impl FlowInNodes {
    pub fn new(len: usize) -> Self {
        Self {
            data: vec![FlowInNode::Noop; len],
        }
    }

    pub fn insert_end_flow_node(&mut self, node: bolt_ts_ast::NodeID, end_flow_node: FlowID) {
        let n = &mut self.data[node.index_as_usize()];
        match n {
            FlowInNode::Noop => {
                self.data[node.index_as_usize()] = FlowInNode::FnLike(FnLikeFlowInNode {
                    end_flow_node: Some(end_flow_node),
                    return_flow_node: None,
                });
            }
            FlowInNode::FnLike(fn_like) => {
                let prev = fn_like.end_flow_node.replace(end_flow_node);
                assert!(prev.is_none());
            }
        }
    }

    pub fn get(&self, node: bolt_ts_ast::NodeID) -> FlowInNode {
        let index = node.index_as_usize();
        debug_assert!(index < self.data.len());
        unsafe { *self.data.get_unchecked(index) }
    }
}
