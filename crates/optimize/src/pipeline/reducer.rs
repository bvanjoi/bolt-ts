use crate::ir;

pub struct ReduceGraph<'ir> {
    graph: &'ir mut ir::GraphArena,
    nodes: &'ir mut ir::Nodes,
}

impl<'ir> ReduceGraph<'ir> {
    pub fn new(nodes: &'ir mut ir::Nodes, graph: &'ir mut ir::GraphArena) -> Self {
        Self { nodes, graph }
    }

    pub fn iter_reducer(&mut self) {}
}
