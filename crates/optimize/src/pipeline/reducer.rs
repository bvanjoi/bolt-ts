use crate::ir;

pub struct ReduceGraph<'ir> {
    nodes: &'ir mut ir::Nodes,
}

impl<'ir> ReduceGraph<'ir> {
    pub fn new(nodes: &'ir mut ir::Nodes) -> Self {
        Self { nodes }
    }

    pub fn iter_reducer(&mut self) {}
}
