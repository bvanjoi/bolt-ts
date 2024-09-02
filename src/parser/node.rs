use super::ast;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct NodeID(u32);
impl NodeID {
    pub(super) fn root() -> NodeID {
        NodeID(0)
    }
    pub(super) fn next(&self) -> NodeID {
        NodeID(self.0 + 1)
    }
    pub fn as_u32(&self) -> u32 {
        self.0
    }
}
#[derive(Debug, Clone, Copy)]
pub enum Node<'cx> {
    Program(&'cx ast::Program<'cx>),
    Stmt(&'cx ast::Stmt<'cx>),
    Expr(&'cx ast::Expr<'cx>),
    NumLit(&'cx ast::NumLit),
    BoolLit(&'cx ast::BoolLit),
}
