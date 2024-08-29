use super::ast;


#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct NodeID(u32);
impl NodeID {
  pub fn root() -> NodeID {
    NodeID(0)
  }
  pub fn next(&self) -> NodeID {
    NodeID(self.0 + 1)
  }
}
#[derive(Clone)]
pub enum Node<'p> {
  Program(&'p ast::Program<'p>),
  Stmt(&'p ast::Stmt<'p>),
  Expr(&'p ast::Expr<'p>),
}