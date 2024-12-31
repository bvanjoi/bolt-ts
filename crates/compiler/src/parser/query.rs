use bolt_ts_span::ModuleID;

use super::ast;
use super::Parser;

impl<'cx> Parser<'cx> {
    #[inline(always)]
    pub fn root(&self, id: ModuleID) -> &ast::Program<'cx> {
        self.get(id)
            .nodes
            .get(ast::NodeID::root(id))
            .expect_program()
    }

    #[inline(always)]
    pub fn node(&self, id: ast::NodeID) -> ast::Node<'cx> {
        self.get(id.module()).nodes.get(id)
    }

    #[inline(always)]
    pub fn parent(&self, id: ast::NodeID) -> Option<ast::NodeID> {
        self.get(id.module()).parent_map.parent(id)
    }

    pub fn is_descendant_of(&self, node: ast::NodeID, ancestor: ast::NodeID) -> bool {
        let mut node = node;
        loop {
            if node == ancestor {
                return true;
            }
            if let Some(parent) = self.parent(node) {
                node = parent
            } else {
                return false;
            }
        }
    }
}
