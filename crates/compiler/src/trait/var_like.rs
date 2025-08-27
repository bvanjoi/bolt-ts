use bolt_ts_ast::{self as ast, NodeFlags};
use bolt_ts_binder::NodeQuery;

pub trait VarLike<'cx>: bolt_ts_ast::r#trait::VarLike<'cx> {
    fn is_var_const(&self, node_query: &NodeQuery) -> bool {
        false
    }
}

impl<'cx> VarLike<'cx> for ast::VarDecl<'cx> {
    fn is_var_const(&self, node_query: &NodeQuery) -> bool {
        let block_scope_kind = node_query
            .get_combined_node_flags(self.id)
            .intersection(NodeFlags::BLOCK_SCOPED);
        block_scope_kind.intersects(
            NodeFlags::CONST
                .union(NodeFlags::USING)
                .union(NodeFlags::AWAIT_USING),
        )
    }
}

impl<'cx> VarLike<'cx> for ast::ParamDecl<'cx> {}

impl<'cx> VarLike<'cx> for ast::ClassPropElem<'cx> {}

impl<'cx> VarLike<'cx> for ast::PropSignature<'cx> {}

impl<'cx> VarLike<'cx> for ast::ObjectPropAssignment<'cx> {}

impl<'cx> VarLike<'cx> for ast::ObjectShorthandMember<'cx> {}
