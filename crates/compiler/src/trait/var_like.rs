use bolt_ts_ast::{self as ast, NodeFlags};
use bolt_ts_binder::NodeQuery;

pub trait VarLike<'cx>: bolt_ts_ast::r#trait::VarLike<'cx> {
    fn is_var_const(&self, _: &NodeQuery) -> bool {
        false
    }
    fn is_declaration_readonly(&self, _: &NodeQuery) -> bool {
        false
    }
}

impl<'cx> VarLike<'cx> for ast::VarDecl<'cx> {
    fn is_var_const(&self, node_query: &NodeQuery) -> bool {
        let block_scope_kind = node_query
            .get_combined_node_flags(self.id)
            .intersection(NodeFlags::BLOCK_SCOPED);
        block_scope_kind == ast::NodeFlags::CONST
            || block_scope_kind == ast::NodeFlags::USING
            || block_scope_kind == ast::NodeFlags::AWAIT_USING
    }
}

impl<'cx> VarLike<'cx> for ast::ParamDecl<'cx> {}

impl<'cx> VarLike<'cx> for ast::ClassPropElem<'cx> {
    fn is_declaration_readonly(&self, nq: &NodeQuery) -> bool {
        nq.get_combined_modifier_flags(self.id)
            .contains(ast::ModifierFlags::READONLY)
    }
}

impl<'cx> VarLike<'cx> for ast::PropSignature<'cx> {
    fn is_declaration_readonly(&self, nq: &NodeQuery) -> bool {
        nq.get_combined_modifier_flags(self.id)
            .contains(ast::ModifierFlags::READONLY)
    }
}

impl<'cx> VarLike<'cx> for ast::ObjectPropAssignment<'cx> {}

impl<'cx> VarLike<'cx> for ast::ObjectShorthandMember<'cx> {}
