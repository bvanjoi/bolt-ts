pub trait HasBindingName<'cx> {
    fn id(&self) -> bolt_ts_ast::NodeID;
    fn name(&self) -> &'cx bolt_ts_ast::Binding<'cx>;
}

impl<'cx> HasBindingName<'cx> for bolt_ts_ast::VarDecl<'cx> {
    fn id(&self) -> bolt_ts_ast::NodeID {
        self.id
    }
    fn name(&self) -> &'cx bolt_ts_ast::Binding<'cx> {
        self.binding
    }
}

impl<'cx> HasBindingName<'cx> for bolt_ts_ast::ParamDecl<'cx> {
    fn id(&self) -> bolt_ts_ast::NodeID {
        self.id
    }
    fn name(&self) -> &'cx bolt_ts_ast::Binding<'cx> {
        self.name
    }
}

pub fn node_id_of_binding<'cx>(has_binding_name: &impl HasBindingName<'cx>) -> bolt_ts_ast::NodeID {
    has_binding_name.id()
}
