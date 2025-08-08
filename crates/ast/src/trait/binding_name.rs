pub trait HasBindingName<'cx> {
    fn id(&self) -> crate::NodeID;
    fn name(&self) -> &'cx crate::Binding<'cx>;
}

impl<'cx> HasBindingName<'cx> for crate::VarDecl<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn name(&self) -> &'cx crate::Binding<'cx> {
        self.binding
    }
}

impl<'cx> HasBindingName<'cx> for crate::ParamDecl<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn name(&self) -> &'cx crate::Binding<'cx> {
        self.name
    }
}

pub fn node_id_of_binding<'cx>(has_binding_name: &impl HasBindingName<'cx>) -> crate::NodeID {
    has_binding_name.id()
}
