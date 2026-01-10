pub trait ObjectLitElementLike<'cx> {
    fn id(&self) -> crate::NodeID;
    fn name(&self) -> Option<crate::PropNameKind<'cx>>;
}

impl<'cx> ObjectLitElementLike<'cx> for crate::ObjectPropAssignment<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn name(&self) -> Option<crate::PropNameKind<'cx>> {
        Some(self.name.kind)
    }
}

impl<'cx> ObjectLitElementLike<'cx> for crate::ObjectShorthandMember<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn name(&self) -> Option<crate::PropNameKind<'cx>> {
        Some(crate::PropNameKind::Ident(self.name))
    }
}

impl<'cx> ObjectLitElementLike<'cx> for crate::ObjectMethodMember<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn name(&self) -> Option<crate::PropNameKind<'cx>> {
        Some(self.name.kind)
    }
}
