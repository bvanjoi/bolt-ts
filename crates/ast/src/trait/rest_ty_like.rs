pub trait RestTyLike<'cx>: Copy {
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>>;
}

impl<'cx> RestTyLike<'cx> for crate::RestTy<'cx> {
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        Some(self.ty)
    }
}

impl<'cx> RestTyLike<'cx> for crate::NamedTupleTy<'cx> {
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        self.dotdotdot.map(|_| self.ty)
    }
}
