use bolt_ts_ast as ast;

pub trait RestTyLike<'cx>: Copy {
    fn ty(&self) -> Option<&'cx ast::Ty<'cx>>;
}

impl<'cx> RestTyLike<'cx> for ast::RestTy<'cx> {
    fn ty(&self) -> Option<&'cx ast::Ty<'cx>> {
        Some(&self.ty)
    }
}

impl<'cx> RestTyLike<'cx> for ast::NamedTupleTy<'cx> {
    fn ty(&self) -> Option<&'cx ast::Ty<'cx>> {
        self.dotdotdot.map(|_| self.ty)
    }
}
