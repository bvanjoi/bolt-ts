use crate::ast;

pub trait ArrayTyLike<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn element(&self) -> Option<&'cx ast::Ty<'cx>>;
    fn elements(&self) -> Option<&'cx [&'cx ast::Ty<'cx>]>;
}

impl<'cx> ArrayTyLike<'cx> for ast::ArrayTy<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn element(&self) -> Option<&'cx ast::Ty<'cx>> {
        Some(self.ele)
    }
    fn elements(&self) -> Option<&'cx [&'cx ast::Ty<'cx>]> {
        None
    }
}

impl<'cx> ArrayTyLike<'cx> for ast::TupleTy<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn element(&self) -> Option<&'cx ast::Ty<'cx>> {
        None
    }
    fn elements(&self) -> Option<&'cx [&'cx ast::Ty<'cx>]> {
        Some(self.tys)
    }
}
