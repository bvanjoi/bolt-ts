use super::ast;
use super::ty;
use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_contextual_ty(&mut self, id: ast::NodeID) -> Option<&'cx ty::Ty<'cx>> {
        let Some(parent) = self.p.get(id.module()).parent_map().parent(id) else {
            unreachable!()
        };
        let parent = self.p.get(id.module()).nodes().get(parent);
        use ast::Node::*;
        match parent {
            VarDecl(decl) => self.get_contextual_ty_for_var_like_decl(decl.id),
            _ => None,
        }
    }

    fn get_contextual_ty_for_var_like_decl(&mut self, id: ast::NodeID) -> Option<&'cx ty::Ty<'cx>> {
        let node = self.p.get(id.module()).nodes().get(id);
        use ast::Node::*;
        match node {
            VarDecl(decl) => {
                if let Some(init) = decl.init {
                    if let Some(decl_ty) = decl.ty {
                        return Some(self.get_ty_from_type_node(decl_ty));
                    }
                }
            }
            _ => unreachable!(),
        };

        None
    }
}
