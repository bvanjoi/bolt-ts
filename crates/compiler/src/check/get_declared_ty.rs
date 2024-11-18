use super::ty;
use super::TyChecker;
use crate::ast;
use crate::bind::SymbolID;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_declared_ty_of_symbol(&mut self, id: SymbolID) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(ty) = self.get_symbol_links(id).get_declared_ty() {
            return Some(ty);
        }
        use crate::bind::SymbolKind::*;
        match &self.symbols.get(id).kind {
            Class { .. } => {
                let ty = self.get_declared_ty_of_class(id);
                self.get_mut_symbol_links(id).set_declared_ty(ty);
                Some(ty)
            }
            _ => None,
        }
    }

    fn get_declared_ty_of_class(&mut self, id: SymbolID) -> &'cx ty::Ty<'cx> {
        let _outer_ty_params = self.get_outer_ty_params_of_class(id);
        // let local_ty_params = self.get
        self.crate_interface_ty(ty::InterfaceTy)
    }

    fn get_outer_ty_params_of_class(&mut self, id: SymbolID) -> Option<ast::TyParams<'cx>> {
        use crate::bind::SymbolKind::*;
        match self.symbols.get(id).kind {
            Class { decl, .. } => self.get_outer_ty_params(decl),
            _ => unreachable!(),
        }
    }

    fn get_outer_ty_params(&mut self, id: ast::NodeID) -> Option<ast::TyParams<'cx>> {
        let mut id = id;
        loop {
            if let Some(next) = self.node_parent_map.parent(id) {
                id = next;
            } else {
                return None;
            }

            // use ast::Node::*;
            // match self.nodes.get(id) {
            //     // ClassDecl(class) => return class.ty_params,
            //     // ClassExpr(class) => return class.ty_params,
            //     // _ => {}
            // }
        }
    }
}
