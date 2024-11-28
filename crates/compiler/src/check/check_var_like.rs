use crate::{ast, ty};

use super::symbol_links::SymbolLinks;
use super::TyChecker;

pub(super) trait VarLikeDecl<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn decl_ty(&self) -> Option<&'cx ast::Ty<'cx>>;
    fn init(&self) -> Option<&'cx ast::Expr<'cx>>;
    fn binding(&self) -> &'cx ast::Ident;
}

impl<'cx> VarLikeDecl<'cx> for ast::VarDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }

    fn decl_ty(&self) -> Option<&'cx ast::Ty<'cx>> {
        self.ty
    }

    fn init(&self) -> Option<&'cx ast::Expr<'cx>> {
        self.init
    }

    fn binding(&self) -> &'cx ast::Ident {
        self.binding
    }
}

impl<'cx> VarLikeDecl<'cx> for ast::ClassPropEle<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }

    fn decl_ty(&self) -> Option<&'cx ast::Ty<'cx>> {
        self.ty
    }

    fn init(&self) -> Option<&'cx ast::Expr<'cx>> {
        self.init
    }

    fn binding(&self) -> &'cx ast::Ident {
        match self.name.kind {
            ast::PropNameKind::Ident(ident) => ident,
        }
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_var_like_decl(&mut self, decl: &'cx impl VarLikeDecl<'cx>) {
        let symbol = self.get_symbol_of_decl(decl.id());
        let decl_ty = self.get_widened_ty_for_var_like_decl(decl);
        let mut ty = decl_ty;
        if let Some(init) = decl.init() {
            let init_ty = self.check_expr(init);
            if let Some(decl_ty) = decl_ty {
                // let v: ty = init
                self.check_type_assignable_to_and_optionally_elaborate(
                    decl.binding().span,
                    init_ty,
                    decl_ty,
                );
            }
            if ty.is_none() {
                ty = Some(init_ty);
            }
        }
        let ty = ty.unwrap_or(self.undefined_ty());
        self.symbol_links
            .entry((decl.id().module(), symbol))
            .or_insert_with(|| SymbolLinks::new().with_ty(ty));
    }

    fn get_widened_ty_for_var_like_decl(
        &mut self,
        decl: &'cx impl VarLikeDecl<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        self.get_ty_for_var_like_decl(decl)
    }

    fn get_ty_for_var_like_decl(
        &mut self,
        decl: &'cx impl VarLikeDecl<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        decl.decl_ty().map(|ty| self.get_ty_from_type_node(ty))
    }
}
