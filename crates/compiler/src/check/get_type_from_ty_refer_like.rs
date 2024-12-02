use crate::{
    ast,
    bind::{Symbol, SymbolID, SymbolKind},
    ty,
};

use super::TyChecker;

pub(super) trait GetTypeFromTyReferLike<'cx> {
    fn id(&self) -> ast::NodeID;
    fn name(&self) -> Option<&'cx ast::Ident>; // TODO: support member name
    fn args(&self) -> Option<ast::Tys<'cx>>;
}

impl<'cx> GetTypeFromTyReferLike<'cx> for ast::Expr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id()
    }
    fn name(&self) -> Option<&'cx ast::Ident> {
        if let ast::ExprKind::Ident(ident) = self.kind {
            Some(ident)
        } else {
            None
        }
    }
    fn args(&self) -> Option<ast::Tys<'cx>> {
        None
    }
}

impl<'cx> GetTypeFromTyReferLike<'cx> for ast::ReferTy<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn name(&self) -> Option<&'cx ast::Ident> {
        Some(self.name)
    }
    fn args(&self) -> Option<ast::Tys<'cx>> {
        self.args
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn resolve_ty_refer_name(&mut self, name: &'cx ast::Ident) -> SymbolID {
        let symbol = self.resolve_symbol_by_ident(name, SymbolKind::is_type);
        if symbol == Symbol::ERR {
            if let Some(error) = self.check_using_type_as_value(&name) {
                self.push_error(name.span.module, error);
            }
        }
        symbol
    }

    pub(super) fn get_ty_refer_type(
        &mut self,
        node: &impl GetTypeFromTyReferLike<'cx>,
        symbol: SymbolID,
    ) -> &'cx ty::Ty<'cx> {
        if symbol == Symbol::ERR {
            return self.error_ty();
        }
        let module = node.id().module();
        let symbol_kind = &self.binder.get(module).symbols.get(symbol).kind;
        if symbol_kind.is_class() || symbol_kind.is_interface() {
            self.get_declared_ty_of_symbol(module, symbol)
        } else if matches!(symbol_kind, SymbolKind::TypeAlias { .. }) {
            self.get_declared_ty_of_symbol(module, symbol)
        } else if let Some(res) = self.try_get_declared_ty_of_symbol(module, symbol) {
            res
        } else {
            self.error_ty()
        }
    }

    pub(super) fn get_ty_from_ty_reference(
        &mut self,
        node: &impl GetTypeFromTyReferLike<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        // TODO: cache
        if let Some(name) = node.name() {
            let symbol = self.resolve_ty_refer_name(name);
            self.get_ty_refer_type(node, symbol)
        } else {
            // TODO:
            self.undefined_ty()
        }
    }
}
