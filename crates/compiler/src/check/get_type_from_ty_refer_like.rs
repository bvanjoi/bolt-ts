use crate::bind::{Symbol, SymbolFlags, SymbolID};
use crate::{ast, keyword, ty};

use super::TyChecker;

pub(super) trait GetTypeFromTyReferLike<'cx> {
    fn id(&self) -> ast::NodeID;
    fn name(&self) -> Option<ast::EntityName<'cx>>;
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>>;
}

impl<'cx> GetTypeFromTyReferLike<'cx> for ast::Expr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id()
    }
    fn name(&self) -> Option<ast::EntityName<'cx>> {
        if let ast::ExprKind::Ident(ident) = self.kind {
            Some(ast::EntityName {
                kind: ast::EntityNameKind::Ident(ident),
            })
        } else {
            None
        }
    }
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>> {
        None
    }
}

impl<'cx> GetTypeFromTyReferLike<'cx> for ast::ReferTy<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn name(&self) -> Option<ast::EntityName<'cx>> {
        Some(*self.name)
    }
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>> {
        self.ty_args
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn resolve_ty_refer_name(&mut self, name: &'cx ast::Ident) -> SymbolID {
        self.resolve_symbol_by_ident(name)
    }

    fn ty_args_from_ty_refer_node(&mut self, ty_args: &'cx ast::Tys<'cx>) -> ty::Tys<'cx> {
        let ty_args = ty_args
            .list
            .iter()
            .map(|arg| self.get_ty_from_type_node(arg))
            .collect::<Vec<_>>();
        self.alloc(ty_args)
    }

    // fn instantiate_ty_with_alias(&mut self) -> &'cx ty::Ty<'cx> {}

    fn get_ty_from_ty_alias_refer(
        &mut self,
        node: &impl GetTypeFromTyReferLike<'cx>,
        symbol: SymbolID,
    ) -> &'cx ty::Ty<'cx> {
        let ty = self.get_declared_ty_of_symbol(symbol);
        if let Some(ty_params) = self.get_symbol_links(symbol).get_ty_params() {
            // let len = node.args().unwrap_or_default().len();
            // if len > ty_params.len() || len < self.get_min_ty_args_count(ty_params) {
            //     todo!()
            // }
            // let alias_symbol = self.get_alias_symbol_for_ty_node(node.id());
            // if self
            //     .p
            //     .get(node.id().module())
            //     .nodes()
            //     .get(node.id())
            //     .is_ty_refer_ty()
            // {
            //     let alias_symbol = self.resolve_ty_refer_name(name)
            // }
            let args = node
                .ty_args()
                .map(|args| self.ty_args_from_ty_refer_node(args))
                .unwrap_or_default();
            self.get_type_alias_instantiation(symbol, args)
        } else {
            todo!()
        }
    }

    fn check_no_ty_args(&mut self, node: &impl GetTypeFromTyReferLike<'cx>) -> bool {
        if node.ty_args().is_some() {
            todo!("error")
        } else {
            true
        }
    }

    pub(super) fn get_ty_refer_type(
        &mut self,
        node: &impl GetTypeFromTyReferLike<'cx>,
        symbol: SymbolID,
    ) -> &'cx ty::Ty<'cx> {
        if symbol == Symbol::ERR {
            return self.error_ty();
        }
        let s = &self.binder.symbol(symbol);
        if (SymbolFlags::CLASS | SymbolFlags::INTERFACE).intersects(s.flags) {
            self.get_declared_ty_of_symbol(symbol)
        } else if s.flags == SymbolFlags::TYPE_ALIAS {
            self.get_ty_from_ty_alias_refer(node, symbol)
        } else if let Some(res) = self.try_get_declared_ty_of_symbol(symbol) {
            if self.check_no_ty_args(node) {
                res
            } else {
                self.error_ty()
            }
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
            if let ast::EntityNameKind::Ident(ident) = name.kind {
                if ident.name == keyword::IDENT_BOOLEAN {
                    self.boolean_ty()
                } else if ident.name == keyword::IDENT_NUMBER {
                    self.number_ty()
                } else if ident.name == keyword::IDENT_STRING {
                    self.string_ty()
                } else if ident.name == keyword::IDENT_ANY {
                    self.any_ty()
                } else if ident.name == keyword::KW_VOID {
                    self.void_ty()
                } else {
                    let symbol = self.resolve_ty_refer_name(ident);
                    self.get_ty_refer_type(node, symbol)
                }
            } else {
                // TODO:
                self.undefined_ty()
            }
        } else {
            // TODO:
            self.undefined_ty()
        }
    }
}
