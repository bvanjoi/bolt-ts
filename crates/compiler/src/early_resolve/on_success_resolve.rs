use crate::bind::Symbol;
use crate::bind::SymbolFlags;
use crate::bind::SymbolID;
use bolt_ts_ast as ast;

use super::Resolver;
use super::errors;

impl<'cx> Resolver<'cx, '_, '_> {
    fn check_param_refer_itself(
        &self,
        ident: &'cx ast::Ident,
        symbol: SymbolID,
        associated_declaration_for_containing_initializer_or_binding_name: Option<ast::NodeID>,
    ) -> Option<errors::ParameterXCannotReferenceItself> {
        if let Some(associated_declaration_for_containing_initializer_or_binding_name) =
            associated_declaration_for_containing_initializer_or_binding_name
        {
            let node = self
                .p
                .node(associated_declaration_for_containing_initializer_or_binding_name);
            if let Some(param_decl) = node.as_param_decl() {
                let id = match param_decl.name.kind {
                    bolt_ts_ast::BindingKind::Ident(ident) => ident.id,
                    bolt_ts_ast::BindingKind::ObjectPat(_) => todo!(),
                    bolt_ts_ast::BindingKind::ArrayPat(_) => todo!(),
                };
                if symbol == self.symbol_of_decl(id) {
                    let error = errors::ParameterXCannotReferenceItself {
                        span: ident.span,
                        name: self.atoms.get(ident.name).to_string(),
                    };
                    return Some(error);
                }
            }
        }
        None
    }

    fn check_ty_param_used_in_static_but_declared_at_class(
        &mut self,
        ident: &'cx ast::Ident,
        symbol: SymbolID,
    ) -> Option<errors::StaticMembersCannotReferenceClassTypeParameters> {
        let symbol = self.symbol(symbol);
        if symbol.flags != SymbolFlags::TYPE_PARAMETER {
            None
        } else if self
            .p
            .node(self.p.parent(symbol.opt_decl().unwrap()).unwrap())
            .is_class_like()
            && self
                .p
                .find_ancestor(
                    ident.id,
                    |node| {
                        if node.is_static() { Some(true) } else { None }
                    },
                )
                .is_some()
        {
            let error =
                errors::StaticMembersCannotReferenceClassTypeParameters { span: ident.span };
            Some(error)
        } else {
            None
        }
    }

    pub(super) fn on_success_resolved_value_symbol(
        &mut self,
        ident: &'cx ast::Ident,
        symbol: SymbolID,
        associated_declaration_for_containing_initializer_or_binding_name: Option<ast::NodeID>,
    ) {
        if let Some(error) = self.check_param_refer_itself(
            ident,
            symbol,
            associated_declaration_for_containing_initializer_or_binding_name,
        ) {
            self.push_error(Box::new(error));
        }
    }

    pub(super) fn on_success_resolved_type_symbol(
        &mut self,
        ident: &'cx ast::Ident,
        symbol: &mut SymbolID,
    ) {
        if let Some(error) =
            self.check_ty_param_used_in_static_but_declared_at_class(ident, *symbol)
        {
            *symbol = Symbol::ERR;
            self.push_error(Box::new(error));
        }
    }
}
