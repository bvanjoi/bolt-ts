use bolt_ts_ast as ast;
use bolt_ts_ast::r#trait::node_id_of_binding;
use bolt_ts_binder::Symbol;
use bolt_ts_binder::SymbolFlags;
use bolt_ts_binder::SymbolID;

use crate::get_symbol;

use super::Resolver;
use super::errors;

impl<'cx> Resolver<'cx, '_, '_> {
    fn check_param_refer_itself(
        &self,
        ident: &'cx ast::Ident,
        result: SymbolID,
        associated_declaration_for_containing_initializer_or_binding_name: ast::Node<'cx>,
    ) -> Option<errors::ParameterXCannotReferenceItself> {
        if let Some(param_decl) =
            associated_declaration_for_containing_initializer_or_binding_name.as_param_decl()
        {
            let id = node_id_of_binding(param_decl);
            if result == self.symbol_of_decl(id) {
                let error = errors::ParameterXCannotReferenceItself {
                    span: ident.span,
                    name: self.atoms.get(ident.name).to_string(),
                };
                return Some(error);
            }
        }
        None
    }

    // TODO: mv this check into on_failed_check
    fn check_ty_param_used_in_static_but_declared_at_class(
        &mut self,
        ident: &'cx ast::Ident,
        symbol: SymbolID,
    ) -> Option<errors::StaticMembersCannotReferenceClassTypeParameters> {
        let symbol = self.symbol(symbol);
        if symbol.flags != SymbolFlags::TYPE_PARAMETER {
            return None;
        }
        let p = self.parent(symbol.opt_decl().unwrap()).unwrap();
        let p_node = self.p.node(p);
        if p_node.is_class_like() {
            if self
                .node_query()
                .find_ancestor(
                    ident.id,
                    |node| {
                        if node.is_static() { Some(true) } else { None }
                    },
                )
                .is_some()
            {
                // ```txt
                // class A<T> {
                //     static x: T;
                //         //    ~ ERROR
                //     static y(a:T) {}
                //        //      ~ ERROR
                // }
                // ```
                //
                let error =
                    errors::StaticMembersCannotReferenceClassTypeParameters { span: ident.span };
                Some(error)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub(super) fn on_success_resolved_value_symbol(
        &mut self,
        ident: &'cx ast::Ident,
        result: SymbolID,
        associated_declaration_for_containing_initializer_or_binding_name: Option<ast::NodeID>,
        within_deferred_context: bool,
    ) {
        if !within_deferred_context
            && let Some(associated_declaration_for_containing_initializer_or_binding_name) =
                associated_declaration_for_containing_initializer_or_binding_name
        {
            let node = self
                .p
                .node(associated_declaration_for_containing_initializer_or_binding_name);
            if let Some(error) = self.check_param_refer_itself(ident, result, node) {
                self.push_error(Box::new(error));
            } else if let s = self.symbol(result)
                && let Some(value_decl) = s.value_decl
                && self.p.node(value_decl).span().hi() > node.span().hi()
                && let nq = self.node_query()
                && let root = nq.get_root_decl(
                    associated_declaration_for_containing_initializer_or_binding_name,
                )
                && let root_parent = self.parent(root).unwrap()
                && let Some(locals) = self.locals(root_parent)
                && get_symbol(self, locals, s.name, SymbolFlags::VALUE)
                    .is_some_and(|res| res == result)
            {
                let error = errors::ParameterXCannotReferenceIdentifierYDeclaredAfterIt {
                    span: ident.span,
                    parameter: self.atoms.get(node.ident_name().unwrap().name).to_string(),
                    identifier: self.atoms.get(ident.name).to_string(),
                };
                self.push_error(Box::new(error));
            }
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
