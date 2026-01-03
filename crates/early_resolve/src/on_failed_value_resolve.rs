use bolt_ts_ast as ast;
use bolt_ts_ast::keyword::is_prim_ty_name;
use bolt_ts_binder::{Symbol, SymbolFlags};

use super::ResolvedResult;
use super::Resolver;
use super::{errors, resolve_symbol_by_ident};

impl<'cx> Resolver<'cx, '_, '_> {
    pub(super) fn on_failed_to_resolve_value_symbol(
        &mut self,
        ident: &'cx ast::Ident,
        mut error: errors::CannotFindName,
    ) -> errors::CannotFindName {
        if let Some(e) = self.check_missing_prefix(ident) {
            error
                .errors
                .push(errors::CannotFindNameHelperKind::DidYouMeanTheStaticMember(
                    e,
                ));
        }
        if let Some(e) = self.check_using_type_as_value(ident) {
            error.errors.push(e);
        }
        if let Some(e) = self.check_using_namespace_as_value(ident) {
            error.errors.push(e);
        }
        if let Some(e) = self.check_object_shorthand_ident(ident) {
            error.errors.push(e);
        }
        error
    }

    pub(super) fn on_property_with_invalid_initializer(
        &mut self,
        ident: &'cx ast::Ident,
        invalid_initializer: ast::NodeID,
        mut error: errors::CannotFindName,
    ) -> errors::CannotFindName {
        if self.emit_standard_class_fields {
            return error;
        }
        let invalid_initializer = self.p.node(invalid_initializer).expect_class_prop_elem();
        let invalid_initializer_name = self
            .atoms
            .get(invalid_initializer.name.kind.as_ident().unwrap().name);
        let sub_error = errors::InitializerOfInstanceMemberVariable0CannotReferenceIdentifier1DeclaredInTheConstructor {
            span: ident.span,
            x: self.atoms.get(ident.name).to_string(),
            y: invalid_initializer_name.to_string(),
        };
        error.errors.push(
            errors::CannotFindNameHelperKind::InitializerOfInstanceMemberVariable0CannotReferenceIdentifier1DeclaredInTheConstructor(
                sub_error,
            ),
        );
        error
    }

    fn check_missing_prefix(
        &mut self,
        ident: &'cx ast::Ident,
    ) -> Option<errors::DidYouMeanTheStaticMember> {
        let mut location = ident.id;
        while let Some(parent) = self.parent(location) {
            location = parent;
            let node = self.p.node(location);
            let ast::Node::ClassDecl(class) = node else {
                continue;
            };
            // TODO: use class symbol;
            if let Some(prop) = class.elems.list.iter().find_map(|ele| {
                let ast::ClassElemKind::Prop(prop) = ele.kind else {
                    return None;
                };
                let ast::PropNameKind::Ident(prop_name) = prop.name.kind else {
                    return None;
                };

                (prop_name.name == ident.name).then_some(prop)
            }) && prop
                .modifiers
                .map(|mods| mods.flags.contains(ast::ModifierKind::Static))
                .unwrap_or_default()
            {
                let ast::PropNameKind::Ident(prop_name) = prop.name.kind else {
                    unreachable!()
                };
                let error = errors::DidYouMeanTheStaticMember {
                    span: prop.name.span(),
                    class_name: self.atoms.get(class.name.unwrap().name).to_string(),
                    prop_name: self.atoms.get(prop_name.name).to_string(),
                };
                return Some(error);
            }
        }

        None
    }

    fn check_object_shorthand_ident(
        &mut self,
        ident: &'cx ast::Ident,
    ) -> Option<errors::CannotFindNameHelperKind> {
        let parent_id = self.parent(ident.id)?;
        if self.p.node(parent_id).is_object_shorthand_member() {
            let error = errors::CannotFindNameHelperKind::ShorthandPropertyNeedAnInitializer(
                errors::ShorthandPropertyNeedAnInitializer { span: ident.span },
            );
            return Some(error);
        }
        None
    }

    fn check_using_namespace_as_value(
        &mut self,
        ident: &'cx ast::Ident,
    ) -> Option<errors::CannotFindNameHelperKind> {
        let symbol = resolve_symbol_by_ident(self, ident, SymbolFlags::NAMESPACE_MODULE).symbol;
        if symbol == Symbol::ERR {
            None
        } else {
            let ns = self.symbol(symbol).decls.as_ref().unwrap()[0];
            let span = self.p.node(ns).expect_module_decl().name.span();
            let error = errors::CannotFindNameHelperKind::CannotUseNamespaceAsTyOrValue(
                errors::CannotUseNamespaceAsTyOrValue { span, is_ty: false },
            );
            Some(error)
        }
    }

    pub(super) fn on_failed_to_resolve_type_symbol(
        &mut self,
        ident: &'cx ast::Ident,
        res: &ResolvedResult,
        mut error: errors::CannotFindName,
    ) -> errors::CannotFindName {
        if let Some(e) = self.check_using_namespace_as_type(ident) {
            error.errors.push(e);
        } else if res.base_class_expression_cannot_reference_class_type_parameters {
            let e =
                errors::BaseClassExpressionsCannotReferenceClassTypeParameters { span: ident.span };
            error.errors.push(errors::CannotFindNameHelperKind::BaseClassExpressionsCannotReferenceClassTypeParameters(e));
        }

        error
    }

    fn check_using_namespace_as_type(
        &mut self,
        ident: &'cx ast::Ident,
    ) -> Option<errors::CannotFindNameHelperKind> {
        let symbol = resolve_symbol_by_ident(self, ident, SymbolFlags::NAMESPACE_MODULE).symbol;
        if symbol == Symbol::ERR {
            None
        } else {
            let ns = self.symbol(symbol).decls.as_ref().unwrap()[0];
            let span = self.p.node(ns).expect_module_decl().name.span();
            let error = errors::CannotFindNameHelperKind::CannotUseNamespaceAsTyOrValue(
                errors::CannotUseNamespaceAsTyOrValue { span, is_ty: true },
            );
            Some(error)
        }
    }

    pub(super) fn check_using_type_as_value(
        &mut self,
        ident: &'cx ast::Ident,
    ) -> Option<errors::CannotFindNameHelperKind> {
        if is_prim_ty_name(ident.name) {
            let grand = self
                .parent(ident.id)
                .and_then(|parent| self.parent(parent))?;
            let container = self.parent(grand)?;
            let grand_node = self.p.node(grand);
            let container_node = self.p.node(container);
            if grand_node.as_class_implements_clause().is_some() && container_node.is_class_like() {
                // report at type check
                return None;
                // return Some(
                //     errors::CannotFindNameHelperKind::AClassCannotImplementAPrimTy(
                //         errors::AClassCannotImplementAPrimTy {
                //             span: ident.span,
                //             ty: self.atoms.get(ident.name).to_string(),
                //         },
                //     ),
                // );
            } else if grand_node.as_interface_extends_clause().is_some()
                && container_node.is_interface_decl()
            {
                return Some(
                    errors::CannotFindNameHelperKind::AnInterfaceCannotExtendAPrimTy(
                        errors::AnInterfaceCannotExtendAPrimTy {
                            span: ident.span,
                            ty: self.atoms.get(ident.name).to_string(),
                        },
                    ),
                );
            }
            return None;
        }

        let res = resolve_symbol_by_ident(self, ident, SymbolFlags::TYPE).symbol;
        if res != Symbol::ERR {
            let error = errors::CannotFindNameHelperKind::OnlyReferToATypeButIsBeingUsedAsValueHere(
                errors::OnlyReferToATypeButIsBeingUsedAsValueHere {
                    span: ident.span,
                    name: self.atoms.get(ident.name).to_string(),
                    defined_here: self
                        .p
                        .node(self.symbol(res).opt_decl().unwrap())
                        .ident_name()
                        .unwrap()
                        .span,
                },
            );
            return Some(error);
        }

        None
    }
}
