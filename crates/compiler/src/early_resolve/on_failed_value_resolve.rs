use crate::ast;
use crate::bind::{Symbol, SymbolFlags};
use crate::keyword::is_prim_ty_name;

use super::Resolver;
use super::{errors, resolve_symbol_by_ident};

impl<'cx> Resolver<'cx, '_> {
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

    fn check_missing_prefix(
        &mut self,
        ident: &'cx ast::Ident,
    ) -> Option<errors::DidYouMeanTheStaticMember> {
        let mut location = ident.id;
        while let Some(parent) = self.p.parent(location) {
            location = parent;
            let node = self.p.node(location);
            let ast::Node::ClassDecl(class) = node else {
                continue;
            };
            // TODO: use class symbol;
            if let Some(prop) = class.elems.elems.iter().find_map(|ele| {
                let ast::ClassEleKind::Prop(prop) = ele.kind else {
                    return None;
                };
                let ast::PropNameKind::Ident(prop_name) = prop.name.kind else {
                    return None;
                };

                (prop_name.name == ident.name).then_some(prop)
            }) {
                if prop
                    .modifiers
                    .map(|mods| mods.flags.contains(ast::ModifierKind::Static))
                    .unwrap_or_default()
                {
                    let ast::PropNameKind::Ident(prop_name) = prop.name.kind else {
                        unreachable!()
                    };
                    let error = errors::DidYouMeanTheStaticMember {
                        span: prop.name.span(),
                        class_name: self.atoms.get(class.name.name).to_string(),
                        prop_name: self.atoms.get(prop_name.name).to_string(),
                    };
                    return Some(error);
                }
            }
        }

        None
    }

    fn check_object_shorthand_ident(
        &mut self,
        ident: &'cx ast::Ident,
    ) -> Option<errors::CannotFindNameHelperKind> {
        let parent_id = self.p.parent(ident.id)?;
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
        let symbol =
            resolve_symbol_by_ident(self, ident, |ns| ns.flags == SymbolFlags::NAMESPACE_MODULE)
                .symbol;
        if symbol == Symbol::ERR {
            None
        } else {
            let ns = self.symbol(symbol).expect_ns().decls[0];
            let span = self.p.node(ns).expect_namespace_decl().name.span();
            let error = errors::CannotFindNameHelperKind::CannotUseNamespaceAsTyOrValue(
                errors::CannotUseNamespaceAsTyOrValue { span, is_ty: false },
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
                .p
                .parent(ident.id)
                .and_then(|parent| self.p.parent(parent))?;
            let container = self.p.parent(grand)?;
            let grand_node = self.p.node(grand);
            let container_node = self.p.node(container);
            if grand_node.as_class_implements_clause().is_some() && container_node.is_class_like() {
                return Some(
                    errors::CannotFindNameHelperKind::AClassCannotImplementAPrimTy(
                        errors::AClassCannotImplementAPrimTy {
                            span: ident.span,
                            ty: self.atoms.get(ident.name).to_string(),
                        },
                    ),
                );
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
        }

        let res = resolve_symbol_by_ident(self, ident, Symbol::is_type).symbol;
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
