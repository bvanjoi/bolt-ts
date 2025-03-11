use crate::bind::{Symbol, SymbolFlags, SymbolID};
use crate::ty::CheckFlags;
use crate::{keyword, ty};
use bolt_ts_ast::{self as ast, pprint_ident};

use super::{TyChecker, errors};

pub(super) trait GetTypeFromTyReferLike<'cx> {
    fn id(&self) -> ast::NodeID;
    fn span(&self) -> bolt_ts_span::Span;
    fn name(&self) -> &'cx ast::EntityName<'cx>;
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>>;
}

impl<'cx> GetTypeFromTyReferLike<'cx> for ast::ReferTy<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn span(&self) -> bolt_ts_span::Span {
        self.span
    }
    fn name(&self) -> &'cx ast::EntityName<'cx> {
        self.name
    }
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>> {
        self.ty_args
    }
}

impl<'cx> GetTypeFromTyReferLike<'cx> for ast::ClassExtendsClause<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn span(&self) -> bolt_ts_span::Span {
        self.span
    }
    fn name(&self) -> &'cx ast::EntityName<'cx> {
        self.name
    }
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>> {
        self.ty_args
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn resolve_ty_refer_name(&mut self, name: &'cx ast::EntityName<'cx>) -> SymbolID {
        self.resolve_entity_name(name)
    }

    pub(super) fn ty_args_from_ty_refer_node(
        &mut self,
        ty_args: Option<&'cx ast::Tys<'cx>>,
    ) -> Option<ty::Tys<'cx>> {
        let ty_args = ty_args?
            .list
            .iter()
            .map(|arg| self.get_ty_from_type_node(arg))
            .collect::<Vec<_>>();
        Some(self.alloc(ty_args))
    }

    fn is_local_ty_alias(&self, symbol: SymbolID) -> bool {
        let s = &self.binder.symbol(symbol);
        if s.flags == SymbolFlags::TYPE_ALIAS {
            let s = s.expect_ty_alias();
            self.p.get_containing_fn(s.decl).is_some()
        } else {
            false
        }
    }

    fn get_ty_from_ty_alias_refer(
        &mut self,
        node: &impl GetTypeFromTyReferLike<'cx>,
        symbol: SymbolID,
    ) -> &'cx ty::Ty<'cx> {
        if self
            .get_check_flags(symbol)
            .intersects(CheckFlags::UNRESOLVED)
        {
            return self.error_ty;
        }
        let ty = self.get_declared_ty_of_symbol(symbol);

        if let Some(ty_params) = self.get_symbol_links(symbol).get_ty_params() {
            // let len = node.args().unwrap_or_default().len();
            let num_ty_args = node
                .ty_args()
                .map(|ty_args| ty_args.list.len())
                .unwrap_or_default();
            let min_ty_arg_count = self.get_min_ty_arg_count(Some(ty_params));
            if num_ty_args < min_ty_arg_count || num_ty_args > ty_params.len() {
                let ty = self.binder.symbol(symbol).name.to_string(self.atoms);
                let error: crate::Diag = if min_ty_arg_count == ty_params.len() {
                    Box::new(errors::GenericTypeXRequiresNTypeArguments {
                        span: node.span(),
                        ty,
                        n: ty_params.len(),
                    })
                } else {
                    Box::new(errors::GenericTypeXRequiresBetweenXAndYTypeArguments {
                        span: node.span(),
                        ty,
                        x: min_ty_arg_count,
                        y: ty_params.len(),
                    })
                };
                self.push_error(error);
                return self.error_ty;
            }
            let alias_symbol = self.get_alias_symbol_for_ty_node(node.id());
            let new_alias_symbol = alias_symbol.and_then(|alias_symbol| {
                if self.is_local_ty_alias(symbol) || !self.is_local_ty_alias(alias_symbol) {
                    Some(alias_symbol)
                } else {
                    None
                }
            });
            let mut alias_ty_args = None;
            if new_alias_symbol.is_some() {
                alias_ty_args = self.get_ty_args_for_alias_symbol(new_alias_symbol)
            } else if self.p.node(node.id()).is_ty_refer_ty() {
                let alias_symbol = self.resolve_ty_refer_name(node.name());
                // if alias_symbol != Symbol::ERR {

                // }
            }
            let ty_args = self
                .ty_args_from_ty_refer_node(node.ty_args())
                .unwrap_or_default();
            self.get_type_alias_instantiation(symbol, ty_args, new_alias_symbol, alias_ty_args)
        } else if self.check_no_ty_args(node) {
            ty
        } else {
            self.error_ty
        }
    }

    fn check_no_ty_args(&mut self, node: &impl GetTypeFromTyReferLike<'cx>) -> bool {
        if node.ty_args().is_some() {
            // TODO: report error
            false
        } else {
            true
        }
    }

    pub(super) fn concatenate<T: Copy>(
        &mut self,
        a: Option<&'cx [T]>,
        b: Option<&'cx [T]>,
    ) -> &'cx [T] {
        match (a, b) {
            (Some(a), Some(b)) => {
                let v = a.iter().chain(b).copied().collect::<Vec<_>>();
                self.alloc(v)
            }
            (Some(a), None) => a,
            (None, Some(b)) => b,
            (None, None) => self.empty_array(),
        }
    }

    pub(super) fn get_ty_from_class_or_interface_refer(
        &mut self,
        node: &impl GetTypeFromTyReferLike<'cx>,
        symbol: SymbolID,
    ) -> &'cx ty::Ty<'cx> {
        let ty = self.get_declared_ty_of_symbol(symbol);
        let i = if let Some(i) = ty.kind.as_object_interface() {
            i
        } else if let Some(r) = ty.kind.as_object_reference() {
            r.target.kind.expect_object_interface()
        } else {
            unreachable!()
        };
        if let Some(ty_params) = i.local_ty_params {
            let num_ty_args = node
                .ty_args()
                .map(|args| args.list.len())
                .unwrap_or_default();
            let min_ty_arg_count = self.get_min_ty_arg_count(Some(ty_params));
            if num_ty_args < min_ty_arg_count || num_ty_args > ty_params.len() {
                let error: crate::Diag = if min_ty_arg_count == ty_params.len() {
                    Box::new(errors::GenericTypeXRequiresNTypeArguments {
                        span: node.span(),
                        ty: self.print_ty(ty).to_string(),
                        n: ty_params.len(),
                    })
                } else {
                    Box::new(errors::GenericTypeXRequiresBetweenXAndYTypeArguments {
                        span: node.span(),
                        ty: self.print_ty(ty).to_string(),
                        x: min_ty_arg_count,
                        y: ty_params.len(),
                    })
                };
                self.push_error(error);
                return self.error_ty;
            }

            let resolved_ty_args = {
                let self_ty_args = self.ty_args_from_ty_refer_node(node.ty_args());
                let self_ty_args =
                    self.fill_missing_ty_args(self_ty_args, i.local_ty_params, min_ty_arg_count);
                self.concatenate(i.outer_ty_params, self_ty_args)
            };
            self.create_reference_ty(ty, Some(resolved_ty_args), ty.get_object_flags())
        } else {
            ty
        }
    }

    pub(super) fn get_ty_refer_type(
        &mut self,
        node: &impl GetTypeFromTyReferLike<'cx>,
        symbol: SymbolID,
    ) -> &'cx ty::Ty<'cx> {
        if symbol == Symbol::ERR {
            return self.error_ty;
        }
        let flags = self.binder.symbol(symbol).flags;
        if flags.intersects(SymbolFlags::CLASS | SymbolFlags::INTERFACE) {
            return self.get_ty_from_class_or_interface_refer(node, symbol);
        } else if flags == SymbolFlags::TYPE_ALIAS {
            return self.get_ty_from_ty_alias_refer(node, symbol);
        }

        if let Some(res) = self.try_get_declared_ty_of_symbol(symbol) {
            if self.check_no_ty_args(node) {
                self.get_regular_ty_of_literal_ty(res)
            } else {
                self.error_ty
            }
        } else {
            self.error_ty
        }
    }

    pub(super) fn get_ty_from_ty_reference(
        &mut self,
        node: &impl GetTypeFromTyReferLike<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(ty) = self.get_node_links(node.id()).get_resolved_ty() {
            return ty;
        }
        let name = node.name();
        if let ast::EntityNameKind::Ident(ident) = name.kind {
            match ident.name {
                keyword::IDENT_BOOLEAN => return self.boolean_ty(),
                keyword::IDENT_NUMBER => return self.number_ty,
                keyword::IDENT_STRING => return self.string_ty,
                keyword::IDENT_ANY => return self.any_ty,
                keyword::KW_VOID => return self.void_ty,
                keyword::IDENT_UNKNOWN => return self.unknown_ty,
                keyword::IDENT_NEVER => return self.never_ty,
                keyword::KW_UNDEFINED => return self.undefined_ty,
                keyword::KW_NULL => return self.null_ty,
                keyword::IDENT_SYMBOL => return self.symbol_ty,
                keyword::IDENT_OBJECT => return self.non_primitive_ty,
                keyword::IDENT_BIGINT => return self.bigint_ty,
                _ => {}
            }
        }
        let symbol = self.resolve_ty_refer_name(name);
        self.get_mut_node_links(node.id())
            .set_resolved_symbol(symbol);
        let ty = self.get_ty_refer_type(node, symbol);
        self.get_mut_node_links(node.id()).set_resolved_ty(ty);
        ty
    }
}
