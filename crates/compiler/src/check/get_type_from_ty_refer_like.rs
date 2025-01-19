use crate::bind::{Symbol, SymbolFlags, SymbolID};
use crate::ty::CheckFlags;
use crate::{ast, keyword, ty};

use super::{errors, TyChecker};

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
    pub(super) fn resolve_ty_refer_name(&mut self, name: &'cx ast::Ident) -> SymbolID {
        self.resolve_symbol_by_ident(name)
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

    // fn instantiate_ty_with_alias(&mut self) -> &'cx ty::Ty<'cx> {}

    fn get_ty_from_ty_alias_refer(
        &mut self,
        node: &impl GetTypeFromTyReferLike<'cx>,
        symbol: SymbolID,
    ) -> &'cx ty::Ty<'cx> {
        if self.check_flags(symbol).intersects(CheckFlags::UNRESOLVED) {
            return self.error_ty();
        }
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
            let args = self
                .ty_args_from_ty_refer_node(node.ty_args())
                .unwrap_or_default();
            self.get_type_alias_instantiation(symbol, args)
        } else if self.check_no_ty_args(node) {
            ty
        } else {
            self.error_ty()
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
                let v = a
                    .into_iter()
                    .chain(b.into_iter())
                    .copied()
                    .collect::<Vec<_>>();
                self.alloc(v)
            }
            (Some(a), None) => a,
            (None, Some(b)) => b,
            (None, None) => &[],
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
            let min_ty_arg_count = self.get_min_ty_args_count(ty_params);
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
            }

            let resolved_ty_args = {
                let self_ty_args = self.ty_args_from_ty_refer_node(node.ty_args());
                let self_ty_args =
                    self.fill_missing_ty_args(self_ty_args, i.local_ty_params, min_ty_arg_count);
                self.concatenate(i.outer_ty_params, self_ty_args)
            };
            self.create_reference_ty(
                ty::ReferenceTy {
                    target: ty,
                    resolved_ty_args,
                },
                ty.get_object_flags(),
            )
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
            return self.error_ty();
        }
        let s = &self.binder.symbol(symbol);
        if s.flags
            .intersects(SymbolFlags::CLASS | SymbolFlags::INTERFACE)
        {
            self.get_ty_from_class_or_interface_refer(node, symbol)
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
        if let Some(ty) = self.get_node_links(node.id()).get_resolved_ty() {
            return ty;
        }
        // TODO: cache
        let name = node.name();
        let ty = if let ast::EntityNameKind::Ident(ident) = name.kind {
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
            } else if ident.name == keyword::IDENT_UNKNOWN {
                self.unknown_ty()
            } else if ident.name == keyword::IDENT_NEVER {
                self.never_ty()
            } else {
                let symbol = self.resolve_ty_refer_name(ident);
                self.get_ty_refer_type(node, symbol)
            }
        } else {
            // TODO:
            self.undefined_ty()
        };
        self.get_mut_node_links(node.id()).set_resolved_ty(ty);
        ty
    }
}
