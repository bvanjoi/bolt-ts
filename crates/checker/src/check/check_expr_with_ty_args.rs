use super::TyChecker;
use super::ty;
use crate::check::{SymbolLinks, create_ty::IntersectionFlags};
use bolt_ts_ast::{self as ast, keyword};
use bolt_ts_binder::SymbolFlags;

pub(super) trait ExprWithTyArgs<'cx> {
    fn id(&self) -> ast::NodeID;
    fn expr_name(&self) -> Option<&'cx ast::EntityName<'cx>>;
    fn expr(&self) -> Option<&'cx ast::Expr<'cx>>;
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>>;
}

impl<'cx> ExprWithTyArgs<'cx> for ast::TypeofTy<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn expr_name(&self) -> Option<&'cx ast::EntityName<'cx>> {
        Some(self.name)
    }
    fn expr(&self) -> Option<&'cx ast::Expr<'cx>> {
        None
    }
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>> {
        self.ty_args
    }
}

impl<'cx> ExprWithTyArgs<'cx> for ast::ExprWithTyArgs<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn expr_name(&self) -> Option<&'cx ast::EntityName<'cx>> {
        None
    }
    fn expr(&self) -> Option<&'cx ast::Expr<'cx>> {
        Some(self.expr)
    }
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>> {
        self.ty_args
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_expr_with_ty_args(
        &mut self,
        node: &impl ExprWithTyArgs<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(ty_args) = node.ty_args() {
            for ty_arg in ty_args.list {
                self.check_ty(ty_arg);
            }
        }
        let expr_ty = if let Some(expr_name) = node.expr_name() {
            // typeof node
            if let ast::EntityNameKind::Ident(ident) = expr_name.kind
                && ident.name == keyword::KW_THIS
            {
                // TODO: check this expr
            }
            self.check_entity_name(expr_name)
        } else {
            // exprWithTyArgs node
            self.check_expr(node.expr().unwrap())
        };
        self.get_instantiation_expr_ty(expr_ty, node)
    }

    fn get_instantiation_expr_ty(
        &mut self,
        expr_ty: &'cx ty::Ty<'cx>,
        node: &impl ExprWithTyArgs<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let Some(ty_args) = node.ty_args() else {
            return expr_ty;
        };
        if expr_ty == self.silent_never_ty || self.is_error(expr_ty) {
            return expr_ty;
        }
        // TODO: cache
        // TODO: handle error
        get_instantiated_ty(self, expr_ty, ty_args, node.id())
    }
}

// struct InstantiatedTyPart<'cx> {
//     result: &'cx ty::Ty<'cx>,
//     has_applicable_sig: bool,
//     has_some_applicable_sig: bool,
// }

fn get_instantiated_ty_part<'cx>(
    checker: &mut TyChecker<'cx>,
    ty: &'cx ty::Ty<'cx>,
    ty_args: &'cx ast::Tys<'cx>,
    // has_sigs: &mut bool,
    // has_applicable_sig: &mut bool,
    // has_some_applicable_sig: &mut bool,
    // non_applicable_ty: &mut Option<&'cx ty::Ty<'cx>>,
    node_id: ast::NodeID,
) -> &'cx ty::Ty<'cx> {
    if ty.kind.is_object() {
        checker.resolve_structured_type_members(ty);
        let resolved = checker.expect_ty_links(ty.id).expect_structured_members();
        // *has_sigs |= !resolved.call_sigs.is_empty() || !resolved.ctor_sigs.is_empty();
        let call_sigs = get_instantiated_sigs(checker, resolved.call_sigs, ty_args);
        let ctor_sigs = get_instantiated_sigs(checker, resolved.ctor_sigs, ty_args);
        // *has_applicable_sig |= !call_sigs.is_empty() || !ctor_sigs.is_empty();
        if !std::ptr::eq(call_sigs, resolved.call_sigs)
            || !std::ptr::eq(ctor_sigs, resolved.ctor_sigs)
        {
            let links = SymbolLinks::default();
            let symbol = checker.create_transient_symbol(
                bolt_ts_binder::SymbolName::InstantiationExpression,
                SymbolFlags::TRANSIENT,
                links,
                None,
                None,
            );
            return checker.create_anonymous_ty_with_resolved(
                Some(symbol),
                ty::ObjectFlags::INSTANTIATION_EXPRESSION_TYPE,
                resolved.members,
                call_sigs,
                ctor_sigs,
                resolved.index_infos,
                Some(node_id),
            );
        }
    } else if ty
        .flags
        .intersects(ty::TypeFlags::INSTANTIABLE_NON_PRIMITIVE)
        && let Some(c) = checker.get_base_constraint_of_ty(ty)
        && let instantiated = get_instantiated_ty_part(
            checker, c, ty_args,
            // has_sigs,
            // has_applicable_sig,
            // has_some_applicable_sig,
            // non_applicable_ty,
            node_id,
        )
        && c != instantiated
    {
        return instantiated;
    } else if let Some(u) = ty.kind.as_union() {
        return checker
            .map_union_ty(
                ty,
                u,
                |this, item| {
                    Some(get_instantiated_ty(
                        this, item, ty_args,
                        // has_some_applicable_sig,
                        // non_applicable_ty,
                        node_id,
                    ))
                },
                false,
            )
            .unwrap();
    } else if let Some(i) = ty.kind.as_intersection() {
        let tys = checker
            .same_map_tys(Some(i.tys), |this, item, _| {
                get_instantiated_ty_part(
                    this, item, ty_args,
                    // has_sigs,
                    // has_applicable_sig,
                    // has_some_applicable_sig,
                    // non_applicable_ty,
                    node_id,
                )
            })
            .unwrap();
        return checker.get_intersection_ty(tys, IntersectionFlags::None, None, None);
    }
    ty
}

fn get_instantiated_sigs<'cx>(
    checker: &mut TyChecker<'cx>,
    sigs: ty::Sigs<'cx>,
    ty_args: &'cx ast::Tys<'cx>,
) -> ty::Sigs<'cx> {
    let applicable_sigs = checker.filter(sigs, |checker, sig| {
        sig.ty_params.is_some() && checker.has_correct_ty_arg_arity(sig, Some(ty_args))
    });
    checker
        .same_map_sigs(Some(applicable_sigs), |this, sig, _| {
            if let Some(ty_arg_tys) = this.check_ty_args(sig, ty_args, true) {
                //TODO: is_js
                let is_js = false;
                this.get_sig_instantiation(sig, Some(ty_arg_tys), is_js, None)
            } else {
                sig
            }
        })
        .unwrap()
}

fn get_instantiated_ty<'cx>(
    checker: &mut TyChecker<'cx>,
    ty: &'cx ty::Ty<'cx>,
    ty_args: &'cx ast::Tys<'cx>,
    node_id: ast::NodeID,
) -> &'cx ty::Ty<'cx> {
    // let mut has_sigs = false;
    // let mut has_applicable_sig = false;
    let result = get_instantiated_ty_part(
        checker, ty, ty_args,
        // &mut has_sigs,
        // &mut has_applicable_sig,
        // has_some_applicable_sig,
        // non_applicable_ty,
        node_id,
    );
    // *has_some_applicable_sig |= has_applicable_sig;
    // if has_sigs && !has_applicable_sig && non_applicable_ty.is_none() {
    //     non_applicable_ty.insert(ty);
    // }
    result
}
