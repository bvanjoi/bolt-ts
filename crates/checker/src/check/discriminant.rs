use super::TyChecker;
use super::ty;

use bolt_ts_ast::{self as ast};
use bolt_ts_binder::SymbolFlags;
use bolt_ts_binder::SymbolID;
use bolt_ts_binder::SymbolName;
use bolt_ts_ty::CheckFlags;

impl<'cx> TyChecker<'cx> {
    fn get_candidate_discriminant_prop_access(
        &mut self,
        refer: ast::NodeID,
        expr_id: ast::NodeID,
    ) -> Option<ast::NodeID> {
        let expr = self.p.node(expr_id);
        let n = self.p.node(refer);
        if n.is_array_pat()
            || n.is_object_pat()
            || n.is_fn_expr_or_arrow_fnc_expr()
            || n.is_object_method_member()
        {
            if let ast::Node::Ident(n) = expr {
                let symbol = self.final_res(n.id);
                let symbol = self.get_export_symbol_of_value_symbol_if_exported(symbol);
                if let Some(declaration) = self.symbol(symbol).value_decl
                    && self.parent(declaration) == Some(refer)
                {
                    let use_declaration = match self.p.node(declaration) {
                        ast::Node::ObjectBindingElem(n)
                            if n.init.is_none() && n.dotdotdot.is_none() =>
                        {
                            true
                        }
                        // TODO: array binding element
                        ast::Node::ParamDecl(n) if n.init.is_none() && n.dotdotdot.is_none() => {
                            true
                        }
                        _ => false,
                    };
                    if use_declaration {
                        return Some(declaration);
                    }
                }
            }
        } else if let ast::Node::PropAccessExpr(ast::PropAccessExpr { expr: target, .. })
        | ast::Node::EleAccessExpr(ast::EleAccessExpr { expr: target, .. }) = expr
        {
            if self.is_matching_reference(refer, target.id()) {
                return Some(expr.id());
            }
        } else if let ast::Node::Ident(ident) = expr {
            // TODO:
        }

        None
    }

    pub(super) fn get_discriminant_prop_access(
        &mut self,
        refer: ast::NodeID,
        expr: ast::NodeID,
        computed_ty: &'cx ty::Ty<'cx>,
        declared_ty: &'cx ty::Ty<'cx>,
    ) -> Option<ast::NodeID> {
        if (declared_ty.flags.contains(ty::TypeFlags::UNION)
            || computed_ty.flags.contains(ty::TypeFlags::UNION))
            && let Some(access) = self.get_candidate_discriminant_prop_access(refer, expr)
        {
            if let Some(name) = self.get_accessed_prop_name(access) {
                let ty = if declared_ty.flags.contains(ty::TypeFlags::UNION)
                    && self.is_ty_subset_of(computed_ty, declared_ty)
                {
                    declared_ty
                } else {
                    computed_ty
                };
                if self.is_discriminant_prop(ty, name) {
                    return Some(access);
                }
            }
        }
        None
    }

    pub(super) fn is_discriminant_prop(&mut self, ty: &'cx ty::Ty<'cx>, name: SymbolName) -> bool {
        if ty.kind.is_union()
            && let Some(prop) = self.get_union_or_intersection_prop::<false>(ty, name)
            && let check_flags = self.get_check_flags(prop)
            && check_flags.contains(CheckFlags::SYNTHETIC_PROPERTY)
        {
            return match self.get_symbol_links(prop).get_is_discriminant_property() {
                Some(is_discriminant) => is_discriminant,
                None => {
                    let is_discriminant = check_flags.intersects(CheckFlags::DISCRIMINANT) && {
                        let ty = self.get_type_of_symbol(prop);
                        !self.is_generic_ty(ty)
                    };
                    self.get_mut_symbol_links(prop)
                        .set_is_discriminant_property(is_discriminant);
                    is_discriminant
                }
            };
        }
        false
    }

    pub(super) fn find_discriminant_props(
        &mut self,
        source_props: &[SymbolID],
        target: &'cx ty::Ty<'cx>,
    ) -> Vec<SymbolID> {
        source_props
            .iter()
            .filter_map(|&prop| {
                let name = self.symbol(prop).name;
                if self.is_discriminant_prop(target, name) {
                    Some(prop)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
    }

    pub(super) fn is_discriminant_with_never_ty(&mut self, symbol: SymbolID) -> bool {
        self.symbol(symbol).flags.intersects(SymbolFlags::OPTIONAL)
            && self
                .get_check_flags(symbol)
                .intersection(CheckFlags::DISCRIMINANT.union(CheckFlags::HAS_NEVER_TYPE))
                == CheckFlags::DISCRIMINANT
            && self
                .get_type_of_symbol(symbol)
                .flags
                .contains(ty::TypeFlags::NEVER)
    }
}
