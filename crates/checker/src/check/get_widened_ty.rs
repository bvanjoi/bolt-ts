use super::CheckMode;
use super::ContextFlags;
use super::TyChecker;
use super::symbol_info::SymbolInfo;
use super::ty;
use super::ty::ObjectFlags;
use super::ty::TypeFlags;

use bolt_ts_ast as ast;
use bolt_ts_ast::r#trait;
use bolt_ts_binder::SymbolFlags;
use bolt_ts_binder::SymbolID;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_widened_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        self.get_widened_ty_with_context(ty)
    }

    // TODO: WideningContext
    fn get_widened_ty_with_context(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if !ty
            .get_object_flags()
            .intersects(ty::ObjectFlags::REQUIRES_WIDENING)
        {
            return ty;
        }
        // TODO: widened cache
        if ty
            .flags
            .intersects(TypeFlags::ANY.union(TypeFlags::NULLABLE))
        {
            self.any_ty
        } else if ty.is_object_literal() {
            self.get_widened_type_of_object_lit(ty)
        } else if self.is_array_or_tuple(ty) {
            let refer = ty.kind.expect_object_reference();
            let ty_args = self.get_ty_arguments(ty);
            let ty_args =
                self.same_map_tys(Some(ty_args), |this, ty_arg, _| this.get_widened_ty(ty_arg));
            assert!(ty_args.is_some());
            self.create_reference_ty(refer.target, ty_args, ObjectFlags::empty())
        } else {
            ty
        }
    }

    pub(super) fn is_fresh_literal_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.flags.intersects(TypeFlags::FRESHABLE)
            && self.get_fresh_ty(ty).is_some_and(|fresh_ty| fresh_ty == ty)
    }

    pub(super) fn get_widened_literal_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty.kind.is_number_lit() && self.is_fresh_literal_ty(ty) {
            self.number_ty
        } else if ty.kind.is_string_lit() && self.is_fresh_literal_ty(ty) {
            self.string_ty
        } else if ty.flags.intersects(TypeFlags::BOOLEAN_LITERAL) && self.is_fresh_literal_ty(ty) {
            self.boolean_ty()
        } else {
            ty
        }
    }

    fn get_widened_prop(&mut self, prop: SymbolID) -> SymbolID {
        if !self.symbol(prop).flags.intersects(SymbolFlags::PROPERTY) {
            prop
        } else {
            let original = self.get_type_of_symbol(prop);
            let widened = self.get_widened_ty(original);
            if original != widened {
                prop
            } else {
                self.create_transient_symbol_with_ty(prop, widened)
            }
        }
    }

    fn get_widened_type_of_object_lit(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        let members = self
            .get_props_of_object_ty(ty)
            .iter()
            .map(|prop| {
                let name = self.symbol(*prop).name;
                let ty = self.get_widened_prop(*prop);
                (name, ty)
            })
            .collect();

        let object_flags = ty.get_object_flags()
            & (ObjectFlags::JS_LITERAL.union(ObjectFlags::NON_INFERRABLE_TYPE));

        self.create_anonymous_ty_with_resolved(
            ty.symbol(),
            object_flags,
            self.alloc(members),
            self.empty_array(),
            self.empty_array(),
            self.empty_array(),
            None,
        )
    }

    fn get_widened_lit_ty_for_init(
        &mut self,
        decl: &impl r#trait::VarLike<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        // TODO: as const
        let id = decl.id();
        let flags = self.node_query(id.module()).get_combined_node_flags(id);
        if flags.intersects(ast::NodeFlags::CONSTANT) {
            ty
        } else {
            self.get_widened_literal_ty(ty)
        }
    }

    pub(super) fn widened_ty_from_init(
        &mut self,
        decl: &impl r#trait::VarLike<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        self.get_widened_lit_ty_for_init(decl, ty)
    }

    fn is_literal_of_contextual_ty(
        &mut self,
        candidate_ty: &'cx ty::Ty<'cx>,
        contextual_ty: Option<&'cx ty::Ty<'cx>>,
    ) -> bool {
        let Some(contextual_ty) = contextual_ty else {
            return false;
        };
        if let Some(tys) = contextual_ty.kind.tys_of_union_or_intersection() {
            tys.iter()
                .any(|ty| self.is_literal_of_contextual_ty(candidate_ty, Some(ty)))
        } else if contextual_ty
            .flags
            .intersects(TypeFlags::INSTANTIABLE_NON_PRIMITIVE)
        {
            let constraint = self
                .get_base_constraint_of_ty(contextual_ty)
                .unwrap_or(self.unknown_ty);
            (constraint.maybe_type_of_kind(TypeFlags::STRING)
                && candidate_ty.maybe_type_of_kind(TypeFlags::STRING_LITERAL))
                || (constraint.maybe_type_of_kind(TypeFlags::NUMBER)
                    && candidate_ty.maybe_type_of_kind(TypeFlags::NUMBER_LITERAL))
                || (constraint.maybe_type_of_kind(TypeFlags::BIG_INT)
                    && candidate_ty.maybe_type_of_kind(TypeFlags::BIG_INT_LITERAL))
                || (constraint.maybe_type_of_kind(TypeFlags::ES_SYMBOL)
                    && candidate_ty.maybe_type_of_kind(TypeFlags::UNIQUE_ES_SYMBOL))
                || self.is_literal_of_contextual_ty(candidate_ty, Some(constraint))
        } else {
            (contextual_ty.flags.intersects(
                TypeFlags::STRING_LITERAL
                    | TypeFlags::INDEX
                    | TypeFlags::TEMPLATE_LITERAL
                    | TypeFlags::STRING_MAPPING,
            ) && candidate_ty.maybe_type_of_kind(TypeFlags::STRING_LITERAL))
                || (contextual_ty.flags.intersects(TypeFlags::NUMBER_LITERAL)
                    && candidate_ty.maybe_type_of_kind(TypeFlags::NUMBER_LITERAL))
                || (contextual_ty.flags.intersects(TypeFlags::BIG_INT_LITERAL)
                    && candidate_ty.maybe_type_of_kind(TypeFlags::BIG_INT_LITERAL))
                || (contextual_ty.flags.intersects(TypeFlags::BOOLEAN_LITERAL)
                    && candidate_ty.maybe_type_of_kind(TypeFlags::BOOLEAN_LITERAL))
                || (contextual_ty.flags.intersects(TypeFlags::UNIQUE_ES_SYMBOL)
                    && candidate_ty.maybe_type_of_kind(TypeFlags::UNIQUE_ES_SYMBOL))
        }
    }

    pub(super) fn get_widened_lit_like_ty_for_contextual_ty_if_needed(
        &mut self,
        mut ty: Option<&'cx ty::Ty<'cx>>,
        contextual_sig_ret_ty: Option<&'cx ty::Ty<'cx>>,
        is_async: bool,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(t) = ty
            && t.is_unit()
        {
            let contextual_ty = contextual_sig_ret_ty.map(|c| if is_async { todo!() } else { c });
            ty = Some(self.get_widened_lit_like_ty_for_contextual_ty(t, contextual_ty));
        }
        ty
    }

    pub(super) fn get_widened_lit_like_ty_for_contextual_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        contextual_ty: Option<&'cx ty::Ty<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        if !self.is_literal_of_contextual_ty(ty, contextual_ty) {
            let ty = self.get_widened_literal_ty(ty);
            self.get_regular_ty_of_literal_ty(ty)
        } else {
            self.get_regular_ty_of_literal_ty(ty)
        }
    }

    pub(super) fn instantiate_contextual_ty(
        &mut self,
        contextual_ty: Option<&'cx ty::Ty<'cx>>,
        node: ast::NodeID,
        context_flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(contextual_ty) = contextual_ty
            && contextual_ty.flags.intersects(TypeFlags::INSTANTIABLE)
        {
            let inference_context = self.get_inference_context(node);
            if let Some(inference_context) = inference_context
                && context_flags
                    .is_some_and(|check_flags| check_flags.intersects(ContextFlags::SIGNATURE))
                && self
                    .inference_infos(inference_context.inference.unwrap())
                    .iter()
                    .any(|i| i.has_inference_candidates_or_default(self))
            {
                // TODO:
                return Some(contextual_ty);
            }
            // TODO:
        }
        contextual_ty
    }

    pub(super) fn get_widened_ty_for_var_like_decl(
        &mut self,
        decl: &impl r#trait::VarLike<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let save_check_mode = self.check_mode;
        self.check_mode = Some(CheckMode::empty());
        let decl_ty = self.get_ty_for_var_like_decl::<true>(decl);
        self.check_mode = save_check_mode;
        self.widen_ty_for_var_like_decl(decl_ty, decl)
    }
}
