use crate::bind::SymbolFlags;
use crate::bind::SymbolID;
use crate::ir;
use crate::ty::ObjectFlags;
use crate::ty::TypeFlags;
use bolt_ts_ast as ast;

use super::ContextFlags;
use super::TyChecker;
use super::ty;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_widened_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        self.get_widened_ty_with_context(ty)
    }

    fn get_widened_ty_with_context(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty
            .get_object_flags()
            .intersects(ty::ObjectFlags::REQUIRES_WIDENING)
        {
            // TODO: cache
            if ty.is_object_literal() {
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
        } else {
            ty
        }
    }

    pub(super) fn is_fresh_literal_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.flags.intersects(TypeFlags::FRESHABLE)
            && self
                .get_ty_links(ty.id)
                .get_fresh_ty()
                .is_some_and(|fresh_ty| fresh_ty == ty)
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
        if !self.symbol(prop).flags().intersects(SymbolFlags::PROPERTY) {
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
                let name = self.symbol(*prop).name();
                let ty = self.get_widened_prop(*prop);
                (name, ty)
            })
            .collect();

        let object_flags =
            ty.get_object_flags() & (ObjectFlags::JS_LITERAL | ObjectFlags::NON_INFERRABLE_TYPE);

        self.create_anonymous_ty_with_resolved(
            ty.symbol(),
            object_flags,
            self.alloc(members),
            &[],
            &[],
            &[],
        )
    }

    fn get_widened_lit_ty_for_init(
        &mut self,
        decl: &impl ir::VarLike<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        // TODO: as const
        let id = decl.id();
        let flags = self.p.get_combined_node_flags(id);
        if flags.intersects(ast::NodeFlags::CONSTANT) {
            ty
        } else {
            self.get_widened_literal_ty(ty)
        }
    }

    pub(super) fn widened_ty_from_init(
        &mut self,
        decl: &impl ir::VarLike<'cx>,
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
            return if constraint.maybe_type_of_kind(TypeFlags::STRING)
                && candidate_ty.maybe_type_of_kind(TypeFlags::STRING_LITERAL)
            {
                true
            } else if constraint.maybe_type_of_kind(TypeFlags::NUMBER)
                && candidate_ty.maybe_type_of_kind(TypeFlags::NUMBER_LITERAL)
            {
                true
            } else if constraint.maybe_type_of_kind(TypeFlags::BIG_INT)
                && candidate_ty.maybe_type_of_kind(TypeFlags::BIG_INT_LITERAL)
            {
                true
            } else if constraint.maybe_type_of_kind(TypeFlags::ES_SYMBOL)
                && candidate_ty.maybe_type_of_kind(TypeFlags::UNIQUE_ES_SYMBOL)
            {
                true
            } else {
                self.is_literal_of_contextual_ty(candidate_ty, Some(constraint))
            };
        } else if contextual_ty.flags.intersects(
            TypeFlags::STRING_LITERAL
                | TypeFlags::INDEX
                | TypeFlags::TEMPLATE_LITERAL
                | TypeFlags::STRING_MAPPING,
        ) && candidate_ty.maybe_type_of_kind(TypeFlags::STRING_LITERAL)
        {
            true
        } else if contextual_ty.flags.intersects(TypeFlags::NUMBER_LITERAL)
            && candidate_ty.maybe_type_of_kind(TypeFlags::NUMBER_LITERAL)
        {
            true
        } else if contextual_ty.flags.intersects(TypeFlags::BIG_INT_LITERAL)
            && candidate_ty.maybe_type_of_kind(TypeFlags::BIG_INT_LITERAL)
        {
            true
        } else if contextual_ty.flags.intersects(TypeFlags::BOOLEAN_LITERAL)
            && candidate_ty.maybe_type_of_kind(TypeFlags::BOOLEAN_LITERAL)
        {
            true
        } else {
            contextual_ty.flags.intersects(TypeFlags::UNIQUE_ES_SYMBOL)
                && candidate_ty.maybe_type_of_kind(TypeFlags::UNIQUE_ES_SYMBOL)
        }
    }

    pub(super) fn get_widened_lit_like_ty_for_contextual_ty(
        &mut self,
        mut ty: &'cx ty::Ty<'cx>,
        contextual_ty: Option<&'cx ty::Ty<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        if !self.is_literal_of_contextual_ty(ty, contextual_ty) {
            ty = self.get_widened_literal_ty(ty);
        }
        self.get_regular_ty_of_literal_ty(ty)
    }

    pub(super) fn instantiate_contextual_ty(
        &mut self,
        contextual_ty: Option<&'cx ty::Ty<'cx>>,
        node: ast::NodeID,
        context_flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(contextual_ty) = contextual_ty {
            if contextual_ty.flags.intersects(TypeFlags::INSTANTIABLE) {
                let inference_context = self.get_inference_context(node);
                if let Some(inference_context) = inference_context {
                    if context_flags
                        .is_some_and(|check_flags| check_flags.intersects(ContextFlags::SIGNATURE))
                        && self
                            .inference_infos(inference_context.inference.unwrap())
                            .iter()
                            .any(|i| i.has_inference_candidates_or_default(self))
                    {
                        // TODO:
                        return Some(contextual_ty);
                    }
                }
                // TODO:
            }
        }
        contextual_ty
    }
}
