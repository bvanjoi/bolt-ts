use super::CheckMode;
use super::ContextFlags;
use super::TyChecker;
use super::create_ty::IntersectionFlags;
use super::get_iteration_tys::IterationTypeKind;
use super::ty;
use super::ty::ObjectFlags;
use super::ty::TypeFlags;
use super::utils::contains_ty;

use bolt_ts_arena::la_arena;
use bolt_ts_ast as ast;
use bolt_ts_binder::SymbolFlags;
use bolt_ts_binder::SymbolID;
use bolt_ts_binder::SymbolName;
use bolt_ts_utils::FxIndexMap;
use bolt_ts_utils::fx_hashmap_with_capacity;

#[derive(Debug, Clone)]

struct WideningContext<'cx> {
    parent: Option<WideningContextId<'cx>>,
    property_name: Option<SymbolName>,
    siblings: Option<ty::Tys<'cx>>,
    resolved_properties: Option<&'cx [SymbolID]>,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct WideningContextId<'cx>(la_arena::Idx<WideningContext<'cx>>);
pub(super) struct WideningContextArena<'cx>(la_arena::Arena<WideningContext<'cx>>);

impl<'cx> WideningContextArena<'cx> {
    pub fn new() -> Self {
        Self(la_arena::Arena::new())
    }
    fn alloc(&mut self, context: WideningContext<'cx>) -> WideningContextId<'cx> {
        WideningContextId(self.0.alloc(context))
    }
    fn set_siblings(&mut self, id: WideningContextId<'cx>, siblings: ty::Tys<'cx>) {
        debug_assert!(self.0[id.0].siblings.is_none());
        self.0[id.0].siblings = Some(siblings);
    }
    fn set_resolved_properties(
        &mut self,
        id: WideningContextId<'cx>,
        resolved_properties: &'cx [SymbolID],
    ) {
        debug_assert!(self.0[id.0].resolved_properties.is_none());
        self.0[id.0].resolved_properties = Some(resolved_properties);
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_widened_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        self.get_widened_ty_with_context(ty, None)
    }

    fn get_widened_ty_with_context(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        context: Option<WideningContextId<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        if !ty
            .get_object_flags()
            .intersects(ty::ObjectFlags::REQUIRES_WIDENING)
        {
            return ty;
        }
        if context.is_none()
            && let Some(widened) = self.widened_tys.get(ty)
        {
            return widened;
        }
        let result = if ty
            .flags
            .intersects(TypeFlags::ANY.union(TypeFlags::NULLABLE))
        {
            self.any_ty
        } else if ty.is_object_literal() {
            self.get_widened_type_of_object_literal(ty, context)
        } else if let Some(u) = ty.kind.as_union() {
            let union_context = match context {
                Some(context) => context,
                None => self.widened_context_arena.alloc(WideningContext {
                    parent: None,
                    property_name: None,
                    siblings: Some(u.tys),
                    resolved_properties: None,
                }),
            };
            let widened_tys = self
                .same_map_tys(Some(u.tys), |this, t, _| {
                    if t.flags.intersects(TypeFlags::NULLABLE) {
                        t
                    } else {
                        this.get_widened_ty_with_context(t, Some(union_context))
                    }
                })
                .unwrap();
            let reduction = if widened_tys.iter().any(|t| self.is_empty_object_ty(t)) {
                ty::UnionReduction::Subtype
            } else {
                ty::UnionReduction::Lit
            };
            self.get_union_ty::<false>(widened_tys, reduction, None, None, None, None)
        } else if let Some(i) = ty.kind.as_intersection() {
            let widened_tys = self
                .same_map_tys(Some(i.tys), |this, t, _| this.get_widened_ty(t))
                .unwrap();
            self.get_intersection_ty(widened_tys, IntersectionFlags::None, None, None)
        } else if self.is_array_or_tuple(ty) {
            let refer = ty.kind.expect_object_reference();
            let ty_args = self.get_ty_arguments(ty);
            let ty_args =
                self.same_map_tys(Some(ty_args), |this, ty_arg, _| this.get_widened_ty(ty_arg));
            assert!(ty_args.is_some());
            self.create_type_reference(refer.target, ty_args, ObjectFlags::empty(), None)
        } else {
            return ty;
        };
        if context.is_none() {
            let prev = self.widened_tys.insert(ty, result);
            debug_assert!(prev.is_none());
        }
        result
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
        } else if ty.flags.contains(TypeFlags::BOOLEAN_LITERAL) && self.is_fresh_literal_ty(ty) {
            self.boolean_ty()
        } else {
            ty
        }
    }

    fn get_widened_prop(
        &mut self,
        prop: SymbolID,
        context: Option<WideningContextId<'cx>>,
    ) -> SymbolID {
        if !self.symbol(prop).flags.contains(SymbolFlags::PROPERTY) {
            prop
        } else {
            let original = self.get_type_of_symbol(prop);
            let prop_context = context.map(|context| {
                self.widened_context_arena.alloc(WideningContext {
                    parent: Some(context),
                    property_name: Some(self.symbol(prop).name),
                    siblings: None,
                    resolved_properties: None,
                })
            });
            let widened = self.get_widened_ty_with_context(original, prop_context);
            if original == widened {
                prop
            } else {
                self.create_transient_symbol_with_ty(prop, widened)
            }
        }
    }

    fn get_properties_of_context(&mut self, context: WideningContextId<'cx>) -> &'cx [SymbolID] {
        if let Some(resolved_properties) =
            self.widened_context_arena.0[context.0].resolved_properties
        {
            return resolved_properties;
        }
        let mut names = fx_hashmap_with_capacity(0);
        let tys = self.get_siblings_of_context(context);
        for t in tys {
            if t.is_object_literal() && !t.get_object_flags().contains(ObjectFlags::CONTAINS_SPREAD)
            {
                for prop in self.get_props_of_object_ty(t) {
                    let name = self.symbol(*prop).name;
                    names.insert(name, *prop);
                }
            }
        }
        let properties = names.into_values().collect::<Vec<_>>();
        let resolved_properties = self.alloc(properties);
        self.widened_context_arena
            .set_resolved_properties(context, resolved_properties);
        resolved_properties
    }

    fn get_siblings_of_context(&mut self, context: WideningContextId<'cx>) -> ty::Tys<'cx> {
        if let Some(siblings) = self.widened_context_arena.0[context.0].siblings {
            return siblings;
        }
        let parent = self.widened_context_arena.0[context.0].parent.unwrap();
        let name = self.widened_context_arena.0[context.0]
            .property_name
            .unwrap();
        let mut siblings = vec![];
        for ty in self.get_siblings_of_context(parent) {
            if ty.is_object_literal() {
                if let Some(property) = self.get_prop_of_object_ty(ty, name) {
                    let ty = self.get_type_of_symbol(property);
                    self.for_each_ty(ty, |_, t| {
                        siblings.push(t);
                    });
                }
            }
        }
        let siblings = self.alloc(siblings);
        self.widened_context_arena.set_siblings(context, siblings);
        siblings
    }

    fn get_undefined_property(&mut self, prop: SymbolID) -> SymbolID {
        // TODO: cache?
        let s = self.symbol(prop);
        let check_flags = self.get_check_flags(prop) & crate::ty::CheckFlags::READONLY;
        let links = crate::check::SymbolLinks::default()
            .with_check_flags(check_flags)
            .with_ty(self.undefined_or_missing_ty)
            .with_target(prop);
        self.create_transient_symbol(
            s.name,
            s.flags | SymbolFlags::TRANSIENT | SymbolFlags::OPTIONAL,
            links,
            s.decls.clone(),
            s.value_decl,
            s.parent,
        )
    }

    fn get_widened_type_of_object_literal(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        context: Option<WideningContextId<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let mut members = self
            .get_props_of_object_ty(ty)
            .iter()
            .map(|prop| {
                let name = self.symbol(*prop).name;
                let ty = self.get_widened_prop(*prop, context);
                (name, ty)
            })
            .collect::<FxIndexMap<_, _>>();

        if let Some(context) = context {
            let properties = self.get_properties_of_context(context);
            for prop in properties {
                let name = self.symbol(*prop).name;
                if !members.contains_key(&name) {
                    let ty = self.get_undefined_property(*prop);
                    let prev = members.insert(name, ty);
                    debug_assert!(prev.is_none());
                }
            }
        }

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
            None,
        )
    }

    pub(super) fn get_widened_lit_ty_for_init(
        &mut self,
        decl: &impl crate::r#trait::VarLike<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        // TODO: as const
        let id = decl.id();
        let nq = self.node_query(id.module());
        let flags = nq.get_combined_node_flags(id);
        if flags.intersects(ast::NodeFlags::CONSTANT) || decl.is_declaration_readonly(&nq) {
            ty
        } else {
            self.get_widened_literal_ty(ty)
        }
    }

    pub(super) fn widened_ty_from_init(
        &mut self,
        decl: &impl crate::r#trait::VarLike<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        self.get_widened_lit_ty_for_init(decl, ty)
    }

    pub(super) fn is_literal_of_contextual_ty(
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
                    .union(TypeFlags::INDEX)
                    .union(TypeFlags::TEMPLATE_LITERAL)
                    .union(TypeFlags::STRING_MAPPING),
            ) && candidate_ty.maybe_type_of_kind(TypeFlags::STRING_LITERAL))
                || (contextual_ty.flags.contains(TypeFlags::NUMBER_LITERAL)
                    && candidate_ty.maybe_type_of_kind(TypeFlags::NUMBER_LITERAL))
                || (contextual_ty.flags.contains(TypeFlags::BIG_INT_LITERAL)
                    && candidate_ty.maybe_type_of_kind(TypeFlags::BIG_INT_LITERAL))
                || (contextual_ty.flags.contains(TypeFlags::BOOLEAN_LITERAL)
                    && candidate_ty.maybe_type_of_kind(TypeFlags::BOOLEAN_LITERAL))
                || (contextual_ty.flags.contains(TypeFlags::UNIQUE_ES_SYMBOL)
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

    pub(super) fn get_widened_lit_like_ty_for_contextual_iteration_ty_if_needed(
        &mut self,
        ty: Option<&'cx ty::Ty<'cx>>,
        contextual_sig_return_ty: Option<&'cx ty::Ty<'cx>>,
        kind: IterationTypeKind,
        is_async_generator: bool,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(ty) = ty
            && ty.is_unit()
        {
            let contextual_ty = contextual_sig_return_ty.and_then(|ty| {
                self.get_iteration_ty_of_generator_fn_return_ty(kind, ty, is_async_generator)
            });
            Some(self.get_widened_lit_like_ty_for_contextual_ty(ty, contextual_ty))
        } else {
            ty
        }
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
            && contextual_ty.maybe_type_of_kind(TypeFlags::INSTANTIABLE)
            && let Some(inference_context) = self.get_inference_context(node)
        {
            if context_flags
                .is_some_and(|check_flags| check_flags.contains(ContextFlags::SIGNATURE))
                && self
                    .inference_infos(inference_context.inference.unwrap())
                    .iter()
                    .any(|i| i.has_inference_candidates_or_default(self))
            {
                let mapper = self
                    .inference(inference_context.inference.unwrap())
                    .non_fixing_mapper;
                let ty = self.instantiate_instantiable_tys(contextual_ty, mapper);
                if !ty.flags.intersects(TypeFlags::ANY_OR_UNKNOWN) {
                    return Some(ty);
                }
            }
            if let Some(inference) = inference_context.inference
                && let Some(ret_mapper) = self.inference(inference).ret_mapper
            {
                let ty = self.instantiate_instantiable_tys(contextual_ty, ret_mapper);
                if !ty.flags.intersects(TypeFlags::ANY_OR_UNKNOWN) {
                    return Some(
                        if ty.kind.as_union().is_some_and(|u| {
                            contains_ty(u.tys, self.regular_false_ty)
                                && contains_ty(u.tys, self.regular_true_ty)
                        }) {
                            self.filter_type(ty, |this, t| {
                                t != this.regular_false_ty && t != this.regular_true_ty
                            })
                        } else {
                            ty
                        },
                    );
                }
            }
        }
        contextual_ty
    }

    pub(super) fn get_widened_ty_for_var_like_decl<const REPORT_ERROR: bool>(
        &mut self,
        decl: &impl crate::r#trait::VarLike<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let decl_ty = self.get_ty_for_var_like_decl::<true>(decl, CheckMode::empty());
        self.widen_ty_for_variable_like_declaration::<REPORT_ERROR>(decl_ty, decl)
    }
}
