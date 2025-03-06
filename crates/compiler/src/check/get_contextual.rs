use crate::bind::SymbolName;
use crate::ty::MappedTyNameTyKind;
use crate::ty::ObjectFlags;
use crate::ty::TypeFlags;

use super::TyChecker;
use super::ast;
use super::create_ty::IntersectionFlags;
use super::ty;

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub(super) struct ContextFlags: u8 {
        const SIGNATURE             = 1 << 0;
        const NO_CONSTRAINTS        = 1 << 1;
        const COMPLETIONS           = 1 << 2;
        const SKIP_BINDING_PATTERNS = 1 << 3;
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_contextual_ty(
        &mut self,
        id: ast::NodeID,
        flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let includes_caches = flags.is_none_or(|flags| ContextFlags::empty() == flags);
        if let Some(ctx) = self.find_context_node(id, includes_caches) {
            return ctx.ty;
        }
        let Some(parent_id) = self.p.parent(id) else {
            unreachable!()
        };
        let parent = self.p.node(parent_id);
        use bolt_ts_ast::Node::*;
        match parent {
            VarDecl(node) => self.get_contextual_ty_for_var_like_decl(node.id),
            AssignExpr(node) if id == node.right.id() => {
                self.get_contextual_ty_for_assign(node.id, parent_id, flags)
            }
            ArrayLit(node) => {
                let ty = self.get_apparent_ty_of_contextual_ty(node.id, flags);
                let element_idx = self.p.index_of_node(node.elems, id);
                let spread_indices = {
                    // TODO: cache
                    let first = None;
                    let last = None;
                    for i in 0..node.elems.len() {
                        // TODO: spread
                    }
                    (first, last)
                };
                self.get_contextual_ty_for_element_expr(
                    ty,
                    element_idx,
                    Some(node.elems.len()),
                    spread_indices.0,
                    spread_indices.1,
                )
            }
            // ObjectShorthandMember(node) => {
            //     self.get_contextual_ty_for_object_literal_ele(node.id, flags)
            // }
            ObjectPropMember(node) => self.get_contextual_ty_for_object_literal_ele(node.id, flags),
            _ => None,
        }
    }

    fn get_contextual_ty_for_object_literal_ele(
        &mut self,
        id: ast::NodeID,
        context_flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let object_literal = {
            let p = self.p.parent(id).unwrap();
            self.p.node(p).expect_object_lit()
        };
        let ty = self.get_apparent_ty_of_contextual_ty(object_literal.id, context_flags)?;
        // TODO: has_bindable_name
        if let Some(member) = self.p.node(id).as_object_prop_member() {
            let symbol = self.get_symbol_of_decl(member.id);
            let name = self.symbol(symbol).name();
            let name_ty = self.get_symbol_links(symbol).get_name_ty();
            self.get_ty_of_prop_of_contextual_ty(ty, name, name_ty)
        } else {
            None
        }
    }

    pub(super) fn get_contextual_ret_ty(
        &mut self,
        id: ast::NodeID,
        flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(ret_ty) = self.get_ret_ty_from_anno(id) {
            Some(ret_ty)
        } else if let Some(iife) = self.p.get_iife(id) {
            self.get_contextual_ty(id, flags)
        } else {
            None
        }
    }

    fn get_contextual_ty_for_assign(
        &mut self,
        node: ast::NodeID,
        parent: ast::NodeID,
        flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let assign = self.p.node(parent).expect_assign_expr();
        // TODO: assign_kind;
        let lhs_symbol = self.get_symbol_for_expr(assign.left.id());
        let decl = lhs_symbol.and_then(|s| self.binder.symbol(s).opt_decl());
        if let Some(decl) = decl {
            let n = self.p.node(decl);
            if n.is_class_prop_ele() || n.is_prop_signature() {
                // TODO: handle this;
                return None;
            }
        }
        Some(self.get_ty_of_expr(assign.left))
    }

    fn get_contextual_ty_for_var_like_decl(&mut self, id: ast::NodeID) -> Option<&'cx ty::Ty<'cx>> {
        let node = self.p.node(id);
        use bolt_ts_ast::Node::*;
        match node {
            VarDecl(decl) => {
                if let Some(init) = decl.init {
                    if let Some(decl_ty) = decl.ty {
                        return Some(self.get_ty_from_type_node(decl_ty));
                    }
                }
            }
            _ => unreachable!(),
        };

        None
    }

    fn get_contextual_ty_for_element_expr(
        &mut self,
        ty: Option<&'cx ty::Ty<'cx>>,
        index: usize,
        length: Option<usize>,
        first_spread_index: Option<usize>,
        last_spread_index: Option<usize>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        self.map_ty(
            ty?,
            |this, t| {
                if let Some(tup) = t.as_tuple() {
                    if first_spread_index
                        .is_none_or(|first_spread_index| index < first_spread_index)
                        && index < tup.fixed_length
                    {
                        let t = this.get_ty_arguments(t)[index];
                        // TODO: resolve_missing_ty
                        return Some(t);
                    }
                    let offset = if last_spread_index.is_none_or(|last| index > last) {
                        length.map_or(0, |len| len.saturating_sub(index))
                    } else {
                        0
                    };
                    let fixed_end_length = if offset > 0
                        && tup.combined_flags.intersects(ty::ElementFlags::VARIABLE)
                    {
                        tup.get_end_elem_count(ty::ElementFlags::FIXED)
                    } else {
                        0
                    };
                    if offset > 0 && offset <= fixed_end_length {
                        let idx = Self::get_ty_reference_arity(t) - offset;
                        return this.get_ty_arguments(t).get(idx).copied();
                    }
                }
                if first_spread_index.is_none_or(|first_spread_index| index < first_spread_index) {
                    if let Some(t) = this.get_ty_of_prop_of_contextual_ty(
                        t,
                        SymbolName::EleNum(index.into()),
                        None,
                    ) {
                        return Some(t);
                    }
                }
                // TODO: this.get_iterated_ty_or_element_ty()
                this.get_index_ty_of_ty(t, this.number_ty)
            },
            true,
        )
    }

    fn get_ty_of_prop_of_contextual_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        name: SymbolName,
        name_ty: Option<&'cx ty::Ty<'cx>>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        self.map_ty(
            ty,
            |this, t| {
                if let Some(i) = t.kind.as_intersection() {
                    let mut tys = Vec::with_capacity(i.tys.len());
                    let mut index_info_candidates = Vec::with_capacity(i.tys.len());
                    let mut ignore_index_infos = false;
                    for constituent_ty in i.tys {
                        if !constituent_ty.kind.is_object() {
                            continue;
                        }
                        if this.is_generic_mapped_ty(constituent_ty)
                            && this.get_mapped_ty_name_ty_kind(constituent_ty)
                                != ty::MappedTyNameTyKind::Remapping
                        {
                            let sub = this.get_indexed_mapped_type_substituted_ty_of_contextual_ty(
                                constituent_ty,
                                name,
                                name_ty,
                            );
                            this.append_contextual_prop_ty_constituent(&mut tys, sub);
                            continue;
                        }
                        let prop_ty = this.get_ty_of_concrete_prop_of_contextual_ty(ty, name);
                        if let Some(prop_ty) = prop_ty {
                            ignore_index_infos = true;
                            index_info_candidates.clear();
                            this.append_contextual_prop_ty_constituent(&mut tys, Some(prop_ty));
                        } else {
                            if !ignore_index_infos {
                                index_info_candidates.push(constituent_ty);
                            }
                            continue;
                        }
                    }

                    if !index_info_candidates.is_empty() {
                        for candidate in index_info_candidates {
                            let index_info_ty = this
                                .get_ty_from_index_infos_of_contextual_ty(candidate, name, name_ty);
                            this.append_contextual_prop_ty_constituent(&mut tys, index_info_ty);
                        }
                    }
                    if tys.is_empty() {
                        None
                    } else if tys.len() == 1 {
                        Some(tys[0])
                    } else {
                        Some(this.get_intersection_ty(&tys, IntersectionFlags::None, None, None))
                    }
                } else if !t.flags.intersects(TypeFlags::OBJECT) {
                    None
                } else if this.is_generic_mapped_ty(t)
                    && this.get_mapped_ty_name_ty_kind(t) != MappedTyNameTyKind::Remapping
                {
                    this.get_indexed_mapped_type_substituted_ty_of_contextual_ty(t, name, name_ty)
                } else {
                    this.get_ty_of_concrete_prop_of_contextual_ty(t, name)
                        .or_else(|| this.get_ty_from_index_infos_of_contextual_ty(t, name, name_ty))
                }
            },
            true,
        )
    }

    fn get_ty_from_index_infos_of_contextual_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        name: SymbolName,
        name_ty: Option<&'cx ty::Ty<'cx>>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if ty.is_tuple() && name.as_numeric().is_some_and(|n| n >= 0.) {
            //TODO: rest_ty
        }
        let index_infos = self.get_index_infos_of_structured_ty(ty);
        let key_ty = name_ty.unwrap_or_else(|| {
            if let Some(atom) = name.as_atom() {
                self.get_string_literal_type(atom)
            } else if let Some(n) = name.as_numeric() {
                self.get_number_literal_type(n)
            } else {
                unreachable!()
            }
        });
        self.find_applicable_index_info(index_infos, key_ty)
            .map(|t| t.val_ty)
    }

    fn get_ty_of_concrete_prop_of_contextual_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        name: SymbolName,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let prop = self.get_prop_of_ty(ty, name);
        if prop.is_none_or(|p| self.is_circular_mapped_prop(p)) {
            return None;
        }
        let t = self.get_type_of_symbol(prop.unwrap());
        // TODO: resolve_missing_ty
        Some(t)
    }

    fn append_contextual_prop_ty_constituent(
        &self,
        tys: &mut Vec<&'cx ty::Ty<'cx>>,
        ty: Option<&'cx ty::Ty<'cx>>,
    ) {
        if let Some(ty) = ty {
            if ty.flags.intersects(TypeFlags::ANY) {
                tys.push(self.unknown_ty);
            } else {
                tys.push(ty);
            }
        }
    }

    fn is_excluded_mapped_property_name(
        &mut self,
        constraint: &'cx ty::Ty<'cx>,
        property_name_ty: &'cx ty::Ty<'cx>,
    ) -> bool {
        if let Some(c) = constraint.kind.as_cond_ty() {
            let true_ty = {
                let t = self.get_true_ty_from_cond_ty(constraint, c);
                self.get_reduced_ty(t)
            };
            let false_ty = {
                let t = self.get_false_ty_from_cond_ty(constraint, c);
                self.get_actual_ty_variable(t)
            };
            true_ty.flags.intersects(TypeFlags::NEVER)
                && false_ty == self.get_actual_ty_variable(c.check_ty)
                && self.is_type_assignable_to(property_name_ty, c.extends_ty)
        } else if let Some(i) = constraint.kind.as_intersection() {
            i.tys
                .iter()
                .any(|t| self.is_excluded_mapped_property_name(t, property_name_ty))
        } else {
            false
        }
    }

    fn get_indexed_mapped_type_substituted_ty_of_contextual_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        name: SymbolName,
        name_ty: Option<&'cx ty::Ty<'cx>>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let property_name_ty = name_ty.unwrap_or_else(|| {
            if let Some(atom) = name.as_atom() {
                self.get_string_literal_type(atom)
            } else if let Some(n) = name.as_numeric() {
                self.get_number_literal_type(n)
            } else {
                unreachable!()
            }
        });
        let constraint = self.get_constraint_ty_from_mapped_ty(ty);
        if self
            .get_ty_links(ty.id)
            .get_mapped_named_ty()
            .is_some_and(|name_ty| self.is_excluded_mapped_property_name(name_ty, property_name_ty))
            || self.is_excluded_mapped_property_name(constraint, property_name_ty)
        {
            return None;
        }
        let constraint_of_constraint = self
            .get_base_constraint_of_ty(constraint)
            .unwrap_or(constraint);
        if !self.is_type_assignable_to(property_name_ty, constraint_of_constraint) {
            return None;
        }
        Some(self.substitute_indexed_mapped_ty(ty, property_name_ty))
    }

    pub(super) fn get_apparent_ty_of_contextual_ty(
        &mut self,
        node: ast::NodeID,
        flags: Option<ContextFlags>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        // TODO: `is_object_literal_method`
        let contextual_ty = self.get_contextual_ty(node, flags);
        let Some(instantiated_ty) = self.instantiate_contextual_ty(contextual_ty, node, flags)
        else {
            return None;
        };

        if flags.is_none_or(|flags| {
            !(flags.intersects(ContextFlags::NO_CONSTRAINTS)
                && instantiated_ty.kind.is_type_variable())
        }) {
            let apparent_ty = self.map_ty(
                instantiated_ty,
                |this, t| {
                    if t.get_object_flags().intersects(ObjectFlags::MAPPED) {
                        Some(t)
                    } else {
                        Some(this.get_apparent_ty(t))
                    }
                },
                true,
            );
            return apparent_ty;
        }
        None
    }

    fn is_arity_smaller(&mut self, sig: &'cx ty::Sig<'cx>, id: ast::NodeID) -> bool {
        let mut target_params_count = 0;
        let params = self.p.node(id).params().unwrap_or_default();
        while target_params_count < params.len() {
            let param = params[target_params_count];
            if param.init.is_some() || param.question.is_some() || param.dotdotdot.is_some() {
                break;
            }
            target_params_count += 1;
        }
        self.has_effective_rest_param(sig) && sig.get_param_count(self) < target_params_count
    }

    fn get_contextual_call_sig(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        id: ast::NodeID,
    ) -> Option<&'cx ty::Sig<'cx>> {
        let sigs = self
            .get_signatures_of_type(ty, ty::SigKind::Call)
            .iter()
            .filter(|sig| !self.is_arity_smaller(sig, id))
            .collect::<Vec<_>>();
        if sigs.is_empty() { None } else { Some(sigs[0]) }
    }

    pub(super) fn get_contextual_sig(&mut self, id: ast::NodeID) -> Option<&'cx ty::Sig<'cx>> {
        assert!(!self.p.node(id).is_class_method_ele());
        let ty = self.get_apparent_ty_of_contextual_ty(id, Some(ContextFlags::SIGNATURE))?;

        if !ty.kind.is_union() {
            return self.get_contextual_call_sig(ty, id);
        }
        None
    }

    pub(super) fn is_context_sensitive_fn_or_object_literal_method(&self, id: ast::NodeID) -> bool {
        let node = self.p.node(id);
        if node.is_fn_expr_or_arrow_fnc_expr() || self.p.is_object_lit_method(id) {
            // TODO: isContextSensitiveFunctionLikeDeclaration
            false
        } else {
            false
        }
    }
}
