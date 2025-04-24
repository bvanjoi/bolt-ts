use crate::bind::{Symbol, SymbolFlags, SymbolID};
use crate::check::create_ty::IntersectionFlags;
use crate::check::cycle_check::ResolutionKey;
use crate::check::get_simplified_ty::SimplifiedKind;
use crate::check::is_deeply_nested_type::RecursionId;
use crate::ty;
use crate::ty::TypeFlags;
use bolt_ts_ast as ast;

use super::symbol_info::SymbolInfo;
use super::{Ternary, TyChecker, errors};

pub(super) trait TyReferTyOrImport<'cx> {
    fn id(&self) -> ast::NodeID;
    fn span(&self) -> bolt_ts_span::Span;
    fn get_ty(&self, checker: &mut TyChecker<'cx>) -> &'cx ty::Ty<'cx>;
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>>;
}

impl<'cx> TyReferTyOrImport<'cx> for ast::ReferTy<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn span(&self) -> bolt_ts_span::Span {
        self.span
    }
    fn get_ty(&self, checker: &mut TyChecker<'cx>) -> &'cx ty::Ty<'cx> {
        let ty = checker.get_ty_from_ty_reference(self);
        checker.get_conditional_flow_of_ty(ty, self.id())
    }
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>> {
        self.ty_args
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_ty_refer_ty_or_import(&mut self, node: &impl TyReferTyOrImport<'cx>) {
        let ty = node.get_ty(self);
        if ty != self.error_ty && node.ty_args().is_some() {
            if let Some(ty_params) = self.get_ty_params_for_ty_refer_ty_or_import(node) {
                self.check_ty_arg_constraints(node, ty_params);
            }
        }
    }

    pub(super) fn get_constraint_of_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if ty.kind.is_param() {
            self.get_constraint_of_ty_param(ty)
        } else if ty.kind.is_indexed_access() {
            self.get_constraint_of_indexed_access(ty)
        } else if ty.kind.is_cond_ty() {
            self.get_constraint_of_cond_ty(ty)
        } else {
            self.get_base_constraint_of_ty(ty)
        }
    }

    fn get_constraint_of_indexed_access(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if self.has_non_circular_constraint(ty) {
            self.get_constraint_from_indexed_access(ty)
        } else {
            None
        }
    }

    fn get_constraint_from_indexed_access(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let indexed_access_ty = ty.kind.expect_indexed_access();
        if let Some(index_constraint) =
            self.get_simplified_ty_or_constraint(indexed_access_ty.index_ty)
        {
            if index_constraint != indexed_access_ty.index_ty {
                if let Some(indexed_access) = self.get_indexed_access_ty_or_undefined(
                    indexed_access_ty.object_ty,
                    index_constraint,
                    Some(indexed_access_ty.access_flags),
                    None,
                ) {
                    return Some(indexed_access);
                }
            }
        }

        if let Some(object_constraint) =
            self.get_simplified_ty_or_constraint(indexed_access_ty.object_ty)
        {
            if object_constraint != indexed_access_ty.object_ty {
                return self.get_indexed_access_ty_or_undefined(
                    object_constraint,
                    indexed_access_ty.index_ty,
                    Some(indexed_access_ty.access_flags),
                    None,
                );
            }
        }

        None
    }

    pub(super) fn get_constraint_of_ty_param(
        &mut self,
        ty_param: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if self.has_non_circular_constraint(ty_param) {
            self.get_constraint_from_ty_param(ty_param)
        } else {
            None
        }
    }

    pub(super) fn get_constraint_decl(
        &self,
        ty_param: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ast::Ty<'cx>> {
        if ty_param.kind.expect_param().is_this_ty {
            None
        } else if let Some(symbol) = ty_param.symbol() {
            if symbol == Symbol::ERR {
                None
            } else {
                let decl = self.binder.symbol(symbol).opt_decl().unwrap();
                let decl = self.p.node(decl).expect_ty_param();
                self.get_effective_constraint_of_ty_param(decl)
            }
        } else {
            None
        }
    }

    fn get_inferred_ty_param_constraint(
        &mut self,
        ty_param: &'cx ty::Ty<'cx>,
        omit_ty_references: bool,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let mut inferences = Vec::with_capacity(16);
        let decl = ty_param.symbol().and_then(|s| self.get_symbol_decl(s))?;
        let parent = self.p.parent(decl)?;
        let parent_parent = self.p.parent(parent)?;
        let (chid_ty_param, grand_parent) = self
            .p
            .walk_up_paren_tys_and_get_parent_and_child(parent_parent);
        let child_ty_param = chid_ty_param.map_or(parent, |n| n.id);
        if !omit_ty_references {
            let grand_parent_node = self.p.node(grand_parent);
            if let Some(grand) = grand_parent_node.as_refer_ty() {
                // TODO:
            } else if grand_parent_node
                .as_param_decl()
                .is_some_and(|n| n.dotdotdot.is_some())
                || grand_parent_node.is_rest_ty()
            // TODO: named tuple member
            {
                inferences.push(self.create_array_ty(self.unknown_ty, false));
            } else if grand_parent_node.is_template_span_ty() {
                inferences.push(self.string_ty);
            } else {
                // TODO: handle more case
            }
        }

        if inferences.is_empty() {
            None
        } else {
            Some(self.get_intersection_ty(&inferences, IntersectionFlags::None, None, None))
        }
    }

    pub(super) fn get_constraint_from_ty_param(
        &mut self,
        ty_param: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let param_ty = ty_param.kind.expect_param();
        let constraint = if let Some(constraint) = self.param_ty_constraint(ty_param) {
            Some(constraint)
        } else if let Some(target) = param_ty.target {
            let constraint =
                if let Some(target_constraint) = self.get_constraint_of_ty_param(target) {
                    let mapper = self.get_ty_links(ty_param.id).expect_param_ty_mapper();
                    self.instantiate_ty(target_constraint, Some(mapper))
                } else {
                    self.no_constraint_ty()
                };
            self.get_mut_ty_links(ty_param.id)
                .set_param_ty_constraint(constraint);
            Some(constraint)
        } else {
            let constraint = if let Some(constraint_decl) = self.get_constraint_decl(ty_param) {
                let mut ty = self.get_ty_from_type_node(constraint_decl);
                if ty.flags.intersects(TypeFlags::ANY) && !self.is_error(ty) {
                    ty = self.error_ty;
                }
                ty
            } else {
                // TODO: get_inferred_tu_param_constraint
                self.get_inferred_ty_param_constraint(ty_param, false)
                    .unwrap_or(self.no_constraint_ty())
            };
            self.get_mut_ty_links(ty_param.id)
                .set_param_ty_constraint(constraint);
            Some(constraint)
        };

        constraint.filter(|c| *c != self.no_constraint_ty())
    }

    pub(super) fn get_constraint_from_cond_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        self.get_constraint_of_distributive_cond_ty(ty)
            .or_else(|| Some(self.get_default_constraint_of_cond_ty(ty)))
    }

    pub(super) fn get_resolved_base_constraint(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(ty) = self.get_ty_links(ty.id).get_resolved_base_constraint() {
            return ty;
        }

        fn get_immediate_base_constraint<'cx>(
            checker: &mut TyChecker<'cx>,
            ty: &'cx ty::Ty<'cx>,
            stack: &mut Vec<RecursionId>,
        ) -> &'cx ty::Ty<'cx> {
            if let Some(ty) =
                checker.common_ty_links_arena[ty.links].get_immediate_base_constraint()
            {
                return ty;
            }
            if !checker.push_ty_resolution(ResolutionKey::ImmediateBaseConstraint(ty.id)) {
                return checker.circular_constraint_ty();
            }

            let id = checker.get_recursion_id(ty);
            let mut result = None;
            if stack.len() < 10 || (stack.len() < 50 && !stack.contains(&id)) {
                stack.push(id);
                let ty = checker.get_simplified_ty(ty, SimplifiedKind::Reading);
                result = compute_base_constraint(checker, ty, stack);
                stack.pop();
            };

            if checker.pop_ty_resolution().has_cycle() {
                if ty.flags.intersects(TypeFlags::TYPE_PARAMETER) {
                    if let Some(decl) = checker.get_constraint_decl(ty) {
                        let error = errors::TypeParameterXHasACircularConstraint {
                            ty: checker.print_ty(ty).to_string(),
                            span: decl.span(),
                        };
                        checker.push_error(Box::new(error));
                    }
                }
                result = Some(checker.circular_constraint_ty());
            }

            let result = result.unwrap_or(checker.no_constraint_ty());
            checker.common_ty_links_arena[ty.links].set_immediate_base_constraint(result);
            result
        }

        fn get_base_constraint<'cx>(
            checker: &mut TyChecker<'cx>,
            ty: &'cx ty::Ty<'cx>,
            stack: &mut Vec<RecursionId>,
        ) -> Option<&'cx ty::Ty<'cx>> {
            let c = get_immediate_base_constraint(checker, ty, stack);
            if c != checker.circular_constraint_ty() && c != checker.no_constraint_ty() {
                Some(c)
            } else {
                None
            }
        }

        fn compute_base_constraint<'cx>(
            checker: &mut TyChecker<'cx>,
            ty: &'cx ty::Ty<'cx>,
            stack: &mut Vec<RecursionId>,
        ) -> Option<&'cx ty::Ty<'cx>> {
            if let Some(ty_param) = ty.kind.as_param() {
                let constraint = checker.get_constraint_from_ty_param(ty);
                if ty_param.is_this_ty {
                    constraint
                } else if let Some(constraint) = constraint {
                    get_base_constraint(checker, constraint, stack)
                } else {
                    None
                }
            } else if let Some(tys) = ty.kind.tys_of_union_or_intersection() {
                let mut base_tys = Vec::with_capacity(tys.len());
                let mut different = false;
                for ty in tys {
                    if let Some(base_ty) = get_base_constraint(checker, ty, stack) {
                        if !base_ty.eq(ty) {
                            different = true;
                        }
                        base_tys.push(base_ty);
                    } else {
                        different = true
                    }
                }
                if !different {
                    return Some(ty);
                };
                if ty.kind.is_union() && base_tys.len() == tys.len() {
                    Some(checker.get_union_ty(&base_tys, ty::UnionReduction::Lit))
                } else if ty.kind.is_intersection() && !base_tys.is_empty() {
                    Some(checker.get_intersection_ty(
                        &base_tys,
                        IntersectionFlags::None,
                        None,
                        None,
                    ))
                } else {
                    None
                }
            } else if ty.flags.intersects(TypeFlags::INDEX) {
                Some(checker.string_number_symbol_ty())
            } else if let Some(t) = ty.kind.as_template_lit_ty() {
                let constraints = t
                    .tys
                    .iter()
                    .map(|t| get_base_constraint(checker, t, stack))
                    .map(|c| c.unwrap())
                    .collect::<Vec<_>>();
                if constraints.len() == t.tys.len() {
                    let tys = checker.alloc(constraints);
                    Some(checker.get_template_lit_ty(t.texts, tys))
                } else {
                    Some(checker.string_ty)
                }
            } else if let Some(s) = ty.kind.as_string_mapping_ty() {
                let constraint = get_base_constraint(checker, s.ty, stack);
                if let Some(constraint) = constraint {
                    if constraint != s.ty {
                        return Some(checker.get_string_mapping_ty(s.symbol, constraint));
                    }
                }
                Some(checker.string_ty)
            } else if let Some(i) = ty.kind.as_indexed_access() {
                // TODO: isMappedTypeGenericIndexedAccess
                let base_object_ty = get_base_constraint(checker, i.object_ty, stack);
                let base_index_ty = get_base_constraint(checker, i.index_ty, stack);
                if let Some(base_object_ty) = base_object_ty {
                    if let Some(base_index_ty) = base_index_ty {
                        if let Some(base_indexed_access) = checker
                            .get_indexed_access_ty_or_undefined(
                                base_object_ty,
                                base_index_ty,
                                Some(i.access_flags),
                                None,
                            )
                        {
                            return get_base_constraint(checker, base_indexed_access, stack);
                        }
                    }
                }
                None
            } else if ty.flags.intersects(TypeFlags::CONDITIONAL) {
                if let Some(constraint) = checker.get_constraint_from_cond_ty(ty) {
                    get_base_constraint(checker, constraint, stack)
                } else {
                    None
                }
            } else if ty.kind.is_substitution_ty() {
                let ty = checker.get_substitution_intersection(ty);
                get_base_constraint(checker, ty, stack)
            } else {
                // TODO: more case
                Some(ty)
            }
        }

        let mut stack = Vec::with_capacity(8);
        let res = get_immediate_base_constraint(self, ty, &mut stack);
        self.get_mut_ty_links(ty.id)
            .set_resolved_base_constraint(res);
        res
    }

    pub(super) fn has_non_circular_constraint(&mut self, ty_param: &'cx ty::Ty<'cx>) -> bool {
        self.get_resolved_base_constraint(ty_param) != self.circular_constraint_ty()
    }

    fn check_ty_arg_constraints(
        &mut self,
        node: &impl TyReferTyOrImport<'cx>,
        ty_params: ty::Tys<'cx>,
    ) -> bool {
        let mut result = true;
        let ty_args = self.get_effective_ty_args(node.id(), ty_params).unwrap();
        assert_eq!(ty_params.len(), ty_args.len());
        let mapper = self.create_ty_mapper(ty_params, ty_args);
        for (idx, (ty_arg, ty_param)) in ty_args.iter().zip(ty_params.iter()).enumerate() {
            if let Some(constraint) = self.get_constraint_of_ty_param(ty_param) {
                if result {
                    let target = self.instantiate_ty(constraint, Some(mapper));
                    let error_node = node.ty_args().and_then(|ty_args| ty_args.list.get(idx));
                    if !self.check_type_assignable_to(ty_arg, target, error_node.map(|n| n.id())) {
                        if let Some(error_node) = error_node {
                            let error = errors::TypeIsNotAssignableToType {
                                ty1: self.print_ty(ty_arg).to_string(),
                                ty2: self.print_ty(target).to_string(),
                                span: error_node.span(),
                            };
                            self.push_error(Box::new(error));
                        };

                        result = false;
                    }
                }
            }
        }
        result
    }

    pub(super) fn get_ty_params_for_ty_refer_ty_or_import(
        &mut self,
        node: &impl TyReferTyOrImport<'cx>,
    ) -> Option<ty::Tys<'cx>> {
        let ty = node.get_ty(self);
        if ty == self.error_ty {
            None
        } else {
            self.get_node_links(node.id())
                .get_resolved_symbol()
                .and_then(|symbol| self.get_ty_params_for_ty_and_symbol(ty, symbol))
        }
    }

    fn get_ty_params_for_ty_and_symbol(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        symbol: SymbolID,
    ) -> Option<ty::Tys<'cx>> {
        if ty == self.error_ty {
            None
        } else if self
            .binder
            .symbol(symbol)
            .flags
            .intersects(SymbolFlags::TYPE_ALIAS)
        {
            self.get_symbol_links(symbol).get_ty_params()
        } else if let Some(reference) = ty.kind.as_object_reference() {
            let refer = reference.interface_target()?.kind.expect_object_interface();
            refer.local_ty_params
        } else {
            None
        }
    }
}
