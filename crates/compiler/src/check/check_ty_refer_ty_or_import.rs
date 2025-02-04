use crate::bind::{Symbol, SymbolFlags, SymbolID};
use crate::check::cycle_check::ResolutionKey;
use crate::check::is_deeply_nested_type::RecursionId;
use crate::ty::TypeFlags;
use crate::{ast, ty};

use super::{errors, Ternary, TyChecker};

pub(super) trait TyReferTyOrImport<'cx> {
    fn id(&self) -> ast::NodeID;
    fn get_ty(&self, checker: &mut TyChecker<'cx>) -> &'cx ty::Ty<'cx>;
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>>;
}

impl<'cx> TyReferTyOrImport<'cx> for ast::ReferTy<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn get_ty(&self, checker: &mut TyChecker<'cx>) -> &'cx ty::Ty<'cx> {
        checker.get_ty_from_ty_reference(self)
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
        } else {
            None
        }
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
                let decl = self.binder.symbol(symbol).expect_ty_param().decl;
                self.get_effective_constraint_of_ty_param(decl)
            }
        } else {
            None
        }
    }

    fn get_inferred_ty_param_constraint(
        &mut self,
        ty_param: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        // TODO:
        None
    }

    fn get_constraint_from_ty_param(
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
                if ty.flags.intersects(TypeFlags::ANY) {
                    ty = self.error_ty;
                }
                ty
            } else {
                self.get_inferred_ty_param_constraint(ty_param)
                    .unwrap_or(self.no_constraint_ty())
            };
            self.get_mut_ty_links(ty_param.id)
                .set_param_ty_constraint(constraint);
            Some(constraint)
        };

        constraint.filter(|c| *c != self.no_constraint_ty())
    }

    fn get_resolved_base_constraint(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if let Some(ty) = self.get_ty_links(ty.id).get_resolved_base_ctor_ty() {
            return ty;
        }

        fn get_immediate_base_constraint<'cx>(
            checker: &mut TyChecker<'cx>,
            ty: &'cx ty::Ty<'cx>,
            stack: &mut Vec<RecursionId>,
        ) -> &'cx ty::Ty<'cx> {
            if let Some(ty) = checker.get_ty_links(ty.id).get_immediate_base_constraint() {
                return ty;
            }
            if !checker.push_ty_resolution(ResolutionKey::ImmediateBaseConstraint(ty.id)) {
                return checker.circular_constraint_ty();
            }

            let id = checker.get_recursion_id(ty);
            let mut result = None;
            if stack.len() < 10 || (stack.len() < 50 && !stack.contains(&id)) {
                stack.push(id);
                result = compute_base_constraint(checker, checker.get_simplified_ty(ty), stack);
                stack.pop();
            };

            if checker.pop_ty_resolution().has_cycle() {
                result = Some(checker.circular_constraint_ty());
            }

            let result = result.unwrap_or(checker.no_constraint_ty());
            checker
                .get_mut_ty_links(ty.id)
                .set_immediate_base_constraint(result);
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
            } else {
                // TODO: more case
                Some(ty)
            }
        }

        let mut stack = vec![];
        get_immediate_base_constraint(self, ty, &mut stack)
    }

    fn has_non_circular_constraint(&mut self, ty_param: &'cx ty::Ty<'cx>) -> bool {
        self.get_resolved_base_constraint(ty_param) != self.circular_constraint_ty()
    }

    fn check_ty_arg_constraints(
        &mut self,
        node: &impl TyReferTyOrImport<'cx>,
        ty_params: ty::Tys<'cx>,
    ) -> bool {
        let mut result = true;
        let ty_args = self.get_effective_ty_args(node.id(), ty_params).unwrap();
        let mapper = self.create_ty_mapper(ty_params, ty_args);
        for (idx, (ty_arg, ty_param)) in ty_args.iter().zip(ty_params.iter()).enumerate() {
            if let Some(constraint) = self.get_constraint_of_ty_param(ty_param) {
                if result {
                    let target = self.instantiate_ty(constraint, Some(mapper));
                    let error_node = node.ty_args().and_then(|ty_args| ty_args.list.get(idx));

                    if self.check_type_assignable_to(ty_arg, target, error_node.map(|n| n.id()))
                        == Ternary::FALSE
                    {
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
            let refer = reference.deep_target().kind.as_object_interface()?;
            refer.local_ty_params
        } else {
            None
        }
    }
}
