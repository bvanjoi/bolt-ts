use bolt_ts_ast::{self as ast, keyword};
use bolt_ts_atom::Atom;
use bolt_ts_binder::SymbolFlags;
use bolt_ts_binder::SymbolName;
use bolt_ts_ty::TypeFacts;

use super::SymbolInfo;
use super::TyChecker;
use super::check_expr::IterationUse;
use super::create_ty::IntersectionFlags;
use super::ty;

pub(super) enum IterationTypeKind {
    Yield,
    Return,
    Next,
}

pub(super) trait IterationTysResolver<'cx>: Copy {
    const ITERATOR_SYMBOL_NAME: Atom;
    fn iterator_symbol_name(&self) -> Atom {
        Self::ITERATOR_SYMBOL_NAME
    }
    fn get_global_iterator_ty<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx>;
    fn get_global_iterable_ty<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx>;
    fn get_global_iterable_iterator_ty<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx>;
    fn get_global_iterator_object_ty<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx>;
    fn get_global_generator_ty<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx>;
    fn get_global_builtin_iterator_tys(&self, c: &mut TyChecker<'cx>) -> ty::Tys<'cx>;
    fn resolve_iteration_ty(
        &self,
        c: &mut TyChecker<'cx>,
        ty: &'cx ty::Ty<'cx>,
        error_node: Option<ast::NodeID>,
    ) -> Option<&'cx ty::Ty<'cx>>;

    fn get_iteration_tys_of_iterable_cached(
        &self,
        c: &TyChecker<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::IterationTys<'cx>>;
    fn set_iteration_tys_of_iterable_cached(
        &self,
        c: &mut TyChecker<'cx>,
        key: &'cx ty::Ty<'cx>,
        value: &'cx ty::IterationTys<'cx>,
    );

    fn get_iteration_tys_of_iterator_cached(
        &self,
        c: &TyChecker<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::IterationTys<'cx>>;
    fn set_iteration_tys_of_iterator_cached(
        &self,
        c: &mut TyChecker<'cx>,
        key: &'cx ty::Ty<'cx>,
        value: &'cx ty::IterationTys<'cx>,
    );
}

#[derive(Clone, Copy)]
pub(super) struct AsyncIterationTysResolver;
impl<'cx> IterationTysResolver<'cx> for AsyncIterationTysResolver {
    const ITERATOR_SYMBOL_NAME: Atom = keyword::IDENT_ITERATOR;

    fn get_global_iterator_ty<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        c.get_global_async_iterator_ty::<REPORT_ERROR>()
    }

    fn get_global_iterable_ty<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        c.get_global_async_iterable_ty::<REPORT_ERROR>()
    }

    fn get_global_iterable_iterator_ty<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        c.get_global_async_iterable_iterator_ty::<REPORT_ERROR>()
    }

    fn get_global_iterator_object_ty<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        c.get_global_async_iterator_object_ty::<REPORT_ERROR>()
    }

    fn get_global_generator_ty<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        c.get_global_async_generator_ty::<REPORT_ERROR>()
    }

    fn get_global_builtin_iterator_tys(&self, c: &mut TyChecker<'cx>) -> ty::Tys<'cx> {
        c.get_global_builtin_async_iterator_tys()
    }

    fn resolve_iteration_ty(
        &self,
        c: &mut TyChecker<'cx>,
        ty: &'cx ty::Ty<'cx>,
        error_node: Option<bolt_ts_ast::NodeID>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        c.get_awaited_ty(ty)
    }

    fn get_iteration_tys_of_iterable_cached(
        &self,
        c: &TyChecker<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::IterationTys<'cx>> {
        c.iteration_tys_of_async_iterable.get(&ty.id).copied()
    }
    fn set_iteration_tys_of_iterable_cached(
        &self,
        c: &mut TyChecker<'cx>,
        key: &'cx ty::Ty<'cx>,
        value: &'cx ty::IterationTys<'cx>,
    ) {
        let prev = c.iteration_tys_of_async_iterable.insert(key.id, value);
        debug_assert!(prev.is_none())
    }
    fn get_iteration_tys_of_iterator_cached(
        &self,
        c: &TyChecker<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::IterationTys<'cx>> {
        c.iteration_tys_of_async_iterator.get(&ty.id).copied()
    }
    fn set_iteration_tys_of_iterator_cached(
        &self,
        c: &mut TyChecker<'cx>,
        key: &'cx ty::Ty<'cx>,
        value: &'cx ty::IterationTys<'cx>,
    ) {
        let prev = c.iteration_tys_of_async_iterator.insert(key.id, value);
        debug_assert!(prev.is_none())
    }
}
#[derive(Clone, Copy)]
pub(super) struct SyncIterationTysResolver;
impl<'cx> IterationTysResolver<'cx> for SyncIterationTysResolver {
    const ITERATOR_SYMBOL_NAME: Atom = keyword::IDENT_ITERATOR;

    fn get_global_iterator_ty<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        c.get_global_iterator_ty::<REPORT_ERROR>()
    }

    fn get_global_iterable_ty<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        c.get_global_iterable_ty::<REPORT_ERROR>()
    }

    fn get_global_iterable_iterator_ty<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        c.get_global_iterable_iterator_ty::<REPORT_ERROR>()
    }

    fn get_global_iterator_object_ty<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        c.get_global_iterator_object_ty::<REPORT_ERROR>()
    }

    fn get_global_generator_ty<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        c.get_global_generator_ty::<REPORT_ERROR>()
    }

    fn get_global_builtin_iterator_tys(&self, c: &mut TyChecker<'cx>) -> ty::Tys<'cx> {
        c.get_global_builtin_iterator_tys()
    }

    fn resolve_iteration_ty(
        &self,
        _: &mut TyChecker<'cx>,
        ty: &'cx ty::Ty<'cx>,
        _: Option<bolt_ts_ast::NodeID>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        Some(ty)
    }

    fn get_iteration_tys_of_iterable_cached(
        &self,
        c: &TyChecker<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::IterationTys<'cx>> {
        c.iteration_tys_of_iterable.get(&ty.id).copied()
    }

    fn set_iteration_tys_of_iterable_cached(
        &self,
        c: &mut TyChecker<'cx>,
        key: &'cx ty::Ty<'cx>,
        value: &'cx ty::IterationTys<'cx>,
    ) {
        let prev = c.iteration_tys_of_iterable.insert(key.id, value);
        debug_assert!(prev.is_none())
    }
    fn get_iteration_tys_of_iterator_cached(
        &self,
        c: &TyChecker<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::IterationTys<'cx>> {
        c.iteration_tys_of_iterator.get(&ty.id).copied()
    }
    fn set_iteration_tys_of_iterator_cached(
        &self,
        c: &mut TyChecker<'cx>,
        key: &'cx ty::Ty<'cx>,
        value: &'cx ty::IterationTys<'cx>,
    ) {
        let prev = c.iteration_tys_of_iterator.insert(key.id, value);
        debug_assert!(prev.is_none())
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_iteration_ty_of_generator_fn_return_ty(
        &mut self,
        kind: IterationTypeKind,
        return_ty: &'cx ty::Ty<'cx>,
        is_async_generator: bool,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if self.is_type_any(return_ty) {
            return None;
        }

        let iteration_tys =
            self.get_iteration_tys_of_generator_fn_return_ty(return_ty, is_async_generator)?;
        Some(match kind {
            IterationTypeKind::Yield => iteration_tys.yield_ty,
            IterationTypeKind::Return => iteration_tys.return_ty,
            IterationTypeKind::Next => iteration_tys.next_ty,
        })
    }

    pub(super) fn get_iteration_tys_of_generator_fn_return_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        is_async_generator: bool,
    ) -> Option<&'cx ty::IterationTys<'cx>> {
        if self.is_type_any(ty) {
            return Some(self.any_iteration_tys());
        }

        let mode = if is_async_generator {
            IterationUse::ASYNC_GENERATOR_RETURN_TYPE
        } else {
            IterationUse::GENERATOR_RETURN_TYPE
        };

        self.get_iteration_tys_of_iterable(ty, mode, None)
            .or_else(|| {
                if is_async_generator {
                    self.get_iteration_tys_of_iterator(ty, AsyncIterationTysResolver, None, None)
                } else {
                    self.get_iteration_tys_of_iterator(ty, SyncIterationTysResolver, None, None)
                }
            })
    }

    fn get_iteration_tys_of_iterator(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        resolver: impl IterationTysResolver<'cx>,
        error_node: Option<ast::NodeID>,
        error_output_container: Option<ast::NodeID>,
    ) -> Option<&'cx ty::IterationTys<'cx>> {
        self.get_iteration_tys_of_iterator_worker(
            ty,
            resolver,
            error_node,
            error_output_container,
            false,
        )
    }

    fn get_iteration_tys_of_iterator_worker(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        resolver: impl IterationTysResolver<'cx>,
        error_node: Option<ast::NodeID>,
        error_output_container: Option<ast::NodeID>,
        mut no_cache: bool,
    ) -> Option<&'cx ty::IterationTys<'cx>> {
        if self.is_type_any(ty) {
            return Some(self.any_iteration_tys());
        }
        let mut iteration_tys = self
            .get_iteration_tys_of_iterator_cached(ty, resolver)
            .or_else(|| self.get_iteration_tys_of_iterator_fast(ty, resolver));
        if let Some(error_node) = error_node
            && let Some(tys) = iteration_tys
            && std::ptr::eq(tys, self.no_iteration_tys())
        {
            iteration_tys = None;
            no_cache = true;
        };
        let iteration_tys = match iteration_tys {
            Some(iteration_tys) => iteration_tys,
            None => self.get_iteration_tys_of_iterator_slow(
                ty,
                resolver,
                error_node,
                error_output_container,
                no_cache,
            ),
        };
        if std::ptr::eq(iteration_tys, self.no_iteration_tys()) {
            None
        } else {
            Some(iteration_tys)
        }
    }

    fn get_iteration_tys_of_method(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        resolver: impl IterationTysResolver<'cx>,
        method_name: bolt_ts_atom::Atom,
        error_node: Option<ast::NodeID>,
        error_output_container: Option<ast::NodeID>,
    ) -> Option<&'cx ty::IterationTys<'cx>> {
        let name = bolt_ts_binder::SymbolName::Atom(method_name);
        let method = self.get_prop_of_ty(ty, name);
        let method_name_is_next = method_name != keyword::IDENT_NEXT;
        if method.is_none() && method_name_is_next {
            return None;
        };
        let method_ty = if let Some(method) = method
            && !(method_name_is_next && self.symbol(method).flags.contains(SymbolFlags::OPTIONAL))
        {
            let mut ty = self.get_type_of_symbol(method);
            if !method_name_is_next {
                ty = self.get_ty_with_facts(ty, TypeFacts::NE_UNDEFINED_OR_NULL)
            }
            Some(ty)
        } else {
            None
        };
        if method_ty.is_some_and(|t| self.is_type_any(t)) {
            return Some(self.any_iteration_tys());
        }
        let method_sigs = if let Some(method_ty) = method_ty {
            self.get_signatures_of_type(method_ty, ty::SigKind::Call)
        } else {
            self.empty_array()
        };
        if method_sigs.is_empty() {
            if let Some(error_node) = error_node {
                todo!()
            }
            return if method_name_is_next {
                Some(self.no_iteration_tys())
            } else {
                None
            };
        }
        if let Some(method_ty) = method_ty
            && method_sigs.len() == 1
        {
            let global_generator_ty = resolver.get_global_generator_ty::<false>(self);
            let global_iterator_ty = resolver.get_global_iterator_ty::<false>(self);
            let is_generator_method = global_generator_ty.symbol().is_some_and(|symbol| {
                self.symbol(symbol)
                    .members
                    .as_ref()
                    .and_then(|members| members.0.get(&name))
                    .copied()
                    == method_ty.symbol()
            });
            let is_iterator_method = !is_generator_method
                && global_iterator_ty.symbol().is_some_and(|symbol| {
                    self.symbol(symbol)
                        .members
                        .as_ref()
                        .and_then(|members| members.0.get(&name))
                        .copied()
                        == method_ty.symbol()
                });
            if is_generator_method || is_iterator_method {
                let global_ty = if is_generator_method {
                    resolver.get_global_generator_ty::<false>(self)
                } else {
                    resolver.get_global_iterator_ty::<false>(self)
                };
                let mapper = method_ty.kind.expect_object_anonymous().mapper.unwrap();
                let ty_arguments = self.get_ty_arguments(global_ty);
                let yield_ty = self.get_mapped_ty(mapper, ty_arguments[0]);
                let return_ty = self.get_mapped_ty(mapper, ty_arguments[1]);
                let next_ty = if method_name_is_next {
                    self.get_mapped_ty(mapper, ty_arguments[2])
                } else {
                    self.unknown_ty
                };
                return Some(self.create_iteration_tys(yield_ty, return_ty, next_ty));
            }
        }

        let mut method_param_tys: Option<Vec<&'cx ty::Ty<'cx>>> = None;
        let mut method_return_tys: Option<Vec<&'cx ty::Ty<'cx>>> = None;

        let method_name_is_throw = method_name == keyword::KW_THROW;
        for sig in method_sigs {
            if !method_name_is_throw && !sig.params.is_empty() {
                let ty = self.get_ty_at_pos(sig, 0);
                match &mut method_param_tys {
                    Some(method_param_tys) => {
                        method_param_tys.push(ty);
                    }
                    None => method_param_tys = Some(vec![ty]),
                }
            }
            let ty = self.get_ret_ty_of_sig(sig);
            match &mut method_return_tys {
                Some(method_return_tys) => {
                    method_return_tys.push(ty);
                }
                None => method_return_tys = Some(vec![ty]),
            }
        }

        let mut return_tys: Option<Vec<&'cx ty::Ty<'cx>>> = None;
        let mut next_ty: Option<&'cx ty::Ty<'cx>> = None;
        if !method_name_is_throw {
            let method_param_ty = if let Some(method_param_tys) = method_param_tys {
                self.get_union_ty::<false>(
                    &method_param_tys,
                    ty::UnionReduction::Lit,
                    None,
                    None,
                    None,
                )
            } else {
                self.unknown_ty
            };
            if method_name_is_next {
                next_ty = Some(method_param_ty);
            } else if method_name == keyword::KW_RETURN {
                let resolved_method_param_ty = resolver
                    .resolve_iteration_ty(self, method_param_ty, error_node)
                    .unwrap_or(self.any_ty);
                match &mut return_tys {
                    Some(return_tys) => {
                        return_tys.push(resolved_method_param_ty);
                    }
                    None => {
                        return_tys = Some(vec![resolved_method_param_ty]);
                    }
                }
            }
        }

        let mut yield_ty: Option<&'cx ty::Ty<'cx>> = None;
        let method_return_ty = if let Some(method_return_tys) = method_return_tys {
            self.get_intersection_ty(&method_return_tys, IntersectionFlags::None, None, None)
        } else {
            self.never_ty
        };
        let resolved_method_return_ty = resolver
            .resolve_iteration_ty(self, method_return_ty, error_node)
            .unwrap_or(self.any_ty);
        let iteration_tys = self.get_iteration_tys_of_iterator_result(resolved_method_return_ty);

        if std::ptr::eq(iteration_tys, self.no_iteration_tys()) {
            if let Some(error_node) = error_node {
                todo!()
            }
            yield_ty = Some(self.any_ty);
            match &mut return_tys {
                Some(return_tys) => {
                    return_tys.push(self.any_ty);
                }
                None => {
                    return_tys = Some(vec![self.any_ty]);
                }
            }
        } else {
            yield_ty = Some(iteration_tys.yield_ty);
            match &mut return_tys {
                Some(return_tys) => {
                    return_tys.push(iteration_tys.return_ty);
                }
                None => {
                    return_tys = Some(vec![iteration_tys.return_ty]);
                }
            }
        }
        let yield_ty = yield_ty.unwrap_or(self.never_ty);
        let return_ty = self.get_union_ty::<false>(
            return_tys.as_ref().unwrap(),
            ty::UnionReduction::Lit,
            None,
            None,
            None,
        );
        let next_ty = next_ty.unwrap_or(self.never_ty);
        Some(self.create_iteration_tys(yield_ty, return_ty, next_ty))
    }

    fn is_iterator_result(&mut self, ty: &'cx ty::Ty<'cx>, kind: IterationTypeKind) -> bool {
        let done = self
            .get_ty_of_prop_of_ty(ty, bolt_ts_binder::SymbolName::Atom(keyword::IDENT_DONE))
            .unwrap_or(self.false_ty);
        let ty = if matches!(kind, IterationTypeKind::Yield) {
            self.false_ty
        } else {
            self.true_ty
        };
        self.is_type_assignable_to(ty, done)
    }

    fn is_yield_iterator_result(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        self.is_iterator_result(ty, IterationTypeKind::Yield)
    }

    fn is_return_iterator_result(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        self.is_iterator_result(ty, IterationTypeKind::Return)
    }

    fn get_iteration_tys_of_iterator_result(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::IterationTys<'cx> {
        if self.is_type_any(ty) {
            return self.any_iteration_tys();
        }
        if let Some(cached) = self.iteration_tys_of_iterator_result.get(&ty.id) {
            return cached;
        };

        let target = self.get_global_iterator_yield_result_ty::<false>();
        if self.is_reference_to_ty(ty, target) {
            let yield_ty = self.get_ty_arguments(ty)[0];
            let tys = self.create_iteration_tys(yield_ty, self.never_ty, self.never_ty);
            let prev = self.iteration_tys_of_iterator_result.insert(ty.id, tys);
            debug_assert!(prev.is_none());
            return tys;
        }

        let target = self.get_global_iterator_return_result_ty::<false>();
        if self.is_reference_to_ty(ty, target) {
            let return_ty = self.get_ty_arguments(ty)[0];
            let tys = self.create_iteration_tys(self.never_ty, return_ty, self.never_ty);
            let prev = self.iteration_tys_of_iterator_result.insert(ty.id, tys);
            debug_assert!(prev.is_none());
            return tys;
        }

        let name = bolt_ts_binder::SymbolName::Atom(keyword::IDENT_VALUE);
        let yield_iterator_result =
            self.filter_type(ty, |this, t| this.is_yield_iterator_result(t));
        let yield_ty = if yield_iterator_result == self.never_ty {
            None
        } else {
            self.get_ty_of_prop_of_ty(yield_iterator_result, name)
        };

        let return_iterator_result =
            self.filter_type(ty, |this, t| this.is_return_iterator_result(t));
        let return_ty = if return_iterator_result == self.never_ty {
            None
        } else {
            self.get_ty_of_prop_of_ty(yield_iterator_result, name)
        };

        if yield_ty.is_none() && return_ty.is_none() {
            let ret = self.no_iteration_tys();
            let prev = self.iteration_tys_of_iterator_result.insert(ty.id, ret);
            debug_assert!(prev.is_none());
            ret
        } else {
            let yield_ty = yield_ty.unwrap_or(self.never_ty);
            let return_ty = return_ty.unwrap_or(self.void_ty);
            let tys = self.create_iteration_tys(yield_ty, return_ty, self.never_ty);
            let prev = self.iteration_tys_of_iterator_result.insert(ty.id, tys);
            debug_assert!(prev.is_none());
            tys
        }
    }

    fn get_prop_name_for_known_symbol_name(&mut self, name: SymbolName) -> SymbolName {
        let ctor_ty = self.get_global_es_symbol_constructor_symbol();
        let unique_ty = ctor_ty.and_then(|ctor_ty| {
            let ty = self.get_type_of_symbol(ctor_ty);
            self.get_ty_of_prop_of_ty(ty, name)
        });
        if let Some(unique_ty) = unique_ty
            && self.is_ty_usable_as_prop_name(unique_ty)
        {
            self.get_prop_name_from_ty(unique_ty)
        } else {
            name
        }
    }

    fn get_iteration_tys_of_iterable_slow(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        resolver: impl IterationTysResolver<'cx>,
        error_node: Option<ast::NodeID>,
        error_output_container: Option<ast::NodeID>,
        mut no_cache: bool,
    ) -> &'cx ty::IterationTys<'cx> {
        let name = bolt_ts_binder::SymbolName::Atom(resolver.iterator_symbol_name());
        let name = self.get_prop_name_for_known_symbol_name(name);
        let method = self.get_prop_of_ty(ty, name);
        let method_ty = if let Some(method) = method
            && !self.symbol(method).flags.contains(SymbolFlags::OPTIONAL)
        {
            Some(self.get_type_of_symbol(method))
        } else {
            None
        };
        if method_ty.is_some_and(|t| self.is_type_any(t)) {
            return self.any_iteration_tys();
        }
        let all_sigs = method_ty.map(|t| self.get_signatures_of_type(t, ty::SigKind::Call));
        let valid_sigs = all_sigs.map(|all_sigs| {
            all_sigs
                .iter()
                .filter(|sig| self.get_min_arg_count(sig) == 0)
                .collect::<Vec<_>>()
        });
        if valid_sigs.as_ref().is_none_or(|sigs| !sigs.is_empty()) {
            if let Some(error_node) = error_node
                && let Some(all_sigs) = all_sigs
                && !all_sigs.is_empty()
            {
                let target = resolver.get_global_iterable_ty::<true>(self);
                self.check_type_assignable_to(ty, target, Some(error_node));
            }
            let tys = self.no_iteration_tys();
            return if no_cache {
                tys
            } else {
                resolver.set_iteration_tys_of_iterable_cached(self, ty, tys);
                tys
            };
        }

        let valid_sigs = valid_sigs.unwrap();

        let iterator_ty = {
            let tys = valid_sigs
                .iter()
                .map(|sig| self.get_ret_ty_of_sig(sig))
                .collect::<Vec<_>>();
            self.get_intersection_ty(&tys, IntersectionFlags::None, None, None)
        };
        let iteration_tys = self
            .get_iteration_tys_of_iterator_worker(
                iterator_ty,
                resolver,
                error_node,
                error_output_container,
                no_cache,
            )
            .unwrap_or(self.no_iteration_tys());
        if no_cache {
            iteration_tys
        } else {
            resolver.set_iteration_tys_of_iterable_cached(self, ty, iteration_tys);
            iteration_tys
        }
    }

    fn get_iteration_tys_of_iterator_slow(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        resolver: impl IterationTysResolver<'cx>,
        error_node: Option<ast::NodeID>,
        error_output_container: Option<ast::NodeID>,
        mut no_cache: bool,
    ) -> &'cx ty::IterationTys<'cx> {
        let next_ty = self.get_iteration_tys_of_method(
            ty,
            resolver,
            keyword::IDENT_NEXT,
            error_node,
            error_output_container,
        );
        let return_ty = self.get_iteration_tys_of_method(
            ty,
            resolver,
            keyword::KW_RETURN,
            error_node,
            error_output_container,
        );
        let throw_ty = self.get_iteration_tys_of_method(
            ty,
            resolver,
            keyword::KW_THROW,
            error_node,
            error_output_container,
        );
        let mut tys = vec![];
        if let Some(next_ty) = next_ty {
            tys.push(next_ty);
        }
        if let Some(return_ty) = return_ty {
            tys.push(return_ty);
        }
        if let Some(throw_ty) = throw_ty {
            tys.push(throw_ty);
        }
        let tys = self.combine_iteration_tys(tys);
        if no_cache {
            tys
        } else {
            resolver.set_iteration_tys_of_iterator_cached(self, ty, tys);
            tys
        }
    }

    fn get_iteration_tys_of_iterator_fast(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        resolver: impl IterationTysResolver<'cx>,
    ) -> Option<&'cx ty::IterationTys<'cx>> {
        if {
            let target = resolver.get_global_iterable_iterator_ty::<false>(self);
            self.is_reference_to_ty(ty, target)
        } || {
            let target = resolver.get_global_iterator_ty::<false>(self);
            self.is_reference_to_ty(ty, target)
        } || {
            let target = resolver.get_global_iterator_object_ty::<false>(self);
            self.is_reference_to_ty(ty, target)
        } || {
            let target = resolver.get_global_generator_ty::<false>(self);
            self.is_reference_to_ty(ty, target)
        } {
            let ty_arguments = self.get_ty_arguments(ty);
            debug_assert!(ty_arguments.len() == 3);
            let yield_ty = ty_arguments[0];
            let ret_ty = ty_arguments[1];
            let next_ty = ty_arguments[2];
            let yield_ty = resolver
                .resolve_iteration_ty(self, yield_ty, None)
                .unwrap_or(yield_ty);
            let ret_ty = resolver
                .resolve_iteration_ty(self, ret_ty, None)
                .unwrap_or(ret_ty);
            let tys = self.create_iteration_tys(yield_ty, ret_ty, next_ty);
            resolver.set_iteration_tys_of_iterator_cached(self, ty, tys);
            Some(tys)
        } else if let builtin_iterator_tys = resolver.get_global_builtin_iterator_tys(self)
            && self.is_reference_to_some_ty(ty, builtin_iterator_tys)
        {
            let ty_arguments = self.get_ty_arguments(ty);
            debug_assert!(ty_arguments.len() == 1);
            let yield_ty = ty_arguments[0];
            let ret_ty = self.get_builtin_iterator_return_ty();
            let next_ty = self.unknown_ty;
            let tys = self.create_iteration_tys(yield_ty, ret_ty, next_ty);
            resolver.set_iteration_tys_of_iterator_cached(self, ty, tys);
            Some(tys)
        } else {
            None
        }
    }

    fn get_iteration_tys_of_iterator_cached(
        &self,
        ty: &'cx ty::Ty<'cx>,
        resolver: impl IterationTysResolver<'cx>,
    ) -> Option<&'cx ty::IterationTys<'cx>> {
        resolver.get_iteration_tys_of_iterator_cached(self, ty)
    }

    pub(super) fn get_iteration_ty_of_iterable(
        &mut self,
        mode: IterationUse,
        kind: IterationTypeKind,
        input_ty: &'cx ty::Ty<'cx>,
        error_node: Option<ast::NodeID>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if self.is_type_any(input_ty) {
            return None;
        }
        let iteration_tys = self.get_iteration_tys_of_iterable(input_ty, mode, error_node)?;
        let ty = match kind {
            IterationTypeKind::Yield => iteration_tys.yield_ty,
            IterationTypeKind::Return => iteration_tys.return_ty,
            IterationTypeKind::Next => iteration_tys.next_ty,
        };
        Some(ty)
    }

    fn get_iteration_tys_of_iterable_fast(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        resolver: impl IterationTysResolver<'cx>,
    ) -> Option<&'cx ty::IterationTys<'cx>> {
        if {
            let target = resolver.get_global_iterable_ty::<false>(self);
            self.is_reference_to_ty(ty, target)
        } || {
            let target = resolver.get_global_iterator_object_ty::<false>(self);
            self.is_reference_to_ty(ty, target)
        } || {
            let target = resolver.get_global_iterable_iterator_ty::<false>(self);
            self.is_reference_to_ty(ty, target)
        } || {
            let target = resolver.get_global_generator_ty::<false>(self);
            self.is_reference_to_ty(ty, target)
        } {
            let ty_arguments = self.get_ty_arguments(ty);
            debug_assert!(ty_arguments.len() == 3);
            let yield_ty = ty_arguments[0];
            let ret_ty = ty_arguments[1];
            let next_ty = ty_arguments[2];
            let yield_ty = resolver
                .resolve_iteration_ty(self, yield_ty, None)
                .unwrap_or(yield_ty);
            let ret_ty = resolver
                .resolve_iteration_ty(self, ret_ty, None)
                .unwrap_or(ret_ty);
            let tys = self.create_iteration_tys(yield_ty, ret_ty, next_ty);
            resolver.set_iteration_tys_of_iterable_cached(self, ty, tys);
            Some(tys)
        } else if let builtin_iterator_tys = resolver.get_global_builtin_iterator_tys(self)
            && self.is_reference_to_some_ty(ty, builtin_iterator_tys)
        {
            let ty_arguments = self.get_ty_arguments(ty);
            debug_assert!(ty_arguments.len() == 1);
            let yield_ty = ty_arguments[0];
            let ret_ty = self.get_builtin_iterator_return_ty();
            let next_ty = self.unknown_ty;
            let yield_ty = resolver
                .resolve_iteration_ty(self, yield_ty, None)
                .unwrap_or(yield_ty);
            let ret_ty = resolver
                .resolve_iteration_ty(self, ret_ty, None)
                .unwrap_or(ret_ty);
            let tys = self.create_iteration_tys(yield_ty, ret_ty, next_ty);
            resolver.set_iteration_tys_of_iterable_cached(self, ty, tys);
            Some(tys)
        } else {
            None
        }
    }

    fn is_reference_to_some_ty(&self, ty: &'cx ty::Ty<'cx>, target: ty::Tys<'cx>) -> bool {
        false
    }

    fn get_iteration_tys_of_iterable_cached(
        &self,
        ty: &'cx ty::Ty<'cx>,
        resolver: impl IterationTysResolver<'cx>,
    ) -> Option<&'cx ty::IterationTys<'cx>> {
        resolver.get_iteration_tys_of_iterable_cached(self, ty)
    }

    fn get_async_from_sync_iteration_tys(
        &mut self,
        tys: &'cx ty::IterationTys<'cx>,
        error_node: Option<ast::NodeID>,
    ) -> &'cx ty::IterationTys<'cx> {
        if std::ptr::eq(tys, self.no_iteration_tys()) {
            return self.no_iteration_tys();
        } else if std::ptr::eq(tys, self.any_iteration_tys()) {
            return self.any_iteration_tys();
        }
        let yield_ty = tys.yield_ty;
        let return_ty = tys.return_ty;
        let next_ty = tys.next_ty;
        if let Some(error_node) = error_node {
            todo!()
        }
        let yield_ty = self.get_awaited_ty(yield_ty).unwrap_or(self.any_ty);
        let return_ty = self.get_awaited_ty(return_ty).unwrap_or(self.any_ty);
        self.create_iteration_tys(yield_ty, return_ty, next_ty)
    }

    fn get_iteration_tys_of_iterable_worker(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mode: IterationUse,
        error_node: Option<ast::NodeID>,
        error_output_container: Option<ast::NodeID>,
    ) -> &'cx ty::IterationTys<'cx> {
        if self.is_type_any(ty) {
            return self.any_iteration_tys();
        }
        let mut no_cache = false;
        if mode.contains(IterationUse::ALLOWS_ASYNC_ITERABLES_FLAG) {
            let iteration_tys = self
                .get_iteration_tys_of_iterable_cached(ty, AsyncIterationTysResolver)
                .or_else(|| self.get_iteration_tys_of_iterable_fast(ty, AsyncIterationTysResolver));
            if let Some(iteration_tys) = iteration_tys {
                if std::ptr::eq(iteration_tys, self.no_iteration_tys()) && error_node.is_some() {
                    no_cache = true;
                } else {
                    return if mode.contains(IterationUse::FOR_OF_FLAG) {
                        self.get_async_from_sync_iteration_tys(iteration_tys, error_node)
                    } else {
                        iteration_tys
                    };
                }
            }
        }

        if mode.contains(IterationUse::ALLOWS_SYNC_ITERABLES_FLAG) {
            let iteration_tys = self
                .get_iteration_tys_of_iterable_cached(ty, SyncIterationTysResolver)
                .or_else(|| self.get_iteration_tys_of_iterable_fast(ty, SyncIterationTysResolver));
            if let Some(iteration_tys) = iteration_tys {
                if std::ptr::eq(iteration_tys, self.no_iteration_tys()) && error_node.is_some() {
                    no_cache = true;
                } else {
                    if mode.contains(IterationUse::ALLOWS_ASYNC_ITERABLES_FLAG) {
                        if !std::ptr::eq(iteration_tys, self.no_iteration_tys()) {
                            let tys =
                                self.get_async_from_sync_iteration_tys(iteration_tys, error_node);
                            return if no_cache {
                                tys
                            } else {
                                AsyncIterationTysResolver
                                    .set_iteration_tys_of_iterable_cached(self, ty, tys);
                                tys
                            };
                        }
                    } else {
                        return iteration_tys;
                    };
                }
            }

            let iteration_tys = self.get_iteration_tys_of_iterable_slow(
                ty,
                SyncIterationTysResolver,
                error_node,
                error_output_container,
                no_cache,
            );
            if !std::ptr::eq(iteration_tys, self.no_iteration_tys()) {
                if mode.contains(IterationUse::ALLOWS_ASYNC_ITERABLES_FLAG) {
                    let iteration_tys =
                        self.get_async_from_sync_iteration_tys(iteration_tys, error_node);
                    return if no_cache {
                        iteration_tys
                    } else {
                        AsyncIterationTysResolver.set_iteration_tys_of_iterable_cached(
                            self,
                            ty,
                            iteration_tys,
                        );
                        iteration_tys
                    };
                } else {
                    return iteration_tys;
                }
            }
        }

        self.no_iteration_tys()
    }

    fn combine_iteration_tys(
        &mut self,
        all_iteration_tys: Vec<&'cx ty::IterationTys<'cx>>,
    ) -> &'cx ty::IterationTys<'cx> {
        let mut yield_tys: Option<Vec<&'cx ty::Ty<'cx>>> = None;
        let mut return_tys: Option<Vec<&'cx ty::Ty<'cx>>> = None;
        let mut next_tys: Option<Vec<&'cx ty::Ty<'cx>>> = None;
        for iteration_tys in all_iteration_tys {
            if std::ptr::eq(iteration_tys, self.no_iteration_tys()) {
                continue;
            }
            if std::ptr::eq(iteration_tys, self.any_iteration_tys()) {
                return self.any_iteration_tys();
            }
            match &mut yield_tys {
                Some(yield_tys) => yield_tys.push(iteration_tys.yield_ty),
                None => yield_tys = Some(vec![iteration_tys.yield_ty]),
            };
            match &mut return_tys {
                Some(return_tys) => return_tys.push(iteration_tys.return_ty),
                None => return_tys = Some(vec![iteration_tys.return_ty]),
            };
            match &mut next_tys {
                Some(next_tys) => next_tys.push(iteration_tys.next_ty),
                None => next_tys = Some(vec![iteration_tys.next_ty]),
            };
        }
        if yield_tys.is_some() || return_tys.is_some() || next_tys.is_some() {
            let yield_ty = yield_tys.map_or(self.never_ty, |yield_tys| {
                self.get_union_ty::<false>(&yield_tys, ty::UnionReduction::Lit, None, None, None)
            });
            let return_ty = return_tys.map_or(self.never_ty, |return_tys| {
                self.get_union_ty::<false>(&return_tys, ty::UnionReduction::Lit, None, None, None)
            });
            let next_ty = next_tys.map_or(self.never_ty, |next_tys| {
                self.get_intersection_ty(
                    &next_tys,
                    super::create_ty::IntersectionFlags::None,
                    None,
                    None,
                )
            });
            self.create_iteration_tys(yield_ty, return_ty, next_ty)
        } else {
            self.no_iteration_tys()
        }
    }

    pub(super) fn get_iteration_tys_of_iterable(
        &mut self,
        mut ty: &'cx ty::Ty<'cx>,
        mode: IterationUse,
        error_node: Option<ast::NodeID>,
    ) -> Option<&'cx ty::IterationTys<'cx>> {
        ty = self.get_reduced_ty(ty);
        if self.is_type_any(ty) {
            return Some(self.any_iteration_tys());
        }

        let Some(u) = ty.kind.as_union() else {
            let error_output_container = error_node;
            let iteration_tys = self.get_iteration_tys_of_iterable_worker(
                ty,
                mode,
                error_node,
                error_output_container,
            );
            if std::ptr::eq(iteration_tys, self.no_iteration_tys()) {
                if let Some(error_node) = error_node {
                    todo!()
                }

                return None;
            }

            // TODO: error_output_container
            return Some(iteration_tys);
        };

        if let Some(cached) = if mode.contains(IterationUse::ALLOWS_ASYNC_ITERABLES_FLAG) {
            AsyncIterationTysResolver.get_iteration_tys_of_iterable_cached(self, ty)
        } else {
            SyncIterationTysResolver.get_iteration_tys_of_iterable_cached(self, ty)
        } {
            return if std::ptr::eq(cached, self.no_iteration_tys()) {
                None
            } else {
                Some(cached)
            };
        }
        let mut all_iteration_tys: Option<Vec<&'cx ty::IterationTys<'cx>>> = None;
        for constituent in u.tys {
            let error_output_container = error_node;
            let iteration_tys = self.get_iteration_tys_of_iterable_worker(
                constituent,
                mode,
                error_node,
                error_output_container,
            );
            if std::ptr::eq(iteration_tys, self.no_iteration_tys()) {
                if let Some(error_node) = error_node {
                    todo!()
                }
                if mode.contains(IterationUse::ALLOWS_ASYNC_ITERABLES_FLAG) {
                    AsyncIterationTysResolver.set_iteration_tys_of_iterable_cached(
                        self,
                        ty,
                        self.no_iteration_tys(),
                    );
                } else {
                    SyncIterationTysResolver.set_iteration_tys_of_iterable_cached(
                        self,
                        ty,
                        self.no_iteration_tys(),
                    )
                }
                return None;
            }

            // TODO: error_output_container
            match &mut all_iteration_tys {
                Some(tys) => tys.push(iteration_tys),
                None => all_iteration_tys = Some(vec![iteration_tys]),
            }
        }

        let iteration_tys = if let Some(all_iteration_tys) = all_iteration_tys {
            self.combine_iteration_tys(all_iteration_tys)
        } else {
            self.no_iteration_tys()
        };

        if mode.contains(IterationUse::ALLOWS_ASYNC_ITERABLES_FLAG) {
            AsyncIterationTysResolver.set_iteration_tys_of_iterable_cached(self, ty, iteration_tys);
        } else {
            SyncIterationTysResolver.set_iteration_tys_of_iterable_cached(self, ty, iteration_tys)
        }

        if std::ptr::eq(iteration_tys, self.no_iteration_tys()) {
            None
        } else {
            Some(iteration_tys)
        }
    }
}
