use bolt_ts_ast::{self as ast, keyword};
use bolt_ts_atom::Atom;

use super::TyChecker;
use super::check_expr::IterationUse;
use super::ty;

pub(super) enum IterationTypeKind {
    Yield,
    Return,
    Next,
}

pub(super) trait IterationTysResolver<'cx> {
    const ITERATOR_SYMBOL_NAME: Atom;
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
    fn get_global_builtin_iterator_tys<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx>;
    fn resolve_iteration_ty(
        &self,
        c: &mut TyChecker<'cx>,
        ty: &'cx ty::Ty<'cx>,
        error_node: Option<ast::NodeID>,
    ) -> Option<&'cx ty::Ty<'cx>>;
}

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
        c.get_global_iterable_iterator_ty::<REPORT_ERROR>()
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

    fn get_global_builtin_iterator_tys<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        todo!()
    }

    fn resolve_iteration_ty(
        &self,
        c: &mut TyChecker<'cx>,
        ty: &'cx ty::Ty<'cx>,
        error_node: Option<bolt_ts_ast::NodeID>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        c.get_awaited_ty(ty)
    }
}
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

    fn get_global_builtin_iterator_tys<const REPORT_ERROR: bool>(
        &self,
        c: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        todo!()
    }

    fn resolve_iteration_ty(
        &self,
        _: &mut TyChecker<'cx>,
        ty: &'cx ty::Ty<'cx>,
        _: Option<bolt_ts_ast::NodeID>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        Some(ty)
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
            self.get_iteration_tys_of_generator_fn_return_ty(return_ty, is_async_generator);
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
    ) -> &'cx ty::IterationTys<'cx> {
        if self.is_type_any(ty) {
            return self.any_iteration_tys();
        }

        let mode = if is_async_generator {
            IterationUse::ASYNC_GENERATOR_RETURN_TYPE
        } else {
            IterationUse::GENERATOR_RETURN_TYPE
        };

        self.get_iteration_tys_of_iterable(ty, mode, None)
    }

    fn get_iteration_tys_of_iterator_worker(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        resolver: impl IterationTysResolver<'cx>,
    ) {
        todo!()
        // if self.is_type_any(ty) {
        //     return self.any_iteration_tys();
        // }
        // let iteration_tys = self.get_iteration_tys_of_iterator_cached(ty, resolver);
    }
}
