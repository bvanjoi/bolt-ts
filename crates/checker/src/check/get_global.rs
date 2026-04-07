use bolt_ts_ast::keyword;
use bolt_ts_binder::SymbolFlags;
use bolt_ts_binder::SymbolID;
use bolt_ts_binder::SymbolName;

use super::TyChecker;
use super::create_ty::IntersectionFlags;
use super::ty;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_global_non_nullable_ty_instantiation(
        &mut self,
        t: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let symbol = self
            .deferred_global_non_nullable_type_alias
            .get_or_init(|| {
                self.get_global_symbol(
                    SymbolName::Atom(keyword::IDENT_NON_NULLABLE),
                    SymbolFlags::TYPE_ALIAS,
                )
            });
        if let Some(symbol) = symbol {
            let tys = self.alloc([t]);
            self.get_type_alias_instantiation(*symbol, tys, None, None)
        } else {
            let tys = &[t, self.empty_object_ty()];
            self.get_intersection_ty(tys, IntersectionFlags::None, None, None)
        }
    }

    fn get_global_symbol(&self, name: SymbolName, meaning: SymbolFlags) -> Option<SymbolID> {
        let symbol = self.global_symbols.0.get(&name).copied()?;
        if self.symbol(symbol).flags.intersects(meaning) {
            Some(symbol)
        } else {
            None
        }
    }

    fn get_global_value_symbol(&self, name: SymbolName) -> Option<SymbolID> {
        self.get_global_symbol(name, SymbolFlags::VALUE)
    }

    fn get_global_ty_symbol(&self, name: SymbolName) -> Option<SymbolID> {
        self.get_global_symbol(name, SymbolFlags::TYPE)
    }

    fn get_global_ty_alias_symbol<const ARITY: u8, const REPORT_ERROR: bool>(
        &mut self,
        name: SymbolName,
    ) -> Option<SymbolID> {
        let Some(symbol) = self.get_global_symbol(name, SymbolFlags::TYPE) else {
            if REPORT_ERROR {
                todo!()
            }
            return None;
        };
        self.get_declared_ty_of_symbol(symbol);
        let ty_params_len = self
            .get_symbol_links(symbol)
            .get_ty_params()
            .map_or(0, |ty_params| ty_params.len());
        if ty_params_len != (ARITY as usize) {
            // TODO: error
            None
        } else {
            Some(symbol)
        }
    }

    pub(super) fn get_global_omit_symbol(&mut self) -> Option<SymbolID> {
        if let Some(symbol) = self.deferred_global_omit_symbol.get() {
            return *symbol;
        }
        let symbol =
            self.get_global_ty_alias_symbol::<2, true>(SymbolName::Atom(keyword::IDENT_OMIT));
        let res = self.deferred_global_omit_symbol.set(symbol);
        debug_assert!(res.is_ok());
        symbol
    }

    fn try_get_global_type<const ARITY: u8, const REPORT_ERROR: bool>(
        &mut self,
        name: SymbolName,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(s) = self.get_global_ty_symbol(name) {
            Some(self.get_declared_ty_of_symbol(s))
        } else if REPORT_ERROR {
            if ARITY != 0 {
                Some(self.empty_object_ty())
            } else {
                Some(self.empty_generic_ty())
            }
        } else {
            None
        }
    }

    pub(super) fn get_global_type<const ARITY: u8, const REPORT_ERROR: bool>(
        &mut self,
        name: SymbolName,
    ) -> &'cx ty::Ty<'cx> {
        let Some(ret) = self.try_get_global_type::<ARITY, REPORT_ERROR>(name) else {
            unreachable!("Global type '{}' not found", name.to_string(&self.atoms));
        };
        ret
    }

    pub(super) fn get_global_awaited_symbol(&mut self) -> Option<SymbolID> {
        if let Some(symbol) = self.deferred_global_awaited_symbol.get() {
            return *symbol;
        }
        let symbol =
            self.get_global_ty_alias_symbol::<1, true>(SymbolName::Atom(keyword::IDENT_AWAITED));
        let res = self.deferred_global_awaited_symbol.set(symbol);
        debug_assert!(res.is_ok());
        symbol
    }

    pub(super) fn get_global_extract_symbol(&mut self) -> Option<SymbolID> {
        if let Some(symbol) = self.deferred_global_extract_symbol.get() {
            return *symbol;
        }
        let symbol =
            self.get_global_ty_alias_symbol::<2, true>(SymbolName::Atom(keyword::IDENT_EXTRACT));
        let res = self.deferred_global_extract_symbol.set(symbol);
        debug_assert!(res.is_ok());
        symbol
    }

    fn get_global_builtin_tys<const ARITY: u8>(
        &mut self,
        names: &[bolt_ts_atom::Atom],
    ) -> Vec<&'cx ty::Ty<'cx>> {
        let mut tys = vec![];
        for name in names {
            let name = SymbolName::Atom(*name);
            let Some(global_ty) = self.try_get_global_type::<ARITY, false>(name) else {
                continue;
            };
            tys.push(global_ty);
        }
        tys
    }

    pub(super) fn get_global_builtin_async_iterator_tys(&mut self) -> ty::Tys<'cx> {
        if let Some(cached) = self.deferred_global_builtin_async_iterator_tys.get() {
            return cached;
        }
        let ret =
            self.get_global_builtin_tys::<1>(&[keyword::IDENT_READABLE_STREAM_ASYNC_ITERATOR]);
        let ret = self.alloc(ret);
        let r = self.deferred_global_builtin_async_iterator_tys.set(ret);
        debug_assert!(r.is_ok());
        ret
    }

    pub(super) fn get_global_builtin_iterator_tys(&mut self) -> ty::Tys<'cx> {
        if let Some(cached) = self.deferred_global_builtin_iterator_tys.get() {
            return cached;
        }
        let ret = self.get_global_builtin_tys::<1>(&[
            keyword::IDENT_ARRAY_ITERATOR,
            keyword::IDENT_MAP_ITERATOR,
            keyword::IDENT_SET_ITERATOR,
            keyword::IDENT_STRING_ITERATOR,
        ]);
        let ret = self.alloc(ret);
        let r = self.deferred_global_builtin_iterator_tys.set(ret);
        debug_assert!(r.is_ok());
        ret
    }

    pub(super) fn get_builtin_iterator_return_ty(&self) -> &'cx ty::Ty<'cx> {
        if self
            .config
            .compiler_options()
            .strict_builtin_iteration_return()
        {
            self.undefined_ty
        } else {
            self.any_ty
        }
    }
}

macro_rules! deferred_global_ty0 {
    (
        $(
            [$name: ident, $ident_name: ident, $fallback: expr, $arity: literal]
        ),*
        $(,)?) => {
        impl<'cx> TyChecker<'cx> {
            $(
                paste::paste! {
                    pub(super) fn [<get_global_ $name _ty>]<const REPORT_ERROR: bool>(&mut self) -> &'cx ty::Ty<'cx> {
                        if let Some(cached) = self.[<deferred_global_ $name _ty>].get() {
                            return cached;
                        }
                        let name = SymbolName::Atom(keyword::[<IDENT_ $ident_name>]);
                        let fallback = self.[<empty_ $fallback _ty>]();
                        let ret = self.try_get_global_type::<$arity, REPORT_ERROR>(name).unwrap_or(fallback);
                        let r = self.[<deferred_global_ $name _ty>].set(ret);
                        debug_assert!(r.is_ok());
                        ret
                    }
                }
            )*
        }
    };
}

macro_rules! deferred_global_ty1 {
    (
        $(
            [$name: ident, $ident_name: ident, $fallback: expr, $arity: literal, $report_error: literal]
        ),*
        $(,)?) => {
        impl<'cx> TyChecker<'cx> {
            $(
                paste::paste! {
                    pub(super) fn [<get_global_ $name _ty>](&mut self) -> &'cx ty::Ty<'cx> {
                        if let Some(cached) = self.[<deferred_global_ $name _ty>].get() {
                            return cached;
                        }
                        let name = SymbolName::Atom(keyword::[<IDENT_ $ident_name>]);
                        let fallback = self.[<empty_ $fallback _ty>]();
                        let ret = self.try_get_global_type::<$arity, $report_error>(name).unwrap_or(fallback);
                        let r = self.[<deferred_global_ $name _ty>].set(ret);
                        debug_assert!(r.is_ok());
                        ret
                    }
                }
            )*
        }
    };
}

macro_rules! deferred_global_constructor_symbol {
    (
        $(
            [$name: ident, $ident_name: ident]
        ),*
        $(,)?) => {
        impl<'cx> TyChecker<'cx> {
            $(
                paste::paste! {
                    pub(super) fn [<get_global_ $name _constructor_symbol>](&mut self) -> Option<SymbolID> {
                        if let Some(symbol) = self.[<deferred_global_ $name _constructor_symbol>].get() {
                            return *symbol;
                        }
                        let name = SymbolName::Atom(keyword::[<IDENT_ $ident_name>]);
                        let symbol = self.get_global_value_symbol(name);
                        let res = self.[<deferred_global_ $name _constructor_symbol>].set(symbol);
                        debug_assert!(res.is_ok());
                        symbol
                    }
                }
            )*
        }
    };
}

deferred_global_ty0!(
    [promise, PROMISE, generic, 1],
    [promise_like, PROMISE_LIKE, generic, 1],
    [iterator, ITERATOR, generic, 3],
    [iterable, ITERABLE, generic, 3],
    [iterable_iterator, ITERABLE_ITERATOR, generic, 3],
    [iterator_object, ITERATOR_OBJECT, generic, 3],
    [generator, GENERATOR, generic, 3],
    [async_iterator, ASYNC_ITERATOR, generic, 3],
    [async_iterable, ASYNC_ITERABLE, generic, 3],
    [async_iterable_iterator, ASYNC_ITERABLE_ITERATOR, generic, 3],
    [async_iterator_object, ASYNC_ITERATOR_OBJECT, generic, 3],
    [async_generator, ASYNC_GENERATOR, generic, 3],
    [iterator_yield_result, ITERATOR_YIELD_RESULT, generic, 1],
    [iterator_return_result, ITERATOR_RETURN_RESULT, generic, 1]
);

deferred_global_ty1!(
    [es_symbol, SYMBOL_CLASS, object, 0, false],
    [bigint, BIGINT_CLASS, object, 0, false],
    [
        template_strings_array,
        TEMPLATE_STRINGS_ARRAY_CLASS,
        object,
        0,
        true
    ],
);

deferred_global_constructor_symbol!([es_symbol, SYMBOL_CLASS], [promise, PROMISE]);
