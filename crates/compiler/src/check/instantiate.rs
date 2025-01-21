use crate::bind::{SymbolFlags, SymbolID};
use crate::ty::ObjectFlags;
use crate::{ast, ty};

use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    pub(super) fn instantiate_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: Option<&'cx ty::TyMapper<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(mapper) = mapper {
            self.instantiate_ty_with_alias(ty, mapper)
        } else {
            ty
        }
    }

    pub fn instantiate_list<T: PartialEq + Copy>(
        &mut self,
        list: &'cx [T],
        mapper: &'cx ty::TyMapper<'cx>,
        f: impl Fn(&mut Self, T, &'cx ty::TyMapper<'cx>) -> T,
    ) -> &'cx [T] {
        let len = list.len();
        for i in 0..len {
            let item = list[i];
            let mapped = f(self, item, mapper);
            if item != mapped {
                let mut result = Vec::with_capacity(list.len());
                result.extend(list[0..i].iter());
                for j in i..len {
                    result.push(f(self, list[j], mapper));
                }
                assert_eq!(result.len(), list.len());
                let result = self.alloc(result);
                return result;
            }
        }
        list
    }

    pub(super) fn instantiate_tys(
        &mut self,
        tys: ty::Tys<'cx>,
        mapper: &'cx ty::TyMapper<'cx>,
    ) -> ty::Tys<'cx> {
        self.instantiate_list(tys, mapper, |this, item, mapper| {
            this.instantiate_ty(item, Some(mapper))
        })
    }

    pub(super) fn could_contain_ty_var(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        if ty.kind.is_instantiable_non_primitive() {
            true
        } else if let Some(object) = ty.kind.as_object() {
            // TODO: !isNonGenericTopLevelType(type)
            if object.kind.is_reference() {
                true
            } else {
                object.kind.is_anonymous()
            }
        } else if ty.kind.is_union_or_intersection() {
            // TODO: condition
            false
        } else {
            false
        }
    }

    pub(super) fn instantiate_ty_with_alias(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx ty::TyMapper<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if !self.could_contain_ty_var(ty) {
            return ty;
        }
        self.instantiate(ty, mapper)
    }

    fn instantiate(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx ty::TyMapper<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        use ty::TyKind::*;
        match ty.kind {
            Param(_) => self.get_mapped_ty(mapper, ty),
            IndexedAccess(indexed) => {
                let object_ty = self.instantiate_ty_with_alias(indexed.object_ty, mapper);
                let index_ty = self.instantiate_ty_with_alias(indexed.index_ty, mapper);
                self.get_indexed_access_ty(object_ty, index_ty, Some(indexed.access_flags), None)
            }
            Cond(_) => self.get_cond_ty_instantiation(ty, mapper),
            Object(_) => self.get_object_ty_instantiation(ty, mapper),
            _ => {
                todo!("{:?}", ty.kind);
            }
        }
    }

    fn instantiate_anonymous_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx ty::TyMapper<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let target = ty.kind.expect_object_anonymous();
        self.create_instantiating_anonymous_ty(target.symbol, ty, mapper, ObjectFlags::empty())
    }

    fn get_object_ty_instantiation(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx ty::TyMapper<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        use ty::ObjectTyKind::*;
        if let Some(refer) = ty.kind.as_object_reference() {
            let ty_args = self.instantiate_tys(refer.resolved_ty_args, mapper);
            if refer.resolved_ty_args == ty_args {
                return ty;
            }
            let object_ty = refer.target.kind.expect_object();
            match &object_ty.kind {
                Tuple(tuple) => {
                    if !std::ptr::eq(ty_args, refer.resolved_ty_args) {
                        self.create_normalized_tuple_ty(
                            ty_args,
                            tuple.element_flags,
                            tuple.combined_flags,
                        )
                    } else {
                        refer.target
                    }
                }
                _ => self.create_reference_ty(
                    ty::ReferenceTy {
                        target: refer.target,
                        resolved_ty_args: ty_args,
                    },
                    Default::default(),
                ),
            }
        } else if let Some(a) = ty.kind.as_object_anonymous() {
            let s = self.binder.symbol(a.symbol);
            let flags = s.flags;
            let decl = if flags.intersects(SymbolFlags::FUNCTION) {
                s.expect_fn().decls[0]
            } else if flags.intersects(SymbolFlags::TYPE_LITERAL) {
                s.expect_ty_lit().decl
            } else if flags.intersects(SymbolFlags::OBJECT_LITERAL) {
                s.expect_object().decl
            } else {
                return ty;
            };
            // TODO: use `node_links`
            let outer_params = self.get_outer_ty_params(decl, true);
            let ty_params = outer_params.unwrap_or_default();
            if !ty_params.is_empty() {
                let combined_mapper = self.combine_ty_mappers(a.mapper, mapper);
                let ty_args = ty_params
                    .iter()
                    .map(|t| self.get_mapped_ty(combined_mapper, t))
                    .collect::<Vec<_>>();
                let new_mapper = ty::TyMapper::create(self.alloc(ty_params), self.alloc(ty_args));
                self.instantiate_anonymous_ty(ty, self.alloc(new_mapper))
            } else {
                ty
            }
        } else {
            ty
        }
    }

    fn get_cond_ty_instantiation(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx ty::TyMapper<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let Some(cond_ty) = ty.kind.as_cond_ty() else {
            unreachable!()
        };
        if let Some(outer_ty_params) = cond_ty.root.outer_ty_params {
            let ty_args = outer_ty_params
                .iter()
                .map(|t| self.get_mapped_ty(mapper, t))
                .collect::<Vec<_>>();
            let ty_args = self.alloc(ty_args);
            let mapper = self.alloc(ty::TyMapper::create(outer_ty_params, ty_args));
            self.get_cond_ty(cond_ty.root, Some(mapper))
        } else {
            ty
        }
    }

    pub(super) fn get_permissive_instantiation(&self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty.kind.is_param() {
            self.any_ty()
        } else {
            ty
        }
    }

    pub(super) fn get_sig_instantiation(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        ty_args: Option<ty::Tys<'cx>>,
        is_js: bool,
        inferred_ty_params: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Sig<'cx> {
        let ty_args = self.fill_missing_ty_args(
            ty_args,
            sig.ty_params,
            self.get_min_ty_args_count(sig.ty_params),
        );
        let sig = self.get_sig_instantiation_without_filling_ty_args(sig, ty_args);
        if let Some(inferred_ty_params) = inferred_ty_params {
            todo!()
        }
        sig
    }

    pub(super) fn create_sig_instantiation(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        ty_args: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Sig<'cx> {
        let ty_params = self.get_ty_params_for_mapper(sig);
        let mapper = self.create_ty_mapper_with_optional_target(ty_params, ty_args);
        self.instantiate_sig(sig, mapper, true)
    }

    fn get_sig_instantiation_without_filling_ty_args(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        ty_args: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Sig<'cx> {
        // TODO: cache
        self.create_sig_instantiation(sig, ty_args)
    }

    pub(super) fn get_restrictive_instantiation(&self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty.kind.is_param() {
            self.get_restrictive_ty_param(ty)
        } else {
            ty
        }
    }

    pub(super) fn get_type_alias_instantiation(
        &mut self,
        symbol: SymbolID,
        args: ty::Tys<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let ty = self.get_declared_ty_of_symbol(symbol);
        let Some(ty_params) = self.get_symbol_links(symbol).get_ty_params() else {
            unreachable!()
        };
        let min_params_count = self.get_min_ty_args_count(Some(ty_params));
        // TODO: cache
        let args = self.fill_missing_ty_args(Some(args), Some(ty_params), min_params_count);
        let mapper = self.create_ty_mapper_with_optional_target(ty_params, args);
        self.instantiate_ty_with_alias(ty, mapper)
    }

    pub(super) fn get_min_ty_args_count(&self, ty_params: Option<ty::Tys<'cx>>) -> usize {
        let Some(ty_params) = ty_params else {
            return 0;
        };
        let mut min = 0;
        for (i, param) in ty_params.iter().enumerate() {
            let param = param.kind.expect_param();
            if !self.has_ty_param_default(param) {
                min = i + 1;
            }
        }
        min
    }

    pub(super) fn fill_missing_ty_args(
        &mut self,
        ty_args: Option<ty::Tys<'cx>>,
        ty_params: Option<ty::Tys<'cx>>,
        min_ty_argument_count: usize,
    ) -> Option<ty::Tys<'cx>> {
        let Some(ty_params) = ty_params else {
            return Some(&[]);
        };
        if ty_params.is_empty() {
            return Some(&[]);
        }
        let args_len = ty_args.map_or(0, |args| args.len());
        let params_len = ty_params.len();
        if args_len >= min_ty_argument_count && args_len <= params_len {
            let mut result = Vec::with_capacity(params_len);
            if let Some(ty_args) = ty_args {
                for arg in ty_args {
                    result.push(*arg);
                }
            }

            let base_default_ty = self.any_ty();
            for param in ty_params.iter().skip(args_len) {
                if let Some(default_ty) = self.get_default_ty_from_ty_param(param) {
                    result.push(default_ty);
                } else {
                    result.push(base_default_ty);
                }
            }
            Some(self.alloc(result))
        } else {
            ty_args
        }
    }

    pub(super) fn get_default_ty_from_ty_param(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let param = ty.kind.expect_param();
        // TODO: cache `self.get_default_of_param(param_ty, id)`
        let default = self.ty_param_node(param).default?;
        Some(self.get_ty_from_type_node(default))
    }

    fn has_ty_param_default(&self, ty_param: &'cx ty::ParamTy) -> bool {
        let param = self.ty_param_node(ty_param);
        param.default.is_some()
    }

    fn ty_param_node(&self, ty_param: &'cx ty::ParamTy) -> &'cx ast::TyParam<'cx> {
        let symbol = self.binder.symbol(ty_param.symbol);
        let symbol = symbol.expect_ty_param();
        let node = self.p.node(symbol.decl);
        let Some(param) = node.as_ty_param() else {
            unreachable!()
        };
        param
    }
}
