use super::TyChecker;
use super::symbol_info::SymbolInfo;
use crate::check::SymbolLinks;
use crate::check::check_expr::IterationUse;
use crate::ty::{self, AccessFlags};
use bolt_ts_binder::SymbolID;

impl<'cx> TyChecker<'cx> {
    fn assign_param_ty(&mut self, param: SymbolID, ctx: Option<&'cx ty::Ty<'cx>>) {
        if let Some(ty) = self.get_symbol_links(param).get_ty()
            && let Some(ctx) = ctx
        {
            assert_eq!(
                ctx, ty,
                "Parameter symbol already has a cached type which differs from newly assigned type"
            );
            return;
        }
        let decl_id = param.decl(self.binder);
        let decl = self.p.node(decl_id).expect_param_decl();
        let ty = if let Some(ctx) = ctx {
            ctx
        } else {
            self.get_widened_ty_for_var_like_decl(decl)
        };
        self.get_mut_symbol_links(param).set_ty(ty);

        use bolt_ts_ast::BindingKind::*;
        match decl.name.kind {
            ObjectPat(pat) => self.assign_object_pat_ele_tys(decl.name, pat, ty),
            ArrayPat(pat) => self.assign_array_pat_ele_tys(decl.name, pat, ty),
            Ident(_) => {}
        }
    }

    fn assign_binding_ele_tys(
        &mut self,
        binding: &'cx bolt_ts_ast::Binding<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) {
        use bolt_ts_ast::BindingKind::*;
        match binding.kind {
            Ident(_) => {
                let symbol = self.get_symbol_of_decl(binding.id);
                let prev = self
                    .symbol_links
                    .insert(symbol, SymbolLinks::default().with_ty(ty));
                assert!(prev.is_none());
            }
            ObjectPat(pat) => self.assign_object_pat_ele_tys(binding, pat, ty),
            ArrayPat(pat) => self.assign_array_pat_ele_tys(binding, pat, ty),
        }
    }

    fn assign_object_pat_ele_tys(
        &mut self,
        binding: &'cx bolt_ts_ast::Binding<'cx>,
        pat: &'cx bolt_ts_ast::ObjectPat<'cx>,
        parent_ty: &'cx ty::Ty<'cx>,
    ) {
        for ele in pat.elems {}
    }

    fn assign_array_pat_ele_tys(
        &mut self,
        binding: &'cx bolt_ts_ast::Binding<'cx>,
        pat: &'cx bolt_ts_ast::ArrayPat<'cx>,
        parent_ty: &'cx ty::Ty<'cx>,
    ) {
        use bolt_ts_ast::ArrayBindingElemKind::*;
        for (idx, ele) in pat.elems.iter().enumerate() {
            match ele.kind {
                Binding(binding) => {
                    let ty = self.get_binding_ele_ty_from_parent_ty(
                        binding.name,
                        binding.id,
                        idx,
                        parent_ty,
                        false,
                    );
                    self.assign_binding_ele_tys(binding.name, ty);
                }
                Omit(_) => {}
            }
        }
    }

    fn get_binding_ele_ty_from_parent_ty(
        &mut self,
        binding: &'cx bolt_ts_ast::Binding<'cx>,
        ele: bolt_ts_ast::NodeID,
        idx: usize,
        mut parent_ty: &'cx ty::Ty<'cx>,
        no_tuple_bounds_check: bool,
    ) -> &'cx ty::Ty<'cx> {
        if self.is_type_any(Some(parent_ty)) {
            return parent_ty;
        }

        let strict_null_check = self.config.strict_null_checks();

        if strict_null_check
            && self
                .p
                .node_flags(binding.id)
                .contains(bolt_ts_ast::NodeFlags::AMBIENT)
            && self
                .node_query(binding.id.module())
                .is_part_of_param_decl(binding.id)
        {
            parent_ty = self.get_non_nullable_ty(parent_ty)
        } else if strict_null_check
            && self
                .p
                .node(self.parent(binding.id).unwrap())
                .initializer()
                .is_some_and(|init| {
                    // let ty = self.get_ty_of_init(init);
                    false
                    // TODO: has_ty_facts
                })
        {
            todo!()
        }

        use bolt_ts_ast::BindingKind::*;

        let access_flags = AccessFlags::EXPRESSION_POSITION
            | if no_tuple_bounds_check {
                // TODO: add `has_default_value` in condition
                AccessFlags::ALLOWING_MISSING
            } else {
                AccessFlags::empty()
            };

        (match binding.kind {
            ObjectPat(pat) => {
                todo!()
            }
            ArrayPat(pat) => {
                let ele = self.p.node(ele).expect_array_binding();
                let has_dotdotdot = ele.dotdotdot.is_some();
                if has_dotdotdot {
                    todo!()
                } else if self.is_array_like_ty(parent_ty) {
                    let index_ty = self.get_number_literal_type_from_number(idx as f64);

                    self.get_indexed_access_ty_or_undefined(
                        parent_ty,
                        index_ty,
                        Some(access_flags),
                        Some(ele.id),
                    )
                    .unwrap_or(self.error_ty)
                } else {
                    self.check_iterated_ty_or_element_ty(
                        IterationUse::DESTRUCTURING
                            | if has_dotdotdot {
                                IterationUse::empty()
                            } else {
                                IterationUse::POSSIBLY_OUT_OF_BOUNDS
                            },
                        parent_ty,
                        self.undefined_ty,
                        Some(ele.id),
                    )
                }
            }
            _ => self.check_iterated_ty_or_element_ty(
                IterationUse::DESTRUCTURING.union(IterationUse::POSSIBLY_OUT_OF_BOUNDS),
                parent_ty,
                self.undefined_ty,
                Some(binding.id),
            ),
        }) as _
    }

    pub(super) fn assign_contextual_param_tys(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        context: &'cx ty::Sig<'cx>,
    ) {
        let ty_params = if context.ty_params.is_some() {
            if sig.ty_params.is_none() {
                // TODO: store context ty_params into sig.
                context.ty_params
            } else {
                return;
            }
        } else {
            sig.ty_params
        };

        let len = sig.params.len() - (if sig.has_rest_param() { 1 } else { 0 });
        for i in 0..len {
            let param = sig.params[i];
            let decl = param.decl(self.binder);
            let decl = self.p.node(decl).expect_param_decl();
            if decl.ty.is_none() {
                let mut ty = self.try_get_ty_at_pos(context, i);
                if let Some(t) = ty
                    && decl.init.is_some()
                    && let init_ty = self.check_decl_init(decl, None)
                    && !self.is_type_assignable_to(init_ty, t)
                    && let target = self.widened_ty_from_init(decl, init_ty)
                    && self.is_type_assignable_to(t, target)
                {
                    ty = Some(init_ty);
                }
                self.assign_param_ty(param, ty);
            }
        }

        if sig.has_rest_param() {
            todo!()
        }
    }

    pub(super) fn assign_non_contextual_param_tys(&mut self, sig: &'cx ty::Sig<'cx>) {
        if let Some(this_param) = sig.this_param {
            // TODO:
        }

        for param in sig.params {
            // TODO:
        }
    }
}
