use super::SymbolLinks;
use super::TyChecker;
use super::ty::{self, CheckFlags};

use bolt_ts_ast as ast;
use bolt_ts_binder::Symbol;
use bolt_ts_binder::SymbolFlags;
use bolt_ts_binder::SymbolID;
use bolt_ts_binder::SymbolName;
use bolt_ts_utils::fx_indexmap_with_capacity;

impl<'cx> TyChecker<'cx> {
    fn get_ty_from_object_pat<const INCLUDE_PATTERN_IN_TY: bool>(
        &mut self,
        pat: &'cx bolt_ts_ast::ObjectPat<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let mut members = fx_indexmap_with_capacity(pat.elems.len());
        let mut string_index_info = None;
        let mut object_flags = ty::ObjectFlags::OBJECT_LITERAL
            .union(ty::ObjectFlags::CONTAINS_OBJECT_OR_ARRAY_LITERAL);
        for elem in pat.elems {
            if elem.dotdotdot.is_some() && string_index_info.is_none() {
                string_index_info = Some(self.alloc(ty::IndexInfo {
                    key_ty: self.string_ty,
                    val_ty: self.any_ty,
                    is_readonly: false,
                    symbol: Symbol::ERR,
                }));
                continue;
            }
            let name = elem.name.name();
            let expr_ty = self.get_literal_ty_from_prop_name(&name);
            if !expr_ty.useable_as_prop_name() {
                object_flags |= ty::ObjectFlags::OBJECT_LITERAL_PATTERN_WITH_COMPUTED_PROPERTIES;
                continue;
            }
            let flags = SymbolFlags::PROPERTY
                | if elem.init.is_some() {
                    SymbolFlags::OPTIONAL
                } else {
                    SymbolFlags::empty()
                };
            let name = match name {
                ast::PropNameKind::Ident(n) => SymbolName::Atom(n.name),
                ast::PropNameKind::PrivateIdent(n) => SymbolName::Atom(n.name),
                ast::PropNameKind::StringLit { raw, .. } => SymbolName::Atom(raw.val),
                ast::PropNameKind::NumLit(n) => SymbolName::EleNum(n.val.into()),
                ast::PropNameKind::BigIntLit(n) => SymbolName::Atom(n.val.1),
                ast::PropNameKind::Computed(_) => SymbolName::Computed,
            };
            let ty = self.get_ty_from_object_binding::<INCLUDE_PATTERN_IN_TY>(elem);
            let links = SymbolLinks::default().with_ty(ty);
            let symbol = self.create_transient_symbol(
                name,
                flags | SymbolFlags::TRANSIENT,
                links,
                None,
                None,
                None,
            );
            members.insert(name, symbol);
        }
        let members = self.alloc(members);
        let index_infos = if let Some(string_index_info) = string_index_info {
            self.alloc([string_index_info])
        } else {
            self.empty_array()
        };
        if INCLUDE_PATTERN_IN_TY {
            // TODO: pattern
            object_flags |= ty::ObjectFlags::CONTAINS_OBJECT_OR_ARRAY_LITERAL;
        }
        let result = self.create_anonymous_ty_with_resolved(
            None,
            object_flags,
            members,
            self.empty_array(),
            self.empty_array(),
            index_infos,
            None,
        );
        result
    }

    pub(super) fn get_ty_from_object_binding<const INCLUDE_PATTERN_IN_TY: bool>(
        &mut self,
        elem: &'cx bolt_ts_ast::ObjectBindingElem<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if elem.init.is_some() {
            let contextual_ty = if let ast::ObjectBindingName::Prop { name, .. } = elem.name
                && let ast::BindingKind::ObjectPat(_) | ast::BindingKind::ArrayPat(_) = name.kind
            {
                self.get_ty_from_binding_pat::<INCLUDE_PATTERN_IN_TY>(name)
            } else {
                self.unknown_ty
            };
            let old_check_mode = self.check_mode;
            self.check_mode = Some(super::CheckMode::empty());
            let ty = self.check_decl_init(elem, Some(contextual_ty));
            self.check_mode = old_check_mode;
            let ty = self.get_widened_lit_ty_for_init(elem, ty);
            return self.add_optionality::<false>(ty, true);
        }

        if let ast::ObjectBindingName::Prop { name, .. } = elem.name
            && let ast::BindingKind::ObjectPat(_) | ast::BindingKind::ArrayPat(_) = name.kind
        {
            return self.get_ty_from_binding_pat::<INCLUDE_PATTERN_IN_TY>(name);
        }

        // TODO: report_errors

        if INCLUDE_PATTERN_IN_TY {
            self.non_inferrable_any_ty
        } else {
            self.any_ty
        }
    }

    fn get_ty_from_array_binding<const INCLUDE_PATTERN_IN_TY: bool>(
        &mut self,
        elem: &'cx bolt_ts_ast::ArrayBinding<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if elem.init.is_some() {
            let contextual_ty = match elem.name.kind {
                ast::BindingKind::Ident(_) => self.unknown_ty,
                ast::BindingKind::ObjectPat(_) | ast::BindingKind::ArrayPat(_) => {
                    self.get_ty_from_binding_pat::<INCLUDE_PATTERN_IN_TY>(elem.name)
                }
            };
            let old_check_mode = self.check_mode;
            self.check_mode = Some(super::CheckMode::empty());
            let ty = self.check_decl_init(elem, Some(contextual_ty));
            self.check_mode = old_check_mode;
            let ty = self.get_widened_lit_ty_for_init(elem, ty);
            return self.add_optionality::<false>(ty, true);
        }

        if let ast::BindingKind::ObjectPat(_) | ast::BindingKind::ArrayPat(_) = elem.name.kind {
            return self.get_ty_from_binding_pat::<INCLUDE_PATTERN_IN_TY>(elem.name);
        }

        // TODO: report_errors

        if INCLUDE_PATTERN_IN_TY {
            self.non_inferrable_any_ty
        } else {
            self.any_ty
        }
    }

    fn get_ty_from_array_pat<const INCLUDE_PATTERN_IN_TY: bool>(
        &mut self,
        pat: &'cx bolt_ts_ast::ArrayPat<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let elements = pat.elems;
        let last_elements = elements.last();
        let rest_elements = if let Some(last) = last_elements
            && let ast::ArrayBindingElemKind::Binding(binding) = last.kind
            && binding.dotdotdot.is_some()
        {
            Some(last)
        } else {
            None
        };
        if elements.is_empty() || (elements.len() == 1 && rest_elements.is_some()) {
            if *self.config.compiler_options().target() >= bolt_ts_config::Target::ES2015 {
                todo!()
            } else {
                return self.any_array_ty();
            }
        }

        let element_types = elements
            .iter()
            .map(|e| match e.kind {
                ast::ArrayBindingElemKind::Omit(_) => self.any_ty,
                ast::ArrayBindingElemKind::Binding(elem) => {
                    self.get_ty_from_array_binding::<INCLUDE_PATTERN_IN_TY>(elem)
                }
            })
            .collect::<Vec<_>>();
        let element_types = self.alloc(element_types);
        let min_len = elements
            .iter()
            .enumerate()
            .rev()
            .find_map(|(idx, e)| {
                if rest_elements.is_none_or(|rest| !std::ptr::eq(e, rest)) {
                    return None;
                }
                match e.kind {
                    ast::ArrayBindingElemKind::Omit(_) => None,
                    ast::ArrayBindingElemKind::Binding(binding) => {
                        if binding.init.is_none() {
                            Some(idx + 1)
                        } else {
                            None
                        }
                    }
                }
            })
            .map_or(0, |idx| idx + 1);
        let element_flags = elements
            .iter()
            .enumerate()
            .map(|(idx, e)| {
                if rest_elements.is_some_and(|rest| std::ptr::eq(rest, e)) {
                    ty::ElementFlags::REST
                } else if idx >= min_len {
                    ty::ElementFlags::OPTIONAL
                } else {
                    ty::ElementFlags::REQUIRED
                }
            })
            .collect::<Vec<_>>();
        let element_flags = self.alloc(element_flags);
        let res = self.create_tuple_ty(element_types, Some(element_flags), false);
        // if INCLUDE_PATTERN_IN_TY {
        //     todo!()
        // };
        res
    }

    pub(super) fn get_ty_from_binding_pat<const INCLUDE_PATTERN_IN_TY: bool>(
        &mut self,
        binding: &'cx ast::Binding<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        // if INCLUDE_PATTERN_IN_TY {
        //     todo!("push")
        // }
        let ret = match binding.kind {
            ast::BindingKind::Ident(_) => unreachable!(),
            ast::BindingKind::ObjectPat(pat) => {
                self.get_ty_from_object_pat::<INCLUDE_PATTERN_IN_TY>(pat)
            }
            ast::BindingKind::ArrayPat(pat) => {
                self.get_ty_from_array_pat::<INCLUDE_PATTERN_IN_TY>(pat)
            }
        };
        // if INCLUDE_PATTERN_IN_TY {
        //     todo!("pop")
        // };
        ret
    }

    fn assign_param_ty(&mut self, param: SymbolID, contextual_ty: Option<&'cx ty::Ty<'cx>>) {
        if let Some(ty) = self.get_symbol_links(param).get_ty()
            && let Some(ctx) = contextual_ty
        {
            assert_eq!(
                ctx, ty,
                "Parameter symbol already has a cached type which differs from newly assigned type"
            );
            return;
        }
        let param_symbol = self.symbol(param);
        let decl = param_symbol.value_decl;
        let decl_node = decl.map(|decl| self.p.node(decl).expect_param_decl());
        let ty = if let Some(ctx) = contextual_ty {
            ctx
        } else if let Some(decl) = decl_node {
            self.get_widened_ty_for_var_like_decl(decl)
        } else {
            self.get_type_of_symbol(param)
        };
        let is_optional = decl_node.is_some_and(|decl_node| {
            // TODO: optional for js
            decl_node.init.is_none() && decl_node.question.is_some()
        });

        let mut ty = self.add_optionality::<false>(ty, is_optional);
        if let Some(declaration) = decl_node
            && !matches!(declaration.name.kind, ast::BindingKind::Ident(_))
            && ty == self.unknown_ty
        {
            ty = self.get_ty_from_binding_pat::<false>(declaration.name);
        }
        self.get_mut_symbol_links(param).set_ty(ty);
        if let Some(declaration) = decl_node
            && !matches!(declaration.name.kind, ast::BindingKind::Ident(_))
        {
            self.assign_binding_element_types(declaration.name, ty);
        }
    }

    fn assign_binding_element_types(
        &mut self,
        binding: &'cx bolt_ts_ast::Binding<'cx>,
        parent_ty: &'cx ty::Ty<'cx>,
    ) {
        use bolt_ts_ast::BindingKind::*;
        match binding.kind {
            ObjectPat(pat) => self.assign_object_pat_elem_tys(pat, parent_ty),
            ArrayPat(pat) => self.assign_array_pat_elem_tys(pat, parent_ty),
            Ident(_) => {}
        };
    }

    fn assign_array_binding_elem_tys(
        &mut self,
        binding: &'cx bolt_ts_ast::ArrayBinding<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) {
        use bolt_ts_ast::BindingKind::*;
        match binding.name.kind {
            Ident(_) => {
                let symbol = self.get_symbol_of_decl(binding.id);
                let prev = self
                    .symbol_links
                    .insert(symbol, SymbolLinks::default().with_ty(ty));
                assert!(prev.is_none());
            }
            ObjectPat(pat) => self.assign_object_pat_elem_tys(pat, ty),
            ArrayPat(pat) => self.assign_array_pat_elem_tys(pat, ty),
        }
    }

    fn assign_object_pat_elem_tys(
        &mut self,
        pat: &'cx bolt_ts_ast::ObjectPat<'cx>,
        parent_ty: &'cx ty::Ty<'cx>,
    ) {
        for elem in pat.elems {
            let ty = self.get_object_binding_element_ty_from_parent_ty(*elem, pat, parent_ty);
            match elem.name {
                ast::ObjectBindingName::Shorthand(_) => {
                    let symbol = self.get_symbol_of_decl(elem.id);
                    let prev = self
                        .symbol_links
                        .insert(symbol, SymbolLinks::default().with_ty(ty));
                    assert!(prev.is_none());
                }
                ast::ObjectBindingName::Prop { name, .. } => {
                    self.assign_binding_element_types(name, ty);
                }
            }
        }
    }

    fn assign_array_pat_elem_tys(
        &mut self,
        pat: &'cx bolt_ts_ast::ArrayPat<'cx>,
        parent_ty: &'cx ty::Ty<'cx>,
    ) {
        use bolt_ts_ast::ArrayBindingElemKind::*;
        for ele in pat.elems {
            match ele.kind {
                Binding(binding) => {
                    let ty = self.get_array_binding_element_ty_from_parent_ty(
                        binding, pat, false, parent_ty,
                    );
                    self.assign_array_binding_elem_tys(binding, ty);
                }
                Omit(_) => {}
            }
        }
    }

    pub(super) fn assign_contextual_param_tys(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        context: &'cx ty::Sig<'cx>,
    ) {
        if let Some(context_ty_params) = self.get_sig_links(context.id).get_ty_params() {
            if self.get_sig_links(sig.id).get_ty_params().is_none() {
                self.get_mut_sig_links(sig.id)
                    .set_ty_params(context_ty_params);
            } else {
                return;
            }
        }

        let sig_has_rest = sig.has_rest_param();
        let len = sig.params.len() - (if sig_has_rest { 1 } else { 0 });
        for i in 0..len {
            let param = sig.params[i];
            let decl = param.decl(&self.binder);
            let decl = self.p.node(decl).expect_param_decl();
            // TODO: !getEffectiveTypeAnnotationNode(decl)
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

        if sig_has_rest {
            let param = sig.params.last().unwrap();
            let p = self.symbol(*param);
            let need_assign = if let Some(value_decl) = p.value_decl {
                let decl = self.p.node(value_decl).expect_param_decl();
                // TODO: !getEffectiveTypeAnnotationNode(decl)
                decl.ty.is_none()
            } else {
                self.get_check_flags(*param)
                    .contains(CheckFlags::DEFERRED_TYPE)
            };
            if need_assign {
                let contextual_parameter_ty = self.get_rest_ty_at_pos(context, len, false);
                self.assign_param_ty(*param, Some(contextual_parameter_ty));
            }
        }
    }

    pub(super) fn assign_non_contextual_param_tys(&mut self, sig: &'cx ty::Sig<'cx>) {
        if let Some(this_param) = sig.this_param {
            self.assign_param_ty(this_param, None);
        }

        for param in sig.params {
            self.assign_param_ty(*param, None);
        }
    }
}
