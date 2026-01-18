use super::CheckMode;
use super::TyChecker;
use super::check_expr::IterationUse;
use super::ty;
use super::ty::AccessFlags;
use super::ty::Ty;

use bolt_ts_ast as ast;
use bolt_ts_ast::r#trait;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_optional_ty(&mut self, ty: &'cx Ty<'cx>, is_property: bool) -> &'cx Ty<'cx> {
        assert!(self.config.strict_null_checks());
        let missing_or_undefined = if is_property {
            self.undefined_or_missing_ty
        } else {
            self.undefined_ty
        };
        if ty == missing_or_undefined
            || ty
                .kind
                .as_union()
                .is_some_and(|u| u.tys[0] == missing_or_undefined)
        {
            ty
        } else {
            self.get_union_ty(
                &[ty, missing_or_undefined],
                ty::UnionReduction::Lit,
                false,
                None,
                None,
            )
        }
    }

    pub(super) fn add_optionality(
        &mut self,
        declared_ty: &'cx Ty<'cx>,
        is_property: bool,
        is_optional: bool,
    ) -> &'cx Ty<'cx> {
        if self.config.strict_null_checks() && is_optional {
            self.get_optional_ty(declared_ty, is_property)
        } else {
            declared_ty
        }
    }

    fn get_ty_for_binding_element_parent(&mut self, pat_id: ast::NodeID) -> Option<&'cx Ty<'cx>> {
        debug_assert!(self.p.node(pat_id).is_object_pat() || self.p.node(pat_id).is_array_pat());
        let parent_id = self.parent(pat_id).unwrap();
        let parent = self.p.node(parent_id);
        debug_assert!(parent.is_binding());
        let parent_parent_id = self.parent(parent_id).unwrap();
        let parent_parent = self.p.node(parent_parent_id);
        match parent_parent {
            ast::Node::VarDecl(var) => self.get_ty_for_var_like_decl::<false>(var),
            _ => {
                // TODO:
                None
            }
        }
    }

    fn get_array_binding_element_ty_from_parent_ty(
        &mut self,
        binding: &'cx ast::ArrayBinding<'cx>,
        parent: &'cx ast::ArrayPat<'cx>,
        parent_parent_ty: &'cx Ty<'cx>,
    ) -> &'cx Ty<'cx> {
        debug_assert!(self.parent(binding.id).is_some());
        if self.is_type_any(parent_parent_ty) {
            return parent_parent_ty;
        }

        let mode = if binding.dotdotdot.is_some() {
            IterationUse::DESTRUCTURING
        } else {
            IterationUse::DESTRUCTURING.union(IterationUse::POSSIBLY_OUT_OF_BOUNDS)
        };

        let element_ty = self.check_iterated_ty_or_element_ty(
            mode,
            parent_parent_ty,
            self.undefined_ty,
            Some(parent.id),
        );

        element_ty
    }

    fn get_object_binding_element_ty_from_parent_ty(
        &mut self,
        binding: &'cx ast::ObjectBindingElem<'cx>,
        parent: &'cx ast::ObjectPat<'cx>,
        parent_parent_ty: &'cx Ty<'cx>,
    ) -> &'cx Ty<'cx> {
        debug_assert!(self.parent(binding.id).is_some());
        if self.is_type_any(parent_parent_ty) {
            return parent_parent_ty;
        }

        let access_flags = AccessFlags::EXPRESSION_POSITION;
        if binding.dotdotdot.is_some() {
            // TODO:
            parent_parent_ty
        } else {
            let index_ty = match binding.name {
                ast::ObjectBindingName::Shorthand(ident) => {
                    self.get_string_literal_type_from_string(ident.name)
                }
                ast::ObjectBindingName::Prop { prop_name, .. } => {
                    self.get_lit_ty_from_prop_name(&prop_name.kind)
                }
            };
            let name = match binding.name {
                ast::ObjectBindingName::Shorthand(ident) => ident.id,
                ast::ObjectBindingName::Prop { prop_name, .. } => prop_name.id(),
            };
            let decl_ty = self.get_indexed_access_ty(
                parent_parent_ty,
                index_ty,
                Some(access_flags),
                Some(name),
            );
            // TODO: getFlowTypeOfDestructuring
            decl_ty
        }
    }

    fn get_ty_for_array_binding(
        &mut self,
        binding: &'cx ast::ArrayBinding<'cx>,
        parent: &'cx ast::ArrayPat<'cx>,
    ) -> Option<&'cx Ty<'cx>> {
        debug_assert!(self.parent(binding.id).is_some_and(|p| p == parent.id));
        let check_mode = if binding.dotdotdot.is_some() {
            CheckMode::REST_BINDING_ELEMENT
        } else {
            CheckMode::empty()
        };
        let old_check_mode = self.check_mode;
        self.check_mode = Some(check_mode);
        let parent_ty = self.get_ty_for_binding_element_parent(parent.id);
        self.check_mode = old_check_mode;
        parent_ty.map(|parent_ty| {
            self.get_array_binding_element_ty_from_parent_ty(binding, parent, parent_ty)
        })
    }

    fn get_ty_for_object_binding_elem(
        &mut self,
        binding: &'cx ast::ObjectBindingElem<'cx>,
        parent: &'cx ast::ObjectPat<'cx>,
    ) -> Option<&'cx Ty<'cx>> {
        debug_assert!(self.parent(binding.id).is_some_and(|p| p == parent.id));
        let check_mode = if binding.dotdotdot.is_some() {
            CheckMode::REST_BINDING_ELEMENT
        } else {
            CheckMode::empty()
        };
        let old_check_mode = self.check_mode;
        self.check_mode = Some(check_mode);
        let parent_ty = self.get_ty_for_binding_element_parent(parent.id);
        self.check_mode = old_check_mode;
        parent_ty.map(|parent_ty| {
            self.get_object_binding_element_ty_from_parent_ty(binding, parent, parent_ty)
        })
    }

    fn check_right_hand_side_of_for_of(
        &mut self,
        stmt: &'cx ast::ForOfStmt<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        // TODO: await
        let mode = IterationUse::FOR_OF;
        let input_ty = self.check_non_null_expr(stmt.expr);
        self.check_iterated_ty_or_element_ty(
            mode,
            input_ty,
            self.undefined_ty,
            Some(stmt.expr.id()),
        )
    }

    pub(super) fn get_ty_for_var_like_decl<const INCLUDE_OPTIONALITY: bool>(
        &mut self,
        decl: &impl r#trait::VarLike<'cx>,
    ) -> Option<&'cx Ty<'cx>> {
        // TODO: for in stmt
        // TODO: for of stmt

        let id = decl.id();
        let parent_id = self.parent(id).unwrap();
        let parent = self.p.node(parent_id);

        if self.p.node(id).is_var_decl()
            && let Some(stmt) = parent.as_for_of_stmt()
        {
            return Some(self.check_right_hand_side_of_for_of(stmt));
        }

        match parent {
            ast::Node::ArrayPat(pat) => {
                let n = self.p.node(id).expect_array_binding();
                return self.get_ty_for_array_binding(n, pat);
            }
            ast::Node::ObjectPat(pat) => {
                let n = self.p.node(id).expect_object_binding_elem();
                return self.get_ty_for_object_binding_elem(n, pat);
            }
            _ => {}
        }

        let decl_node = self.p.node(id);
        let is_property = decl_node.is_prop_signature();
        let is_optional = INCLUDE_OPTIONALITY && decl_node.is_optional_decl();

        if let Some(decl_ty) = decl.decl_ty() {
            let ty = self.get_ty_from_type_node(decl_ty);
            return Some(self.add_optionality(ty, is_property, is_optional));
        }

        if decl.is_param() {
            if let Some(setter) = parent.as_setter_decl() {
                // TODO: has bindable name
                let symbol = self.get_symbol_of_decl(setter.id);
                let getter = self
                    .binder
                    .symbol(symbol)
                    .get_declaration_of_kind(|id| self.p.node(id).is_getter_decl());
                if let Some(getter) = getter {
                    let getter_sig = self.get_sig_from_decl(getter);
                    // TODO: this_param
                    return Some(self.get_ret_ty_of_sig(getter_sig));
                }
            }
        }

        if let Some(init) = decl.init() {
            let init_ty = self.check_expr_cached(init);
            Some(self.widened_ty_from_init(decl, init_ty))
        } else {
            None
        }
    }

    pub(super) fn widen_ty_for_var_like_decl(
        &mut self,
        ty: Option<&'cx Ty<'cx>>,
        decl: &impl r#trait::VarLike<'cx>,
    ) -> &'cx Ty<'cx> {
        if let Some(ty) = ty {
            return self.get_widened_ty(ty);
        }
        if let Some(decl) = self.p.node(decl.id()).as_param_decl() {
            if decl.dotdotdot.is_some() {
                self.any_array_ty()
            } else {
                self.any_ty
            }
        } else {
            self.any_ty
        }
    }
}
