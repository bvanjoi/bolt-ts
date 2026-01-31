use super::CheckMode;
use super::SymbolInfo;
use super::TyChecker;
use super::check_expr::IterationUse;
use super::errors;
use super::relation;
use super::ty;
use super::ty::AccessFlags;
use super::ty::Ty;

use bolt_ts_ast as ast;
use bolt_ts_ast::keyword;
use bolt_ts_ast::r#trait;
use bolt_ts_binder::SymbolFlags;
use bolt_ts_binder::SymbolID;
use bolt_ts_utils::fx_indexmap_with_capacity;

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
        if self.check_mode.is_some_and(|m| m != CheckMode::empty()) {
            match parent_parent {
                ast::Node::VarDecl(var) => self.get_ty_for_var_like_decl::<false>(var),
                _ => {
                    // TODO:
                    None
                }
            }
        } else {
            // TODO: cache
            match parent_parent {
                ast::Node::VarDecl(var) => self.get_ty_for_var_like_decl::<false>(var),
                _ => {
                    // TODO:
                    None
                }
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
        mut parent_parent_ty: &'cx Ty<'cx>,
    ) -> &'cx Ty<'cx> {
        debug_assert!(self.parent(binding.id).is_some());
        if self.is_type_any(parent_parent_ty) {
            return parent_parent_ty;
        }

        let access_flags = AccessFlags::EXPRESSION_POSITION;
        if binding.dotdotdot.is_some() {
            parent_parent_ty = self.get_reduced_ty(parent_parent_ty);
            if parent_parent_ty.flags.contains(ty::TypeFlags::UNKNOWN)
                || !self.is_valid_spread_ty(parent_parent_ty)
            {
                let error = errors::RestTypesMayOnlyBeCreatedFromObjectTypes { span: binding.span };
                self.push_error(Box::new(error));
                return self.error_ty;
            }
            let mut literal_members = Vec::with_capacity(parent.elems.len());
            for elem in parent.elems {
                if elem.dotdotdot.is_none() {
                    match elem.name {
                        ast::ObjectBindingName::Shorthand(ident) => {
                            literal_members.push(ast::PropNameKind::Ident(ident));
                        }
                        ast::ObjectBindingName::Prop { prop_name, .. } => {
                            literal_members.push(prop_name.kind);
                        }
                    }
                }
            }
            let symbol = self.get_symbol_of_decl(binding.id);
            // TODO: getFlowTypeOfDestructuring
            self.get_rest_ty(parent_parent_ty, &literal_members, Some(symbol))
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

    fn is_spreadable_property(&self, prop: SymbolID) -> bool {
        let s = self.symbol(prop);
        // TODO: s.decls.iter().any(|decl| self.is_private_identifier_class_element_declaration(decl))
        let flags = s.flags;
        return !flags.intersects(
            SymbolFlags::METHOD
                .union(SymbolFlags::GET_ACCESSOR)
                .union(SymbolFlags::SET_ACCESSOR),
        ) || s.decls.as_ref().is_none_or(|decls| {
            decls.iter().any(|decl| {
                if let Some(p) = self.parent(*decl) {
                    self.p.node(p).is_class_like()
                } else {
                    false
                }
            })
        });
    }

    fn get_rest_ty(
        &mut self,
        source: &'cx Ty<'cx>,
        props: &[ast::PropNameKind<'cx>],
        symbol: Option<SymbolID>,
    ) -> &'cx Ty<'cx> {
        let source = self.filter_type(source, |this, t| !t.flags.contains(ty::TypeFlags::NULLABLE));
        if source.flags.contains(ty::TypeFlags::NEVER) {
            return self.empty_object_ty();
        } else if source.flags.contains(ty::TypeFlags::UNION) {
            return self
                .map_ty(
                    source,
                    |this, t| Some(this.get_rest_ty(t, props, symbol)),
                    false,
                )
                .unwrap();
        }

        let mut omit_key_ty = {
            let props = props
                .iter()
                .map(|prop| self.get_lit_ty_from_prop_name(prop))
                .collect::<Vec<_>>();
            self.get_union_ty(&props, ty::UnionReduction::Lit, false, None, None)
        };

        let mut spreadable_props = vec![];
        let mut unsparedable_to_rest_keys = vec![];

        for &prop in self.get_props_of_ty(source) {
            let lit_ty_from_property = self.get_lit_ty_from_prop(
                prop,
                ty::TypeFlags::STRING_OR_NUMBER_LITERAL_OR_UNIQUE,
                false,
            );
            if !self.is_type_related_to(
                lit_ty_from_property,
                omit_key_ty,
                relation::RelationKind::Assignable,
            ) && (self.get_declaration_modifier_flags_from_symbol(prop, None)
                & (ast::ModifierKind::Private | ast::ModifierKind::Protected))
                .is_empty()
                && self.is_spreadable_property(prop)
            {
                spreadable_props.push(prop);
            } else {
                unsparedable_to_rest_keys.push(lit_ty_from_property);
            }
        }

        if self.is_generic_object_ty(source) || self.is_generic_index_ty(omit_key_ty) {
            if unsparedable_to_rest_keys.len() > 0 {
                // If the type we're spreading from has properties that cannot
                // be spread into the rest type (e.g. getters, methods), ensure
                // they are explicitly omitted, as they would in the non-generic case.
                let mut tys = Vec::with_capacity(unsparedable_to_rest_keys.len() + 1);
                tys.push(omit_key_ty);
                tys.extend(unsparedable_to_rest_keys);
                let tys = self.alloc(tys);
                omit_key_ty = self.get_union_ty(tys, ty::UnionReduction::Lit, false, None, None);
            }

            if omit_key_ty.flags.contains(ty::TypeFlags::NEVER) {
                return source;
            }

            let Some(omit_type_alias) = self.get_global_ty_alias_symbol(
                bolt_ts_binder::SymbolName::Atom(keyword::IDENT_OMIT),
                2,
                true,
            ) else {
                return self.error_ty;
            };
            let ty_args = self.alloc([source, omit_key_ty]);
            self.get_type_alias_instantiation(omit_type_alias, ty_args, None, None)
        } else {
            let mut members = fx_indexmap_with_capacity(spreadable_props.len());
            for prop in spreadable_props {
                let spared_symbol = self.get_spread_symbol(prop, false);
                let name = self.symbol(prop).name;
                members.insert(name, spared_symbol);
            }
            let members = self.alloc(members);
            let index_infos = self.get_index_infos_of_ty(source);
            self.create_anonymous_ty_with_resolved(
                symbol,
                ty::ObjectFlags::OBJECT_REST_TYPE,
                members,
                self.empty_array(),
                self.empty_array(),
                index_infos,
                None,
            )
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
