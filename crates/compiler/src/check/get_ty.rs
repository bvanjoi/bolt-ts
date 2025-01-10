use bolt_ts_atom::AtomId;

use super::symbol_links::SymbolLinks;
use super::ty::{self, Ty, TyKind};
use super::{CheckMode, F64Represent, InferenceContextId, PropName, TyChecker};
use crate::ast;
use crate::bind::{SymbolFlags, SymbolID, SymbolName};
use crate::keyword;
use crate::ty::{AccessFlags, CheckFlags, ElementFlags, TupleShape, TyMapper, TypeFlags};

impl<'cx> TyChecker<'cx> {
    pub(crate) fn get_type_of_symbol(&mut self, id: SymbolID) -> &'cx Ty<'cx> {
        if let Some(t) = self.binder.get_transient(id) {
            if let Some(ty) = t.links.get_ty() {
                return ty;
            }
        };

        let check_flags = self.check_flags(id);
        let symbol = self.binder.symbol(id);

        let ty = if check_flags.intersects(CheckFlags::INSTANTIATED) {
            self.get_type_of_instantiated_symbol(id)
        } else if symbol.flags.intersects(SymbolFlags::CLASS) {
            let ty = self.get_type_of_class_decl(id);
            // TODO: delete
            if let Some(ty) = self.get_symbol_links(id).get_ty() {
                return ty;
            }
            self.get_mut_symbol_links(id).set_ty(ty);
            // ---
            ty
        } else if symbol.flags.intersects(SymbolFlags::FUNCTION) {
            self.get_type_of_func_decl(id)
        } else if symbol.is_variable() || symbol.flags.intersects(SymbolFlags::PROPERTY) {
            self.get_type_of_var_like(id)
        } else if symbol.flags == SymbolFlags::OBJECT_LITERAL {
            let ty = self.get_type_of_object(id);
            // TODO: delete
            if let Some(ty) = self.get_symbol_links(id).get_ty() {
                return ty;
            }
            self.get_mut_symbol_links(id).set_ty(ty);
            // ---
            ty
        } else {
            self.undefined_ty()
        };

        self.resolve_structured_type_members(ty);
        ty
    }

    fn get_type_of_instantiated_symbol(&mut self, symbol: SymbolID) -> &'cx Ty<'cx> {
        let links = self.get_symbol_links(symbol);
        if let Some(ty) = links.get_ty() {
            return ty;
        }

        let target = links.get_target().unwrap();
        let mapper = links.get_ty_mapper();
        let ty = self.get_type_of_symbol(target);
        let ty = self.instantiate_ty(ty, mapper);
        self.get_mut_symbol_links(symbol).set_ty(ty);
        ty
    }

    fn get_type_of_object(&mut self, symbol: SymbolID) -> &'cx Ty<'cx> {
        self.undefined_ty()
    }

    fn get_base_type_variable_of_class(&mut self, symbol: SymbolID) -> Option<&'cx Ty<'cx>> {
        let class_ty = self.get_declared_ty_of_symbol(symbol);
        self.ty_structured_members[&class_ty.id].base_ctor_ty
    }

    // fn get_intersection_ty(&mut self, tys: &'cx [&'cx Ty<'cx>]) -> &'cx Ty<'cx> {

    // }

    fn get_type_of_class_decl(&mut self, symbol: SymbolID) -> &'cx Ty<'cx> {
        let ty = self.create_anonymous_ty(ty::AnonymousTy {
            symbol,
            target: None,
            mapper: None,
        });
        if let Some(base) = self.get_base_type_variable_of_class(symbol) {
            // TODO: get_intersection_ty
            base
        } else {
            ty
        };
        ty
    }

    fn get_type_of_func_decl(&mut self, symbol: SymbolID) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(symbol).get_ty() {
            return ty;
        }
        let ty = self.create_anonymous_ty(ty::AnonymousTy {
            symbol,
            target: None,
            mapper: None,
        });
        self.get_mut_symbol_links(symbol).set_ty(ty);
        ty
    }

    pub(crate) fn get_ty_from_inference(
        &mut self,
        inference: InferenceContextId,
        idx: usize,
    ) -> Option<&'cx Ty<'cx>> {
        let inference = &self.inferences[inference.as_usize()].inferences[idx];
        if let Some(tys) = &inference.candidates {
            Some(self.create_union_type(tys.to_vec(), ty::UnionReduction::Subtype))
        } else if let Some(tys) = &inference.contra_candidates {
            todo!("intersection")
        } else {
            None
        }
    }

    pub(crate) fn get_ty_from_type_node(&mut self, ty: &ast::Ty<'cx>) -> &'cx Ty<'cx> {
        // TODO: cache
        use ast::TyKind::*;
        match ty.kind {
            Refer(refer) => self.get_ty_from_ty_reference(refer),
            Array(array) => self.get_ty_from_array_node(array),
            Tuple(tuple) => self.get_ty_from_tuple_node(tuple),
            Fn(f) => self.get_ty_from_fn_node(f),
            ObjectLit(lit) => {
                let symbol = self.binder.final_res(lit.id);
                let object = &self.binder.symbol(symbol).expect_object();
                let members = self.alloc(object.members.clone());
                let declared_props = self.get_props_from_members(members);
                self.create_object_lit_ty(ty::ObjectLitTy {
                    members,
                    declared_props,
                    symbol,
                })
            }
            NumLit(num) => self.get_number_literal_type(num.val),
            StringLit(s) => self.get_string_literal_type(s.val),
            Rest(rest) => self.get_ty_from_rest_ty_node(rest),
            IndexedAccess(node) => self.get_ty_from_indexed_access_node(node),
            Cond(node) => self.get_ty_from_cond_node(node),
            Union(node) => self.get_ty_from_union_ty_node(node),
            Typeof(node) => self.get_ty_from_typeof_node(node),
            Intersection(_) => self.undefined_ty(),
            BooleanLit(_) => todo!(),
            NullLit(_) => todo!(),
            Mapped(n) => self.undefined_ty(),
            TyOp(ty_op) => self.undefined_ty(),
        }
    }

    fn get_ty_from_fn_node(&mut self, f: &'cx ast::FnTy<'cx>) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_node_links(f.id).get_resolved_ty() {
            return ty;
        }

        let ty = self.create_anonymous_ty(ty::AnonymousTy {
            symbol: self.binder.final_res(f.id),
            target: None,
            mapper: None,
        });
        self.get_mut_node_links(f.id).set_resolved_ty(ty);
        ty
    }

    fn get_ty_from_typeof_node(&mut self, node: &'cx ast::TypeofTy<'cx>) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_node_links(node.id).get_resolved_ty() {
            return ty;
        }
        let ty = match node.name.kind {
            ast::EntityNameKind::Ident(ident) => self.check_ident(ident),
            ast::EntityNameKind::Qualified(_) => {
                // TODO: fix
                self.undefined_ty()
            }
        };
        // TODO: ty args
        self.get_mut_node_links(node.id).set_resolved_ty(ty);
        ty
    }

    fn get_ty_from_union_ty_node(&mut self, node: &'cx ast::UnionTy<'cx>) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_node_links(node.id).get_resolved_ty() {
            return ty;
        }
        // let alias_symbol = self.get_alias_symbol_for_ty_node(node.id);
        let tys = node
            .tys
            .iter()
            .map(|ty| self.get_ty_from_type_node(ty))
            .collect::<Vec<_>>();
        let ty = self.create_union_type(tys, ty::UnionReduction::Lit);
        self.get_mut_node_links(node.id).set_resolved_ty(ty);
        ty
    }

    fn get_ty_from_rest_ty_node(&mut self, rest: &'cx ast::RestTy<'cx>) -> &'cx Ty<'cx> {
        self.get_ty_from_type_node(rest.ty)
    }

    fn get_prop_name_from_ty(&self, ty: &'cx Ty<'cx>) -> PropName {
        if let Some(lit) = ty.kind.as_string_lit() {
            PropName::String(lit.val)
        } else if let Some(lit) = ty.kind.as_number_lit() {
            PropName::Num(lit.val)
        } else {
            unreachable!()
        }
    }

    fn get_prop_name_from_index(&self, index_ty: &'cx Ty<'cx>) -> PropName {
        self.get_prop_name_from_ty(index_ty)
    }

    fn get_prop_ty_for_index_ty(
        &mut self,
        origin_object_ty: &'cx Ty<'cx>,
        object_ty: &'cx Ty<'cx>,
        index_ty: &'cx Ty<'cx>,
    ) -> &'cx Ty<'cx> {
        let prop_name = self.get_prop_name_from_index(index_ty);
        let symbol_name = match prop_name {
            PropName::String(atom_id) => SymbolName::Ele(atom_id),
            PropName::Num(num) => SymbolName::EleNum(num.into()),
        };
        let symbol = self.get_prop_of_ty(object_ty, symbol_name);
        if let Some(symbol) = symbol {
            return self.get_type_of_symbol(symbol);
        }

        if !index_ty.kind.is_nullable()
            && self.is_type_assignable_to_kind(
                index_ty,
                |t| t.kind.is_string_like() || t.kind.is_number_like(),
                TypeFlags::STRING_LIKE | TypeFlags::NUMBER_LIKE | TypeFlags::ES_SYMBOL_LIKE,
                false,
            )
        {
            if object_ty.kind.is_any() {
                return object_ty;
            }
            let index_info = self
                .get_applicable_index_info(object_ty, index_ty)
                .or_else(|| self.get_index_info_of_ty(object_ty, self.string_ty()));
            if let Some(index_info) = index_info {
                return index_info.val_ty;
            }
        }

        self.undefined_ty()
    }

    pub(super) fn get_indexed_access_ty(
        &mut self,
        object_ty: &'cx Ty<'cx>,
        index_ty: &'cx Ty<'cx>,
        access_flags: Option<AccessFlags>,
        access_node: Option<ast::NodeID>,
    ) -> &'cx Ty<'cx> {
        let mut access_flags = access_flags.unwrap_or(AccessFlags::empty());
        let is_generic_index = if let Some(access_node) = access_node {
            if !self.p.node(access_node).is_indexed_access_ty() {
                object_ty.kind.is_generic_tuple_type()
            } else {
                object_ty.kind.is_generic_object()
            }
        } else {
            object_ty.kind.is_generic_object()
        };

        if is_generic_index {
            access_flags.insert(AccessFlags::PERSISTENT);
            let ty = self.alloc(ty::IndexedAccessTy {
                object_ty,
                index_ty,
                access_flags,
            });
            return self.new_ty(ty::TyKind::IndexedAccess(ty));
        };

        let apparent_object_ty = self.get_reduced_apparent_ty(object_ty);
        self.get_prop_ty_for_index_ty(object_ty, apparent_object_ty, index_ty)
    }

    fn get_ty_from_indexed_access_node(
        &mut self,
        node: &'cx ast::IndexedAccessTy<'cx>,
    ) -> &'cx Ty<'cx> {
        let object_ty = self.get_ty_from_type_node(node.ty);
        let index_ty = self.get_ty_from_type_node(node.index_ty);
        self.get_indexed_access_ty(object_ty, index_ty, None, None)
    }

    pub(super) fn get_alias_symbol_for_ty_node(&self, node: ast::NodeID) -> Option<SymbolID> {
        assert!(self.p.node(node).is_ty());
        let mut host = self.p.parent(node);
        while let Some(node_id) = host {
            let node = self.p.node(node);
            if node.is_paren_type_node() {
                host = self.p.parent(node_id);
            } else {
                break;
            }
        }
        host.and_then(|node_id| self.p.node(node_id).is_type_decl().then_some(node_id))
            .map(|node_id| {
                let symbol = self.binder.final_res(node_id);
                assert!(self.binder.symbol(symbol).flags == SymbolFlags::TYPE_ALIAS);
                symbol
            })
    }

    pub(super) fn get_local_ty_params_of_class_or_interface_or_type_alias(
        &mut self,
        symbol: SymbolID,
    ) -> Option<ty::Tys<'cx>> {
        let s = self.binder.symbol(symbol);
        let decl = if s.flags == SymbolFlags::TYPE_ALIAS {
            let alias = s.expect_ty_alias();
            alias.decl
        } else if s.flags.intersects(SymbolFlags::INTERFACE) {
            let i = s.expect_interface();
            i.decl
        } else {
            return None;
        };
        let mut res = vec![];
        let ty_params = self.get_effective_ty_param_decls(decl);
        self.append_ty_params(&mut res, ty_params);
        Some(self.alloc(res))
    }

    fn is_deferred_ty(&self, ty: &'cx Ty<'cx>, check_tuples: bool) -> bool {
        ty.kind.is_generic()
            || (check_tuples
                && ty
                    .kind
                    .as_object_tuple()
                    .map_or(false, |tup| tup.tys.iter().any(|ty| ty.kind.is_generic())))
    }

    pub(super) fn get_restrictive_ty_param(&self, ty: &'cx Ty<'cx>) -> &'cx Ty<'cx> {
        // TODO:
        ty
    }

    pub fn combine_ty_mappers(
        &self,
        m1: Option<&'cx TyMapper<'cx>>,
        m2: &'cx TyMapper<'cx>,
    ) -> &'cx TyMapper<'cx> {
        if let Some(m1) = m1 {
            self.alloc(TyMapper::make_composite(m1, m2))
        } else {
            m2
        }
    }

    pub fn create_ty_mapper_with_optional_target(
        &self,
        sources: ty::Tys<'cx>,
        targets: Option<ty::Tys<'cx>>,
    ) -> &'cx TyMapper<'cx> {
        if let Some(targets) = targets {
            self.alloc(TyMapper::create(sources, targets))
        } else if sources.len() == 1 {
            let mapper = ty::SimpleTyMapper {
                source: sources[0],
                target: self.any_ty(),
            };
            self.alloc(TyMapper::Simple(mapper))
        } else {
            let mapper = ty::ArrayTyMapper {
                sources,
                targets: None,
            };
            self.alloc(TyMapper::Array(mapper))
        }
    }

    pub fn create_ty_eraser(&self, sources: ty::Tys<'cx>) -> &'cx TyMapper<'cx> {
        self.create_ty_mapper_with_optional_target(sources, None)
    }

    pub(super) fn get_mapped_ty(&mut self, mapper: &TyMapper<'cx>, ty: &'cx Ty) -> &'cx Ty<'cx> {
        match mapper {
            TyMapper::Simple(mapper) => {
                if ty == mapper.source {
                    mapper.target
                } else {
                    ty
                }
            }
            TyMapper::Array(mapper) => {
                for (idx, source) in mapper.sources.iter().enumerate() {
                    assert!(source.kind.is_param());
                    if source.eq(&ty) {
                        if let Some(targets) = &mapper.targets {
                            return targets[idx];
                        } else {
                            return self.any_ty();
                        }
                    }
                }
                ty
            }
            TyMapper::Fn(_) => todo!(),
            TyMapper::Composite(mapper) => {
                let t1 = self.get_mapped_ty(mapper.mapper1, ty);
                if t1 != ty {
                    self.instantiate_ty_with_alias(t1, mapper.mapper2)
                } else {
                    self.get_mapped_ty(mapper.mapper2, t1)
                }
            }
            TyMapper::Merged(_) => todo!(),
        }
    }

    pub(super) fn get_cond_ty(
        &mut self,
        mut root: &'cx ty::CondTyRoot<'cx>,
        mut mapper: Option<&'cx TyMapper<'cx>>,
    ) -> &'cx Ty<'cx> {
        let can_tail_recurse =
            |this: &mut Self, new_ty: &'cx Ty<'cx>, new_mapper: Option<&'cx TyMapper<'cx>>| {
                if let Some(cond) = new_ty.kind.as_cond_ty() {
                    if let Some(new_mapper) = new_mapper {
                        let new_root = cond.root;
                        if let Some(out_ty_params) = new_root.outer_ty_params {
                            let ty_param_mapper = this.combine_ty_mappers(cond.mapper, new_mapper);
                            let ty_args = out_ty_params
                                .iter()
                                .map(|t| this.get_mapped_ty(ty_param_mapper, t))
                                .collect::<Vec<_>>();
                            let new_root_mapper =
                                this.alloc(TyMapper::create(out_ty_params, this.alloc(ty_args)));
                            let use_new = if new_root.is_distributive {
                                let new_check_ty =
                                    this.get_mapped_ty(new_root_mapper, new_root.check_ty);
                                new_check_ty == new_root.check_ty || !new_check_ty.kind.is_union()
                            } else {
                                true
                            };
                            if use_new {
                                return Some((new_root, new_root_mapper));
                            }
                        }
                    }
                }
                None
            };
        let mut tailed = 0;
        loop {
            if tailed > 100 {
                panic!()
            }
            let check_ty = self.instantiate_ty(root.check_ty, mapper);
            let extends_ty = self.instantiate_ty(root.extends_ty, mapper);

            let check_tuples = false;
            let effective_check_ty = check_ty;
            let check_ty_deferred = self.is_deferred_ty(effective_check_ty, check_tuples);
            let inferred_extends_ty = extends_ty;

            if !check_ty_deferred && !self.is_deferred_ty(inferred_extends_ty, check_tuples) {
                if !inferred_extends_ty.kind.is_any_or_unknown()
                    && (effective_check_ty.kind.is_any()
                        || !self.is_type_assignable_to(
                            self.get_permissive_instantiation(effective_check_ty),
                            self.get_permissive_instantiation(inferred_extends_ty),
                        ))
                {
                    let false_ty = self.get_ty_from_type_node(root.node.false_ty);
                    if false_ty.kind.as_cond_ty().is_some() {
                        if let Some((new_root, new_root_mapper)) =
                            can_tail_recurse(self, false_ty, mapper)
                        {
                            root = new_root;
                            mapper = Some(new_root_mapper);
                            tailed += 1;
                            continue;
                        }
                    }
                    break self.instantiate_ty(false_ty, mapper);
                }

                if inferred_extends_ty.kind.is_any_or_unknown()
                    || self.is_type_assignable_to(
                        self.get_restrictive_instantiation(effective_check_ty),
                        self.get_restrictive_instantiation(inferred_extends_ty),
                    )
                {
                    let true_ty = self.get_ty_from_type_node(root.node.true_ty);
                    break self.instantiate_ty(true_ty, mapper);
                }
            }

            let check_ty = self.instantiate_ty(root.check_ty, mapper);
            let extends_ty = self.instantiate_ty(root.extends_ty, mapper);
            let cond_ty = self.alloc(ty::CondTy {
                root,
                check_ty,
                extends_ty,
                mapper: if let Some(mapper) = mapper {
                    Some(mapper)
                } else {
                    None
                },
            });
            break self.new_ty(ty::TyKind::Cond(cond_ty));
        }
    }

    fn get_ty_from_cond_node(&mut self, node: &'cx ast::CondTy<'cx>) -> &'cx Ty<'cx> {
        let check_ty = self.get_ty_from_type_node(node.check_ty);
        let alias_symbol = self.get_alias_symbol_for_ty_node(node.id);
        let alias_ty_args = alias_symbol.and_then(|symbol| {
            self.get_local_ty_params_of_class_or_interface_or_type_alias(symbol)
        });
        let all_outer_ty_params = self.get_outer_ty_params(node.id, true);
        let outer_ty_params: Option<ty::Tys<'cx>> = if alias_ty_args.is_some() {
            if let Some(all_outer_ty_params) = all_outer_ty_params {
                Some(self.alloc(all_outer_ty_params))
            } else {
                None
            }
        } else {
            todo!()
        };
        let extends_ty = self.get_ty_from_type_node(node.extends_ty);
        let root = self.alloc(ty::CondTyRoot {
            check_ty,
            extends_ty,
            outer_ty_params,
            node,
            is_distributive: check_ty.kind.is_param(),
        });
        self.get_cond_ty(root, None)
    }

    fn get_ty_from_array_node(&mut self, node: &'cx ast::ArrayTy<'cx>) -> &'cx Ty<'cx> {
        let ele_ty = self.get_ty_from_type_node(node.ele);
        let refer = self.global_array_ty().kind.expect_object_reference();
        self.create_reference_ty(ty::ReferenceTy {
            target: refer.target,
            resolved_ty_args: self.alloc(vec![ele_ty]),
        })
    }

    fn get_array_ele_ty_node(node: &'cx ast::Ty<'cx>) -> Option<&'cx ast::Ty<'cx>> {
        use ast::TyKind::*;
        match node.kind {
            Tuple(tup) if tup.tys.len() == 1 => {
                let node = tup.tys[0];
                if let Rest(rest) = node.kind {
                    Self::get_array_ele_ty_node(rest.ty)
                } else {
                    None
                }
            }
            Array(arr) => Some(arr.ele),
            _ => None,
        }
    }

    fn get_rest_ty_ele_flags(node: &'cx ast::RestTy<'cx>) -> ElementFlags {
        if Self::get_array_ele_ty_node(node.ty).is_some() {
            ElementFlags::REST
        } else {
            ElementFlags::VARIADIC
        }
    }

    fn create_tuple_shape(&mut self, elem_tys: &[&'cx Ty]) -> &'cx TupleShape<'cx> {
        let key = elem_tys.len() as u32;
        if let Some(shape) = self.tuple_shapes.get(&key) {
            return shape;
        };

        let length_symbol_name = SymbolName::Ele(keyword::IDENT_LENGTH);
        let ty = self.get_number_literal_type(elem_tys.len() as f64);
        let length_symbol = self.binder.create_transient_symbol(
            length_symbol_name,
            SymbolFlags::PROPERTY,
            None,
            SymbolLinks::default().with_ty(ty),
        );

        let element_symbols = elem_tys.iter().enumerate().map(|(idx, _)| {
            let name = SymbolName::EleNum(idx.into());
            let index_ty = self.get_number_literal_type(idx as f64);
            let symbol = self.binder.create_transient_symbol(
                name,
                SymbolFlags::PROPERTY,
                None,
                SymbolLinks::default().with_ty(index_ty),
            );
            symbol
        });

        let declared_props = std::iter::once(length_symbol)
            .chain(element_symbols)
            .collect::<Vec<_>>();
        let declared_props = self.alloc(declared_props);
        let shape = self.alloc(ty::TupleShape {
            declared_props,
            fixed_length: declared_props.len(),
        });
        let prev = self.tuple_shapes.insert(key, shape);
        assert!(prev.is_none());
        shape
    }

    pub(super) fn create_normalized_tuple_ty(
        &mut self,
        elem_tys: &'cx [&'cx Ty<'cx>],
        elem_flags: &'cx [ElementFlags],
        combined_flags: ElementFlags,
    ) -> &'cx ty::Ty<'cx> {
        // ====
        if !combined_flags.intersects(ElementFlags::NON_REQUIRED) {
            let shape = self.create_tuple_shape(elem_tys);
            return self.create_tuple_ty(ty::TupleTy {
                combined_flags,
                element_flags: elem_flags,
                tys: elem_tys,
                shape,
            });
        }
        let mut expanded_tys = vec![];
        let mut expanded_flags = vec![];
        let mut last_required_index = usize::MAX;
        let mut first_rest_index = usize::MAX;
        let mut last_optional_or_rest_index = usize::MAX;

        let mut add_ele = |ty: &'cx Ty<'cx>, flags: ElementFlags| {
            if flags.intersects(ElementFlags::REQUIRED) {
                last_required_index = expanded_tys.len();
            }
            if flags.intersects(ElementFlags::REST) && first_rest_index == usize::MAX {
                first_rest_index = expanded_tys.len()
            }
            if flags.intersects(ElementFlags::OPTIONAL | ElementFlags::REST) {
                last_optional_or_rest_index = expanded_tys.len();
            }
            if flags.intersects(ElementFlags::OPTIONAL) {
                todo!()
            } else {
                expanded_tys.push(ty);
            }
            expanded_flags.push(flags);
        };

        for (i, ele) in elem_tys.iter().enumerate() {
            let flag = elem_flags[i];
            if flag.intersects(ElementFlags::VARIADIC) {
                if ele.kind.is_any() {
                    todo!()
                } else if ele.kind.is_instantiable_non_primitive() {
                    add_ele(ele, ElementFlags::VARIADIC);
                } else if ele.kind.is_tuple() {
                    let tuple = ele
                        .kind
                        .expect_object_reference()
                        .target
                        .kind
                        .expect_object_tuple();
                    for i in 0..tuple.tys.len() {
                        add_ele(tuple.tys[i], tuple.element_flags[i]);
                    }
                } else if let Some(tuple) = ele.kind.as_object_tuple() {
                    for i in 0..tuple.tys.len() {
                        add_ele(tuple.tys[i], tuple.element_flags[i]);
                    }
                } else {
                    todo!()
                }
            } else {
                add_ele(ele, flag)
            }
        }

        let expand_tys = self.alloc(expanded_tys);
        let combined_flags = expanded_flags
            .iter()
            .fold(ElementFlags::empty(), |flags, current| flags | *current);
        let shape = self.create_tuple_shape(expand_tys);
        let tuple = self.create_tuple_ty(ty::TupleTy {
            tys: expand_tys,
            element_flags: self.alloc(expanded_flags),
            combined_flags,
            shape,
        });
        self.create_reference_ty(ty::ReferenceTy {
            target: tuple,
            resolved_ty_args: expand_tys,
        })
    }

    fn get_ty_from_tuple_node(&mut self, node: &'cx ast::TupleTy<'cx>) -> &'cx Ty<'cx> {
        let tys = node
            .tys
            .iter()
            .map(|ty| self.get_ty_from_type_node(ty))
            .collect::<Vec<_>>();

        let get_tuple_ele_flags = |node: &'cx ast::Ty<'cx>| {
            use ast::TyKind::*;
            match node.kind {
                Rest(rest) => Self::get_rest_ty_ele_flags(rest),
                _ => ElementFlags::REQUIRED,
            }
        };
        let flags: Vec<_> = node.tys.iter().map(|ty| get_tuple_ele_flags(ty)).collect();
        let combined_flags = flags
            .iter()
            .fold(ElementFlags::empty(), |flags, current| flags | *current);
        self.create_normalized_tuple_ty(self.alloc(tys), self.alloc(flags), combined_flags)
    }

    pub(super) fn get_number_literal_type(&mut self, val: f64) -> &'cx Ty<'cx> {
        let key = F64Represent::new(val);
        if let Some(id) = self.num_lit_tys.get(&key) {
            self.tys[id.as_usize()]
        } else {
            let kind = TyKind::NumberLit(self.alloc(ty::NumberLitTy { val }));
            let ty = self.new_ty(kind);
            self.num_lit_tys.insert(key, ty.id);
            ty
        }
    }

    pub(super) fn get_string_literal_type(&mut self, val: AtomId) -> &'cx Ty<'cx> {
        if let Some(id) = self.string_lit_tys.get(&val) {
            self.tys[id.as_usize()]
        } else {
            let kind = TyKind::StringLit(self.alloc(ty::StringLitTy { val }));
            let ty = self.new_ty(kind);
            self.string_lit_tys.insert(val, ty.id);
            ty
        }
    }

    pub(super) fn get_global_type(&mut self, name: SymbolName) -> &'cx Ty<'cx> {
        let Some(s) = self.global_symbols.get(name) else {
            unreachable!()
        };
        self.get_declared_ty_of_symbol(s)
    }

    fn get_ret_ty_of_ty_tag(&mut self, id: ast::NodeID) -> Option<&'cx Ty<'cx>> {
        self.get_sig_of_ty_tag(id).map(|_| unreachable!())
    }

    pub fn get_ret_ty_from_anno(&mut self, id: ast::NodeID) -> Option<&'cx ty::Ty<'cx>> {
        let n = self.p.node(id);
        if n.as_class_ctor().is_some() {
            let class = self.p.parent(id).unwrap();
            assert!(self.p.node(class).is_class_like());
            let symbol = self.get_symbol_of_decl(class);
            Some(self.get_declared_ty_of_symbol(symbol))
        } else if let Some(ty_node) = n.ret_ty() {
            Some(self.get_ty_from_type_node(ty_node))
        } else {
            self.get_ret_ty_of_ty_tag(id)
        }
    }

    pub fn get_ret_ty_from_body(&mut self, id: ast::NodeID) -> &'cx ty::Ty<'cx> {
        let n = self.p.node(id);
        let Some(body) = n.fn_body() else {
            return self.error_ty();
        };

        let flags = n.fn_flags();
        let ret_ty;
        if let ast::ArrowFnExprBody::Expr(expr) = body {
            let old = if let Some(check_mode) = self.check_mode {
                let old = self.check_mode;
                self.check_mode = Some(check_mode & !CheckMode::SKIP_GENERIC_FUNCTIONS);
                old
            } else {
                None
            };
            ret_ty = self.check_expr_with_cache(expr);
            self.check_mode = old;
        } else if let ast::ArrowFnExprBody::Block(body) = body {
            let Some(tys) = self.check_and_aggregate_ret_expr_tys(id, body) else {
                return self.undefined_ty();
            };
            if tys.is_empty() {
                if let Some(contextual_ret_ty) = self.get_contextual_ret_ty(id, None) {
                    ret_ty = contextual_ret_ty;
                } else {
                    ret_ty = self.void_ty();
                }
            } else {
                ret_ty = self.create_union_type(tys, ty::UnionReduction::Subtype)
            }
        } else {
            unreachable!()
        };
        ret_ty
    }

    pub(super) fn get_ty_params_for_mapper(&mut self, sig: &'cx ty::Sig<'cx>) -> ty::Tys<'cx> {
        let tys = sig
            .ty_params
            .unwrap_or_default()
            .iter()
            .map(|tp| {
                if let Some(mapper) = self.param_ty_mapper(tp) {
                    self.instantiate_ty(tp, Some(mapper))
                } else {
                    tp
                }
            })
            .collect::<Vec<_>>();
        self.alloc(tys)
    }

    pub(super) fn get_ty_of_expr(&mut self, expr: &'cx ast::Expr<'cx>) -> &'cx ty::Ty<'cx> {
        // TODO: quick check
        let old_check_mode = self.check_mode;
        let ty = self.check_expr(expr);
        self.check_mode = old_check_mode;
        ty
    }
}
