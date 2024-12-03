use std::thread::panicking;
use std::usize;

use bolt_ts_span::ModuleID;
use thin_vec::thin_vec;

use super::ty::{self, Ty, TyKind};
use super::{F64Represent, TyChecker};
use crate::ast;
use crate::atoms::AtomId;
use crate::bind::{SymbolID, SymbolKind, SymbolName};
use crate::keyword;
use crate::ty::{AccessFlags, ElementFlags, TyMapper};

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_type_of_symbol(&mut self, module: ModuleID, id: SymbolID) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(module, id).get_ty() {
            return ty;
        }
        use crate::bind::SymbolKind::*;
        let ty = match &self.binder.get(module).symbols.get(id).kind {
            Err => return self.error_ty(),
            FunctionScopedVar => return self.undefined_ty(),
            BlockScopedVar => return self.undefined_ty(),
            Class { .. } => {
                let ty = self.get_type_of_class_decl(module, id);
                // TODO: delete
                if let Some(ty) = self.get_symbol_links(module, id).get_ty() {
                    return ty;
                }
                self.get_mut_symbol_links(module, id).set_ty(ty);
                // ---
                ty
            }
            Function { .. } | FnExpr { .. } => self.get_type_of_func_decl(module, id),
            Property { .. } => self.get_type_of_prop(module, id),
            Object { .. } => {
                let ty = self.get_type_of_object(module, id);
                // TODO: delete
                if let Some(ty) = self.get_symbol_links(module, id).get_ty() {
                    return ty;
                }
                self.get_mut_symbol_links(module, id).set_ty(ty);
                // ---
                ty
            }
            BlockContainer { .. } => return self.undefined_ty(),
            Interface { .. } => return self.undefined_ty(),
            Index { .. } => return self.undefined_ty(),
            TyAlias { .. } => return self.undefined_ty(),
            TyParam { .. } => return self.undefined_ty(),
        };
        ty
    }

    fn get_type_of_object(&mut self, module: ModuleID, symbol: SymbolID) -> &'cx Ty<'cx> {
        self.undefined_ty()
    }

    fn get_base_type_variable_of_class(
        &mut self,
        module: ModuleID,
        symbol: SymbolID,
    ) -> &'cx Ty<'cx> {
        let class_ty = self.get_declared_ty_of_symbol(module, symbol);
        let Some(i) = class_ty.kind.as_object_interface() else {
            unreachable!()
        };

        i.base_ctor_ty.unwrap()
    }

    fn get_type_of_prop(&mut self, module: ModuleID, symbol: SymbolID) -> &'cx Ty<'cx> {
        let decl = match self.binder.get(module).symbols.get(symbol).kind {
            crate::bind::SymbolKind::Property { decl, .. } => decl,
            _ => unreachable!(),
        };
        let ty = match self.p.get(decl.module()).nodes().get(decl) {
            ast::Node::ClassPropEle(prop) => prop
                .ty
                .map(|ty| self.get_ty_from_type_node(ty))
                .unwrap_or(self.undefined_ty()),
            ast::Node::PropSignature(prop) => prop
                .ty
                .map(|ty| self.get_ty_from_type_node(ty))
                .unwrap_or(self.undefined_ty()),
            _ => unreachable!(),
        };
        ty
    }

    fn get_type_of_class_decl(&mut self, module: ModuleID, symbol: SymbolID) -> &'cx Ty<'cx> {
        let base = self.get_base_type_variable_of_class(module, symbol);
        self.create_class_ty(ty::ClassTy { module, symbol })
    }

    fn get_type_of_func_decl(&mut self, module: ModuleID, symbol: SymbolID) -> &'cx Ty<'cx> {
        let params = self.get_sig_of_symbol(module, symbol);
        self.create_fn_ty(ty::FnTy {
            params,
            ret: self.undefined_ty(),
            module,
            symbol,
        })
    }

    fn get_sig_of_symbol(&mut self, module: ModuleID, id: SymbolID) -> &'cx [&'cx Ty<'cx>] {
        use crate::bind::SymbolKind::*;
        let decls = match &self.binder.get(module).symbols.get(id).kind {
            Err => todo!(),
            Function { decls, .. } => decls.clone(),
            FnExpr { decl } => thin_vec![*decl],
            FunctionScopedVar => todo!(),
            BlockScopedVar => todo!(),
            Object { .. } => todo!(),
            Property { .. } => todo!(),
            Class { .. } => todo!(),
            BlockContainer { .. } => todo!(),
            Interface { .. } => todo!(),
            Index { .. } => todo!(),
            TyAlias { .. } => todo!(),
            TyParam { .. } => todo!(),
        };

        for decl in decls {
            let params = match self.p.get(decl.module()).nodes().get(decl) {
                ast::Node::FnDecl(f) => f.params,
                ast::Node::ArrowFnExpr(f) => f.params,
                ast::Node::FnExpr(f) => f.params,
                ast::Node::ClassMethodEle(m) => m.params,
                _ => unreachable!("{:#?}", self.p.get(decl.module()).nodes().get(decl)),
            };
            let params = params
                .iter()
                .map(|param| {
                    param
                        .ty
                        .map(|ty| self.get_ty_from_type_node(ty))
                        .unwrap_or(self.any_ty())
                })
                .collect::<Vec<_>>();
            return self.alloc(params);
        }
        todo!()
    }

    pub(super) fn get_ty_from_type_node(&mut self, ty: &'cx ast::Ty<'cx>) -> &'cx Ty<'cx> {
        // TODO: cache
        use ast::TyKind::*;
        match ty.kind {
            Refer(refer) => {
                if refer.name.name == keyword::IDENT_BOOLEAN {
                    assert!(refer.args.is_none());
                    self.boolean_ty()
                } else if refer.name.name == keyword::IDENT_NUMBER {
                    assert!(refer.args.is_none());
                    self.number_ty()
                } else if refer.name.name == keyword::IDENT_STRING {
                    assert!(refer.args.is_none());
                    self.string_ty()
                } else if refer.name.name == keyword::IDENT_ANY {
                    assert!(refer.args.is_none());
                    self.any_ty()
                } else if refer.name.name == keyword::IDENT_VOID {
                    assert!(refer.args.is_none());
                    self.void_ty()
                } else {
                    self.get_ty_from_ty_reference(refer)
                }
            }
            Array(array) => self.get_ty_from_array_node(array),
            Tuple(tuple) => self.get_ty_from_tuple_node(tuple),
            Fn(_) => self.undefined_ty(),
            Lit(lit) => {
                if !self
                    .binder
                    .get(lit.id.module())
                    .final_res
                    .contains_key(&lit.id)
                {
                    unreachable!()
                }

                let module = lit.id.module();
                let symbol = self.binder.get(module).final_res[&lit.id];
                let SymbolKind::Object(object) = &self.binder.get(module).symbols.get(symbol).kind
                else {
                    unreachable!()
                };
                let members = self.alloc(object.members.clone());
                let declared_props = self.get_props_from_members(module, members);
                self.create_object_lit_ty(ty::ObjectLitTy {
                    members,
                    declared_props,
                    module: lit.id.module(),
                    symbol,
                })
            }
            ExprWithArg(expr) => self.get_ty_from_ty_reference(expr),
            NumLit(num) => self.get_number_literal_type(num.val),
            StringLit(s) => self.get_string_literal_type(s.val),
            Rest(rest) => self.get_ty_from_rest_ty_node(rest),
            IndexedAccess(node) => self.get_ty_from_indexed_access_node(node),
            Cond(node) => self.get_ty_from_cond_node(node),
        }
    }

    fn get_ty_from_rest_ty_node(&mut self, rest: &'cx ast::RestTy<'cx>) -> &'cx Ty<'cx> {
        self.get_ty_from_type_node(rest.ty)
    }

    fn get_prop_name_from_ty(&self, ty: &'cx Ty<'cx>) -> AtomId {
        if let Some(lit) = ty.kind.as_string_lit() {
            lit.val
        } else if let Some(lit) = ty.kind.as_number_lit() {
            todo!()
        } else {
            todo!()
        }
    }

    fn get_prop_name_from_index(&self, index_ty: &'cx Ty<'cx>) -> AtomId {
        self.get_prop_name_from_ty(index_ty)
    }

    // FIXME: use `get_prop_of_ty`
    fn get_prop_of_ty_temp(
        &mut self,
        object_ty: &'cx Ty<'cx>,
        prop: AtomId,
    ) -> Option<&'cx Ty<'cx>> {
        if let Some(object) = object_ty.kind.as_object() {
            // TODO: use `object.members`
            if let Some(tuple) = object.kind.as_tuple() {
                if self.atoms.eq_str(prop, "length") {
                    return Some(self.get_number_literal_type(tuple.tys.len() as f64));
                }
            }
        }
        None
    }

    fn get_prop_ty_from_index_ty(
        &mut self,
        object_ty: &'cx Ty<'cx>,
        index_ty: &'cx Ty<'cx>,
    ) -> &'cx Ty<'cx> {
        let prop_name = self.get_prop_name_from_index(index_ty);
        self.get_prop_of_ty_temp(object_ty, prop_name)
            .unwrap_or(self.undefined_ty())
    }

    pub(super) fn get_indexed_access_ty(
        &mut self,
        object_ty: &'cx Ty<'cx>,
        index_ty: &'cx Ty<'cx>,
        access_flags: Option<AccessFlags>,
    ) -> &'cx Ty<'cx> {
        let mut access_flags = access_flags.unwrap_or(AccessFlags::empty());
        if object_ty.kind.is_generic_object() {
            access_flags.insert(AccessFlags::PERSISTENT);
            let ty = self.alloc(ty::IndexedAccessTy {
                object_ty,
                index_ty,
                access_flags,
            });
            self.new_ty(ty::TyKind::IndexedAccess(ty))
        } else {
            self.get_prop_ty_from_index_ty(object_ty, index_ty)
        }
    }

    fn get_ty_from_indexed_access_node(
        &mut self,
        node: &'cx ast::IndexedAccessTy<'cx>,
    ) -> &'cx Ty<'cx> {
        let object_ty = self.get_ty_from_type_node(node.ty);
        let index_ty = self.get_ty_from_type_node(node.index_ty);
        self.get_indexed_access_ty(object_ty, index_ty, None)
    }

    pub(super) fn get_alias_symbol_for_ty_node(&self, node: ast::NodeID) -> Option<SymbolID> {
        let nodes = self.p.get(node.module()).nodes();
        assert!(nodes.get(node).is_ty_node());
        let parent_map = self.p.get(node.module()).parent_map();
        let mut host = parent_map.parent(node);
        while let Some(node_id) = host {
            let node = nodes.get(node);
            if node.is_paren_type_node() {
                host = parent_map.parent(node_id);
            } else {
                break;
            }
        }
        host.and_then(|node_id| nodes.get(node_id).is_type_decl().then(|| node_id))
            .and_then(|node_id| {
                let binder = self.binder.get(node_id.module());
                let symbol = binder.final_res.get(&node_id).copied().unwrap();
                assert!(binder.symbols.get(symbol).kind.is_ty_alias());
                Some(symbol)
            })
    }

    pub(super) fn get_local_ty_params_of_class_or_interface_or_type_alias(
        &mut self,
        module: ModuleID,
        symbol: SymbolID,
    ) -> Option<ty::Tys<'cx>> {
        use SymbolKind::*;
        match &self.binder.get(module).symbols.get(symbol).kind {
            TyAlias(alias) => {
                let mut res = vec![];
                let ty_params = self.get_effective_ty_param_decls(alias.decl);
                self.append_ty_params(&mut res, ty_params);
                Some(self.alloc(res))
            }
            _ => None,
        }
    }

    fn is_deferred_ty(&self, ty: &'cx Ty<'cx>, check_tuples: bool) -> bool {
        ty.kind.is_generic()
            || (check_tuples
                && ty
                    .kind
                    .as_object_tuple()
                    .map_or(false, |tup| tup.tys.iter().any(|ty| ty.kind.is_generic())))
    }

    fn get_permissive_instantiation(&self, ty: &'cx Ty<'cx>) -> &'cx Ty<'cx> {
        if ty.kind.is_param() {
            self.any_ty()
        } else {
            ty
        }
    }

    fn get_restrictive_ty_param(&self, ty: &'cx Ty<'cx>) -> &'cx Ty<'cx> {
        // TODO:
        ty
    }

    fn get_restrictive_instantiation(&self, ty: &'cx Ty<'cx>) -> &'cx Ty<'cx> {
        if ty.kind.is_param() {
            self.get_restrictive_ty_param(ty)
        } else {
            ty
        }
    }

    pub fn combine_ty_mappers(
        &self,
        m1: Option<&'cx TyMapper<'cx>>,
        m2: TyMapper<'cx>,
    ) -> TyMapper<'cx> {
        if let Some(m1) = m1 {
            TyMapper::make_composite(m1, self.alloc(m2))
        } else {
            m2
        }
    }

    pub(super) fn get_mapped_ty(&mut self, mapper: &TyMapper<'cx>, ty: &'cx Ty) -> &'cx Ty<'cx> {
        match mapper {
            TyMapper::Simple(_) => todo!(),
            TyMapper::Array(mapper) => {
                for (idx, source) in mapper.sources.iter().enumerate() {
                    assert!(source.kind.is_param());
                    if source.eq(&ty) {
                        return mapper.targets[idx];
                    }
                }
                ty
            }
            TyMapper::Deferred(_) => todo!(),
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
        mut mapper: Option<TyMapper<'cx>>,
    ) -> &'cx Ty<'cx> {
        let can_tail_recurse = |this: &mut Self,
                                new_ty: &'cx Ty<'cx>,
                                new_mapper: Option<TyMapper<'cx>>| {
            if let Some(cond) = new_ty.kind.as_cond_ty() {
                if let Some(new_mapper) = new_mapper {
                    let new_root = cond.root;
                    if let Some(out_ty_params) = new_root.outer_ty_params {
                        let ty_param_mapper = this.combine_ty_mappers(cond.mapper, new_mapper);
                        let ty_args = out_ty_params
                            .iter()
                            .map(|t| this.get_mapped_ty(&ty_param_mapper, t))
                            .collect::<Vec<_>>();
                        let new_root_mapper = TyMapper::create(out_ty_params, this.alloc(ty_args));
                        let use_new = if new_root.is_distributive {
                            let new_check_ty =
                                this.get_mapped_ty(&new_root_mapper, new_root.check_ty);
                            if new_check_ty == new_root.check_ty || !new_check_ty.kind.is_union() {
                                true
                            } else {
                                false
                            }
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
            if tailed > 1024 {
                panic!()
            }
            let check_ty = self.instantiate_ty(root.check_ty, mapper.as_ref());
            let extends_ty = self.instantiate_ty(root.extends_ty, mapper.as_ref());

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
                    if let Some(_) = false_ty.kind.as_cond_ty() {
                        if let Some((new_root, new_root_mapper)) =
                            can_tail_recurse(self, false_ty, mapper)
                        {
                            root = new_root;
                            mapper = Some(new_root_mapper);
                            tailed += 1;
                            continue;
                        }
                    }
                    break self.instantiate_ty(false_ty, mapper.as_ref());
                }

                if inferred_extends_ty.kind.is_any_or_unknown()
                    || self.is_type_assignable_to(
                        self.get_restrictive_instantiation(effective_check_ty),
                        self.get_restrictive_instantiation(inferred_extends_ty),
                    )
                {
                    let true_ty = self.get_ty_from_type_node(root.node.true_ty);
                    break self.instantiate_ty(true_ty, mapper.as_ref());
                }
            }

            let check_ty = self.instantiate_ty(root.check_ty, mapper.as_ref());
            let extends_ty = self.instantiate_ty(root.extends_ty, mapper.as_ref());
            let cond_ty = self.alloc(ty::CondTy {
                root,
                check_ty,
                extends_ty,
                mapper: if let Some(mapper) = mapper {
                    Some(self.alloc(mapper))
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
            self.get_local_ty_params_of_class_or_interface_or_type_alias(node.id.module(), symbol)
        });
        let all_outer_ty_params = self.get_outer_ty_params(node.id);
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
        self.create_array_ty(ty::ArrayTy { ty: ele_ty })
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

    fn create_normalized_tuple_ty(
        &mut self,
        elem_tys: &[&'cx Ty<'cx>],
        elem_flags: &[ElementFlags],
    ) -> &'cx ty::TyReference<'cx> {
        let mut expanded_tys = vec![];
        let mut expanded_flags = vec![];
        let mut last_required_index = usize::MAX;
        let mut first_rest_index = usize::MAX;
        let mut last_optional_or_rest_index = usize::MAX;

        let mut add_ele = |ty: &'cx Ty<'cx>, flags: ElementFlags| {
            if flags.contains(ElementFlags::REQUIRED) {
                last_required_index = expanded_tys.len();
            }
            if flags.contains(ElementFlags::REST) && first_rest_index == usize::MAX {
                first_rest_index = expanded_tys.len()
            }
            if flags.contains(ElementFlags::OPTIONAL | ElementFlags::REST) {
                last_optional_or_rest_index = expanded_tys.len();
            }
            if flags.contains(ElementFlags::OPTIONAL) {
                todo!()
            } else {
                expanded_tys.push(ty);
            }
            expanded_flags.push(flags);
        };

        for (i, ele) in elem_tys.iter().enumerate() {
            let flag = elem_flags[i];
            if flag.contains(ElementFlags::VARIADIC) {
                if ele.kind.is_any() {
                    todo!()
                } else if ele.kind.is_instantiable_non_primitive() {
                    add_ele(ele, ElementFlags::VARIADIC);
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

        self.alloc(ty::TyReference {
            ty_args: self.alloc(expanded_tys)
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
        let refer = self.create_normalized_tuple_ty(&tys, &flags);
        self.create_tuple_ty(ty::TupleTy {
            tys: self.alloc(tys),
            element_flags: self.alloc(flags),
            combined_flags,
            refer,
        })
    }

    pub(super) fn get_number_literal_type(&mut self, val: f64) -> &'cx Ty<'cx> {
        let key = F64Represent::new(val);
        if let Some(id) = self.num_lit_tys.get(&key) {
            self.tys[id]
        } else {
            let kind = TyKind::NumberLit(self.alloc(ty::NumberLitTy { val }));
            let ty = self.new_ty(kind);
            self.num_lit_tys.insert(key, ty.id);
            ty
        }
    }

    pub(super) fn get_string_literal_type(&mut self, val: AtomId) -> &'cx Ty<'cx> {
        if let Some(id) = self.string_lit_tys.get(&val) {
            self.tys[id]
        } else {
            let kind = TyKind::StringLit(self.alloc(ty::StringLitTy { val }));
            let ty = self.new_ty(kind);
            self.string_lit_tys.insert(val, ty.id);
            ty
        }
    }

    pub(super) fn get_global_type(&mut self, name: SymbolName) -> &'cx Ty<'cx> {
        let Some((m, s)) = self.global_symbols.get(name) else {
            unreachable!()
        };
        self.get_declared_ty_of_symbol(m, s)
    }
}
