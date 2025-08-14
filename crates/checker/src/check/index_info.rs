use super::TyChecker;
use super::symbol_info::SymbolInfo;
use crate::ty::{self, TypeFlags};
use bolt_ts_binder::{Symbol, SymbolID, SymbolName};

use bolt_ts_ast as ast;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_index_symbol(&mut self, symbol: SymbolID) -> Option<SymbolID> {
        if self.binder.symbol(symbol).members.is_some() {
            let members = self.get_members_of_symbol(symbol);
            members.0.get(&SymbolName::Index).copied()
        } else {
            None
        }
    }

    pub(super) fn get_index_infos_of_symbol(&mut self, symbol: SymbolID) -> ty::IndexInfos<'cx> {
        self.get_index_symbol(symbol)
            .map(|index_symbol| self.get_index_infos_of_index_symbol(index_symbol))
            .unwrap_or_default()
    }

    pub(super) fn get_index_infos_of_index_symbol(
        &mut self,
        symbol: SymbolID,
    ) -> ty::IndexInfos<'cx> {
        let s = self.symbol(symbol);
        let Some(decls) = s.decls.clone() else {
            return self.empty_array();
        };
        // TODO: sibling_symbols;
        let mut index_infos = Vec::with_capacity(decls.len() * 2);
        let mut has_computed_number_property = false;
        let mut readonly_computed_number_property = true;
        let mut has_computed_symbol_property = false;
        let mut readonly_computed_symbol_property = true;
        let mut has_computed_string_property = false;
        let mut readonly_computed_string_property = true;
        let mut computed_property_symbols = Vec::with_capacity(decls.len());
        for decl in decls {
            let n = self.p.node(decl);
            if n.is_index_sig_decl() {
                let decl = n.expect_index_sig_decl();
                let val_ty = self.get_ty_from_type_node(decl.ty);
                index_infos.extend(decl.params.iter().map(|param| {
                    let Some(ty) = param.ty else { unreachable!() };
                    let key_ty = self.get_ty_from_type_node(ty);
                    let is_readonly = param
                        .modifiers
                        .is_some_and(|mods| mods.flags.contains(ast::ModifierKind::Readonly));
                    self.alloc(ty::IndexInfo {
                        key_ty,
                        val_ty,
                        symbol,
                        is_readonly,
                    })
                }));
            } else if self.has_late_bindable_index_signature(decl) {
                // TODO: is_binary_expression
                let name = self
                    .node_query(decl.module())
                    .get_name_of_decl(decl)
                    .unwrap();
                use bolt_ts_ast::DeclarationName;
                let key_ty = match name {
                    DeclarationName::Computed(n) => self.check_computed_prop_name(n),
                    // TODO: element_ty
                    _ => unreachable!(),
                };
                if self.find_index_info(&index_infos, key_ty).is_some() {
                    continue;
                }
                let n = self.p.node(decl);
                if self.is_type_assignable_to(key_ty, self.string_number_symbol_ty()) {
                    if self.is_type_assignable_to(key_ty, self.number_ty) {
                        has_computed_number_property = true;
                        if !n.has_effective_readonly_modifier() {
                            readonly_computed_number_property = false;
                        }
                    } else if self.is_type_assignable_to(key_ty, self.es_symbol_ty) {
                        has_computed_symbol_property = true;
                        if !n.has_effective_readonly_modifier() {
                            readonly_computed_symbol_property = false;
                        }
                    } else if self.is_type_assignable_to(key_ty, self.string_ty) {
                        has_computed_string_property = true;
                        if !n.has_effective_readonly_modifier() {
                            readonly_computed_string_property = false;
                        }
                    } else {
                        unreachable!()
                    }
                    let decl_s = self.get_symbol_of_decl(decl);
                    computed_property_symbols.push(decl_s);
                }
            } else {
                unreachable!()
            }
        }

        let all_property_symbols = computed_property_symbols;
        if has_computed_string_property
            && self.find_index_info(&index_infos, self.string_ty).is_none()
        {
            let info = self.get_object_lit_index_info(
                readonly_computed_string_property,
                0,
                &all_property_symbols,
                self.string_ty,
            );
            index_infos.push(info);
        }
        if has_computed_number_property
            && self.find_index_info(&index_infos, self.number_ty).is_none()
        {
            let info = self.get_object_lit_index_info(
                readonly_computed_number_property,
                0,
                &all_property_symbols,
                self.number_ty,
            );
            index_infos.push(info);
        }
        if has_computed_symbol_property
            && self
                .find_index_info(&index_infos, self.es_symbol_ty)
                .is_none()
        {
            let info = self.get_object_lit_index_info(
                readonly_computed_symbol_property,
                0,
                &all_property_symbols,
                self.es_symbol_ty,
            );
            index_infos.push(info);
        }
        self.alloc(index_infos)
    }

    fn get_object_lit_index_info(
        &mut self,
        is_readonly: bool,
        offset: usize,
        props: &[SymbolID],
        key_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::IndexInfo<'cx> {
        assert!(offset < props.len());
        let mut prop_tys = Vec::with_capacity(props.len());
        let mut components = Vec::with_capacity(props.len());
        for i in offset..prop_tys.len() {
            let p = props[i];
            if (key_ty == self.string_ty && !self.is_symbol_with_symbol_name(p))
                || (key_ty == self.number_ty && self.is_symbol_with_numeric_name(p))
                || (key_ty == self.es_symbol_ty && self.is_symbol_with_symbol_name(p))
            {
                let ty = self.get_type_of_symbol(p);
                prop_tys.push(ty);
                if self.is_symbol_with_computed_name(p) {
                    let decl = self
                        .symbol(p)
                        .decls
                        .as_ref()
                        .unwrap()
                        .first()
                        .copied()
                        .unwrap();
                    components.push(decl);
                }
            }
        }
        let union_ty = if prop_tys.is_empty() {
            self.undefined_ty
        } else {
            self.get_union_ty(&prop_tys, ty::UnionReduction::Subtype, false, None, None)
        };
        self.alloc(ty::IndexInfo {
            key_ty,
            val_ty: union_ty,
            symbol: Symbol::ERR,
            is_readonly,
        })
    }

    fn is_symbol_with_computed_name(&mut self, symbol: SymbolID) -> bool {
        let Some(first_decl) = self
            .symbol(symbol)
            .decls
            .as_ref()
            .and_then(|decls| decls.first())
            .copied()
        else {
            return false;
        };
        self.p
            .node(first_decl)
            .name()
            .is_some_and(|name| matches!(name, ast::DeclarationName::Computed(_)))
    }

    fn is_symbol_with_numeric_name(&mut self, symbol: SymbolID) -> bool {
        let s = self.symbol(symbol);
        if matches!(s.name, SymbolName::EleNum(_)) {
            return true;
        }
        let Some(first_decl) = s.decls.as_ref().and_then(|decls| decls.first()).copied() else {
            return false;
        };
        self.p
            .node(first_decl)
            .name()
            .is_some_and(|name| matches!(name, ast::DeclarationName::NumLit(_)))
    }

    fn is_symbol_with_symbol_name(&mut self, symbol: SymbolID) -> bool {
        let s = self.symbol(symbol);
        if matches!(s.name, SymbolName::ESSymbol { .. }) {
            return true;
        }
        let Some(first_decl) = s.decls.as_ref().and_then(|decls| decls.first()).copied() else {
            return false;
        };
        self.p.node(first_decl).name().is_some_and(|name| {
            if let ast::DeclarationName::Computed(n) = name {
                let ty = self.check_computed_prop_name(n);
                if self.is_type_assignable_to_kind(ty, TypeFlags::ES_SYMBOL, false) {
                    return true;
                }
            }
            false
        })
    }

    pub(super) fn get_index_infos_of_structured_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> ty::IndexInfos<'cx> {
        if ty.kind.is_structured() {
            self.resolve_structured_type_members(ty);
            self.index_infos_of_ty(ty)
        } else {
            self.empty_array()
        }
    }

    pub(super) fn find_index_info(
        &self,
        index_infos: &[&'cx ty::IndexInfo<'cx>],
        key_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::IndexInfo<'cx>> {
        index_infos
            .iter()
            .find(|info| info.key_ty == key_ty)
            .copied()
    }

    pub(super) fn get_index_info_of_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        key_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::IndexInfo<'cx>> {
        let index_infos = self.get_index_infos_of_ty(ty);
        self.find_index_info(index_infos, key_ty)
    }

    pub(super) fn get_index_infos_of_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> ty::IndexInfos<'cx> {
        let ty = self.get_reduced_apparent_ty(ty);
        self.get_index_infos_of_structured_ty(ty)
    }

    pub(super) fn find_applicable_index_info(
        &mut self,
        index_infos: &'cx [&'cx ty::IndexInfo<'cx>],
        key_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::IndexInfo<'cx>> {
        let mut string_index_info: Option<&ty::IndexInfo<'_>> = None;
        let mut applicable_info: Option<&ty::IndexInfo<'_>> = None;
        let mut applicable_infos: Option<Vec<&ty::IndexInfo<'_>>> = None;

        for info in index_infos {
            if info.key_ty == self.string_ty {
                string_index_info = Some(info)
            } else if self.is_applicable_index_ty(key_ty, info.key_ty) {
                if applicable_info.is_none() {
                    applicable_info = Some(info)
                } else if let Some(applicable_infos) = applicable_infos.as_mut() {
                    applicable_infos.push(info)
                } else {
                    applicable_infos = Some(vec![applicable_info.unwrap(), info])
                }
            }
        }

        if let Some(applicable_infos) = applicable_infos {
            // TODO: create index info
            applicable_info
        } else if let Some(applicable_info) = applicable_info {
            Some(applicable_info)
        } else if string_index_info.is_some() && self.is_applicable_index_ty(key_ty, self.string_ty)
        {
            string_index_info
        } else {
            None
        }
    }

    pub(super) fn get_applicable_index_info(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        key_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::IndexInfo<'cx>> {
        let index_infos = self.get_index_infos_of_ty(ty);
        self.find_applicable_index_info(index_infos, key_ty)
    }

    pub(super) fn get_applicable_index_for_name(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        name: SymbolName,
    ) -> Option<&'cx ty::IndexInfo<'cx>> {
        // TODO: is late name
        let key_ty = if let Some(name) = name.as_atom() {
            self.get_string_literal_type(name)
        } else if let Some(v) = name.as_numeric() {
            self.get_number_literal_type_from_number(v)
        } else {
            unreachable!()
        };
        self.get_applicable_index_info(ty, key_ty)
    }
}
