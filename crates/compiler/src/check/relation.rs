use bolt_ts_atom::AtomId;
use bolt_ts_span::Span;

use bolt_ts_utils::fx_hashset_with_capacity;
use rustc_hash::FxHashSet;

use super::{errors, SymbolLinks};
use crate::ast;
use crate::bind::{SymbolFlags, SymbolID, SymbolName};
use crate::ty::{self, CheckFlags, ObjectFlags};
use crate::ty::{ObjectShape, ObjectTy, ObjectTyKind, Ty, TyKind};

use super::{Ternary, TyChecker};

#[derive(Clone, Copy, Debug, PartialEq, Hash, Eq)]
pub(super) enum RelationKind {
    Subtype,
    StrictSubtype,
    Assignable,
    Comparable,
    Identity,
    Enum,
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub(super) struct SigCheckMode: u8 {
        const BIVARIANT_CALLBACK    = 1 << 0;
        const STRICT_CALLBACK       = 1 << 1;
        const IGNORE_RETURN_TYPES   = 1 << 2;
        const STRICT_ARITY          = 1 << 3;
        const STRICT_TOP_SIGNATURE  = 1 << 4;
        const CALLBACK              = Self::BIVARIANT_CALLBACK.bits() | Self::STRICT_CALLBACK.bits();
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn is_excess_property_check_target(&self, target: &'cx Ty<'cx>) -> bool {
        if let Some(_) = target.kind.as_object() {
            // TODO: exclude literal computed name
            true
        } else if let Some(union) = target.kind.as_union() {
            union
                .tys
                .iter()
                .any(|ty| self.is_excess_property_check_target(ty))
        } else {
            false
        }
    }

    pub(super) fn is_simple_type_related_to(
        &self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        if target.kind.is_any() || source.kind.is_never() {
            true
        } else if source.kind.is_number_like() && target.kind.is_number() {
            true
        } else if source.kind.is_string_like() && target.kind.is_string() {
            true
        } else if source.kind.is_boolean_like() && target.kind.is_boolean() {
            true
        } else if source.kind.is_undefined() && !target.kind.is_union_or_intersection()
            || (target.kind.is_undefined() || target.kind.is_void())
        {
            true
        } else if source.kind.is_object()
            && target == self.non_primitive_ty()
            && !(relation == RelationKind::StrictSubtype
                && self.is_empty_anonymous_object_ty(source)
                && !(source
                    .get_object_flags()
                    .intersects(ObjectFlags::FRESH_LITERAL)))
        {
            true
        } else if relation == RelationKind::Assignable || relation == RelationKind::Comparable {
            source.kind.is_any()
        } else {
            false
        }
    }

    pub(super) fn is_type_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
    ) -> Ternary {
        if source.id == target.id {
            assert!(std::ptr::eq(&source.kind, &target.kind));
            return Ternary::TRUE;
        }
        if relation != RelationKind::Identity {
            if (relation == RelationKind::Comparable
                && !target.kind.is_never()
                && self.is_simple_type_related_to(target, source, relation))
                || self.is_simple_type_related_to(source, target, relation)
            {
                return Ternary::TRUE;
            }
        }

        if source.kind.is_object() && target.kind.is_object() {
            // skip
        }

        if source.kind.is_structured_or_instantiable()
            || target.kind.is_structured_or_instantiable()
        {
            self.check_type_related_to(source, target, relation, None)
        } else {
            Ternary::FALSE
        }
    }

    pub(super) fn is_empty_array_lit_ty(&self, ty: &'cx Ty<'cx>) -> bool {
        if ty.kind.is_array(self) {
            ty.kind
                .as_object_reference()
                .map(|refer| {
                    refer.resolved_ty_args.len() == 1
                        && refer.resolved_ty_args[0] == self.never_ty()
                })
                .unwrap_or_default()
        } else {
            false
        }
    }

    pub(super) fn check_type_assignable_to_and_optionally_elaborate(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        error_node: Option<ast::NodeID>,
        expr: Option<ast::NodeID>,
    ) -> Ternary {
        self.check_type_related_to_and_optionally_elaborate(
            source,
            target,
            RelationKind::Assignable,
            error_node,
            expr,
            |this, span, source, target| {
                let source = if (source.kind.is_number_lit() && target.kind.is_number_lit())
                    || (source.kind.is_string_lit() && target.kind.is_string_lit())
                {
                    source
                } else {
                    this.get_base_ty_of_literal_ty(source)
                };
                Box::new(errors::TypeIsNotAssignableToType {
                    span,
                    ty1: this.print_ty(source).to_string(),
                    ty2: this.print_ty(target).to_string(),
                })
            },
        )
    }

    pub(super) fn check_type_related_to_and_optionally_elaborate(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
        error_node: Option<ast::NodeID>,
        expr: Option<ast::NodeID>,
        error: impl FnOnce(&mut Self, Span, &'cx Ty<'cx>, &'cx Ty<'cx>) -> crate::Diag,
    ) -> Ternary {
        if self.is_type_related_to(source, target, relation) != Ternary::FALSE {
            return Ternary::TRUE;
        }
        if error_node.is_none() || !self.elaborate_error(expr, source, target, relation, error_node)
        {
            if self.check_type_related_to(source, target, relation, error_node) == Ternary::FALSE {
                if error_node.is_none() {
                    return Ternary::FALSE;
                }
                let span = self.p.node(error_node.unwrap()).span();
                let error = error(self, span, source, target);
                self.push_error(error);
                return Ternary::FALSE;
            } else {
                return Ternary::TRUE;
            }
        }

        Ternary::FALSE
    }

    pub(super) fn check_type_assignable_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        error_node: Option<ast::NodeID>,
    ) -> Ternary {
        self.check_type_related_to(source, target, RelationKind::Assignable, error_node)
    }

    fn _get_props_of_object_ty(
        &self,
        self_ty: &'cx ty::Ty<'cx>,
        ty: &'cx ObjectTy<'cx>,
    ) -> &'cx [SymbolID] {
        if let ObjectTyKind::Interface(_) = ty.kind {
            self.properties_of_object_type(self_ty)
        } else if let ObjectTyKind::Reference(_) = ty.kind {
            self.properties_of_object_type(self_ty)
        } else if let ObjectTyKind::Anonymous(_) = ty.kind {
            self.properties_of_object_type(self_ty)
        } else {
            &[]
        }
    }

    pub(super) fn get_props_of_object_ty(&mut self, ty: &'cx Ty<'cx>) -> &'cx [SymbolID] {
        if let TyKind::Object(object_ty) = ty.kind {
            self.resolve_structured_type_members(ty);
            self._get_props_of_object_ty(ty, object_ty)
        } else {
            &[]
        }
    }

    pub(super) fn get_props_of_ty(&mut self, ty: &'cx Ty<'cx>) -> &'cx [SymbolID] {
        if ty.kind.is_object() {
            self.get_props_of_object_ty(ty)
        } else {
            &[]
        }
    }

    pub(super) fn get_prop_of_ty(
        &mut self,
        ty: &'cx Ty<'cx>,
        name: SymbolName,
    ) -> Option<SymbolID> {
        let ty = self.get_reduced_apparent_ty(ty);
        if let TyKind::Object(object_ty) = ty.kind {
            self.resolve_structured_type_members(ty);

            let symbol = if object_ty.kind.is_interface()
                || object_ty.kind.is_anonymous()
                || object_ty.kind.is_reference()
            {
                self.expect_ty_links(ty.id)
                    .expect_structured_members()
                    .members
                    .get(&name)
                    .copied()
            } else if let Some(ty) = object_ty.kind.as_tuple() {
                ObjectShape::get_member(ty, &name)
            } else {
                unreachable!("ty: {ty:#?}")
            };

            if symbol.is_some() {
                return symbol;
            }

            let fn_ty = if ty.id == self.any_fn_ty().id {
                Some(self.global_array_ty())
            } else if self
                .get_ty_links(ty.id)
                .get_structured_members()
                .map(|item| !item.call_sigs.is_empty())
                .unwrap_or_default()
            {
                Some(self.global_callable_fn_ty())
            } else if self
                .get_ty_links(ty.id)
                .get_structured_members()
                .map(|item| !item.ctor_sigs.is_empty())
                .unwrap_or_default()
            {
                Some(self.global_newable_fn_ty())
            } else {
                None
            };

            if let Some(fn_ty) = fn_ty {
                if let Some(symbol) = self.get_prop_of_object_ty(fn_ty, name) {
                    return Some(symbol);
                }
            }

            self.get_prop_of_object_ty(self.global_object_ty(), name)
        } else if let Some(_) = ty.kind.as_union() {
            self.get_prop_of_union_or_intersection_ty(ty, name)
        } else {
            None
        }
    }

    fn get_prop_of_union_or_intersection_ty(
        &mut self,
        ty: &'cx Ty<'cx>,
        name: SymbolName,
    ) -> Option<SymbolID> {
        self.get_union_or_intersection_prop(ty, name)
            .and_then(|prop| {
                if !self
                    .binder
                    .get_check_flags(prop)
                    .intersects(CheckFlags::READ_PARTIAL)
                {
                    Some(prop)
                } else {
                    None
                }
            })
    }

    fn get_union_or_intersection_prop(
        &mut self,
        containing_ty: &'cx Ty<'cx>,
        name: SymbolName,
    ) -> Option<SymbolID> {
        // TODO: cache
        self.create_union_or_intersection_prop(containing_ty, name)
    }

    fn create_union_or_intersection_prop(
        &mut self,
        containing_ty: &'cx Ty<'cx>,
        name: SymbolName,
    ) -> Option<SymbolID> {
        let (is_union, tys) = if let Some(union) = containing_ty.kind.as_union() {
            (true, union.tys)
        } else {
            unreachable!("containing_ty: {containing_ty:#?}")
        };

        let mut single_prop = None;
        let mut prop_set = fx_hashset_with_capacity(tys.len());

        let check_flags = if is_union {
            CheckFlags::empty()
        } else {
            CheckFlags::READONLY
        };

        for current in tys {
            let ty = self.get_apparent_ty(current);
            if ty != self.error_ty() && ty != self.never_ty() {
                if let Some(prop) = self.get_prop_of_ty(ty, name) {
                    if let Some(single_prop) = single_prop {
                        if single_prop != prop {
                            if prop_set.is_empty() {
                                prop_set.insert(single_prop);
                            }
                            prop_set.insert(prop);
                        }
                    } else {
                        single_prop = Some(prop);
                    }
                }
            }
        }

        let props = if prop_set.is_empty() {
            if let Some(single_prop) = single_prop {
                vec![single_prop]
            } else {
                vec![]
            }
        } else {
            prop_set.into_iter().collect::<Vec<_>>()
        };

        let mut first_ty = None;
        let mut name_ty = None;
        let mut prop_tys = Vec::with_capacity(props.len());

        for prop in props {
            let ty = self.get_type_of_symbol(prop);
            if first_ty.is_none() {
                first_ty = Some(ty);
                name_ty = self.get_symbol_links(prop).get_name_ty();
            }
            let write_ty = self.get_write_type_of_symbol(prop);
            // if first_ty.map_or(true, |first_ty| first_ty != ty)
            // {}
            prop_tys.push(ty);
        }

        let symbol_flags = SymbolFlags::PROPERTY;
        let links = SymbolLinks::default().with_containing_ty(containing_ty);
        let links = if let Some(name_ty) = name_ty {
            links.with_name_ty(name_ty)
        } else {
            links
        };
        let links = if prop_tys.len() > 2 {
            links
        } else {
            let ty = if is_union {
                self.create_union_type(prop_tys, ty::UnionReduction::Lit)
            } else {
                todo!()
            };
            links.with_ty(ty)
        };

        let result = self
            .binder
            .create_transient_symbol(name, symbol_flags, None, links);

        Some(result)
    }

    fn get_prop_of_object_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        name: SymbolName,
    ) -> Option<SymbolID> {
        let TyKind::Object(_) = ty.kind else {
            return None;
        };
        self.resolve_structured_type_members(ty);
        self.expect_ty_links(ty.id)
            .expect_structured_members()
            .members
            .get(&name)
            .copied()
    }

    pub fn get_unmatched_prop(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
    ) -> Option<(FxHashSet<AtomId>, SymbolID)> {
        self.get_unmatched_props(source, target)
    }

    fn get_unmatched_props(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
    ) -> Option<(FxHashSet<AtomId>, SymbolID)> {
        let symbols = self.get_props_of_ty(target);
        let set: FxHashSet<_> = symbols
            .iter()
            .filter_map(|symbol| {
                let name = self.binder.symbol(*symbol).name;
                if self.get_prop_of_ty(source, name).is_none() {
                    Some(name.expect_atom())
                } else {
                    None
                }
            })
            .collect();
        if set.is_empty() {
            None
        } else {
            let ty = target.kind.expect_object();
            fn recur(ty: &ObjectTy) -> SymbolID {
                match ty.kind {
                    ObjectTyKind::Reference(ty) => recur(ty.target.kind.expect_object()),
                    ObjectTyKind::Interface(ty) => ty.symbol,
                    ObjectTyKind::Anonymous(ty) => ty.symbol,
                    _ => unreachable!(),
                }
            }
            let symbol = recur(ty);
            Some((set, symbol))
        }
    }

    pub(super) fn is_type_assignable_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
    ) -> Ternary {
        self.is_type_related_to(source, target, RelationKind::Assignable)
    }
}
