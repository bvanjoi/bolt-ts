use super::super::get_declared_ty::EnumMemberValue;
use super::super::ty;
use super::NodeCheckFlags;
use super::links;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExhaustiveState {
    Computing,
    True,
    False,
}

links!(
    NodeLinks,
    (resolved_ty, &'cx ty::Ty<'cx>),
    (resolved_sig, &'cx ty::Sig<'cx>),
    (resolved_symbol, bolt_ts_binder::SymbolID),
    (flags, NodeCheckFlags),
    (outer_ty_params, ty::Tys<'cx>),
    (effects_sig, &'cx ty::Sig<'cx>),
    (skip_direct_inference, bool),
    (non_existent_prop_checked, bool),
    (enum_member_value, EnumMemberValue),
    (assertion_expression_ty, &'cx ty::Ty<'cx>),
    (context_free_ty, &'cx ty::Ty<'cx>),
    (is_exhaustive, ExhaustiveState),
    (type_checked, bool),
    (switch_tys, ty::Tys<'cx>),
    (parameter_initializer_contains_undefined, bool)
);

impl NodeLinks<'_> {
    pub fn flags(&self) -> NodeCheckFlags {
        self.get_flags().unwrap()
    }
}
