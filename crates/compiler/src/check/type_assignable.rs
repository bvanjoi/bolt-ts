use crate::ty::{Ty, TypeFlags};

use super::{Ternary, TyChecker};

impl<'cx> TyChecker<'cx> {
    pub(super) fn is_type_assignable_to_kind(
        &mut self,
        source: &'cx Ty<'cx>,
        f: impl FnOnce(&'cx Ty<'cx>) -> bool,
        flags: TypeFlags,
        strict: bool,
    ) -> Ternary {
        if f(source) {
            return Ternary::TRUE;
        }
        if strict
            && (source.kind.is_any_or_unknown()
                || source.kind.is_void()
                || source.kind.is_undefined()
                || source.kind.is_null())
        {
            return Ternary::FALSE;
        }

        if flags.intersects(TypeFlags::NUMBER_LIKE) {
            self.is_type_assignable_to(source, self.number_ty())
        } else if flags.intersects(TypeFlags::STRING_LIKE) {
            self.is_type_assignable_to(source, self.string_ty())
        } else if flags.intersects(TypeFlags::BOOLEAN_LIKE) {
            self.is_type_assignable_to(source, self.boolean_ty())
        } else if flags.intersects(TypeFlags::VOID) {
            self.is_type_assignable_to(source, self.void_ty())
        } else if flags.intersects(TypeFlags::NULL) {
            self.is_type_assignable_to(source, self.null_ty())
        } else if flags.intersects(TypeFlags::UNDEFINED) {
            self.is_type_assignable_to(source, self.undefined_ty())
        } else {
            unreachable!()
        }
    }
}
