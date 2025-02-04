use crate::ty::{Ty, TypeFlags};

use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    pub(super) fn is_type_assignable_to_kind(
        &mut self,
        source: &'cx Ty<'cx>,
        flags: TypeFlags,
        strict: bool,
    ) -> bool {
        if source.flags.intersects(flags) {
            return true;
        }
        if strict
            && (source.flags.intersects(TypeFlags::ANY_OR_UNKNOWN)
                || source.flags.intersects(TypeFlags::VOID)
                || source.flags.intersects(TypeFlags::UNDEFINED)
                || source.flags.intersects(TypeFlags::NULL))
        {
            return false;
        }

        if flags.intersects(TypeFlags::NUMBER_LIKE) {
            self.is_type_assignable_to(source, self.number_ty)
        } else if flags.intersects(TypeFlags::STRING_LIKE) {
            self.is_type_assignable_to(source, self.string_ty)
        } else if flags.intersects(TypeFlags::BOOLEAN_LIKE) {
            self.is_type_assignable_to(source, self.boolean_ty())
        } else if flags.intersects(TypeFlags::VOID) {
            self.is_type_assignable_to(source, self.void_ty)
        } else if flags.intersects(TypeFlags::NULL) {
            self.is_type_assignable_to(source, self.null_ty)
        } else if flags.intersects(TypeFlags::UNDEFINED) {
            self.is_type_assignable_to(source, self.undefined_ty)
        } else {
            unreachable!()
        }
    }
}
