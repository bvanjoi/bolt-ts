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

        macro_rules! check_assignable {
            ($flags:expr, $source:expr, $self:expr, { $($flag:ident => $ty:expr),* }) => {
                $(
                    if $flags.intersects(TypeFlags::$flag) && $self.is_type_assignable_to($source, $ty) {
                        return true;
                    }
                )*
            };
        }

        check_assignable!(flags, source, self, {
            NUMBER_LIKE => self.number_ty,
            BIG_INT_LIKE => self.bigint_ty,
            STRING_LIKE => self.string_ty,
            BOOLEAN_LIKE => self.boolean_ty(),
            VOID => self.void_ty,
            NULL => self.null_ty,
            NEVER => self.never_ty,
            NULL => self.null_ty,
            UNDEFINED => self.undefined_ty,
            ES_SYMBOL_LIKE => self.symbol_ty,
            NON_PRIMITIVE => self.non_primitive_ty
        });

        false
    }
}
