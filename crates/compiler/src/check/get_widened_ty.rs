use crate::bind::SymbolFlags;
use crate::bind::SymbolID;
use crate::ty::ObjectFlags;

use super::ty;
use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_widened_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        self.get_widened_ty_with_context(ty)
    }

    fn get_widened_ty_with_context(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty
            .get_object_flags()
            .intersects(ty::ObjectFlags::REQUIRES_WIDENING)
        {
            // TODO: cache
            if ty.is_object_literal() {
                self.get_widened_type_of_object_lit(ty)
            } else if self.is_array_or_tuple(ty) {
                let refer = ty.kind.expect_object_reference();
                let ty_args = refer
                    .resolved_ty_args
                    .iter()
                    .map(|arg| self.get_widened_ty(arg))
                    .collect::<Vec<_>>();
                let ty_args = self.alloc(ty_args);
                self.create_reference_ty(refer.target, ty_args, ObjectFlags::empty())
            } else {
                ty
            }
        } else {
            ty
        }
    }

    pub(super) fn get_widened_literal_ty(&self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty.kind.is_number_lit() {
            self.number_ty
        } else if ty.kind.is_string_lit() {
            self.string_ty
        } else {
            ty
        }
    }

    fn get_widened_prop(&mut self, prop: SymbolID) -> SymbolID {
        if !self.symbol(prop).flags().intersects(SymbolFlags::PROPERTY) {
            prop
        } else {
            let original = self.get_type_of_symbol(prop);
            let widened = self.get_widened_ty(original);
            if original != widened {
                prop
            } else {
                self.create_transient_symbol_with_ty(prop, widened)
            }
        }
    }

    fn get_widened_type_of_object_lit(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        let members = self
            .get_props_of_object_ty(ty)
            .iter()
            .map(|prop| {
                let name = self.symbol(*prop).name();
                let ty = self.get_widened_prop(*prop);
                (name, ty)
            })
            .collect();

        let object_flags =
            ty.get_object_flags() & (ObjectFlags::JS_LITERAL | ObjectFlags::NON_INFERRABLE_TYPE);

        self.create_anonymous_ty_with_resolved(
            ty.symbol(),
            object_flags,
            self.alloc(members),
            &[],
            &[],
            &[],
        )
    }

    fn get_widened_lit_ty_for_init(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        // TODO: as const
        self.get_widened_literal_ty(ty)
    }

    pub(super) fn widened_ty_from_init(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        self.get_widened_lit_ty_for_init(ty)
    }
}
