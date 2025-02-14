use crate::bind::SymbolFlags;
use crate::bind::SymbolID;
use crate::ty::ObjectFlags;
use crate::ty::TypeFlags;

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
                let ty_args = self.get_ty_arguments(ty);
                let ty_args =
                    self.same_map_tys(Some(ty_args), |this, ty_arg, _| this.get_widened_ty(ty_arg));
                assert!(ty_args.is_some());
                self.create_reference_ty(refer.target, ty_args, ObjectFlags::empty())
            } else {
                ty
            }
        } else {
            ty
        }
    }

    pub(super) fn is_fresh_literal_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.flags.intersects(TypeFlags::FRESHABLE)
            && self
                .get_ty_links(ty.id)
                .get_fresh_ty()
                .is_some_and(|fresh_ty| fresh_ty == ty)
    }

    pub(super) fn get_widened_literal_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty.kind.is_number_lit() && self.is_fresh_literal_ty(ty) {
            self.number_ty
        } else if ty.kind.is_string_lit() && self.is_fresh_literal_ty(ty) {
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
