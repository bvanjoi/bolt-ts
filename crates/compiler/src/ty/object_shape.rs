use super::object_ty::TupleTy;
use crate::bind::{SymbolID, SymbolName};
use crate::keyword;

// TODO: delete
pub trait ObjectShape<'cx> {
    fn get_member(&self, name: &SymbolName) -> Option<SymbolID>;
}

impl<'cx> ObjectShape<'cx> for TupleTy<'cx> {
    fn get_member(&self, name: &SymbolName) -> Option<SymbolID> {
        if let Some(atom) = name.as_atom() {
            if atom == keyword::IDENT_LENGTH {
                Some(
                    self.ty
                        .kind
                        .expect_object_interface()
                        .declared_members
                        .props[self.fixed_length],
                )
            } else {
                None
            }
        } else if let Some(idx) = name.as_numeric() {
            assert_eq!(idx.fract(), 0.0);
            // query later
            None
        } else {
            unreachable!()
        }
    }
}
