use super::object_ty::{ObjectLitTy, TupleTy};
use crate::bind::{SymbolID, SymbolName};
use crate::keyword;

pub trait ObjectShape<'cx> {
    fn get_member(&self, name: &SymbolName) -> Option<SymbolID>;
    fn props(&self) -> &[SymbolID];
}

impl<'cx> ObjectShape<'cx> for ObjectLitTy<'cx> {
    fn get_member(&self, name: &SymbolName) -> Option<SymbolID> {
        self.members.get(name).copied()
    }
    fn props(&self) -> &[SymbolID] {
        self.declared_props
    }
}

impl<'cx> ObjectShape<'cx> for TupleTy<'cx> {
    fn get_member(&self, name: &SymbolName) -> Option<SymbolID> {
        if let Some(atom) = name.as_atom() {
            if atom == keyword::IDENT_LENGTH {
                Some(self.shape.declared_props[0])
            } else {
                None
            }
        } else {
            todo!("index literal")
        }
    }
    fn props(&self) -> &[SymbolID] {
        self.shape.declared_props
    }
}
