use super::object_ty::{InterfaceTy, ObjectLitTy, TupleTy};
use crate::bind::{SymbolID, SymbolName};

pub trait ObjectLikeTy {
    fn get_member(&self, name: &SymbolName) -> Option<SymbolID>;
    fn props(&self) -> &[SymbolID];
}

impl<'cx> ObjectLikeTy for InterfaceTy<'cx> {
    fn get_member(&self, name: &SymbolName) -> Option<SymbolID> {
        self.members.get(name).copied()
    }
    fn props(&self) -> &[SymbolID] {
        self.declared_props
    }
}

impl<'cx> ObjectLikeTy for ObjectLitTy<'cx> {
    fn get_member(&self, name: &SymbolName) -> Option<SymbolID> {
        self.members.get(name).copied()
    }
    fn props(&self) -> &[SymbolID] {
        self.declared_props
    }
}

impl<'cx> ObjectLikeTy for TupleTy<'cx> {
    fn get_member(&self, name: &SymbolName) -> Option<SymbolID> {
        self.shape.members.get(name).copied()
    }
    fn props(&self) -> &[SymbolID] {
        self.shape.declared_props
    }
}
