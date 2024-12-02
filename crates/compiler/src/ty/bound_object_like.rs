use rustc_hash::FxHashMap;

use super::object_ty::{InterfaceTy, ObjectLitTy};
use crate::bind::{SymbolID, SymbolName};

pub trait ObjectLikeTy<'cx> {
    fn members(&self) -> &'cx FxHashMap<SymbolName, SymbolID>;
    fn props(&self) -> &'cx [SymbolID];
}

impl<'cx> ObjectLikeTy<'cx> for InterfaceTy<'cx> {
    fn members(&self) -> &'cx FxHashMap<SymbolName, SymbolID> {
        self.members
    }
    fn props(&self) -> &'cx [SymbolID] {
        self.declared_props
    }
}

impl<'cx> ObjectLikeTy<'cx> for ObjectLitTy<'cx> {
    fn members(&self) -> &'cx FxHashMap<SymbolName, SymbolID> {
        self.members
    }
    fn props(&self) -> &'cx [SymbolID] {
        self.declared_props
    }
}
