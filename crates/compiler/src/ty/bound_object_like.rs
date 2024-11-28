use rustc_hash::FxHashMap;

use crate::bind::{SymbolID, SymbolName};
use crate::ty;

pub trait ObjectLikeTy<'cx> {
    fn members(&self) -> &'cx FxHashMap<SymbolName, SymbolID>;
    fn props(&self) -> &'cx [SymbolID];
}

impl<'cx> ObjectLikeTy<'cx> for ty::InterfaceTy<'cx> {
    fn members(&self) -> &'cx FxHashMap<SymbolName, SymbolID> {
        self.members
    }
    fn props(&self) -> &'cx [SymbolID] {
        self.declared_props
    }
}

impl<'cx> ObjectLikeTy<'cx> for ty::ObjectLitTy<'cx> {
    fn members(&self) -> &'cx FxHashMap<SymbolName, SymbolID> {
        self.members
    }
    fn props(&self) -> &'cx [SymbolID] {
        self.declared_props
    }
}
