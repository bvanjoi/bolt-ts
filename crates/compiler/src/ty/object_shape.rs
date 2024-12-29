use super::object_ty::{ObjectLitTy, TupleTy};
use super::AnonymousTy;
use crate::bind::{SymbolID, SymbolName};
use crate::keyword;

pub trait ObjectShape<'cx> {
    fn get_member(&self, name: &SymbolName) -> Option<SymbolID>;
    fn props(&self) -> &[SymbolID];
    fn declared_call_sigs(&self) -> super::Sigs<'cx>;
}

impl<'cx> ObjectShape<'cx> for ObjectLitTy<'cx> {
    fn get_member(&self, name: &SymbolName) -> Option<SymbolID> {
        self.members.get(name).copied()
    }
    fn props(&self) -> &[SymbolID] {
        self.declared_props
    }
    fn declared_call_sigs(&self) -> super::Sigs<'cx> {
        &[]
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
    fn declared_call_sigs(&self) -> super::Sigs<'cx> {
        &[]
    }
}

// impl<'cx> ObjectShape for ArrayTy<'cx> {
//     fn get_member(&self, name: &SymbolName) -> Option<SymbolID> {
//         if let Some(atom) = name.as_atom() {
//             if atom == keyword::IDENT_LENGTH {
//                 Some(self.shape.declared_props[0])
//             } else {
//                 None
//             }
//         } else {
//             todo!("index literal")
//         }
//     }
//     fn props(&self) -> &[SymbolID] {
//         self.shape.declared_props
//     }
// }

impl<'cx> ObjectShape<'cx> for AnonymousTy<'cx> {
    fn get_member(&self, _name: &SymbolName) -> Option<SymbolID> {
        None
    }
    fn props(&self) -> &[SymbolID] {
        &[]
    }
    fn declared_call_sigs(&self) -> super::Sigs<'cx> {
        self.call_sigs
    }
}
