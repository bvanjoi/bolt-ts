use super::object_ty::{InterfaceTy, ObjectLitTy, TupleTy};
use super::{FnTy, Sig};
use crate::bind::{SymbolID, SymbolName};
use crate::keyword;

pub trait ObjectShape<'cx> {
    fn get_member(&self, name: &SymbolName) -> Option<SymbolID>;
    fn props(&self) -> &[SymbolID];
    fn declared_call_sigs(&self) -> super::Sigs<'cx>;
    fn get_base_tys(&self) -> super::Tys<'cx>;
    fn get_index_infos(&self) -> super::IndexInfos<'cx>;
}

impl<'cx> ObjectShape<'cx> for InterfaceTy<'cx> {
    fn get_member(&self, name: &SymbolName) -> Option<SymbolID> {
        self.members.get(name).copied()
    }
    fn props(&self) -> &[SymbolID] {
        self.declared_props
    }
    fn declared_call_sigs(&self) -> super::Sigs<'cx> {
        self.declared_call_sigs
    }
    fn get_base_tys(&self) -> super::Tys<'cx> {
        self.base_tys
    }
    fn get_index_infos(&self) -> super::IndexInfos<'cx> {
        self.declared_index_infos
    }
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
    fn get_base_tys(&self) -> super::Tys<'cx> {
        &[]
    }
    fn get_index_infos(&self) -> super::IndexInfos<'cx> {
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
    fn get_base_tys(&self) -> super::Tys<'cx> {
        &[]
    }
    fn get_index_infos(&self) -> super::IndexInfos<'cx> {
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

impl<'cx> ObjectShape<'cx> for FnTy<'cx> {
    fn get_member(&self, _name: &SymbolName) -> Option<SymbolID> {
        None
    }
    fn props(&self) -> &[SymbolID] {
        &[]
    }
    fn declared_call_sigs(&self) -> super::Sigs<'cx> {
        self.declared_sigs
    }
    fn get_base_tys(&self) -> super::Tys<'cx> {
        &[]
    }
    fn get_index_infos(&self) -> super::IndexInfos<'cx> {
        &[]
    }
}
