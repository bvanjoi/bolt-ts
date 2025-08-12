use super::SymbolName;
use bolt_ts_atom::AtomIntern;

impl<'cx> SymbolName {
    pub fn to_string(self, atoms: &'cx AtomIntern) -> String {
        match self {
            SymbolName::Atom(atom_id) => atoms.get(atom_id).to_string(),
            SymbolName::EleNum(val) => val.val().to_string(),
            _ => todo!("{:?}", self),
        }
    }
}
