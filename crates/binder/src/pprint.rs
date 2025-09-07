use super::SymbolName;
use bolt_ts_atom::AtomIntern;

impl<'cx> SymbolName {
    pub fn to_string(self, atoms: &'cx AtomIntern) -> String {
        match self {
            SymbolName::Atom(atom_id) => atoms.get(atom_id).to_string(),
            SymbolName::EleNum(val) => val.val().to_string(),
            SymbolName::Container => "__container".to_string(),
            SymbolName::ClassExpr => "__container".to_string(),
            SymbolName::Array => "__container".to_string(),
            SymbolName::Object => "__container".to_string(),
            SymbolName::Fn => "__container".to_string(),
            SymbolName::Constructor => "__container".to_string(),
            SymbolName::New => "__container".to_string(),
            SymbolName::Call => "__container".to_string(),
            SymbolName::Interface => "__container".to_string(),
            SymbolName::Index => "__container".to_string(),
            SymbolName::Type => "__container".to_string(),
            SymbolName::Missing => "__container".to_string(),
            SymbolName::Resolving => "__container".to_string(),
            SymbolName::ExportStar => "__container".to_string(),
            SymbolName::ExportEquals => "__container".to_string(),
            SymbolName::ExportDefault => "__container".to_string(),
            SymbolName::Computed => "__container".to_string(),
            SymbolName::ParamIdx(_) => "__container".to_string(),
            SymbolName::ESSymbol {
                escaped_name,
                symbol_id,
            } => format!("{}#", atoms.get(escaped_name)),
        }
    }
}
