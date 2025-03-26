use super::SymbolName;
use bolt_ts_atom::AtomMap;

impl<'cx> SymbolName {
    pub(crate) fn debug_print(&self, atoms: &AtomMap<'_>) -> String {
        match &self {
            SymbolName::Normal(atom_id) => format!("Normal({})", atoms.get(*atom_id)),
            SymbolName::Ele(atom_id) => format!("Ele({})", atoms.get(*atom_id)),
            SymbolName::EleNum(f64_represent) => {
                format!("EleNum({:#?})", Into::<f64>::into(*f64_represent))
            }
            SymbolName::Container => "Inner(Container)".to_string(),
            SymbolName::ClassExpr => "Inner(ClassExpr)".to_string(),
            SymbolName::Array => "Inner(Array)".to_string(),
            SymbolName::Object => "Inner(Object)".to_string(),
            SymbolName::Fn => "Inner(Fn)".to_string(),
            SymbolName::Constructor => "Inner(Constructor)".to_string(),
            SymbolName::New => "Inner(New)".to_string(),
            SymbolName::Call => "Inner(Call)".to_string(),
            SymbolName::Interface => "Inner(Interface)".to_string(),
            SymbolName::Index => "Inner(Index)".to_string(),
            SymbolName::Type => "Inner(Type)".to_string(),
            SymbolName::Missing => todo!(),
            SymbolName::Resolving => todo!(),
            SymbolName::ExportStar => todo!(),
        }
    }
    pub(crate) fn to_string(self, atoms: &'cx AtomMap<'cx>) -> String {
        match self {
            SymbolName::Normal(atom_id) => atoms.get(atom_id).to_string(),
            SymbolName::Ele(atom_id) => atoms.get(atom_id).to_string(),
            SymbolName::EleNum(val) => val.val().to_string(),
            _ => todo!("{:?}", self),
        }
    }
}
