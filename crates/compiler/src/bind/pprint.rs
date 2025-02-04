use bolt_ts_atom::AtomMap;

use super::Symbol;

impl<'cx> Symbol {
    pub(crate) fn print_name(&self, atoms: &AtomMap<'cx>) -> String {
        self.name.print(atoms)
    }
    pub(crate) fn to_string(&self, atoms: &'cx AtomMap<'cx>) -> &'cx str {
        use super::SymbolName;
        match self.name {
            SymbolName::Normal(atom_id) => atoms.get(atom_id),
            _ => todo!(),
        }
    }
}

use super::SymbolName;

impl SymbolName {
    pub(crate) fn print(&self, atoms: &AtomMap<'_>) -> String {
        match &self {
            super::SymbolName::Normal(atom_id) => format!("Normal({})", atoms.get(*atom_id)),
            super::SymbolName::Ele(atom_id) => format!("Ele({})", atoms.get(*atom_id)),
            super::SymbolName::EleNum(f64_represent) => {
                format!("EleNum({:#?})", Into::<f64>::into(*f64_represent))
            }
            super::SymbolName::Container => "Inner(Container)".to_string(),
            super::SymbolName::ClassExpr => "Inner(ClassExpr)".to_string(),
            super::SymbolName::Array => "Inner(Array)".to_string(),
            super::SymbolName::Object => "Inner(Object)".to_string(),
            super::SymbolName::Fn => "Inner(Fn)".to_string(),
            super::SymbolName::Constructor => "Inner(Constructor)".to_string(),
            super::SymbolName::New => "Inner(New)".to_string(),
            super::SymbolName::Call => "Inner(Call)".to_string(),
            super::SymbolName::Interface => "Inner(Interface)".to_string(),
            super::SymbolName::Index => "Inner(Index)".to_string(),
            super::SymbolName::Type => "Inner(Type)".to_string(),
        }
    }
}
