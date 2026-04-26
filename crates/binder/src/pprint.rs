use super::SymbolName;
use bolt_ts_atom::AtomIntern;

impl SymbolName {
    pub fn to_string(self, atoms: &AtomIntern) -> String {
        match self {
            SymbolName::Atom(atom_id) => atoms.get(atom_id).to_string(),
            SymbolName::EleNum(val) => val.val().to_string(),
            SymbolName::InstantiationExpression => "__InstantiationExpression".to_string(),
            SymbolName::Container => "__Container".to_string(),
            SymbolName::ClassExpr => "__ClassExpr".to_string(),
            SymbolName::Array => "__Array".to_string(),
            SymbolName::Object => "__Object".to_string(),
            SymbolName::Fn => "__Fn".to_string(),
            SymbolName::Constructor => "__Constructor".to_string(),
            SymbolName::New => "__New".to_string(),
            SymbolName::Call => "__Call".to_string(),
            SymbolName::Index => "__Index".to_string(),
            SymbolName::Type => "__Type".to_string(),
            SymbolName::Missing => "__Missing".to_string(),
            SymbolName::Resolving => "<resolving>".to_string(),
            SymbolName::ExportStar => "__ExportStar".to_string(),
            SymbolName::ExportEquals => "export =".to_string(),
            SymbolName::ExportDefault => "__ExportDefault".to_string(),
            SymbolName::Computed => "<computed>".to_string(),
            SymbolName::ParamIndex(_) => "__ParamIndex".to_string(),
            SymbolName::ESSymbol { escaped_name, .. } => format!("{}#", atoms.get(escaped_name)),
        }
    }
}
