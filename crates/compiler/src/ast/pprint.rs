use bolt_ts_atom::AtomMap;

pub fn pprint_ident(ident: &super::Ident, atoms: &AtomMap) -> String {
    atoms.get(ident.name).to_string()
}
