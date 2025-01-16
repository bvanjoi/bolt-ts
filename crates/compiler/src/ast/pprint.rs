use bolt_ts_atom::AtomMap;

#[inline]
pub fn pprint_ident(ident: &super::Ident, atoms: &AtomMap) -> String {
    atoms.get(ident.name).to_string()
}

pub fn debug_ident(ident: &super::Ident, atoms: &AtomMap) -> String {
    format!(
        "{}({}:{})",
        pprint_ident(ident, atoms),
        ident.id.module().as_usize(),
        ident.id.index_as_usize()
    )
}
