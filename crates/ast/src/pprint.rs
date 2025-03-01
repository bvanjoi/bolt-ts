use bolt_ts_atom::AtomMap;

#[inline]
pub fn pprint_ident(ident: &super::Ident, atoms: &AtomMap) -> String {
    atoms.get(ident.name).to_string()
}

#[cfg(debug_assertions)]
pub fn pprint_entity_name(name: &super::EntityName, atoms: &AtomMap) -> String {
    match name.kind {
        super::EntityNameKind::Ident(ident) => pprint_ident(ident, atoms),
        super::EntityNameKind::Qualified(q) => {
            let mut name = pprint_entity_name(q.left, atoms);
            name.push('.');
            name
        }
    }
}

pub fn debug_ident(ident: &super::Ident, atoms: &AtomMap) -> String {
    format!(
        "{}({}:{})",
        pprint_ident(ident, atoms),
        ident.id.module().as_usize(),
        ident.id.index_as_usize()
    )
}
