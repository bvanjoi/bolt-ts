use bolt_ts_atom::AtomMap;

#[inline]
pub fn pprint_ident(ident: &super::Ident, atoms: &AtomMap) -> String {
    atoms.get(ident.name).to_string()
}

pub fn pprint_binding(binding: &super::Binding<'_>, atoms: &AtomMap) -> String {
    match binding.kind {
        super::BindingKind::Ident(ident) => pprint_ident(ident, atoms),
        super::BindingKind::ObjectPat(_) => todo!(),
        crate::BindingKind::ArrayPat(_) => todo!(),
    }
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
    format!("{}({})", pprint_ident(ident, atoms), ident.span)
}
