use bolt_ts_atom::AtomIntern;

#[inline]
pub fn pprint_ident(ident: &super::Ident, atoms: &AtomIntern) -> String {
    atoms.get(ident.name).to_string()
}

pub fn print_declaration_name(node: &super::DeclarationName, atoms: &AtomIntern) -> String {
    use super::DeclarationName::*;
    match node {
        Ident(ident) => pprint_ident(ident, atoms),
        NumLit(lit) => lit.val.to_string(),
        StringLit { raw, .. } => atoms.get(raw.val).to_string(),
        Computed(_) => "todo: computed name".to_string(),
    }
}

pub fn pprint_binding(binding: &super::Binding<'_>, atoms: &AtomIntern) -> String {
    match binding.kind {
        super::BindingKind::Ident(ident) => pprint_ident(ident, atoms),
        super::BindingKind::ObjectPat(_) => todo!(),
        crate::BindingKind::ArrayPat(_) => todo!(),
    }
}

#[cfg(debug_assertions)]
pub fn pprint_entity_name(name: &super::EntityName, atoms: &AtomIntern) -> String {
    match name.kind {
        super::EntityNameKind::Ident(ident) => pprint_ident(ident, atoms),
        super::EntityNameKind::Qualified(q) => {
            let mut name = pprint_entity_name(q.left, atoms);
            name.push('.');
            name
        }
    }
}

pub fn debug_ident(ident: &super::Ident, atoms: &AtomIntern) -> String {
    format!("{}({})", pprint_ident(ident, atoms), ident.span)
}
