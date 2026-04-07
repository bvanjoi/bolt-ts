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
        PrivateIdent(_n) => todo!(),
        BigIntLit(_n) => todo!(),
    }
}

pub fn pprint_binding(binding: &super::Binding<'_>, atoms: &AtomIntern) -> String {
    match binding.kind {
        super::BindingKind::Ident(ident) => pprint_ident(ident, atoms),
        super::BindingKind::ObjectPat(_) => todo!(),
        super::BindingKind::ArrayPat(_) => todo!(),
    }
}

pub fn pprint_entity_name(name: &super::EntityName, atoms: &AtomIntern) -> String {
    match name.kind {
        super::EntityNameKind::Ident(ident) => pprint_ident(ident, atoms),
        super::EntityNameKind::Qualified(q) => {
            let mut name = pprint_entity_name(q.left, atoms);
            name.push('.');
            name.push_str(&pprint_ident(q.right, atoms));
            name
        }
    }
}

pub fn debug_ident(ident: &super::Ident, atoms: &AtomIntern) -> String {
    format!("{}({})", pprint_ident(ident, atoms), ident.span)
}

pub fn pprint_prop_access_expr(n: &super::PropAccessExpr, atoms: &AtomIntern) -> String {
    let mut ret = String::new();
    ret.push_str(&match n.expr.kind {
        super::ExprKind::Ident(ident) => pprint_ident(ident, atoms),
        super::ExprKind::PropAccess(expr) => pprint_prop_access_expr(expr, atoms),
        _ => unreachable!(),
    });
    ret.push('.');
    ret.push_str(&pprint_ident(n.name, atoms));
    ret
}

pub fn pprint_elem_access_expr(n: &super::EleAccessExpr, atoms: &AtomIntern) -> String {
    let mut ret = String::new();
    ret.push_str(&match n.expr.kind {
        super::ExprKind::Ident(ident) => pprint_ident(ident, atoms),
        super::ExprKind::PropAccess(expr) => pprint_prop_access_expr(expr, atoms),
        super::ExprKind::EleAccess(expr) => pprint_elem_access_expr(expr, atoms),
        _ => unreachable!(),
    });
    ret.push('[');
    ret.push_str(&match n.arg.kind {
        super::ExprKind::Ident(ident) => pprint_ident(ident, atoms),
        super::ExprKind::NumLit(expr) => expr.val.to_string(),
        super::ExprKind::StringLit(expr) => atoms.get(expr.val).to_string(),
        super::ExprKind::PropAccess(expr) => pprint_prop_access_expr(expr, atoms),
        super::ExprKind::EleAccess(expr) => pprint_elem_access_expr(expr, atoms),
        _ => unreachable!(),
    });
    ret.push(']');
    ret
}
