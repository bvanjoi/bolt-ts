use bolt_ts_atom::AtomIntern;

#[inline]
pub fn pprint_ident(ident: &super::Ident, atoms: &AtomIntern) -> String {
    atoms.get(ident.name).to_string()
}

pub fn pprint_prop_name(node: &super::PropNameKind<'_>, atoms: &AtomIntern) -> String {
    use super::PropNameKind::*;
    match node {
        Ident(ident) => pprint_ident(ident, atoms),
        PrivateIdent(n) => atoms.get(n.name).to_string(),
        StringLit { raw, .. } => atoms.get(raw.val).to_string(),
        BigIntLit(lit) => atoms.get(lit.val.1).to_string(),
        NumLit(lit) => lit.val.to_string(),
        Computed(n) => pprint_computed_prop_name(n, atoms),
    }
}

fn pprint_computed_prop_name(node: &super::ComputedPropName<'_>, atoms: &AtomIntern) -> String {
    let expr = pprint_expression(node.expr, atoms);
    format!("[{}]", expr)
}

fn pprint_string_literal(node: &super::StringLit, atoms: &AtomIntern) -> String {
    let s = atoms.get(node.val);
    format!("\"{}\"", s)
}

fn pprint_expression(node: &super::Expr<'_>, atoms: &AtomIntern) -> String {
    match node.kind {
        super::ExprKind::Ident(ident) => pprint_ident(ident, atoms),
        super::ExprKind::NumLit(lit) => lit.val.to_string(),
        super::ExprKind::StringLit(node) => pprint_string_literal(node, atoms),
        super::ExprKind::PropAccess(expr) => pprint_prop_access_expr(expr, atoms),
        super::ExprKind::EleAccess(expr) => pprint_elem_access_expr(expr, atoms),
        super::ExprKind::Assign(n) => {
            let left = pprint_expression(n.left, atoms);
            let right = pprint_expression(n.right, atoms);
            format!("{} {} {}", left, n.op.as_str(), right)
        }
        _ => "UNSUPPORTED_EXPRESSION".to_string(),
    }
}

pub fn print_declaration_name(node: &super::DeclarationName, atoms: &AtomIntern) -> String {
    use super::DeclarationName::*;
    match node {
        Ident(ident) => pprint_ident(ident, atoms),
        NumLit(lit) => lit.val.to_string(),
        StringLit { raw, .. } => pprint_string_literal(*raw, atoms),
        Computed(n) => pprint_computed_prop_name(n, atoms),
        PrivateIdent(_n) => todo!(),
        BigIntLit(_n) => todo!(),
        ElementAccess(_) => todo!(),
    }
}

pub fn binding_name_text<'cx>(binding: &super::Binding<'cx>) -> &'cx super::Ident {
    match binding.kind {
        super::BindingKind::Ident(ident) => ident,
        super::BindingKind::ObjectPat(pat) => match pat.elems[0].name {
            crate::ObjectBindingName::Shorthand(ident) => ident,
            crate::ObjectBindingName::Prop { name, .. } => binding_name_text(*name),
        },
        super::BindingKind::ArrayPat(pat) => match pat.elems[0].kind {
            crate::ArrayBindingElemKind::Omit(_) => unreachable!(),
            crate::ArrayBindingElemKind::Binding(n) => binding_name_text(n.name),
        },
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
        _ => unreachable!("expr: {:#?}", n.expr.span()),
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
