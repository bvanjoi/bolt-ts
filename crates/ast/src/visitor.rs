pub fn visit_program<'cx>(v: &mut impl Visitor<'cx>, program: &'cx super::Program<'cx>) {
    for stmt in program.stmts {
        v.visit_stmt(stmt);
    }
}

pub fn visit_stmt<'cx>(v: &mut impl Visitor<'cx>, stmt: &'cx super::Stmt) {
    use super::StmtKind::*;
    match stmt.kind {
        Block(node) => v.visit_block_stmt(node),
        Class(node) => v.visit_class_decl(node),
        Import(node) => v.visit_import_decl(node),
        Interface(node) => v.visit_interface_decl(node),
        Expr(node) => v.visit_expr_stmt(node),
        Var(node) => v.visit_var_stmt(node),
        Try(node) => v.visit_try_stmt(node),
        TypeAlias(node) => v.visit_type_alias_decl(node),
        Module(node) => v.visit_module_decl(node),
        While(node) => v.visit_while_stmt(node),
        If(node) => v.visit_if_stmt(node),
        Enum(node) => v.visit_enum_decl(node),
        Fn(node) => v.visit_fn_decl(node),
        Export(_) => {}
        ExportAssign(_) => {}
        Empty(_) => (),
        Ret(_) => (),
        Throw(_) => (),
        For(_) => (),
        ForOf(_) => (),
        ForIn(_) => (),
        Break(_) => (),
        Continue(_) => {}
        Do(_) => {}
        Debugger(_) => {}
        Labeled(_) => {}
        Switch(_) => {}
    }
}

fn visit_binding<'cx>(v: &mut impl Visitor<'cx>, node: &'cx super::Binding<'cx>) {
    use crate::BindingKind::*;
    match node.kind {
        Ident(n) => v.visit_ident(n),
        ObjectPat(n) => {
            // TODO:
        }
        ArrayPat(n) => {
            // TODO:
        }
    }
}

fn visit_param_decl<'cx>(v: &mut impl Visitor<'cx>, node: &'cx super::ParamDecl<'cx>) {
    v.visit_binding(node.name);
    if let Some(ty) = node.ty {
        v.visit_ty(ty);
    }
    if let Some(init) = node.init {
        v.visit_expr(init);
    }
}

pub fn visit_fn_decl<'cx>(v: &mut impl Visitor<'cx>, node: &'cx super::FnDecl<'cx>) {
    if let Some(name) = node.name {
        v.visit_ident(name);
    }
    for param in node.params {
        v.visit_param_decl(param);
    }
    if let Some(body) = node.body {
        v.visit_block_stmt(body);
    }
}

pub fn visit_enum_decl<'cx>(v: &mut impl Visitor<'cx>, enum_decl: &'cx super::EnumDecl<'cx>) {
    v.visit_ident(enum_decl.name);
    for member in enum_decl.members {
        v.visit_enum_member(member);
    }
}

pub fn visit_enum_member<'cx>(v: &mut impl Visitor<'cx>, member: &'cx super::EnumMember<'cx>) {
    v.visit_prop_name(member.name);
    if let Some(init) = member.init {
        v.visit_expr(init);
    }
}

fn visit_var_stmt<'cx>(v: &mut impl Visitor<'cx>, stmt: &'cx super::VarStmt<'cx>) {
    for item in stmt.list {
        v.visit_var_decl(item);
    }
}

pub fn visit_type_alias_decl<'cx>(v: &mut impl Visitor<'cx>, decl: &'cx super::TypeAliasDecl<'cx>) {
    v.visit_ident(decl.name);
    v.visit_ty(decl.ty);
}

pub fn visit_module_decl<'cx>(v: &mut impl Visitor<'cx>, decl: &'cx super::ModuleDecl<'cx>) {
    visit_module_name(v, decl.name);
    if let Some(block) = decl.block {
        for stmt in block.stmts {
            v.visit_stmt(stmt);
        }
    }
}

fn visit_module_name<'cx>(v: &mut impl Visitor<'cx>, name: super::ModuleName<'cx>) {
    match name {
        super::ModuleName::Ident(ident) => v.visit_ident(ident),
        super::ModuleName::StringLit(lit) => v.visit_string_lit(lit),
    }
}

pub fn visit_var_decl<'cx>(v: &mut impl Visitor<'cx>, decl: &'cx super::VarDecl<'cx>) {
    if let Some(ty) = decl.ty {
        v.visit_ty(ty);
    }
    if let Some(init) = decl.init {
        v.visit_expr(init);
    }
}
pub fn visit_class_decl<'cx>(v: &mut impl Visitor<'cx>, class: &'cx super::ClassDecl<'cx>) {
    for ele in class.elems.list {
        v.visit_class_elem(ele);
    }
}
pub fn visit_interface_decl<'cx>(_: &mut impl Visitor<'cx>, _: &'cx super::InterfaceDecl<'cx>) {}
pub fn visit_import_decl<'cx>(_: &mut impl Visitor<'cx>, _: &'cx super::ImportDecl<'cx>) {}
pub fn visit_class_elem<'cx>(v: &mut impl Visitor<'cx>, elem: &'cx super::ClassElem<'cx>) {
    use super::ClassElemKind::*;
    if let Method(n) = elem.kind {
        v.visit_class_method_elem(n)
    }
}
pub fn visit_class_method_elem<'cx>(
    v: &mut impl Visitor<'cx>,
    method: &'cx super::ClassMethodElem<'cx>,
) {
    if let Some(body) = method.body {
        v.visit_block_stmt(body);
    }
}
pub fn visit_ty<'cx>(v: &mut impl Visitor<'cx>, ty: &'cx super::Ty<'cx>) {
    use crate::TyKind::*;
    match ty.kind {
        Refer(n) => visit_refer_ty(v, n),
        Array(n) => visit_array_ty(v, n),
        IndexedAccess(n) => visit_indexed_access_ty(v, n),
        Fn(n) => visit_fn_ty(v, n),
        Ctor(n) => visit_ctor_ty(v, n),
        ObjectLit(n) => visit_object_lit_ty(v, n),
        Lit(n) => visit_lit_ty(v, n),
        NamedTuple(n) => visit_named_tuple_ty(v, n),
        Tuple(n) => visit_tuple_ty(v, n),
        Rest(n) => visit_rest_ty(v, n),
        Cond(n) => visit_cond_ty(v, n),
        Union(n) => visit_union_ty(v, n),
        Intersection(n) => visit_intersection_ty(v, n),
        Typeof(n) => visit_typeof_ty(v, n),
        Mapped(n) => visit_mapped_ty(v, n),
        TyOp(n) => visit_ty_op_ty(v, n),
        Pred(n) => visit_pred_ty(v, n),
        Paren(n) => visit_paren_ty(v, n),
        Infer(n) => visit_infer_ty(v, n),
        Intrinsic(n) => visit_intrinsic_ty(v, n),
        Nullable(n) => visit_nullable_ty(v, n),
        TemplateLit(n) => visit_template_lit_ty(v, n),
        This(_) => {}
    }
}
pub fn visit_refer_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::ReferTy<'cx>) {
    // TODO: name
    if let Some(ty_args) = n.ty_args {
        for ty in ty_args.list {
            v.visit_ty(ty);
        }
    }
}
pub fn visit_array_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::ArrayTy<'cx>) {
    v.visit_ty(n.ele);
}
pub fn visit_indexed_access_ty<'cx>(
    v: &mut impl Visitor<'cx>,
    n: &'cx super::IndexedAccessTy<'cx>,
) {
    v.visit_ty(n.ty);
    v.visit_ty(n.index_ty);
}
pub fn visit_fn_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::FnTy<'cx>) {
    // if let Some(ty_params) = n.ty_params {
    //     for ty_param in ty_params {
    //         v.visit_ty_param(ty_param);
    //     }
    // }
    // for param in n.params {
    //     v.visit_param(param);
    // }
    v.visit_ty(n.ty);
}
pub fn visit_ctor_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::CtorTy<'cx>) {
    // for param in n.params {
    //     v.visit_param(param);
    // }
    v.visit_ty(n.ty);
}
pub fn visit_object_lit_ty<'cx>(_: &mut impl Visitor<'cx>, _: &'cx super::ObjectLitTy<'cx>) {
    // for prop in n.members {
    //     v.visit_ty_element(prop);
    // }
}
pub fn visit_lit_ty<'cx>(_: &mut impl Visitor<'cx>, _: &'cx super::LitTy) {
    // Literal types have no child nodes to visit
}
pub fn visit_named_tuple_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::NamedTupleTy<'cx>) {
    v.visit_ty(n.ty);
}
pub fn visit_tuple_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::TupleTy<'cx>) {
    for ty in n.tys {
        v.visit_ty(ty);
    }
}
pub fn visit_rest_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::RestTy<'cx>) {
    v.visit_ty(n.ty);
}
pub fn visit_cond_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::CondTy<'cx>) {
    v.visit_ty(n.check_ty);
    v.visit_ty(n.extends_ty);
    v.visit_ty(n.true_ty);
    v.visit_ty(n.false_ty);
}
pub fn visit_union_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::UnionTy<'cx>) {
    for ty in n.tys {
        v.visit_ty(ty);
    }
}
pub fn visit_intersection_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::IntersectionTy<'cx>) {
    for ty in n.tys {
        v.visit_ty(ty);
    }
}
pub fn visit_typeof_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::TypeofTy<'cx>) {
    if let Some(ty_args) = n.ty_args {
        for ty in ty_args.list {
            v.visit_ty(ty);
        }
    }
}
pub fn visit_mapped_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::MappedTy<'cx>) {
    if let Some(ty) = n.ty {
        v.visit_ty(ty);
    }
    if let Some(name_ty) = n.name_ty {
        v.visit_ty(name_ty);
    }
}
pub fn visit_ty_op_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::TyOp<'cx>) {
    v.visit_ty(n.ty);
}
pub fn visit_pred_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::PredTy<'cx>) {
    use super::PredTyName::*;
    match n.name {
        Ident(ident) => visit_ident(v, ident),
        This(_) => {}
    }
    if let Some(ty) = n.ty {
        v.visit_ty(ty);
    }
}
pub fn visit_paren_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::ParenTy<'cx>) {
    v.visit_ty(n.ty);
}
pub fn visit_infer_ty<'cx>(_: &mut impl Visitor<'cx>, _: &'cx super::InferTy<'cx>) {
    // Infer types have no child nodes to visit
}
pub fn visit_intrinsic_ty<'cx>(_: &mut impl Visitor<'cx>, _: &'cx super::IntrinsicTy) {
    // Intrinsic types have no child nodes to visit
}
pub fn visit_nullable_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::NullableTy<'cx>) {
    v.visit_ty(n.ty);
}
pub fn visit_template_lit_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::TemplateLitTy<'cx>) {
    for span in n.spans {
        v.visit_ty(span.ty);
    }
}
pub fn visit_prop_name<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::PropName<'cx>) {
    match n.kind {
        super::PropNameKind::Ident(ident) => v.visit_ident(ident),
        super::PropNameKind::StringLit { raw, .. } => v.visit_string_lit(raw),
        super::PropNameKind::NumLit(_) => {}
        super::PropNameKind::Computed(expr) => v.visit_computed_prop_name(expr),
    }
}
pub fn visit_computed_prop_name<'cx>(
    v: &mut impl Visitor<'cx>,
    n: &'cx super::ComputedPropName<'cx>,
) {
    v.visit_expr(n.expr);
}
pub fn visit_ident<'cx>(_: &mut impl Visitor<'cx>, _: &'cx super::Ident) {}
pub fn visit_expr<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::Expr<'cx>) {
    use super::ExprKind::*;
    match n.kind {
        ObjectLit(n) => v.visit_object_lit(n),
        ArrowFn(n) => v.visit_arrow_fn_expr(n),
        Bin(n) => v.visit_bin_expr(n),
        _ => {}
    }
}
pub fn visit_object_lit<'cx>(_: &mut impl Visitor<'cx>, _: &'cx super::ObjectLit<'cx>) {}
pub fn visit_try_stmt<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::TryStmt<'cx>) {
    v.visit_block_stmt(n.try_block);
    if let Some(catch) = n.catch_clause {
        if let Some(var) = catch.var {
            v.visit_var_decl(var);
        }
        v.visit_block_stmt(catch.block);
    }
    if let Some(finally) = n.finally_block {
        v.visit_block_stmt(finally);
    }
}
pub fn visit_block_stmt<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::BlockStmt<'cx>) {
    for stmt in n.stmts {
        v.visit_stmt(stmt);
    }
}
pub fn visit_expr_stmt<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::ExprStmt<'cx>) {
    v.visit_expr(n.expr);
}
pub fn visit_arrow_fn_expr<'cx>(_: &mut impl Visitor<'cx>, _: &'cx super::ArrowFnExpr<'cx>) {}
pub fn visit_bin_expr<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::BinExpr<'cx>) {
    v.visit_expr(n.left);
    v.visit_expr(n.right);
}
pub fn visit_string_lit<'cx>(_: &mut impl Visitor<'cx>, _: &'cx super::StringLit) {}
pub fn visit_while_stmt<'cx>(_: &mut impl Visitor<'cx>, _: &'cx super::WhileStmt<'cx>) {}
pub fn visit_if_stmt<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::IfStmt<'cx>) {
    v.visit_expr(n.expr);
    v.visit_stmt(n.then);
    if let Some(else_then) = n.else_then {
        v.visit_stmt(else_then);
    }
}

macro_rules! make_visitor {
    ( $( ($visit_node: ident, $ty: ty) ),* $(,)? ) => {
      pub trait Visitor<'cx>: Sized {
        $(
          fn $visit_node(&mut self, node: &'cx $ty) {
            $visit_node(self, node)
          }
        )*
      }
    };
}

make_visitor!(
    (visit_program, super::Program<'cx>),
    (visit_stmt, super::Stmt<'cx>),
    (visit_import_decl, super::ImportDecl<'cx>),
    (visit_interface_decl, super::InterfaceDecl<'cx>),
    (visit_class_decl, super::ClassDecl<'cx>),
    (visit_class_elem, super::ClassElem<'cx>),
    (visit_class_method_elem, super::ClassMethodElem<'cx>),
    (visit_ident, super::Ident),
    (visit_ty, super::Ty<'cx>),
    (visit_refer_ty, super::ReferTy<'cx>),
    (visit_array_ty, super::ArrayTy<'cx>),
    (visit_indexed_access_ty, super::IndexedAccessTy<'cx>),
    (visit_fn_ty, super::FnTy<'cx>),
    (visit_ctor_ty, super::CtorTy<'cx>),
    (visit_object_lit_ty, super::ObjectLitTy<'cx>),
    (visit_lit_ty, super::LitTy),
    (visit_named_tuple_ty, super::NamedTupleTy<'cx>),
    (visit_tuple_ty, super::TupleTy<'cx>),
    (visit_rest_ty, super::RestTy<'cx>),
    (visit_cond_ty, super::CondTy<'cx>),
    (visit_union_ty, super::UnionTy<'cx>),
    (visit_intersection_ty, super::IntersectionTy<'cx>),
    (visit_typeof_ty, super::TypeofTy<'cx>),
    (visit_mapped_ty, super::MappedTy<'cx>),
    (visit_ty_op_ty, super::TyOp<'cx>),
    (visit_pred_ty, super::PredTy<'cx>),
    (visit_paren_ty, super::ParenTy<'cx>),
    (visit_infer_ty, super::InferTy<'cx>),
    (visit_intrinsic_ty, super::IntrinsicTy),
    (visit_nullable_ty, super::NullableTy<'cx>),
    (visit_template_lit_ty, super::TemplateLitTy<'cx>),
    (visit_var_stmt, super::VarStmt<'cx>),
    (visit_var_decl, super::VarDecl<'cx>),
    (visit_expr, super::Expr<'cx>),
    (visit_object_lit, super::ObjectLit<'cx>),
    (visit_try_stmt, super::TryStmt<'cx>),
    (visit_block_stmt, super::BlockStmt<'cx>),
    (visit_expr_stmt, super::ExprStmt<'cx>),
    (visit_arrow_fn_expr, super::ArrowFnExpr<'cx>),
    (visit_bin_expr, super::BinExpr<'cx>),
    (visit_type_alias_decl, super::TypeAliasDecl<'cx>),
    (visit_module_decl, super::ModuleDecl<'cx>),
    (visit_string_lit, super::StringLit),
    (visit_while_stmt, super::WhileStmt<'cx>),
    (visit_if_stmt, super::IfStmt<'cx>),
    (visit_enum_decl, super::EnumDecl<'cx>),
    (visit_enum_member, super::EnumMember<'cx>),
    (visit_prop_name, super::PropName<'cx>),
    (visit_computed_prop_name, super::ComputedPropName<'cx>),
    (visit_param_decl, super::ParamDecl<'cx>),
    (visit_fn_decl, super::FnDecl<'cx>),
    (visit_binding, super::Binding<'cx>),
);

pub fn visit_node<'cx>(v: &mut impl Visitor<'cx>, node: &super::Node<'cx>) {
    macro_rules! v {
        ($( ($variant: ident, $node: ident ) ),* $(,)?) => {
            match node {
                $(
                    super::Node::$variant(n) => v.$node(n),
                )*
                _ => {}, // TODO: Handle all other variants
            }
        };
    }
    v!((Program, visit_program),);
}
