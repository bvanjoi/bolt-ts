use bolt_ts_ast as ast;

use super::Visitor;
use super::VisitorResult;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ControlFlow {
    Continue,
    Break,
}

impl ControlFlow {
    #[inline]
    pub fn is_break(self) -> bool {
        matches!(self, Self::Break)
    }
}

#[macro_export]
macro_rules! visit_return {
    ($e:expr) => {{
        let result = $e;
        if matches!(
            $crate::VisitorResult::branch(&result),
            $crate::ControlFlow::Break
        ) {
            return result;
        }
    }};
}

pub fn visit_type_parameters<'cx, V: Visitor<'cx>>(
    v: &mut V,
    ty_params: Option<ast::TyParams<'cx>>,
) -> V::Result {
    if let Some(ty_params) = ty_params {
        for ty_param in ty_params {
            visit_return!(v.visit_ty_param(ty_param));
        }
    }
    V::Result::output()
}

pub fn visit_param_decls<'cx, V: Visitor<'cx>>(
    v: &mut V,
    params: ast::ParamsDecl<'cx>,
) -> V::Result {
    for param in params {
        visit_return!(v.visit_param_decl(param));
    }
    V::Result::output()
}

pub fn visit_optional_ty<'cx, V: Visitor<'cx>>(
    v: &mut V,
    ty: Option<&'cx ast::Ty<'cx>>,
) -> V::Result {
    if let Some(ty) = ty {
        return v.visit_ty(ty);
    }
    V::Result::output()
}

pub fn visit_tys<'cx, V: Visitor<'cx>>(v: &mut V, tys: &'cx [&'cx ast::Ty<'cx>]) -> V::Result {
    for ty in tys {
        visit_return!(v.visit_ty(ty));
    }
    V::Result::output()
}

pub fn visit_type_arguments<'cx, V: Visitor<'cx>>(
    v: &mut V,
    ty_args: Option<&'cx ast::Tys<'cx>>,
) -> V::Result {
    if let Some(ty_args) = ty_args {
        visit_tys(v, ty_args.list)
    } else {
        V::Result::output()
    }
}

pub fn visit_stmts<'cx, V: Visitor<'cx>>(v: &mut V, stmts: ast::Stmts<'cx>) -> V::Result {
    for stmt in stmts {
        visit_return!(v.visit_stmt(stmt));
    }
    V::Result::output()
}

pub fn visit_exprs<'cx, V: Visitor<'cx>>(
    v: &mut V,
    exprs: &'cx [&'cx ast::Expr<'cx>],
) -> V::Result {
    for expr in exprs {
        visit_return!(v.visit_expr(expr));
    }
    V::Result::output()
}

pub fn visit_optional_expr<'cx, V: Visitor<'cx>>(
    v: &mut V,
    expr: Option<&'cx ast::Expr<'cx>>,
) -> V::Result {
    if let Some(expr) = expr {
        return v.visit_expr(expr);
    }
    V::Result::output()
}

pub fn visit_optional_block_stmt<'cx, V: Visitor<'cx>>(
    v: &mut V,
    body: Option<&'cx ast::BlockStmt<'cx>>,
) -> V::Result {
    if let Some(body) = body {
        return v.visit_block_stmt(body);
    }
    V::Result::output()
}

pub fn visit_call_like_signature<'cx, V: Visitor<'cx>>(
    v: &mut V,
    ty_params: Option<ast::TyParams<'cx>>,
    params: ast::ParamsDecl<'cx>,
    ty: Option<&'cx ast::Ty<'cx>>,
) -> V::Result {
    visit_return!(visit_type_parameters(v, ty_params));
    visit_return!(visit_param_decls(v, params));
    visit_optional_ty(v, ty)
}

pub fn visit_refer_tys<'cx, V: Visitor<'cx>>(
    v: &mut V,
    tys: &'cx [&'cx ast::ReferTy<'cx>],
) -> V::Result {
    for ty in tys {
        visit_return!(v.visit_refer_ty(ty));
    }
    V::Result::output()
}

pub fn visit_optional_exprs<'cx, V: Visitor<'cx>>(
    v: &mut V,
    exprs: Option<&'cx [&'cx ast::Expr<'cx>]>,
) -> V::Result {
    if let Some(exprs) = exprs {
        visit_exprs(v, exprs)
    } else {
        V::Result::output()
    }
}

pub fn visit_fn_like_with_optional_body<'cx, V: Visitor<'cx>>(
    v: &mut V,
    ty_params: Option<ast::TyParams<'cx>>,
    params: ast::ParamsDecl<'cx>,
    ty: Option<&'cx ast::Ty<'cx>>,
    body: Option<&'cx ast::BlockStmt<'cx>>,
) -> V::Result {
    visit_return!(visit_call_like_signature(v, ty_params, params, ty));
    visit_optional_block_stmt(v, body)
}

pub fn visit_object_ty_members<'cx, V: Visitor<'cx>>(
    v: &mut V,
    members: &'cx [&'cx ast::ObjectTyMember<'cx>],
) -> V::Result {
    for member in members {
        visit_return!(visit_object_ty_member(v, member));
    }
    V::Result::output()
}

pub fn visit_program<'cx, V: Visitor<'cx>>(
    v: &mut V,
    program: &'cx ast::Program<'cx>,
) -> V::Result {
    visit_stmts(v, program.stmts())
}

pub fn visit_export_assign<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ExportAssign<'cx>,
) -> V::Result {
    v.visit_expr(n.expr)
}

pub fn visit_empty_stmt<'cx, V: Visitor<'cx>>(_v: &mut V, _n: &'cx ast::EmptyStmt) -> V::Result {
    V::Result::output()
}

pub fn visit_import_equals_decl<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ImportEqualsDecl<'cx>,
) -> V::Result {
    visit_return!(v.visit_ident(n.name));
    match n.module_reference {
        ast::ModuleReferenceKind::EntityName(n) => v.visit_entity_name(n),
        ast::ModuleReferenceKind::ExternalModuleReference(n) => v.visit_string_lit(n.module_spec()),
    }
}

pub fn visit_stmt<'cx, V: Visitor<'cx>>(v: &mut V, stmt: &'cx ast::Stmt) -> V::Result {
    use ast::StmtKind::*;
    match stmt.kind {
        Block(node) => v.visit_block_stmt(node),
        Class(node) => v.visit_class_decl(node),
        Import(node) => v.visit_import_decl(node),
        ImportEquals(node) => v.visit_import_equals_decl(node),
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
        Ret(node) => v.visit_ret_stmt(node),
        Export(node) => v.visit_export_decl(node),
        ExportAssign(node) => v.visit_export_assign(node),
        Empty(node) => v.visit_empty_stmt(node),
        Throw(node) => v.visit_throw_stmt(node),
        For(node) => v.visit_for_stmt(node),
        ForOf(node) => v.visit_for_of_stmt(node),
        ForIn(node) => v.visit_for_in_stmt(node),
        Break(node) => v.visit_break_stmt(node),
        Continue(node) => v.visit_continue_stmt(node),
        Do(node) => v.visit_do_while_stmt(node),
        Debugger(node) => v.visit_debugger_stmt(node),
        Labeled(node) => v.visit_labeled_stmt(node),
        Switch(node) => v.visit_switch_stmt(node),
    }
}

pub fn visit_binding<'cx, V: Visitor<'cx>>(v: &mut V, node: &'cx ast::Binding<'cx>) -> V::Result {
    use ast::BindingKind::*;
    match node.kind {
        Ident(n) => v.visit_ident(n),
        ObjectPat(n) => v.visit_object_pat(n),
        ArrayPat(n) => v.visit_array_pat(n),
    }
}

pub fn visit_param_decl<'cx, V: Visitor<'cx>>(
    v: &mut V,
    node: &'cx ast::ParamDecl<'cx>,
) -> V::Result {
    visit_return!(v.visit_binding(node.name));
    visit_return!(visit_optional_ty(v, node.ty));
    visit_optional_expr(v, node.init)
}

pub fn visit_fn_decl<'cx, V: Visitor<'cx>>(v: &mut V, node: &'cx ast::FnDecl<'cx>) -> V::Result {
    if let Some(name) = node.name {
        visit_return!(v.visit_ident(name));
    }
    visit_return!(visit_type_parameters(v, node.ty_params));
    visit_return!(visit_param_decls(v, node.params));
    visit_return!(visit_optional_ty(v, node.ty));
    visit_optional_block_stmt(v, node.body)
}

pub fn visit_enum_decl<'cx, V: Visitor<'cx>>(
    v: &mut V,
    enum_decl: &'cx ast::EnumDecl<'cx>,
) -> V::Result {
    visit_return!(v.visit_ident(enum_decl.name));
    for member in enum_decl.members {
        visit_return!(v.visit_enum_member(member));
    }
    V::Result::output()
}

pub fn visit_enum_member_name_kind<'cx, V: Visitor<'cx>>(
    v: &mut V,
    name: &ast::EnumMemberNameKind<'cx>,
) -> V::Result {
    use ast::EnumMemberNameKind::*;
    match name {
        Ident(ident) => v.visit_ident(ident),
        StringLit { raw, .. } => v.visit_string_lit(raw),
    }
}

pub fn visit_enum_member<'cx, V: Visitor<'cx>>(
    v: &mut V,
    member: &'cx ast::EnumMember<'cx>,
) -> V::Result {
    visit_return!(visit_enum_member_name_kind(v, &member.name));
    visit_optional_expr(v, member.init)
}

pub fn visit_var_stmt<'cx, V: Visitor<'cx>>(v: &mut V, stmt: &'cx ast::VarStmt<'cx>) -> V::Result {
    for item in stmt.list {
        visit_return!(v.visit_var_decl(item));
    }
    V::Result::output()
}

pub fn visit_type_alias_decl<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::TypeAliasDecl<'cx>,
) -> V::Result {
    visit_return!(v.visit_ident(n.name));
    visit_return!(visit_type_parameters(v, n.ty_params));
    v.visit_ty(n.ty)
}

pub fn visit_module_decl<'cx, V: Visitor<'cx>>(
    v: &mut V,
    decl: &'cx ast::ModuleDecl<'cx>,
) -> V::Result {
    visit_return!(visit_module_name(v, decl.name));
    if let Some(block) = decl.block {
        return v.visit_module_block(block);
    }
    V::Result::output()
}

pub fn visit_module_name<'cx, V: Visitor<'cx>>(v: &mut V, name: ast::ModuleName<'cx>) -> V::Result {
    match name {
        ast::ModuleName::Ident(ident) => v.visit_ident(ident),
        ast::ModuleName::StringLit(lit) => v.visit_string_lit(lit),
    }
}

pub fn visit_var_decl<'cx, V: Visitor<'cx>>(v: &mut V, decl: &'cx ast::VarDecl<'cx>) -> V::Result {
    visit_return!(v.visit_binding(decl.name));
    visit_return!(visit_optional_ty(v, decl.ty));
    visit_optional_expr(v, decl.init)
}

pub fn visit_class_decl<'cx, V: Visitor<'cx>>(
    v: &mut V,
    class: &'cx ast::ClassDecl<'cx>,
) -> V::Result {
    if let Some(name) = class.name {
        visit_return!(v.visit_ident(name));
    }
    visit_return!(visit_type_parameters(v, class.ty_params));
    if let Some(extends) = class.extends {
        visit_return!(v.visit_class_extends_clause(extends));
    }
    if let Some(implements) = class.implements {
        visit_return!(v.visit_class_implements_clause(implements));
    }
    for ele in class.elems.list {
        visit_return!(v.visit_class_elem(ele));
    }
    V::Result::output()
}

pub fn visit_interface_decl<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::InterfaceDecl<'cx>,
) -> V::Result {
    visit_return!(v.visit_ident(n.name));
    visit_return!(visit_type_parameters(v, n.ty_params));
    if let Some(extends) = n.extends {
        visit_return!(v.visit_interface_extends_clause(extends));
    }
    visit_object_ty_members(v, n.members)
}

fn visit_object_ty_member<'cx, V: Visitor<'cx>>(
    v: &mut V,
    member: &'cx ast::ObjectTyMember<'cx>,
) -> V::Result {
    use ast::ObjectTyMemberKind::*;
    match member.kind {
        IndexSig(n) => v.visit_index_sig_decl(n),
        Prop(n) => v.visit_prop_signature(n),
        Method(n) => v.visit_method_signature(n),
        CallSig(n) => v.visit_call_sig_decl(n),
        CtorSig(n) => v.visit_ctor_sig_decl(n),
        Setter(n) => v.visit_setter_decl(n),
        Getter(n) => v.visit_getter_decl(n),
    }
}

pub fn visit_prop_signature<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::PropSignature<'cx>,
) -> V::Result {
    visit_return!(v.visit_prop_name(n.name));
    visit_optional_ty(v, n.ty)
}

pub fn visit_method_signature<'cx, V: Visitor<'cx>>(
    v: &mut V,
    node: &'cx ast::MethodSignature<'cx>,
) -> V::Result {
    visit_return!(v.visit_prop_name(node.name));
    visit_call_like_signature(v, node.ty_params, node.params, node.ty)
}

pub fn visit_import_decl<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ImportDecl<'cx>,
) -> V::Result {
    if let Some(clause) = n.clause {
        visit_return!(v.visit_import_clause(clause));
    }
    v.visit_string_lit(n.module)
}

pub fn visit_class_elem<'cx, V: Visitor<'cx>>(
    v: &mut V,
    elem: &'cx ast::ClassElem<'cx>,
) -> V::Result {
    use ast::ClassElemKind::*;
    match elem.kind {
        Ctor(n) => v.visit_class_ctor(n),
        Prop(n) => v.visit_class_prop_elem(n),
        Method(n) => v.visit_class_method_elem(n),
        IndexSig(n) => v.visit_index_sig_decl(n),
        Getter(n) => v.visit_getter_decl(n),
        Setter(n) => v.visit_setter_decl(n),
        StaticBlockDecl(n) => v.visit_class_static_block_decl(n),
        Semi(n) => v.visit_class_semi_elem(n),
    }
}

pub fn visit_class_ctor<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::ClassCtor<'cx>) -> V::Result {
    visit_return!(visit_param_decls(v, n.params));
    visit_return!(visit_optional_ty(v, n.ret));
    visit_optional_block_stmt(v, n.body)
}

pub fn visit_index_sig_decl<'cx, V: Visitor<'cx>>(
    v: &mut V,
    node: &'cx ast::IndexSigDecl<'cx>,
) -> V::Result {
    visit_return!(v.visit_binding(node.key));
    visit_return!(v.visit_ty(node.key_ty));
    v.visit_ty(node.ty)
}

pub fn visit_class_prop_elem<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ClassPropElem<'cx>,
) -> V::Result {
    visit_return!(v.visit_prop_name(n.name));
    visit_return!(visit_optional_ty(v, n.ty));
    visit_optional_expr(v, n.init)
}

pub fn visit_class_method_elem<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ClassMethodElem<'cx>,
) -> V::Result {
    visit_return!(v.visit_prop_name(n.name));
    visit_fn_like_with_optional_body(v, n.ty_params, n.params, n.ty, n.body)
}

pub fn visit_ty<'cx, V: Visitor<'cx>>(v: &mut V, ty: &'cx ast::Ty<'cx>) -> V::Result {
    use ast::TyKind::*;
    match ty.kind {
        Refer(n) => v.visit_refer_ty(n),
        Array(n) => v.visit_array_ty(n),
        IndexedAccess(n) => v.visit_indexed_access_ty(n),
        Fn(n) => v.visit_fn_ty(n),
        Ctor(n) => v.visit_ctor_ty(n),
        ObjectLit(n) => v.visit_object_lit_ty(n),
        Lit(n) => v.visit_lit_ty(n),
        NamedTuple(n) => v.visit_named_tuple_ty(n),
        Tuple(n) => v.visit_tuple_ty(n),
        Rest(n) => v.visit_rest_ty(n),
        Cond(n) => v.visit_cond_ty(n),
        Union(n) => v.visit_union_ty(n),
        Intersection(n) => v.visit_intersection_ty(n),
        Typeof(n) => v.visit_typeof_ty(n),
        Mapped(n) => v.visit_mapped_ty(n),
        TypeOp(n) => v.visit_ty_op_ty(n),
        Pred(n) => v.visit_pred_ty(n),
        Paren(n) => v.visit_paren_ty(n),
        Infer(n) => v.visit_infer_ty(n),
        Intrinsic(n) => v.visit_intrinsic_ty(n),
        Nullable(n) => v.visit_nullable_ty(n),
        TemplateLit(n) => v.visit_template_lit_ty(n),
        This(n) => v.visit_this_ty(n),
        Import(n) => v.visit_import_type(n),
    }
}

pub fn visit_this_ty<'cx, V: Visitor<'cx>>(_v: &mut V, _n: &'cx ast::ThisTy) -> V::Result {
    V::Result::output()
}

pub fn visit_yield_expr<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::YieldExpr<'cx>) -> V::Result {
    visit_optional_expr(v, n.expr)
}

pub fn visit_refer_ty<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::ReferTy<'cx>) -> V::Result {
    visit_return!(v.visit_entity_name(n.name));
    visit_type_arguments(v, n.ty_args)
}

pub fn visit_array_ty<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::ArrayTy<'cx>) -> V::Result {
    v.visit_ty(n.ele)
}

pub fn visit_indexed_access_ty<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::IndexedAccessTy<'cx>,
) -> V::Result {
    visit_return!(v.visit_ty(n.ty));
    v.visit_ty(n.index_ty)
}

pub fn visit_fn_ty<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::FnTy<'cx>) -> V::Result {
    visit_call_like_signature(v, n.ty_params, n.params, Some(n.ty))
}

pub fn visit_ctor_ty<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::CtorTy<'cx>) -> V::Result {
    visit_call_like_signature(v, n.ty_params, n.params, Some(n.ty))
}

pub fn visit_object_lit_ty<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ObjectLitTy<'cx>,
) -> V::Result {
    visit_object_ty_members(v, n.members)
}

pub fn visit_call_sig_decl<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::CallSigDecl<'cx>,
) -> V::Result {
    visit_call_like_signature(v, n.ty_params, n.params, n.ty)
}

pub fn visit_ctor_sig_decl<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::CtorSigDecl<'cx>,
) -> V::Result {
    visit_call_like_signature(v, n.ty_params, n.params, n.ty)
}

pub fn visit_setter_decl<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::SetterDecl<'cx>,
) -> V::Result {
    visit_return!(v.visit_prop_name(n.name));
    visit_return!(visit_param_decls(v, n.params));
    visit_optional_block_stmt(v, n.body)
}

pub fn visit_getter_decl<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::GetterDecl<'cx>,
) -> V::Result {
    visit_return!(v.visit_prop_name(n.name));
    visit_return!(visit_optional_ty(v, n.ty));
    visit_optional_block_stmt(v, n.body)
}

pub fn visit_lit_ty<'cx, V: Visitor<'cx>>(_v: &mut V, n: &'cx ast::LitTy) -> V::Result {
    use ast::LitTyKind::*;
    match n.kind {
        Null | True | False | Undefined | Void | Num(_) | String(_) | BigInt { .. } => {}
    }
    V::Result::output()
}

pub fn visit_named_tuple_ty<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::NamedTupleTy<'cx>,
) -> V::Result {
    visit_return!(v.visit_ident(n.name));
    v.visit_ty(n.ty)
}

pub fn visit_tuple_ty<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::TupleTy<'cx>) -> V::Result {
    visit_tys(v, n.tys)
}

pub fn visit_rest_ty<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::RestTy<'cx>) -> V::Result {
    v.visit_ty(n.ty)
}

pub fn visit_cond_ty<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::CondTy<'cx>) -> V::Result {
    visit_return!(v.visit_ty(n.check_ty));
    visit_return!(v.visit_ty(n.extends_ty));
    visit_return!(v.visit_ty(n.true_ty));
    v.visit_ty(n.false_ty)
}

pub fn visit_union_ty<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::UnionTy<'cx>) -> V::Result {
    visit_tys(v, n.tys)
}

pub fn visit_intersection_ty<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::IntersectionTy<'cx>,
) -> V::Result {
    visit_tys(v, n.tys)
}

pub fn visit_typeof_ty<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::TypeofTy<'cx>) -> V::Result {
    visit_return!(v.visit_entity_name(n.name));
    visit_type_arguments(v, n.ty_args)
}

pub fn visit_mapped_ty<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::MappedTy<'cx>) -> V::Result {
    visit_return!(v.visit_ty_param(n.ty_param));
    if let Some(name_ty) = n.name_ty {
        visit_return!(v.visit_ty(name_ty));
    }
    visit_optional_ty(v, n.ty)
}

pub fn visit_ty_op_ty<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::TypeOp<'cx>) -> V::Result {
    v.visit_ty(n.ty)
}

pub fn visit_pred_ty<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::PredTy<'cx>) -> V::Result {
    use ast::PredTyName::*;
    match n.name {
        Ident(ident) => visit_return!(v.visit_ident(ident)),
        This(n) => visit_return!(v.visit_this_ty(n)),
    }
    visit_optional_ty(v, n.ty)
}

pub fn visit_paren_ty<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::ParenTy<'cx>) -> V::Result {
    v.visit_ty(n.ty)
}

pub fn visit_infer_ty<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::InferTy<'cx>) -> V::Result {
    v.visit_ty_param(n.ty_param)
}

pub fn visit_intrinsic_ty<'cx, V: Visitor<'cx>>(
    _v: &mut V,
    _n: &'cx ast::IntrinsicTy,
) -> V::Result {
    V::Result::output()
}

pub fn visit_nullable_ty<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::NullableTy<'cx>,
) -> V::Result {
    v.visit_ty(n.ty)
}

pub fn visit_template_lit_ty<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::TemplateLitTy<'cx>,
) -> V::Result {
    visit_return!(v.visit_template_head(n.head));
    for span in n.spans {
        visit_return!(v.visit_template_span_ty(span));
    }
    V::Result::output()
}

pub fn visit_prop_name<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::PropName<'cx>) -> V::Result {
    match n.kind {
        ast::PropNameKind::Ident(ident) => v.visit_ident(ident),
        ast::PropNameKind::StringLit { raw, .. } => v.visit_string_lit(raw),
        ast::PropNameKind::Computed(expr) => v.visit_computed_prop_name(expr),
        ast::PropNameKind::PrivateIdent(n) => v.visit_private_ident(n),
        ast::PropNameKind::NumLit(n) => v.visit_num_lit(n),
        ast::PropNameKind::BigIntLit(n) => v.visit_big_int_lit(n),
    }
}

pub fn visit_computed_prop_name<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ComputedPropName<'cx>,
) -> V::Result {
    v.visit_expr(n.expr)
}

pub fn visit_ident<'cx, V: Visitor<'cx>>(_v: &mut V, _n: &'cx ast::Ident) -> V::Result {
    V::Result::output()
}

pub fn visit_private_ident<'cx, V: Visitor<'cx>>(
    _v: &mut V,
    _n: &'cx ast::PrivateIdent,
) -> V::Result {
    V::Result::output()
}

pub fn visit_entity_name<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::EntityName) -> V::Result {
    match n.kind {
        ast::EntityNameKind::Ident(node) => v.visit_ident(node),
        ast::EntityNameKind::Qualified(node) => {
            visit_return!(v.visit_entity_name(node.left));
            v.visit_ident(node.right)
        }
    }
}

pub fn visit_expr<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::Expr<'cx>) -> V::Result {
    use ast::ExprKind::*;
    match n.kind {
        Assign(n) => v.visit_assign_expr(n),
        Bin(n) => v.visit_bin_expr(n),
        This(n) => v.visit_this_expr(n),
        BoolLit(n) => v.visit_bool_lit(n),
        NumLit(n) => v.visit_num_lit(n),
        BigIntLit(n) => v.visit_big_int_lit(n),
        StringLit(n) => v.visit_string_lit(n),
        NoSubstitutionTemplateLit(n) => v.visit_no_substitution_template_lit(n),
        NullLit(n) => v.visit_null_lit(n),
        RegExpLit(n) => v.visit_reg_exp_lit(n),
        ArrayLit(n) => v.visit_array_lit(n),
        Import(n) => v.visit_import_expression(n),
        Ident(n) => v.visit_ident(n),
        Omit(n) => v.visit_omit_expr(n),
        Paren(n) => v.visit_paren_expr(n),
        Cond(n) => v.visit_cond_expr(n),
        ObjectLit(n) => v.visit_object_lit(n),
        ExprWithTyArgs(n) => v.visit_expr_with_ty_args(n),
        Call(n) => v.visit_call_expr(n),
        Fn(n) => v.visit_fn_expr(n),
        Class(n) => v.visit_class_expr(n),
        New(n) => v.visit_new_expr(n),
        ArrowFn(n) => v.visit_arrow_fn_expr(n),
        PrefixUnary(n) => v.visit_prefix_unary_expr(n),
        PostfixUnary(n) => v.visit_postfix_unary_expr(n),
        PropAccess(n) => v.visit_prop_access_expr(n),
        EleAccess(n) => v.visit_ele_access_expr(n),
        Super(n) => v.visit_super_expr(n),
        Typeof(n) => v.visit_typeof_expr(n),
        Void(n) => v.visit_void_expr(n),
        Await(n) => v.visit_await_expr(n),
        Yield(n) => v.visit_yield_expr(n),
        As(n) => v.visit_as_expr(n),
        Satisfies(n) => v.visit_satisfies_expr(n),
        NonNull(n) => v.visit_non_null_expr(n),
        Template(n) => v.visit_template_expr(n),
        TaggedTemplate(n) => v.visit_tagged_template_expr(n),
        Delete(n) => v.visit_delete_expr(n),
        TyAssertion(n) => v.visit_ty_assertion_expr(n),
        SpreadElement(n) => v.visit_spread_element(n),
        JsxElem(n) => v.visit_jsx_elem(n),
        JsxSelfClosingElem(n) => v.visit_jsx_self_closing_elem(n),
        JsxFrag(n) => v.visit_jsx_frag(n),
        NewMetaProperty(n) => v.visit_new_meta_property(n),
    }
}

pub fn visit_paren_expr<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::ParenExpr<'cx>) -> V::Result {
    v.visit_expr(n.expr)
}

pub fn visit_call_expr<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::CallExpr<'cx>) -> V::Result {
    visit_return!(v.visit_expr(n.expr));
    visit_return!(visit_type_arguments(v, n.ty_args));
    visit_exprs(v, n.args)
}

pub fn visit_object_lit<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::ObjectLit<'cx>) -> V::Result {
    for member in n.members {
        use ast::ObjectMemberKind::*;
        visit_return!(match member.kind {
            PropAssignment(n) => v.visit_object_prop_assignment(n),
            Method(n) => v.visit_object_method_member(n),
            Shorthand(n) => v.visit_object_shorthand_member(n),
            SpreadAssignment(n) => v.visit_spread_assignment(n),
            Getter(n) => v.visit_getter_decl(n),
            Setter(n) => v.visit_setter_decl(n),
        });
    }
    V::Result::output()
}

pub fn visit_object_method_member<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ObjectMethodMember<'cx>,
) -> V::Result {
    visit_return!(v.visit_prop_name(n.name));
    visit_return!(visit_call_like_signature(v, n.ty_params, n.params, n.ty));
    v.visit_block_stmt(n.body)
}

pub fn visit_try_stmt<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::TryStmt<'cx>) -> V::Result {
    visit_return!(v.visit_block_stmt(n.try_block));
    if let Some(catch) = n.catch_clause {
        visit_return!(v.visit_catch_clause(catch));
    }
    if let Some(finally) = n.finally_block {
        return v.visit_block_stmt(finally);
    }
    V::Result::output()
}

pub fn visit_block_stmt<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::BlockStmt<'cx>) -> V::Result {
    visit_stmts(v, n.stmts)
}

pub fn visit_expr_stmt<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::ExprStmt<'cx>) -> V::Result {
    v.visit_expr(n.expr)
}

pub fn visit_arrow_fn_expr<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ArrowFnExpr<'cx>,
) -> V::Result {
    visit_return!(visit_call_like_signature(v, n.ty_params, n.params, n.ty));
    match n.body {
        ast::ArrowFnExprBody::Expr(expr) => v.visit_expr(expr),
        ast::ArrowFnExprBody::Block(block) => v.visit_block_stmt(block),
    }
}

pub fn visit_bin_expr<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::BinExpr<'cx>) -> V::Result {
    visit_return!(v.visit_expr(n.left));
    v.visit_expr(n.right)
}

pub fn visit_string_lit<'cx, V: Visitor<'cx>>(_v: &mut V, _n: &'cx ast::StringLit) -> V::Result {
    V::Result::output()
}

pub fn visit_num_lit<'cx, V: Visitor<'cx>>(_v: &mut V, _n: &'cx ast::NumLit) -> V::Result {
    V::Result::output()
}

pub fn visit_while_stmt<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::WhileStmt<'cx>) -> V::Result {
    visit_return!(v.visit_expr(n.expr));
    v.visit_stmt(n.stmt)
}

pub fn visit_if_stmt<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::IfStmt<'cx>) -> V::Result {
    visit_return!(v.visit_expr(n.expr));
    visit_return!(v.visit_stmt(n.then));
    if let Some(else_then) = n.else_then {
        return v.visit_stmt(else_then);
    }
    V::Result::output()
}

pub fn visit_ty_param<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::TyParam<'cx>) -> V::Result {
    visit_return!(v.visit_ident(n.name));
    visit_return!(visit_optional_ty(v, n.constraint));
    visit_optional_ty(v, n.default)
}

pub fn visit_ret_stmt<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::RetStmt<'cx>) -> V::Result {
    visit_optional_expr(v, n.expr)
}

pub fn visit_assign_expr<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::AssignExpr<'cx>,
) -> V::Result {
    visit_return!(v.visit_expr(n.left));
    v.visit_expr(n.right)
}

pub fn visit_cond_expr<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::CondExpr<'cx>) -> V::Result {
    visit_return!(v.visit_expr(n.cond));
    visit_return!(v.visit_expr(n.when_true));
    v.visit_expr(n.when_false)
}

pub fn visit_expr_with_ty_args<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ExprWithTyArgs<'cx>,
) -> V::Result {
    visit_return!(v.visit_expr(n.expr));
    visit_type_arguments(v, n.ty_args)
}

pub fn visit_prop_access_expr<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::PropAccessExpr<'cx>,
) -> V::Result {
    visit_return!(v.visit_expr(n.expr));
    v.visit_ident(n.name)
}

pub fn visit_export_named_spec<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ExportNamedSpec<'cx>,
) -> V::Result {
    visit_return!(v.visit_module_export_name(n.name));
    v.visit_module_export_name(n.prop_name)
}

pub fn visit_export_shorthand_spec<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ExportShorthandSpec<'cx>,
) -> V::Result {
    v.visit_ident(n.name)
}

fn visit_for_init_kind<'cx, V: Visitor<'cx>>(v: &mut V, init: ast::ForInitKind<'cx>) -> V::Result {
    match init {
        ast::ForInitKind::Var(decls) => {
            for decl in decls {
                visit_return!(v.visit_var_decl(decl));
            }
            V::Result::output()
        }
        ast::ForInitKind::Expr(expr) => v.visit_expr(expr),
    }
}

fn visit_export_clause<'cx, V: Visitor<'cx>>(
    v: &mut V,
    clause: &'cx ast::ExportClause<'cx>,
) -> V::Result {
    match clause.kind {
        ast::ExportClauseKind::Glob(n) => v.visit_glob_export(n),
        ast::ExportClauseKind::Ns(n) => v.visit_ns_export(n),
        ast::ExportClauseKind::Specs(n) => v.visit_specs_export(n),
    }
}

pub fn visit_modifier<'cx, V: Visitor<'cx>>(_v: &mut V, _n: &'cx ast::Modifier) -> V::Result {
    V::Result::output()
}

pub fn visit_class_extends_clause<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ClassExtendsClause<'cx>,
) -> V::Result {
    v.visit_expr_with_ty_args(n.expr_with_ty_args)
}

pub fn visit_import_shorthand_spec<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ImportShorthandSpec<'cx>,
) -> V::Result {
    v.visit_ident(n.name)
}

pub fn visit_ns_import<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::NsImport<'cx>) -> V::Result {
    v.visit_ident(n.name)
}

pub fn visit_module_export_name<'cx, V: Visitor<'cx>>(
    v: &mut V,
    name: &'cx ast::ModuleExportName<'cx>,
) -> V::Result {
    match name.kind {
        ast::ModuleExportNameKind::Ident(n) => v.visit_ident(n),
        ast::ModuleExportNameKind::StringLit(n) => v.visit_string_lit(n),
    }
}

pub fn visit_ns_export<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::NsExport<'cx>) -> V::Result {
    visit_return!(v.visit_module_export_name(n.name));
    v.visit_string_lit(n.module)
}

pub fn visit_glob_export<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::GlobExport<'cx>,
) -> V::Result {
    v.visit_string_lit(n.module)
}

pub fn visit_specs_export<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::SpecsExport<'cx>,
) -> V::Result {
    for spec in n.list {
        match spec.kind {
            ast::ExportSpecKind::Shorthand(n) => visit_return!(v.visit_export_shorthand_spec(n)),
            ast::ExportSpecKind::Named(n) => visit_return!(v.visit_export_named_spec(n)),
        }
    }
    if let Some(module) = n.module {
        return v.visit_string_lit(module);
    }
    V::Result::output()
}

pub fn visit_import_named_spec<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ImportNamedSpec<'cx>,
) -> V::Result {
    visit_return!(v.visit_module_export_name(n.prop_name));
    v.visit_ident(n.name)
}

pub fn visit_import_clause<'cx, V: Visitor<'cx>>(
    v: &mut V,
    clause: &'cx ast::ImportClause<'cx>,
) -> V::Result {
    if let Some(name) = clause.name {
        visit_return!(v.visit_ident(name));
    }
    if let Some(kind) = clause.kind {
        match kind {
            ast::ImportClauseKind::Ns(n) => v.visit_ns_import(n),
            ast::ImportClauseKind::Specs(specs) => {
                for spec in specs {
                    match spec.kind {
                        ast::ImportSpecKind::Shorthand(n) => {
                            visit_return!(v.visit_import_shorthand_spec(n));
                        }
                        ast::ImportSpecKind::Named(n) => {
                            visit_return!(v.visit_import_named_spec(n));
                        }
                    }
                }
                V::Result::output()
            }
        }
    } else {
        V::Result::output()
    }
}

pub fn visit_object_pat<'cx, V: Visitor<'cx>>(
    v: &mut V,
    pat: &'cx ast::ObjectPat<'cx>,
) -> V::Result {
    for elem in pat.elems {
        visit_return!(v.visit_object_binding_elem(elem));
    }
    V::Result::output()
}

pub fn visit_object_binding_elem<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ObjectBindingElem<'cx>,
) -> V::Result {
    visit_return!(visit_optional_expr(v, n.init));
    match &n.name {
        ast::ObjectBindingName::Shorthand(ident) => v.visit_ident(ident),
        ast::ObjectBindingName::Prop { prop_name, name } => {
            visit_return!(v.visit_prop_name(prop_name));
            v.visit_binding(name)
        }
    }
}

pub fn visit_array_pat<'cx, V: Visitor<'cx>>(v: &mut V, pat: &'cx ast::ArrayPat<'cx>) -> V::Result {
    for elem in pat.elems {
        match elem.kind {
            ast::ArrayBindingElemKind::Omit(_) => {}
            ast::ArrayBindingElemKind::Binding(binding) => {
                visit_return!(v.visit_array_binding(binding));
            }
        }
    }
    V::Result::output()
}

pub fn visit_array_binding<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ArrayBinding<'cx>,
) -> V::Result {
    visit_return!(v.visit_binding(n.name));
    visit_optional_expr(v, n.init)
}

pub fn visit_object_shorthand_member<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ObjectShorthandMember<'cx>,
) -> V::Result {
    visit_return!(v.visit_ident(n.name));
    visit_optional_expr(v, n.object_assignment_initializer)
}

pub fn visit_object_prop_assignment<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ObjectPropAssignment<'cx>,
) -> V::Result {
    visit_return!(v.visit_prop_name(n.name));
    v.visit_expr(n.init)
}

pub fn visit_spread_assignment<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::SpreadAssignment<'cx>,
) -> V::Result {
    v.visit_expr(n.expr)
}

pub fn visit_spread_element<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::SpreadElement<'cx>,
) -> V::Result {
    v.visit_expr(n.expr)
}

pub fn visit_template_head<'cx, V: Visitor<'cx>>(
    _v: &mut V,
    _n: &'cx ast::TemplateHead,
) -> V::Result {
    V::Result::output()
}

pub fn visit_template_span<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::TemplateSpan<'cx>,
) -> V::Result {
    v.visit_expr(n.expr)
}

pub fn visit_case_clause<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::CaseClause<'cx>,
) -> V::Result {
    visit_return!(v.visit_expr(n.expr));
    visit_stmts(v, n.stmts)
}

pub fn visit_default_clause<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::DefaultClause<'cx>,
) -> V::Result {
    visit_stmts(v, n.stmts)
}

pub fn visit_case_block<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::CaseBlock<'cx>) -> V::Result {
    for clause in n.clauses {
        match clause {
            ast::CaseOrDefaultClause::Case(n) => visit_return!(v.visit_case_clause(n)),
            ast::CaseOrDefaultClause::Default(n) => visit_return!(v.visit_default_clause(n)),
        }
    }
    V::Result::output()
}

pub fn visit_class_semi_elem<'cx, V: Visitor<'cx>>(
    _v: &mut V,
    _n: &'cx ast::ClassSemiElem,
) -> V::Result {
    V::Result::output()
}

pub fn visit_class_static_block_decl<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ClassStaticBlockDecl<'cx>,
) -> V::Result {
    v.visit_block_stmt(n.body)
}

pub fn visit_interface_extends_clause<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::InterfaceExtendsClause<'cx>,
) -> V::Result {
    visit_refer_tys(v, n.list)
}

pub fn visit_class_implements_clause<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ClassImplementsClause<'cx>,
) -> V::Result {
    visit_refer_tys(v, n.list)
}

pub fn visit_module_block<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ModuleBlock<'cx>,
) -> V::Result {
    visit_stmts(v, n.stmts)
}

pub fn visit_throw_stmt<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::ThrowStmt<'cx>) -> V::Result {
    v.visit_expr(n.expr)
}

pub fn visit_external_module_reference<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ExternalModuleReference<'cx>,
) -> V::Result {
    v.visit_string_lit(n.module_spec())
}

pub fn visit_export_decl<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ExportDecl<'cx>,
) -> V::Result {
    visit_export_clause(v, n.clause)
}

pub fn visit_for_stmt<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::ForStmt<'cx>) -> V::Result {
    if let Some(init) = n.init {
        visit_return!(visit_for_init_kind(v, init));
    }
    if let Some(cond) = n.cond {
        visit_return!(v.visit_expr(cond));
    }
    if let Some(incr) = n.incr {
        visit_return!(v.visit_expr(incr));
    }
    v.visit_stmt(n.body)
}

pub fn visit_for_in_stmt<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ForInStmt<'cx>,
) -> V::Result {
    visit_return!(visit_for_init_kind(v, n.init));
    visit_return!(v.visit_expr(n.expr));
    v.visit_stmt(n.body)
}

pub fn visit_for_of_stmt<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ForOfStmt<'cx>,
) -> V::Result {
    visit_return!(visit_for_init_kind(v, n.init));
    visit_return!(v.visit_expr(n.expr));
    v.visit_stmt(n.body)
}

pub fn visit_do_while_stmt<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::DoWhileStmt<'cx>,
) -> V::Result {
    visit_return!(v.visit_stmt(n.stmt));
    v.visit_expr(n.expr)
}

pub fn visit_break_stmt<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::BreakStmt<'cx>) -> V::Result {
    if let Some(label) = n.label {
        v.visit_ident(label)
    } else {
        V::Result::output()
    }
}

pub fn visit_continue_stmt<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ContinueStmt<'cx>,
) -> V::Result {
    if let Some(label) = n.label {
        v.visit_ident(label)
    } else {
        V::Result::output()
    }
}

pub fn visit_catch_clause<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::CatchClause<'cx>,
) -> V::Result {
    if let Some(var) = n.var {
        visit_return!(v.visit_var_decl(var));
    }
    v.visit_block_stmt(n.block)
}

pub fn visit_debugger_stmt<'cx, V: Visitor<'cx>>(
    _v: &mut V,
    _n: &'cx ast::DebuggerStmt,
) -> V::Result {
    V::Result::output()
}

pub fn visit_labeled_stmt<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::LabeledStmt<'cx>,
) -> V::Result {
    visit_return!(v.visit_ident(n.label));
    v.visit_stmt(n.stmt)
}

pub fn visit_switch_stmt<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::SwitchStmt<'cx>,
) -> V::Result {
    visit_return!(v.visit_expr(n.expr));
    v.visit_case_block(n.case_block)
}

pub fn visit_omit_expr<'cx, V: Visitor<'cx>>(_v: &mut V, _n: &'cx ast::OmitExpr) -> V::Result {
    V::Result::output()
}

pub fn visit_fn_expr<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::FnExpr<'cx>) -> V::Result {
    if let Some(name) = n.name {
        visit_return!(v.visit_ident(name));
    }
    visit_return!(visit_call_like_signature(v, n.ty_params, n.params, n.ty));
    v.visit_block_stmt(n.body)
}

pub fn visit_class_expr<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::ClassExpr<'cx>) -> V::Result {
    if let Some(name) = n.name {
        visit_return!(v.visit_ident(name));
    }
    visit_return!(visit_type_parameters(v, n.ty_params));
    if let Some(extends) = n.extends {
        visit_return!(v.visit_class_extends_clause(extends));
    }
    if let Some(implements) = n.implements {
        visit_return!(v.visit_class_implements_clause(implements));
    }
    for ele in n.elems.list {
        visit_return!(v.visit_class_elem(ele));
    }
    V::Result::output()
}

pub fn visit_new_expr<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::NewExpr<'cx>) -> V::Result {
    visit_return!(v.visit_expr(n.expr));
    visit_return!(visit_type_arguments(v, n.ty_args));
    visit_optional_exprs(v, n.args)
}

pub fn visit_prefix_unary_expr<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::PrefixUnaryExpr<'cx>,
) -> V::Result {
    v.visit_expr(n.expr)
}

pub fn visit_postfix_unary_expr<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::PostfixUnaryExpr<'cx>,
) -> V::Result {
    v.visit_expr(n.expr)
}

pub fn visit_ele_access_expr<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::EleAccessExpr<'cx>,
) -> V::Result {
    visit_return!(v.visit_expr(n.expr));
    v.visit_expr(n.arg)
}

pub fn visit_this_expr<'cx, V: Visitor<'cx>>(_v: &mut V, _n: &'cx ast::ThisExpr) -> V::Result {
    V::Result::output()
}

pub fn visit_typeof_expr<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::TypeofExpr<'cx>,
) -> V::Result {
    v.visit_expr(n.expr)
}

pub fn visit_void_expr<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::VoidExpr<'cx>) -> V::Result {
    v.visit_expr(n.expr)
}

pub fn visit_await_expr<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::AwaitExpr<'cx>) -> V::Result {
    v.visit_expr(n.expr)
}

pub fn visit_super_expr<'cx, V: Visitor<'cx>>(_v: &mut V, _n: &'cx ast::SuperExpr) -> V::Result {
    V::Result::output()
}

pub fn visit_as_expr<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::AsExpr<'cx>) -> V::Result {
    visit_return!(v.visit_expr(n.expr));
    v.visit_ty(n.ty)
}

pub fn visit_ty_assertion_expr<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::TyAssertion<'cx>,
) -> V::Result {
    visit_return!(v.visit_expr(n.expr));
    v.visit_ty(n.ty)
}

pub fn visit_satisfies_expr<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::SatisfiesExpr<'cx>,
) -> V::Result {
    visit_return!(v.visit_expr(n.expr));
    v.visit_ty(n.ty)
}

pub fn visit_non_null_expr<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::NonNullExpr<'cx>,
) -> V::Result {
    v.visit_expr(n.expr)
}

pub fn visit_template_expr<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::TemplateExpr<'cx>,
) -> V::Result {
    visit_return!(v.visit_template_head(n.head));
    for span in n.spans {
        visit_return!(v.visit_template_span(span));
    }
    V::Result::output()
}

pub fn visit_tagged_template_expr<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::TaggedTemplateExpr<'cx>,
) -> V::Result {
    visit_return!(v.visit_expr(n.tag));
    visit_return!(visit_type_arguments(v, n.ty_args));
    v.visit_expr(n.tpl)
}

pub fn visit_delete_expr<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::DeleteExpr<'cx>,
) -> V::Result {
    v.visit_expr(n.expr)
}

pub fn visit_import_expression<'cx, V: Visitor<'cx>>(
    _v: &mut V,
    _n: &'cx ast::ImportExpression,
) -> V::Result {
    V::Result::output()
}

pub fn visit_big_int_lit<'cx, V: Visitor<'cx>>(_v: &mut V, _n: &'cx ast::BigIntLit) -> V::Result {
    V::Result::output()
}

pub fn visit_bool_lit<'cx, V: Visitor<'cx>>(_v: &mut V, _n: &'cx ast::BoolLit) -> V::Result {
    V::Result::output()
}

pub fn visit_null_lit<'cx, V: Visitor<'cx>>(_v: &mut V, _n: &'cx ast::NullLit) -> V::Result {
    V::Result::output()
}

pub fn visit_reg_exp_lit<'cx, V: Visitor<'cx>>(_v: &mut V, _n: &'cx ast::RegExpLit) -> V::Result {
    V::Result::output()
}

pub fn visit_no_substitution_template_lit<'cx, V: Visitor<'cx>>(
    _v: &mut V,
    _n: &'cx ast::NoSubstitutionTemplateLit,
) -> V::Result {
    V::Result::output()
}

pub fn visit_array_lit<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::ArrayLit<'cx>) -> V::Result {
    visit_exprs(v, n.elems)
}

pub fn visit_import_type<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::ImportType<'cx>,
) -> V::Result {
    visit_return!(v.visit_ty(n.argument));
    if let Some(qualifier) = n.qualifier {
        visit_return!(v.visit_entity_name(qualifier));
    }
    visit_type_arguments(v, n.type_arguments)
}

pub fn visit_template_span_ty<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::TemplateSpanTy<'cx>,
) -> V::Result {
    v.visit_ty(n.ty)
}

pub fn visit_qualified_name<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::QualifiedName<'cx>,
) -> V::Result {
    visit_return!(v.visit_entity_name(n.left));
    v.visit_ident(n.right)
}

pub fn visit_new_meta_property<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::NewMetaProperty<'cx>,
) -> V::Result {
    v.visit_ident(n.name)
}

pub fn visit_jsx_text<'cx, V: Visitor<'cx>>(_v: &mut V, _n: &'cx ast::JsxText) -> V::Result {
    V::Result::output()
}

pub fn visit_jsx_opening_frag<'cx, V: Visitor<'cx>>(
    _v: &mut V,
    _n: &'cx ast::JsxOpeningFrag,
) -> V::Result {
    V::Result::output()
}

pub fn visit_jsx_closing_frag<'cx, V: Visitor<'cx>>(
    _v: &mut V,
    _n: &'cx ast::JsxClosingFrag,
) -> V::Result {
    V::Result::output()
}

fn visit_jsx_tag_name<'cx, V: Visitor<'cx>>(v: &mut V, name: ast::JsxTagName<'cx>) -> V::Result {
    use ast::JsxTagName::*;
    match name {
        Ident(n) => v.visit_ident(n),
        This(n) => v.visit_this_expr(n),
        Ns(n) => v.visit_jsx_ns_name(n),
        PropAccess(n) => v.visit_prop_access_expr(n),
    }
}

fn visit_jsx_attr_name<'cx, V: Visitor<'cx>>(v: &mut V, name: ast::JsxAttrName<'cx>) -> V::Result {
    use ast::JsxAttrName::*;
    match name {
        Ident(n) => v.visit_ident(n),
        Ns(n) => v.visit_jsx_ns_name(n),
    }
}

fn visit_jsx_attr_value<'cx, V: Visitor<'cx>>(
    v: &mut V,
    value: ast::JsxAttrValue<'cx>,
) -> V::Result {
    use ast::JsxAttrValue::*;
    match value {
        StringLit(n) => v.visit_string_lit(n),
        Expr(n) => v.visit_jsx_expr(n),
        Ele(n) => v.visit_jsx_elem(n),
        SelfClosingEle(n) => v.visit_jsx_self_closing_elem(n),
        Frag(n) => v.visit_jsx_frag(n),
    }
}

fn visit_jsx_attr<'cx, V: Visitor<'cx>>(v: &mut V, attr: &ast::JsxAttr<'cx>) -> V::Result {
    use ast::JsxAttr::*;
    match attr {
        Spread(n) => v.visit_jsx_spread_attr(n),
        Named(n) => v.visit_jsx_named_attr(n),
    }
}

fn visit_jsx_child<'cx, V: Visitor<'cx>>(v: &mut V, child: ast::JsxChild<'cx>) -> V::Result {
    use ast::JsxChild::*;
    match child {
        Text(n) => v.visit_jsx_text(n),
        Expr(n) => v.visit_jsx_expr(n),
        Elem(n) => v.visit_jsx_elem(n),
        SelfClosingEle(n) => v.visit_jsx_self_closing_elem(n),
        Frag(n) => v.visit_jsx_frag(n),
    }
}

fn visit_jsx_attrs<'cx, V: Visitor<'cx>>(v: &mut V, attrs: ast::JsxAttrs<'cx>) -> V::Result {
    for attr in attrs {
        visit_return!(visit_jsx_attr(v, attr));
    }
    V::Result::output()
}

pub fn visit_jsx_opening_elem<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::JsxOpeningElem<'cx>,
) -> V::Result {
    visit_return!(visit_jsx_tag_name(v, n.tag_name));
    visit_return!(visit_type_arguments(v, n.ty_args));
    visit_jsx_attrs(v, n.attrs)
}

pub fn visit_jsx_closing_elem<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::JsxClosingElem<'cx>,
) -> V::Result {
    visit_jsx_tag_name(v, n.tag_name)
}

pub fn visit_jsx_self_closing_elem<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::JsxSelfClosingElem<'cx>,
) -> V::Result {
    visit_return!(visit_jsx_tag_name(v, n.tag_name));
    visit_return!(visit_type_arguments(v, n.ty_args));
    visit_jsx_attrs(v, n.attrs)
}

pub fn visit_jsx_spread_attr<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::JsxSpreadAttr<'cx>,
) -> V::Result {
    v.visit_expr(n.expr)
}

pub fn visit_jsx_ns_name<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::JsxNsName<'cx>,
) -> V::Result {
    visit_return!(v.visit_ident(n.ns));
    v.visit_ident(n.name)
}

pub fn visit_jsx_named_attr<'cx, V: Visitor<'cx>>(
    v: &mut V,
    n: &'cx ast::JsxNamedAttr<'cx>,
) -> V::Result {
    visit_return!(visit_jsx_attr_name(v, n.name.clone()));
    if let Some(init) = n.init {
        return visit_jsx_attr_value(v, init);
    }
    V::Result::output()
}

pub fn visit_jsx_expr<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::JsxExpr<'cx>) -> V::Result {
    visit_optional_expr(v, n.expr)
}

pub fn visit_jsx_frag<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::JsxFrag<'cx>) -> V::Result {
    visit_return!(v.visit_jsx_opening_frag(n.opening_frag));
    for child in n.children {
        visit_return!(visit_jsx_child(v, *child));
    }
    v.visit_jsx_closing_frag(n.closing_frag)
}

pub fn visit_jsx_elem<'cx, V: Visitor<'cx>>(v: &mut V, n: &'cx ast::JsxElem<'cx>) -> V::Result {
    visit_return!(v.visit_jsx_opening_elem(n.opening_elem));
    for child in n.children {
        visit_return!(visit_jsx_child(v, *child));
    }
    v.visit_jsx_closing_elem(n.closing_elem)
}

pub fn visit_node<'cx, V: Visitor<'cx>>(v: &mut V, node: &ast::Node<'cx>) -> V::Result {
    use ast::Node::*;
    match node {
        Program(n) => v.visit_program(n),
        Modifier(n) => v.visit_modifier(n),
        VarDecl(n) => v.visit_var_decl(n),
        ParamDecl(n) => v.visit_param_decl(n),
        ClassExtendsClause(n) => v.visit_class_extends_clause(n),
        ImportShorthandSpec(n) => v.visit_import_shorthand_spec(n),
        ExportShorthandSpec(n) => v.visit_export_shorthand_spec(n),
        NsImport(n) => v.visit_ns_import(n),
        NsExport(n) => v.visit_ns_export(n),
        GlobExport(n) => v.visit_glob_export(n),
        SpecsExport(n) => v.visit_specs_export(n),
        ExportNamedSpec(n) => v.visit_export_named_spec(n),
        ImportNamedSpec(n) => v.visit_import_named_spec(n),
        ImportClause(n) => v.visit_import_clause(n),
        ObjectPat(n) => v.visit_object_pat(n),
        ObjectBindingElem(n) => v.visit_object_binding_elem(n),
        ArrayPat(n) => v.visit_array_pat(n),
        ArrayBinding(n) => v.visit_array_binding(n),
        EnumMember(n) => v.visit_enum_member(n),
        ObjectShorthandMember(n) => v.visit_object_shorthand_member(n),
        ObjectPropAssignment(n) => v.visit_object_prop_assignment(n),
        ObjectMethodMember(n) => v.visit_object_method_member(n),
        SpreadAssignment(n) => v.visit_spread_assignment(n),
        SpreadElement(n) => v.visit_spread_element(n),
        TemplateHead(n) => v.visit_template_head(n),
        TemplateSpan(n) => v.visit_template_span(n),
        CaseClause(n) => v.visit_case_clause(n),
        DefaultClause(n) => v.visit_default_clause(n),
        CaseBlock(n) => v.visit_case_block(n),
        VarStmt(n) => v.visit_var_stmt(n),
        FnDecl(n) => v.visit_fn_decl(n),
        IfStmt(n) => v.visit_if_stmt(n),
        RetStmt(n) => v.visit_ret_stmt(n),
        EmptyStmt(n) => v.visit_empty_stmt(n),
        ClassDecl(n) => v.visit_class_decl(n),
        ModuleDecl(n) => v.visit_module_decl(n),
        ClassCtor(n) => v.visit_class_ctor(n),
        ClassPropElem(n) => v.visit_class_prop_elem(n),
        ClassMethodElem(n) => v.visit_class_method_elem(n),
        ClassSemiElem(n) => v.visit_class_semi_elem(n),
        ClassStaticBlockDecl(n) => v.visit_class_static_block_decl(n),
        GetterDecl(n) => v.visit_getter_decl(n),
        SetterDecl(n) => v.visit_setter_decl(n),
        InterfaceDecl(n) => v.visit_interface_decl(n),
        TypeAliasDecl(n) => v.visit_type_alias_decl(n),
        InterfaceExtendsClause(n) => v.visit_interface_extends_clause(n),
        ClassImplementsClause(n) => v.visit_class_implements_clause(n),
        BlockStmt(n) => v.visit_block_stmt(n),
        ModuleBlock(n) => v.visit_module_block(n),
        ThrowStmt(n) => v.visit_throw_stmt(n),
        EnumDecl(n) => v.visit_enum_decl(n),
        ImportDecl(n) => v.visit_import_decl(n),
        ImportEqualsDecl(n) => v.visit_import_equals_decl(n),
        ExternalModuleReference(n) => v.visit_external_module_reference(n),
        ExportDecl(n) => v.visit_export_decl(n),
        ExportAssign(n) => v.visit_export_assign(n),
        ForStmt(n) => v.visit_for_stmt(n),
        ForInStmt(n) => v.visit_for_in_stmt(n),
        ForOfStmt(n) => v.visit_for_of_stmt(n),
        WhileStmt(n) => v.visit_while_stmt(n),
        DoWhileStmt(n) => v.visit_do_while_stmt(n),
        BreakStmt(n) => v.visit_break_stmt(n),
        ContinueStmt(n) => v.visit_continue_stmt(n),
        TryStmt(n) => v.visit_try_stmt(n),
        CatchClause(n) => v.visit_catch_clause(n),
        DebuggerStmt(n) => v.visit_debugger_stmt(n),
        LabeledStmt(n) => v.visit_labeled_stmt(n),
        SwitchStmt(n) => v.visit_switch_stmt(n),
        ExprStmt(n) => v.visit_expr_stmt(n),
        BinExpr(n) => v.visit_bin_expr(n),
        OmitExpr(n) => v.visit_omit_expr(n),
        ParenExpr(n) => v.visit_paren_expr(n),
        CondExpr(n) => v.visit_cond_expr(n),
        CallExpr(n) => v.visit_call_expr(n),
        FnExpr(n) => v.visit_fn_expr(n),
        ClassExpr(n) => v.visit_class_expr(n),
        NewExpr(n) => v.visit_new_expr(n),
        AssignExpr(n) => v.visit_assign_expr(n),
        ArrowFnExpr(n) => v.visit_arrow_fn_expr(n),
        PrefixUnaryExpr(n) => v.visit_prefix_unary_expr(n),
        PostfixUnaryExpr(n) => v.visit_postfix_unary_expr(n),
        PropAccessExpr(n) => v.visit_prop_access_expr(n),
        EleAccessExpr(n) => v.visit_ele_access_expr(n),
        ThisExpr(n) => v.visit_this_expr(n),
        TypeofExpr(n) => v.visit_typeof_expr(n),
        VoidExpr(n) => v.visit_void_expr(n),
        AwaitExpr(n) => v.visit_await_expr(n),
        YieldExpr(n) => v.visit_yield_expr(n),
        SuperExpr(n) => v.visit_super_expr(n),
        AsExpr(n) => v.visit_as_expr(n),
        TyAssertionExpr(n) => v.visit_ty_assertion_expr(n),
        SatisfiesExpr(n) => v.visit_satisfies_expr(n),
        NonNullExpr(n) => v.visit_non_null_expr(n),
        TemplateExpr(n) => v.visit_template_expr(n),
        TaggedTemplateExpr(n) => v.visit_tagged_template_expr(n),
        DeleteExpr(n) => v.visit_delete_expr(n),
        ImportExpression(n) => v.visit_import_expression(n),
        NumLit(n) => v.visit_num_lit(n),
        BigIntLit(n) => v.visit_big_int_lit(n),
        BoolLit(n) => v.visit_bool_lit(n),
        NullLit(n) => v.visit_null_lit(n),
        RegExpLit(n) => v.visit_reg_exp_lit(n),
        StringLit(n) => v.visit_string_lit(n),
        NoSubstitutionTemplateLit(n) => v.visit_no_substitution_template_lit(n),
        ArrayLit(n) => v.visit_array_lit(n),
        ObjectLit(n) => v.visit_object_lit(n),
        Ident(n) => v.visit_ident(n),
        PrivateIdent(n) => v.visit_private_ident(n),
        ComputedPropName(n) => v.visit_computed_prop_name(n),
        ExprWithTyArgs(n) => v.visit_expr_with_ty_args(n),
        NewMetaProperty(n) => v.visit_new_meta_property(n),
        LitTy(n) => v.visit_lit_ty(n),
        ReferTy(n) => v.visit_refer_ty(n),
        ArrayTy(n) => v.visit_array_ty(n),
        ImportType(n) => v.visit_import_type(n),
        IndexedAccessTy(n) => v.visit_indexed_access_ty(n),
        FnTy(n) => v.visit_fn_ty(n),
        CtorTy(n) => v.visit_ctor_ty(n),
        ObjectLitTy(n) => v.visit_object_lit_ty(n),
        TyParam(n) => v.visit_ty_param(n),
        IndexSigDecl(n) => v.visit_index_sig_decl(n),
        CallSigDecl(n) => v.visit_call_sig_decl(n),
        CtorSigDecl(n) => v.visit_ctor_sig_decl(n),
        PropSignature(n) => v.visit_prop_signature(n),
        MethodSignature(n) => v.visit_method_signature(n),
        RestTy(n) => v.visit_rest_ty(n),
        NamedTupleTy(n) => v.visit_named_tuple_ty(n),
        TupleTy(n) => v.visit_tuple_ty(n),
        CondTy(n) => v.visit_cond_ty(n),
        IntersectionTy(n) => v.visit_intersection_ty(n),
        UnionTy(n) => v.visit_union_ty(n),
        TypeofTy(n) => v.visit_typeof_ty(n),
        MappedTy(n) => v.visit_mapped_ty(n),
        TyOp(n) => v.visit_ty_op_ty(n),
        PredTy(n) => v.visit_pred_ty(n),
        ParenTy(n) => v.visit_paren_ty(n),
        InferTy(n) => v.visit_infer_ty(n),
        NullableTy(n) => v.visit_nullable_ty(n),
        TemplateLitTy(n) => v.visit_template_lit_ty(n),
        TemplateSpanTy(n) => v.visit_template_span_ty(n),
        IntrinsicTy(n) => v.visit_intrinsic_ty(n),
        ThisTy(n) => v.visit_this_ty(n),
        QualifiedName(n) => v.visit_qualified_name(n),
        JsxText(n) => v.visit_jsx_text(n),
        JsxOpeningFrag(n) => v.visit_jsx_opening_frag(n),
        JsxClosingFrag(n) => v.visit_jsx_closing_frag(n),
        JsxOpeningElem(n) => v.visit_jsx_opening_elem(n),
        JsxClosingElem(n) => v.visit_jsx_closing_elem(n),
        JsxSelfClosingElem(n) => v.visit_jsx_self_closing_elem(n),
        JsxSpreadAttr(n) => v.visit_jsx_spread_attr(n),
        JsxNsName(n) => v.visit_jsx_ns_name(n),
        JsxNamedAttr(n) => v.visit_jsx_named_attr(n),
        JsxExpr(n) => v.visit_jsx_expr(n),
        JsxFrag(n) => v.visit_jsx_frag(n),
        JsxElem(n) => v.visit_jsx_elem(n),
    }
}
