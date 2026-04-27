use bolt_ts_ast as ast;

pub fn visit_program<'cx>(v: &mut impl Visitor<'cx>, program: &'cx ast::Program<'cx>) {
    for stmt in program.stmts() {
        v.visit_stmt(stmt);
    }
}

pub fn visit_export_assign<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::ExportAssign<'cx>) {
    v.visit_expr(n.expr);
}

pub fn visit_empty_stmt<'cx>(_: &mut impl Visitor<'cx>, _: &'cx ast::EmptyStmt) {
    // Empty statements have no child nodes to visit
}

pub fn visit_stmt<'cx>(v: &mut impl Visitor<'cx>, stmt: &'cx ast::Stmt) {
    use ast::StmtKind::*;
    match stmt.kind {
        Block(node) => v.visit_block_stmt(node),
        Class(node) => v.visit_class_decl(node),
        Import(node) => v.visit_import_decl(node),
        ImportEquals(_node) => {}
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
        Export(_) => {}
        ExportAssign(node) => v.visit_export_assign(node),
        Empty(node) => v.visit_empty_stmt(node),
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

fn visit_binding<'cx>(v: &mut impl Visitor<'cx>, node: &'cx ast::Binding<'cx>) {
    use ast::BindingKind::*;
    match node.kind {
        Ident(n) => v.visit_ident(n),
        ObjectPat(_) => {
            // TODO:
        }
        ArrayPat(_) => {
            // TODO:
        }
    }
}

pub fn visit_param_decl<'cx>(v: &mut impl Visitor<'cx>, node: &'cx ast::ParamDecl<'cx>) {
    v.visit_binding(node.name);
    if let Some(ty) = node.ty {
        v.visit_ty(ty);
    }
    if let Some(init) = node.init {
        v.visit_expr(init);
    }
}

pub fn visit_fn_decl<'cx>(v: &mut impl Visitor<'cx>, node: &'cx ast::FnDecl<'cx>) {
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

pub fn visit_enum_decl<'cx>(v: &mut impl Visitor<'cx>, enum_decl: &'cx ast::EnumDecl<'cx>) {
    v.visit_ident(enum_decl.name);
    for member in enum_decl.members {
        v.visit_enum_member(member);
    }
}

pub fn visit_enum_member_name_kind<'cx>(
    v: &mut impl Visitor<'cx>,
    name: &ast::EnumMemberNameKind<'cx>,
) {
    use ast::EnumMemberNameKind::*;
    match name {
        Ident(ident) => v.visit_ident(ident),
        StringLit { raw, .. } => v.visit_string_lit(raw),
    }
}
pub fn visit_enum_member<'cx>(v: &mut impl Visitor<'cx>, member: &'cx ast::EnumMember<'cx>) {
    visit_enum_member_name_kind(v, &member.name);
    if let Some(init) = member.init {
        v.visit_expr(init);
    }
}

fn visit_var_stmt<'cx>(v: &mut impl Visitor<'cx>, stmt: &'cx ast::VarStmt<'cx>) {
    for item in stmt.list {
        v.visit_var_decl(item);
    }
}

pub fn visit_type_alias_decl<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::TypeAliasDecl<'cx>) {
    v.visit_ident(n.name);
    v.visit_ty(n.ty);
}

pub fn visit_module_decl<'cx>(v: &mut impl Visitor<'cx>, decl: &'cx ast::ModuleDecl<'cx>) {
    visit_module_name(v, decl.name);
    if let Some(block) = decl.block {
        for stmt in block.stmts {
            v.visit_stmt(stmt);
        }
    }
}

pub fn visit_module_name<'cx>(v: &mut impl Visitor<'cx>, name: ast::ModuleName<'cx>) {
    match name {
        ast::ModuleName::Ident(ident) => v.visit_ident(ident),
        ast::ModuleName::StringLit(lit) => v.visit_string_lit(lit),
    }
}

pub fn visit_var_decl<'cx>(v: &mut impl Visitor<'cx>, decl: &'cx ast::VarDecl<'cx>) {
    if let Some(ty) = decl.ty {
        v.visit_ty(ty);
    }
    if let Some(init) = decl.init {
        v.visit_expr(init);
    }
}
pub fn visit_class_decl<'cx>(v: &mut impl Visitor<'cx>, class: &'cx ast::ClassDecl<'cx>) {
    for ele in class.elems.list {
        v.visit_class_elem(ele);
    }
}
pub fn visit_interface_decl<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::InterfaceDecl<'cx>) {
    v.visit_ident(n.name);
    // TODO: node.extends
    for member in n.members {
        visit_object_ty_member(v, member);
    }
}
fn visit_object_ty_member<'cx>(v: &mut impl Visitor<'cx>, member: &'cx ast::ObjectTyMember<'cx>) {
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
pub fn visit_prop_signature<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::PropSignature<'cx>) {
    v.visit_prop_name(n.name);
    if let Some(ty) = n.ty {
        v.visit_ty(ty);
    }
}
pub fn visit_method_signature<'cx>(
    v: &mut impl Visitor<'cx>,
    node: &'cx ast::MethodSignature<'cx>,
) {
    v.visit_prop_name(node.name);
    if let Some(ty) = node.ty {
        v.visit_ty(ty);
    }
}
pub fn visit_import_decl<'cx>(_: &mut impl Visitor<'cx>, _: &'cx ast::ImportDecl<'cx>) {}
pub fn visit_class_elem<'cx>(v: &mut impl Visitor<'cx>, elem: &'cx ast::ClassElem<'cx>) {
    use ast::ClassElemKind::*;
    match elem.kind {
        Ctor(_n) => {}
        Prop(n) => v.visit_class_prop_elem(n),
        Method(n) => v.visit_class_method_elem(n),
        IndexSig(n) => v.visit_index_sig_decl(n),
        Getter(n) => v.visit_getter_decl(n),
        Setter(n) => v.visit_setter_decl(n),
        StaticBlockDecl(_n) => {}
        Semi(_) => {}
    }
}
pub fn visit_index_sig_decl<'cx>(v: &mut impl Visitor<'cx>, node: &'cx ast::IndexSigDecl<'cx>) {
    v.visit_binding(node.key);
    v.visit_ty(node.key_ty);
    v.visit_ty(node.ty);
}
pub fn visit_class_prop_elem<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::ClassPropElem<'cx>) {
    v.visit_prop_name(n.name);
    if let Some(ty) = n.ty {
        v.visit_ty(ty);
    }
    if let Some(init) = n.init {
        v.visit_expr(init);
    }
}
pub fn visit_class_method_elem<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::ClassMethodElem<'cx>) {
    v.visit_prop_name(n.name);
    if let Some(ty_params) = n.ty_params {
        for ty_param in ty_params {
            v.visit_ty_param(ty_param);
        }
    }
    for param in n.params {
        v.visit_param_decl(param);
    }
    if let Some(ty) = n.ty {
        v.visit_ty(ty);
    }
    if let Some(body) = n.body {
        v.visit_block_stmt(body);
    }
}
pub fn visit_ty<'cx>(v: &mut impl Visitor<'cx>, ty: &'cx ast::Ty<'cx>) {
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
    }
}
pub fn visit_this_ty<'cx>(_: &mut impl Visitor<'cx>, _: &'cx ast::ThisTy) {}
pub fn visit_yield_expr<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::YieldExpr<'cx>) {
    if let Some(expr) = n.expr {
        v.visit_expr(expr);
    }
}
pub fn visit_refer_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::ReferTy<'cx>) {
    v.visit_entity_name(n.name);
    if let Some(ty_args) = n.ty_args {
        for ty in ty_args.list {
            v.visit_ty(ty);
        }
    }
}
pub fn visit_array_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::ArrayTy<'cx>) {
    v.visit_ty(n.ele);
}
pub fn visit_indexed_access_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::IndexedAccessTy<'cx>) {
    v.visit_ty(n.ty);
    v.visit_ty(n.index_ty);
}
pub fn visit_fn_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::FnTy<'cx>) {
    if let Some(ty_params) = n.ty_params {
        for ty_param in ty_params {
            v.visit_ty_param(ty_param);
        }
    }
    for param in n.params {
        v.visit_param_decl(param);
    }
    v.visit_ty(n.ty);
}
pub fn visit_ctor_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::CtorTy<'cx>) {
    if let Some(ty_params) = n.ty_params {
        for ty_param in ty_params {
            v.visit_ty_param(ty_param);
        }
    }
    for param in n.params {
        v.visit_param_decl(param);
    }
    v.visit_ty(n.ty);
}
pub fn visit_object_lit_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::ObjectLitTy<'cx>) {
    for prop in n.members {
        use ast::ObjectTyMemberKind::*;
        match prop.kind {
            IndexSig(n) => v.visit_index_sig_decl(n),
            Prop(n) => v.visit_prop_signature(n),
            Method(n) => v.visit_method_signature(n),
            CallSig(n) => v.visit_call_sig_decl(n),
            CtorSig(n) => v.visit_ctor_sig_decl(n),
            Setter(n) => v.visit_setter_decl(n),
            Getter(n) => v.visit_getter_decl(n),
        }
    }
}
pub fn visit_call_sig_decl<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::CallSigDecl<'cx>) {
    if let Some(ty_params) = n.ty_params {
        for ty_param in ty_params {
            v.visit_ty_param(ty_param);
        }
    }
    for param in n.params {
        v.visit_param_decl(param);
    }
    if let Some(ty) = n.ty {
        v.visit_ty(ty);
    }
}
pub fn visit_ctor_sig_decl<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::CtorSigDecl<'cx>) {
    if let Some(ty_params) = n.ty_params {
        for ty_param in ty_params {
            v.visit_ty_param(ty_param);
        }
    }
    for param in n.params {
        v.visit_param_decl(param);
    }
    if let Some(ty) = n.ty {
        v.visit_ty(ty);
    }
}
pub fn visit_setter_decl<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::SetterDecl<'cx>) {
    v.visit_prop_name(n.name);
    for param in n.params {
        v.visit_param_decl(param);
    }
    if let Some(body) = n.body {
        v.visit_block_stmt(body);
    }
}
pub fn visit_getter_decl<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::GetterDecl<'cx>) {
    v.visit_prop_name(n.name);
    if let Some(ty) = n.ty {
        v.visit_ty(ty);
    }
    if let Some(body) = n.body {
        v.visit_block_stmt(body);
    }
}
pub fn visit_lit_ty<'cx>(_: &mut impl Visitor<'cx>, _: &'cx ast::LitTy) {
    // Literal types have no child nodes to visit
}
pub fn visit_named_tuple_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::NamedTupleTy<'cx>) {
    v.visit_ty(n.ty);
}
pub fn visit_tuple_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::TupleTy<'cx>) {
    for ty in n.tys {
        v.visit_ty(ty);
    }
}
pub fn visit_rest_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::RestTy<'cx>) {
    v.visit_ty(n.ty);
}
pub fn visit_cond_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::CondTy<'cx>) {
    v.visit_ty(n.check_ty);
    v.visit_ty(n.extends_ty);
    v.visit_ty(n.true_ty);
    v.visit_ty(n.false_ty);
}
pub fn visit_union_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::UnionTy<'cx>) {
    for ty in n.tys {
        v.visit_ty(ty);
    }
}
pub fn visit_intersection_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::IntersectionTy<'cx>) {
    for ty in n.tys {
        v.visit_ty(ty);
    }
}
pub fn visit_typeof_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::TypeofTy<'cx>) {
    v.visit_entity_name(n.name);
    if let Some(ty_args) = n.ty_args {
        for ty in ty_args.list {
            v.visit_ty(ty);
        }
    }
}
pub fn visit_mapped_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::MappedTy<'cx>) {
    v.visit_ty_param(n.ty_param);
    if let Some(name_ty) = n.name_ty {
        v.visit_ty(name_ty);
    }
    if let Some(ty) = n.ty {
        v.visit_ty(ty);
    }
    for member in n.members {
        visit_object_ty_member(v, member);
    }
}
pub fn visit_ty_op_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::TypeOp<'cx>) {
    v.visit_ty(n.ty);
}
pub fn visit_pred_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::PredTy<'cx>) {
    use ast::PredTyName::*;
    match n.name {
        Ident(ident) => visit_ident(v, ident),
        This(_) => {}
    }
    if let Some(ty) = n.ty {
        v.visit_ty(ty);
    }
}
pub fn visit_paren_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::ParenTy<'cx>) {
    v.visit_ty(n.ty);
}
pub fn visit_infer_ty<'cx>(_: &mut impl Visitor<'cx>, _: &'cx ast::InferTy<'cx>) {
    // Infer types have no child nodes to visit
}
pub fn visit_intrinsic_ty<'cx>(_: &mut impl Visitor<'cx>, _: &'cx ast::IntrinsicTy) {
    // Intrinsic types have no child nodes to visit
}
pub fn visit_nullable_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::NullableTy<'cx>) {
    v.visit_ty(n.ty);
}
pub fn visit_template_lit_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::TemplateLitTy<'cx>) {
    for span in n.spans {
        v.visit_ty(span.ty);
    }
}
pub fn visit_prop_name<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::PropName<'cx>) {
    match n.kind {
        ast::PropNameKind::Ident(ident) => v.visit_ident(ident),
        ast::PropNameKind::StringLit { raw, .. } => v.visit_string_lit(raw),
        ast::PropNameKind::Computed(expr) => v.visit_computed_prop_name(expr),
        ast::PropNameKind::PrivateIdent(n) => v.visit_private_ident(n),
        ast::PropNameKind::NumLit(_) => {}
        ast::PropNameKind::BigIntLit(_) => {}
    }
}
pub fn visit_computed_prop_name<'cx>(
    v: &mut impl Visitor<'cx>,
    n: &'cx ast::ComputedPropName<'cx>,
) {
    v.visit_expr(n.expr);
}
pub fn visit_ident<'cx>(_: &mut impl Visitor<'cx>, _: &'cx ast::Ident) {}
pub fn visit_private_ident<'cx>(_: &mut impl Visitor<'cx>, _: &'cx ast::PrivateIdent) {}
pub fn visit_entity_name<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::EntityName) {
    match n.kind {
        ast::EntityNameKind::Ident(node) => v.visit_ident(node),
        ast::EntityNameKind::Qualified(node) => {
            v.visit_entity_name(node.left);
            v.visit_ident(node.right);
        }
    }
}
pub fn visit_expr<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::Expr<'cx>) {
    use ast::ExprKind::*;
    match n.kind {
        ObjectLit(n) => v.visit_object_lit(n),
        ArrowFn(n) => v.visit_arrow_fn_expr(n),
        Bin(n) => v.visit_bin_expr(n),
        Call(n) => v.visit_call_expr(n),
        Assign(n) => v.visit_assign_expr(n),
        Yield(n) => v.visit_yield_expr(n),
        Ident(n) => v.visit_ident(n),
        _ => {}
    }
}
pub fn visit_call_expr<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::CallExpr<'cx>) {
    v.visit_expr(n.expr);
    for arg in n.args {
        v.visit_expr(arg);
    }
}
pub fn visit_object_lit<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::ObjectLit<'cx>) {
    for member in n.members {
        use ast::ObjectMemberKind::*;
        match member.kind {
            PropAssignment(node) => {
                v.visit_prop_name(node.name);
                v.visit_expr(node.init);
            }
            Method(node) => {
                v.visit_object_method_member(node);
            }
            Shorthand(n) => v.visit_ident(n.name),
            _ => {
                // TODO:
            }
        }
    }
}
pub fn visit_object_method_member<'cx>(
    v: &mut impl Visitor<'cx>,
    n: &'cx ast::ObjectMethodMember<'cx>,
) {
    v.visit_prop_name(n.name);
    if let Some(ty_params) = n.ty_params {
        for ty_param in ty_params {
            v.visit_ty_param(ty_param);
        }
    }
    for param in n.params {
        v.visit_param_decl(param);
    }
    if let Some(ty) = n.ty {
        v.visit_ty(ty);
    }
    v.visit_block_stmt(n.body);
}
pub fn visit_try_stmt<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::TryStmt<'cx>) {
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
pub fn visit_block_stmt<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::BlockStmt<'cx>) {
    for stmt in n.stmts {
        v.visit_stmt(stmt);
    }
}
pub fn visit_expr_stmt<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::ExprStmt<'cx>) {
    v.visit_expr(n.expr);
}
pub fn visit_arrow_fn_expr<'cx>(_: &mut impl Visitor<'cx>, _: &'cx ast::ArrowFnExpr<'cx>) {}
pub fn visit_bin_expr<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::BinExpr<'cx>) {
    v.visit_expr(n.left);
    v.visit_expr(n.right);
}
pub fn visit_string_lit<'cx>(_: &mut impl Visitor<'cx>, _: &'cx ast::StringLit) {}
pub fn visit_while_stmt<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::WhileStmt<'cx>) {
    v.visit_expr(n.expr);
    v.visit_stmt(n.stmt);
}
pub fn visit_if_stmt<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::IfStmt<'cx>) {
    v.visit_expr(n.expr);
    v.visit_stmt(n.then);
    if let Some(else_then) = n.else_then {
        v.visit_stmt(else_then);
    }
}
pub fn visit_ty_param<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::TyParam<'cx>) {
    v.visit_ident(n.name);
    if let Some(ty) = n.constraint {
        v.visit_ty(ty);
    }
    if let Some(default_ty) = n.default {
        v.visit_ty(default_ty)
    }
}
pub fn visit_ret_stmt<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::RetStmt<'cx>) {
    if let Some(expr) = n.expr {
        v.visit_expr(expr);
    }
}
pub fn visit_assign_expr<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::AssignExpr<'cx>) {
    v.visit_expr(n.left);
    v.visit_expr(n.right);
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
    (visit_program, ast::Program<'cx>),
    (visit_stmt, ast::Stmt<'cx>),
    (visit_import_decl, ast::ImportDecl<'cx>),
    (visit_interface_decl, ast::InterfaceDecl<'cx>),
    (visit_class_decl, ast::ClassDecl<'cx>),
    (visit_class_elem, ast::ClassElem<'cx>),
    (visit_class_prop_elem, ast::ClassPropElem<'cx>),
    (visit_class_method_elem, ast::ClassMethodElem<'cx>),
    (visit_index_sig_decl, ast::IndexSigDecl<'cx>),
    (visit_entity_name, ast::EntityName<'cx>),
    (visit_ident, ast::Ident),
    (visit_private_ident, ast::PrivateIdent),
    (visit_ty, ast::Ty<'cx>),
    (visit_refer_ty, ast::ReferTy<'cx>),
    (visit_array_ty, ast::ArrayTy<'cx>),
    (visit_indexed_access_ty, ast::IndexedAccessTy<'cx>),
    (visit_fn_ty, ast::FnTy<'cx>),
    (visit_ctor_ty, ast::CtorTy<'cx>),
    (visit_object_lit_ty, ast::ObjectLitTy<'cx>),
    (visit_lit_ty, ast::LitTy),
    (visit_named_tuple_ty, ast::NamedTupleTy<'cx>),
    (visit_tuple_ty, ast::TupleTy<'cx>),
    (visit_rest_ty, ast::RestTy<'cx>),
    (visit_cond_ty, ast::CondTy<'cx>),
    (visit_union_ty, ast::UnionTy<'cx>),
    (visit_intersection_ty, ast::IntersectionTy<'cx>),
    (visit_typeof_ty, ast::TypeofTy<'cx>),
    (visit_mapped_ty, ast::MappedTy<'cx>),
    (visit_ty_op_ty, ast::TypeOp<'cx>),
    (visit_ty_param, ast::TyParam<'cx>),
    (visit_pred_ty, ast::PredTy<'cx>),
    (visit_paren_ty, ast::ParenTy<'cx>),
    (visit_infer_ty, ast::InferTy<'cx>),
    (visit_intrinsic_ty, ast::IntrinsicTy),
    (visit_nullable_ty, ast::NullableTy<'cx>),
    (visit_template_lit_ty, ast::TemplateLitTy<'cx>),
    (visit_var_stmt, ast::VarStmt<'cx>),
    (visit_var_decl, ast::VarDecl<'cx>),
    (visit_expr, ast::Expr<'cx>),
    (visit_object_lit, ast::ObjectLit<'cx>),
    (visit_object_method_member, ast::ObjectMethodMember<'cx>),
    (visit_try_stmt, ast::TryStmt<'cx>),
    (visit_block_stmt, ast::BlockStmt<'cx>),
    (visit_expr_stmt, ast::ExprStmt<'cx>),
    (visit_arrow_fn_expr, ast::ArrowFnExpr<'cx>),
    (visit_bin_expr, ast::BinExpr<'cx>),
    (visit_type_alias_decl, ast::TypeAliasDecl<'cx>),
    (visit_module_decl, ast::ModuleDecl<'cx>),
    (visit_string_lit, ast::StringLit),
    (visit_while_stmt, ast::WhileStmt<'cx>),
    (visit_if_stmt, ast::IfStmt<'cx>),
    (visit_enum_decl, ast::EnumDecl<'cx>),
    (visit_enum_member, ast::EnumMember<'cx>),
    (visit_prop_name, ast::PropName<'cx>),
    (visit_computed_prop_name, ast::ComputedPropName<'cx>),
    (visit_param_decl, ast::ParamDecl<'cx>),
    (visit_fn_decl, ast::FnDecl<'cx>),
    (visit_binding, ast::Binding<'cx>),
    (visit_call_expr, ast::CallExpr<'cx>),
    (visit_prop_signature, ast::PropSignature<'cx>),
    (visit_method_signature, ast::MethodSignature<'cx>),
    (visit_this_ty, ast::ThisTy),
    (visit_yield_expr, ast::YieldExpr),
    (visit_ret_stmt, ast::RetStmt<'cx>),
    (visit_assign_expr, ast::AssignExpr<'cx>),
    (visit_call_sig_decl, ast::CallSigDecl<'cx>),
    (visit_ctor_sig_decl, ast::CtorSigDecl<'cx>),
    (visit_setter_decl, ast::SetterDecl<'cx>),
    (visit_getter_decl, ast::GetterDecl<'cx>),
    (visit_export_assign, ast::ExportAssign<'cx>),
    (visit_empty_stmt, ast::EmptyStmt)
);

pub fn visit_node<'cx>(v: &mut impl Visitor<'cx>, node: &ast::Node<'cx>) {
    use ast::Node::*;
    match node {
        Program(n) => v.visit_program(n),
        Modifier(_n) => todo!(),
        VarDecl(n) => v.visit_var_decl(n),
        ParamDecl(_n) => todo!(),
        ClassExtendsClause(_n) => todo!(),
        ImportShorthandSpec(_n) => todo!(),
        ExportShorthandSpec(_n) => todo!(),
        NsImport(_n) => todo!(),
        NsExport(_n) => todo!(),
        GlobExport(_n) => todo!(),
        SpecsExport(_n) => todo!(),
        ExportNamedSpec(_n) => todo!(),
        ImportNamedSpec(_n) => todo!(),
        ImportClause(_n) => todo!(),
        ObjectPat(_n) => todo!(),
        ObjectBindingElem(_n) => todo!(),
        ArrayPat(_n) => todo!(),
        ArrayBinding(_n) => todo!(),
        EnumMember(_n) => todo!(),
        ObjectShorthandMember(_n) => todo!(),
        ObjectPropAssignment(_n) => todo!(),
        ObjectMethodMember(_n) => todo!(),
        SpreadAssignment(_n) => todo!(),
        SpreadElement(_n) => todo!(),
        TemplateHead(_n) => todo!(),
        TemplateSpan(_n) => todo!(),
        CaseClause(_n) => todo!(),
        DefaultClause(_n) => todo!(),
        CaseBlock(_n) => todo!(),
        VarStmt(_n) => todo!(),
        FnDecl(_n) => todo!(),
        IfStmt(_n) => todo!(),
        RetStmt(_n) => todo!(),
        EmptyStmt(_n) => todo!(),
        ClassDecl(_n) => todo!(),
        ModuleDecl(_n) => todo!(),
        ClassCtor(_n) => todo!(),
        ClassPropElem(_n) => todo!(),
        ClassMethodElem(n) => v.visit_class_method_elem(n),
        ClassStaticBlockDecl(_n) => todo!(),
        GetterDecl(_n) => todo!(),
        SetterDecl(_n) => todo!(),
        InterfaceDecl(n) => v.visit_interface_decl(n),
        TypeAliasDecl(n) => v.visit_type_alias_decl(n),
        InterfaceExtendsClause(_n) => todo!(),
        ClassImplementsClause(_n) => todo!(),
        BlockStmt(_n) => todo!(),
        ModuleBlock(_n) => todo!(),
        ThrowStmt(_n) => todo!(),
        EnumDecl(_n) => todo!(),
        ImportDecl(_n) => todo!(),
        ExportDecl(_n) => todo!(),
        ExportAssign(_n) => todo!(),
        ForStmt(_n) => todo!(),
        ForInStmt(_n) => todo!(),
        ForOfStmt(_n) => todo!(),
        WhileStmt(_n) => todo!(),
        DoWhileStmt(_n) => todo!(),
        BreakStmt(_n) => todo!(),
        ContinueStmt(_n) => todo!(),
        TryStmt(_n) => todo!(),
        CatchClause(_n) => todo!(),
        DebuggerStmt(_n) => todo!(),
        LabeledStmt(_n) => todo!(),
        SwitchStmt(_n) => todo!(),
        ExprStmt(_n) => todo!(),
        BinExpr(_n) => todo!(),
        OmitExpr(_n) => todo!(),
        ParenExpr(_n) => todo!(),
        CondExpr(_n) => todo!(),
        CallExpr(_n) => todo!(),
        FnExpr(_n) => todo!(),
        ClassExpr(_n) => todo!(),
        NewExpr(_n) => todo!(),
        AssignExpr(_n) => todo!(),
        ArrowFnExpr(_n) => todo!(),
        PrefixUnaryExpr(_n) => todo!(),
        PostfixUnaryExpr(_n) => todo!(),
        PropAccessExpr(_n) => todo!(),
        EleAccessExpr(_n) => todo!(),
        ThisExpr(_n) => todo!(),
        TypeofExpr(_n) => todo!(),
        VoidExpr(_n) => todo!(),
        AwaitExpr(_n) => todo!(),
        SuperExpr(_n) => todo!(),
        AsExpr(_n) => todo!(),
        TyAssertionExpr(_n) => todo!(),
        SatisfiesExpr(_n) => todo!(),
        NonNullExpr(_n) => todo!(),
        TemplateExpr(_n) => todo!(),
        TaggedTemplateExpr(_n) => todo!(),
        DeleteExpr(_n) => todo!(),
        NumLit(_n) => todo!(),
        BigIntLit(_n) => todo!(),
        BoolLit(_n) => todo!(),
        NullLit(_n) => todo!(),
        RegExpLit(_n) => todo!(),
        StringLit(_n) => todo!(),
        NoSubstitutionTemplateLit(_n) => todo!(),
        ArrayLit(_n) => todo!(),
        ObjectLit(_n) => todo!(),
        Ident(_n) => todo!(),
        ComputedPropName(_n) => todo!(),
        ExprWithTyArgs(_n) => todo!(),
        LitTy(n) => v.visit_lit_ty(n),
        ReferTy(n) => v.visit_refer_ty(n),
        ArrayTy(n) => v.visit_array_ty(n),
        IndexedAccessTy(n) => v.visit_indexed_access_ty(n),
        FnTy(n) => v.visit_fn_ty(n),
        CtorTy(n) => v.visit_ctor_ty(n),
        ObjectLitTy(n) => v.visit_object_lit_ty(n),
        TyParam(n) => v.visit_ty_param(n),
        IndexSigDecl(n) => v.visit_index_sig_decl(n),
        CallSigDecl(_n) => todo!(),
        CtorSigDecl(_n) => todo!(),
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
        ParenTy(_n) => todo!(),
        InferTy(n) => v.visit_infer_ty(n),
        NullableTy(n) => v.visit_nullable_ty(n),
        TemplateLitTy(n) => v.visit_template_lit_ty(n),
        TemplateSpanTy(_n) => todo!(),
        IntrinsicTy(_n) => todo!(),
        ThisTy(_n) => todo!(),
        QualifiedName(_n) => todo!(),
        JsxText(_n) => todo!(),
        JsxOpeningFrag(_n) => todo!(),
        JsxClosingFrag(_n) => todo!(),
        JsxOpeningElem(_n) => todo!(),
        JsxClosingElem(_n) => todo!(),
        JsxSelfClosingElem(_n) => todo!(),
        JsxSpreadAttr(_n) => todo!(),
        JsxNsName(_n) => todo!(),
        JsxNamedAttr(_n) => todo!(),
        JsxExpr(_n) => todo!(),
        JsxFrag(_n) => todo!(),
        JsxElem(_n) => todo!(),
        PrivateIdent(_n) => todo!(),
        YieldExpr(_n) => todo!(),
        ImportEqualsDecl(_n) => todo!(),
        ExternalModuleReference(_n) => todo!(),
        ClassSemiElem(_n) => {}
    }
}
