use bolt_ts_ast as ast;

pub fn visit_program<'cx>(v: &mut impl Visitor<'cx>, program: &'cx ast::Program<'cx>) {
    for stmt in program.stmts {
        v.visit_stmt(stmt);
    }
}

pub fn visit_stmt<'cx>(v: &mut impl Visitor<'cx>, stmt: &'cx ast::Stmt) {
    use ast::StmtKind::*;
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

pub fn visit_enum_member<'cx>(v: &mut impl Visitor<'cx>, member: &'cx ast::EnumMember<'cx>) {
    v.visit_prop_name(member.name);
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

fn visit_module_name<'cx>(v: &mut impl Visitor<'cx>, name: ast::ModuleName<'cx>) {
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
        use ast::ObjectTyMemberKind::*;
        match member.kind {
            IndexSig(n) => v.visit_index_sig_decl(n),
            Prop(n) => v.visit_prop_signature(n),
            Method(n) => v.visit_method_signature(n),
            CallSig(n) => {}
            CtorSig(n) => {}
            Setter(n) => {}
            Getter(n) => {}
        }
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
        Ctor(n) => {}
        Prop(n) => {}
        Method(n) => v.visit_class_method_elem(n),
        IndexSig(n) => v.visit_index_sig_decl(n),
        Getter(n) => {}
        Setter(n) => {}
        StaticBlockDecl(n) => {}
    }
}
pub fn visit_index_sig_decl<'cx>(v: &mut impl Visitor<'cx>, node: &'cx ast::IndexSigDecl<'cx>) {}
pub fn visit_class_method_elem<'cx>(
    v: &mut impl Visitor<'cx>,
    node: &'cx ast::ClassMethodElem<'cx>,
) {
    if let Some(body) = node.body {
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
        TyOp(n) => v.visit_ty_op_ty(n),
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
    // TODO: name
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
pub fn visit_object_lit_ty<'cx>(_: &mut impl Visitor<'cx>, _: &'cx ast::ObjectLitTy<'cx>) {
    // for prop in n.members {
    //     v.visit_ty_element(prop);
    // }
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
    if let Some(ty_args) = n.ty_args {
        for ty in ty_args.list {
            v.visit_ty(ty);
        }
    }
}
pub fn visit_mapped_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::MappedTy<'cx>) {
    if let Some(ty) = n.ty {
        v.visit_ty(ty);
    }
    if let Some(name_ty) = n.name_ty {
        v.visit_ty(name_ty);
    }
}
pub fn visit_ty_op_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx ast::TyOp<'cx>) {
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
            // TODO:
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
    (visit_ty_op_ty, ast::TyOp<'cx>),
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
    (visit_assign_expr, ast::AssignExpr<'cx>)
);

pub fn visit_node<'cx>(v: &mut impl Visitor<'cx>, node: &ast::Node<'cx>) {
    use ast::Node::*;
    match node {
        Program(n) => v.visit_program(n),
        Modifier(n) => todo!(),
        VarDecl(n) => todo!(),
        ParamDecl(n) => todo!(),
        ClassExtendsClause(n) => todo!(),
        ImportExportShorthandSpec(n) => todo!(),
        NsImport(n) => todo!(),
        NsExport(n) => todo!(),
        GlobExport(n) => todo!(),
        SpecsExport(n) => todo!(),
        ExportNamedSpec(n) => todo!(),
        ImportNamedSpec(n) => todo!(),
        ImportClause(n) => todo!(),
        ObjectPat(n) => todo!(),
        ObjectBindingElem(n) => todo!(),
        ArrayPat(n) => todo!(),
        ArrayBinding(n) => todo!(),
        EnumMember(n) => todo!(),
        ObjectShorthandMember(n) => todo!(),
        ObjectPropAssignment(n) => todo!(),
        ObjectMethodMember(n) => todo!(),
        SpreadAssignment(n) => todo!(),
        SpreadElement(n) => todo!(),
        TemplateHead(n) => todo!(),
        TemplateSpan(n) => todo!(),
        CaseClause(n) => todo!(),
        DefaultClause(n) => todo!(),
        CaseBlock(n) => todo!(),
        VarStmt(n) => todo!(),
        FnDecl(n) => todo!(),
        IfStmt(n) => todo!(),
        RetStmt(n) => todo!(),
        EmptyStmt(n) => todo!(),
        ClassDecl(n) => todo!(),
        ModuleDecl(n) => todo!(),
        ClassCtor(n) => todo!(),
        ClassPropElem(n) => todo!(),
        ClassMethodElem(n) => todo!(),
        ClassStaticBlockDecl(n) => todo!(),
        GetterDecl(n) => todo!(),
        SetterDecl(n) => todo!(),
        InterfaceDecl(n) => visit_interface_decl(v, n),
        TypeAliasDecl(n) => visit_type_alias_decl(v, n),
        InterfaceExtendsClause(n) => todo!(),
        ClassImplementsClause(n) => todo!(),
        BlockStmt(n) => todo!(),
        ModuleBlock(n) => todo!(),
        ThrowStmt(n) => todo!(),
        EnumDecl(n) => todo!(),
        ImportDecl(n) => todo!(),
        ExportDecl(n) => todo!(),
        ExportAssign(n) => todo!(),
        ForStmt(n) => todo!(),
        ForInStmt(n) => todo!(),
        ForOfStmt(n) => todo!(),
        WhileStmt(n) => todo!(),
        DoWhileStmt(n) => todo!(),
        BreakStmt(n) => todo!(),
        ContinueStmt(n) => todo!(),
        TryStmt(n) => todo!(),
        CatchClause(n) => todo!(),
        DebuggerStmt(n) => todo!(),
        LabeledStmt(n) => todo!(),
        SwitchStmt(n) => todo!(),
        ExprStmt(n) => todo!(),
        BinExpr(n) => todo!(),
        OmitExpr(n) => todo!(),
        ParenExpr(n) => todo!(),
        CondExpr(n) => todo!(),
        CallExpr(n) => todo!(),
        FnExpr(n) => todo!(),
        ClassExpr(n) => todo!(),
        NewExpr(n) => todo!(),
        AssignExpr(n) => todo!(),
        ArrowFnExpr(n) => todo!(),
        PrefixUnaryExpr(n) => todo!(),
        PostfixUnaryExpr(n) => todo!(),
        PropAccessExpr(n) => todo!(),
        EleAccessExpr(n) => todo!(),
        ThisExpr(n) => todo!(),
        TypeofExpr(n) => todo!(),
        VoidExpr(n) => todo!(),
        AwaitExpr(n) => todo!(),
        SuperExpr(n) => todo!(),
        AsExpr(n) => todo!(),
        TyAssertionExpr(n) => todo!(),
        SatisfiesExpr(n) => todo!(),
        NonNullExpr(n) => todo!(),
        TemplateExpr(n) => todo!(),
        TaggedTemplateExpr(n) => todo!(),
        DeleteExpr(n) => todo!(),
        NumLit(n) => todo!(),
        BigIntLit(n) => todo!(),
        BoolLit(n) => todo!(),
        NullLit(n) => todo!(),
        RegExpLit(n) => todo!(),
        StringLit(n) => todo!(),
        NoSubstitutionTemplateLit(n) => todo!(),
        ArrayLit(n) => todo!(),
        ObjectLit(n) => todo!(),
        Ident(n) => todo!(),
        ComputedPropName(n) => todo!(),
        ExprWithTyArgs(n) => todo!(),
        LitTy(n) => todo!(),
        ReferTy(n) => todo!(),
        ArrayTy(n) => todo!(),
        IndexedAccessTy(n) => todo!(),
        FnTy(n) => visit_fn_ty(v, n),
        CtorTy(n) => todo!(),
        ObjectLitTy(n) => todo!(),
        TyParam(n) => todo!(),
        IndexSigDecl(n) => todo!(),
        CallSigDecl(n) => todo!(),
        CtorSigDecl(n) => todo!(),
        PropSignature(n) => todo!(),
        MethodSignature(n) => todo!(),
        RestTy(n) => todo!(),
        NamedTupleTy(n) => todo!(),
        TupleTy(n) => todo!(),
        CondTy(n) => todo!(),
        IntersectionTy(n) => todo!(),
        UnionTy(n) => todo!(),
        TypeofTy(n) => todo!(),
        MappedTy(n) => todo!(),
        TyOp(n) => todo!(),
        PredTy(n) => todo!(),
        ParenTy(n) => todo!(),
        InferTy(n) => todo!(),
        NullableTy(n) => todo!(),
        TemplateLitTy(n) => todo!(),
        TemplateSpanTy(n) => todo!(),
        IntrinsicTy(n) => todo!(),
        ThisTy(n) => todo!(),
        QualifiedName(n) => todo!(),
        JsxText(n) => todo!(),
        JsxOpeningFrag(n) => todo!(),
        JsxClosingFrag(n) => todo!(),
        JsxOpeningElem(n) => todo!(),
        JsxClosingElem(n) => todo!(),
        JsxSelfClosingElem(n) => todo!(),
        JsxSpreadAttr(n) => todo!(),
        JsxNsName(n) => todo!(),
        JsxNamedAttr(n) => todo!(),
        JsxExpr(n) => todo!(),
        JsxFrag(n) => todo!(),
        JsxElem(n) => todo!(),
        PrivateIdent(n) => todo!(),
        YieldExpr(n) => todo!(),
    }
}
