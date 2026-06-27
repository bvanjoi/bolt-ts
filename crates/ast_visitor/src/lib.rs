mod visit;

use bolt_ts_ast as ast;

pub use self::visit::ControlFlow;
pub use self::visit::*;

pub trait VisitorResult {
    fn output() -> Self;
    fn branch(&self) -> ControlFlow;
}

impl VisitorResult for () {
    fn output() -> Self {
        ()
    }

    fn branch(&self) -> ControlFlow {
        ControlFlow::Continue
    }
}

impl VisitorResult for ControlFlow {
    fn output() -> Self {
        ControlFlow::Continue
    }

    fn branch(&self) -> ControlFlow {
        *self
    }
}

macro_rules! make_visitor {
    ( $( ($visit_node: ident, $ty: ty) ),* $(,)? ) => {
      pub trait Visitor<'cx>: Sized {
        type Result: VisitorResult;
        $(
          fn $visit_node(&mut self, node: &'cx $ty) -> Self::Result {
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
    (visit_class_ctor, ast::ClassCtor<'cx>),
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
    (visit_block_module_decl, ast::BlockModuleDecl<'cx>),
    (visit_nested_module_decl, ast::NestedModuleDecl<'cx>),
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
    (visit_empty_stmt, ast::EmptyStmt),
    (visit_cond_expr, ast::CondExpr<'cx>),
    (visit_paren_expr, ast::ParenExpr<'cx>),
    (visit_expr_with_ty_args, ast::ExprWithTyArgs<'cx>),
    (visit_prop_access_expr, ast::PropAccessExpr<'cx>),
    (visit_import_equals_decl, ast::ImportEqualsDecl<'cx>),
    (visit_num_lit, ast::NumLit),
    (visit_export_named_spec, ast::ExportNamedSpec<'cx>),
    (visit_export_shorthand_spec, ast::ExportShorthandSpec<'cx>),
    (visit_modifier, ast::Modifier),
    (visit_class_extends_clause, ast::ClassExtendsClause<'cx>),
    (visit_import_shorthand_spec, ast::ImportShorthandSpec<'cx>),
    (visit_ns_import, ast::NsImport<'cx>),
    (visit_ns_export, ast::NsExport<'cx>),
    (visit_glob_export, ast::GlobExport<'cx>),
    (visit_specs_export, ast::SpecsExport<'cx>),
    (visit_import_named_spec, ast::ImportNamedSpec<'cx>),
    (visit_import_clause, ast::ImportClause<'cx>),
    (visit_module_export_name, ast::ModuleExportName<'cx>),
    (visit_object_pat, ast::ObjectPat<'cx>),
    (visit_object_binding_elem, ast::ObjectBindingElem<'cx>),
    (visit_array_pat, ast::ArrayPat<'cx>),
    (visit_array_binding, ast::ArrayBinding<'cx>),
    (
        visit_object_shorthand_member,
        ast::ObjectShorthandMember<'cx>
    ),
    (visit_object_prop_assignment, ast::ObjectPropAssignment<'cx>),
    (visit_spread_assignment, ast::SpreadAssignment<'cx>),
    (visit_spread_element, ast::SpreadElement<'cx>),
    (visit_template_head, ast::TemplateHead),
    (visit_template_span, ast::TemplateSpan<'cx>),
    (visit_case_clause, ast::CaseClause<'cx>),
    (visit_default_clause, ast::DefaultClause<'cx>),
    (visit_case_block, ast::CaseBlock<'cx>),
    (visit_class_semi_elem, ast::ClassSemiElem),
    (
        visit_class_static_block_decl,
        ast::ClassStaticBlockDecl<'cx>
    ),
    (
        visit_interface_extends_clause,
        ast::InterfaceExtendsClause<'cx>
    ),
    (
        visit_class_implements_clause,
        ast::ClassImplementsClause<'cx>
    ),
    (visit_module_block, ast::ModuleBlock<'cx>),
    (visit_throw_stmt, ast::ThrowStmt<'cx>),
    (
        visit_external_module_reference,
        ast::ExternalModuleReference<'cx>
    ),
    (visit_export_decl, ast::ExportDecl<'cx>),
    (visit_for_stmt, ast::ForStmt<'cx>),
    (visit_for_in_stmt, ast::ForInStmt<'cx>),
    (visit_for_of_stmt, ast::ForOfStmt<'cx>),
    (visit_do_while_stmt, ast::DoWhileStmt<'cx>),
    (visit_break_stmt, ast::BreakStmt<'cx>),
    (visit_continue_stmt, ast::ContinueStmt<'cx>),
    (visit_catch_clause, ast::CatchClause<'cx>),
    (visit_debugger_stmt, ast::DebuggerStmt),
    (visit_labeled_stmt, ast::LabeledStmt<'cx>),
    (visit_switch_stmt, ast::SwitchStmt<'cx>),
    (visit_omit_expr, ast::OmitExpr),
    (visit_fn_expr, ast::FnExpr<'cx>),
    (visit_class_expr, ast::ClassExpr<'cx>),
    (visit_new_expr, ast::NewExpr<'cx>),
    (visit_prefix_unary_expr, ast::PrefixUnaryExpr<'cx>),
    (visit_postfix_unary_expr, ast::PostfixUnaryExpr<'cx>),
    (visit_ele_access_expr, ast::EleAccessExpr<'cx>),
    (visit_this_expr, ast::ThisExpr),
    (visit_typeof_expr, ast::TypeofExpr<'cx>),
    (visit_void_expr, ast::VoidExpr<'cx>),
    (visit_await_expr, ast::AwaitExpr<'cx>),
    (visit_super_expr, ast::SuperExpr),
    (visit_as_expr, ast::AsExpr<'cx>),
    (visit_ty_assertion_expr, ast::TyAssertion<'cx>),
    (visit_satisfies_expr, ast::SatisfiesExpr<'cx>),
    (visit_non_null_expr, ast::NonNullExpr<'cx>),
    (visit_template_expr, ast::TemplateExpr<'cx>),
    (visit_tagged_template_expr, ast::TaggedTemplateExpr<'cx>),
    (visit_delete_expr, ast::DeleteExpr<'cx>),
    (visit_import_expression, ast::ImportExpression),
    (visit_big_int_lit, ast::BigIntLit),
    (visit_bool_lit, ast::BoolLit),
    (visit_null_lit, ast::NullLit),
    (visit_reg_exp_lit, ast::RegExpLit),
    (
        visit_no_substitution_template_lit,
        ast::NoSubstitutionTemplateLit
    ),
    (visit_array_lit, ast::ArrayLit<'cx>),
    (visit_import_type, ast::ImportType<'cx>),
    (visit_template_span_ty, ast::TemplateSpanTy<'cx>),
    (visit_qualified_name, ast::QualifiedName<'cx>),
    (visit_new_meta_property, ast::NewMetaProperty<'cx>),
    (visit_jsx_text, ast::JsxText),
    (visit_jsx_opening_frag, ast::JsxOpeningFrag),
    (visit_jsx_closing_frag, ast::JsxClosingFrag),
    (visit_jsx_opening_elem, ast::JsxOpeningElem<'cx>),
    (visit_jsx_closing_elem, ast::JsxClosingElem<'cx>),
    (visit_jsx_self_closing_elem, ast::JsxSelfClosingElem<'cx>),
    (visit_jsx_spread_attr, ast::JsxSpreadAttr<'cx>),
    (visit_jsx_ns_name, ast::JsxNsName<'cx>),
    (visit_jsx_named_attr, ast::JsxNamedAttr<'cx>),
    (visit_jsx_expr, ast::JsxExpr<'cx>),
    (visit_jsx_frag, ast::JsxFrag<'cx>),
    (visit_jsx_elem, ast::JsxElem<'cx>),
);

#[macro_export]
macro_rules! noop_visit_node {
    ( $( ( $visit_node:ident, $ty:ty ) ),* $(,)? ) => {
        $(
            fn $visit_node(&mut self, _: &'cx $ty) -> Self::Result {
                <Self::Result as $crate::VisitorResult>::output()
            }
        )*
    };
}

#[macro_export]
macro_rules! noop_visit_type_node {
    () => {
        $crate::noop_visit_node!(
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
            (visit_this_ty, ast::ThisTy),
            (visit_interface_decl, ast::InterfaceDecl<'cx>),
            (visit_type_alias_decl, ast::TypeAliasDecl<'cx>),
            (visit_index_sig_decl, ast::IndexSigDecl<'cx>),
            (visit_prop_signature, ast::PropSignature<'cx>),
            (visit_method_signature, ast::MethodSignature<'cx>),
            (visit_call_sig_decl, ast::CallSigDecl<'cx>),
            (visit_ctor_sig_decl, ast::CtorSigDecl<'cx>),
        );
    };
}

#[macro_export]
macro_rules! noop_visit_function_like_node {
    () => {
        $crate::noop_visit_node!(
            (visit_fn_decl, ast::FnDecl<'cx>),
            (visit_class_method_elem, ast::ClassMethodElem<'cx>),
            (visit_object_method_member, ast::ObjectMethodMember<'cx>),
            (visit_class_ctor, ast::ClassCtor<'cx>),
            (visit_getter_decl, ast::GetterDecl<'cx>),
            (visit_setter_decl, ast::SetterDecl<'cx>),
            (visit_fn_expr, ast::FnExpr<'cx>),
            (visit_arrow_fn_expr, ast::ArrowFnExpr<'cx>),
            (visit_method_signature, ast::MethodSignature<'cx>),
            (visit_call_sig_decl, ast::CallSigDecl<'cx>),
            // TODO: js_doc_sig
            (visit_ctor_sig_decl, ast::CtorSigDecl<'cx>),
            (visit_index_sig_decl, ast::IndexSigDecl<'cx>),
            (visit_fn_ty, ast::FnTy<'cx>),
            (visit_ctor_ty, ast::CtorTy<'cx>),
        );
    };
}
