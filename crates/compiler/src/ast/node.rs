use bolt_ts_span::Span;

use crate::ast;

bolt_ts_span::new_index_with_module!(NodeID);

#[derive(Debug, Clone, Copy)]
pub enum Node<'cx> {
    Program(&'cx ast::Program<'cx>),

    // stmt
    VarStmt(&'cx ast::VarStmt<'cx>),
    ParamDecl(&'cx ast::ParamDecl<'cx>),
    FnDecl(&'cx ast::FnDecl<'cx>),
    IfStmt(&'cx ast::IfStmt<'cx>),
    RetStmt(&'cx ast::RetStmt<'cx>),
    EmptyStmt(&'cx ast::EmptyStmt),
    ClassDecl(&'cx ast::ClassDecl<'cx>),
    ClassCtor(&'cx ast::ClassCtor<'cx>),
    ClassPropEle(&'cx ast::ClassPropEle<'cx>),
    ClassMethodEle(&'cx ast::ClassMethodEle<'cx>),
    GetterDecl(&'cx ast::GetterDecl<'cx>),
    SetterDecl(&'cx ast::SetterDecl<'cx>),
    ClassExtendsClause(&'cx ast::ClassExtendsClause<'cx>),
    InterfaceDecl(&'cx ast::InterfaceDecl<'cx>),
    TypeDecl(&'cx ast::TypeDecl<'cx>),
    InterfaceExtendsClause(&'cx ast::InterfaceExtendsClause<'cx>),
    ImplementsClause(&'cx ast::ImplementsClause<'cx>),
    BlockStmt(&'cx ast::BlockStmt<'cx>),
    Modifier(&'cx ast::Modifier),

    // expr
    VarDecl(&'cx ast::VarDecl<'cx>),
    BinExpr(&'cx ast::BinExpr<'cx>),
    NumLit(&'cx ast::NumLit),
    BoolLit(&'cx ast::BoolLit),
    NullLit(&'cx ast::NullLit),
    StringLit(&'cx ast::StringLit),
    ArrayLit(&'cx ast::ArrayLit<'cx>),
    Ident(&'cx ast::Ident),
    OmitExpr(&'cx ast::OmitExpr),
    ParenExpr(&'cx ast::ParenExpr<'cx>),
    CondExpr(&'cx ast::CondExpr<'cx>),
    ObjectMemberField(&'cx ast::ObjectMemberField<'cx>),
    ObjectLit(&'cx ast::ObjectLit<'cx>),
    CallExpr(&'cx ast::CallExpr<'cx>),
    FnExpr(&'cx ast::FnExpr<'cx>),
    ClassExpr(&'cx ast::ClassExpr<'cx>),
    NewExpr(&'cx ast::NewExpr<'cx>),
    AssignExpr(&'cx ast::AssignExpr<'cx>),
    ArrowFnExpr(&'cx ast::ArrowFnExpr<'cx>),
    PrefixUnaryExpr(&'cx ast::PrefixUnaryExpr<'cx>),
    PropAccessExpr(&'cx ast::PropAccessExpr<'cx>),
    ThisExpr(&'cx ast::ThisExpr),

    // ty
    ArrayTy(&'cx ast::ArrayTy<'cx>),
    IndexedAccessTy(&'cx ast::IndexedAccessTy<'cx>),
    FnTy(&'cx ast::FnTy<'cx>),
    LitTy(&'cx ast::LitTy<'cx>),
    TyParam(&'cx ast::TyParam<'cx>),
    IndexSigDecl(&'cx ast::IndexSigDecl<'cx>),
    CallSigDecl(&'cx ast::CallSigDecl<'cx>),
    PropSignature(&'cx ast::PropSignature<'cx>),
    MethodSignature(&'cx ast::MethodSignature<'cx>),
    RestTy(&'cx ast::RestTy<'cx>),
    TupleTy(&'cx ast::TupleTy<'cx>),
    CondTy(&'cx ast::CondTy<'cx>),
    ReferTy(&'cx ast::ReferTy<'cx>),
}

impl Node<'_> {
    pub fn is_class_like(&self) -> bool {
        use Node::*;
        matches!(self, ClassDecl(_) | ClassExpr(_))
    }

    pub fn is_fn_decl_like(&self) -> bool {
        use Node::*;
        matches!(
            self,
            FnDecl(_) | FnExpr(_) | ClassMethodEle(_) | ClassCtor(_) | ArrowFnExpr(_)
        )
    }

    pub fn is_fn_like(&self) -> bool {
        use Node::*;
        matches!(self, IndexSigDecl(_)) | self.is_fn_decl_like()
    }

    pub fn is_class_static_block_decl(&self) -> bool {
        false
        // use Node::*;
        // matches!(self, _)
    }

    pub fn is_fn_like_or_class_static_block_decl(&self) -> bool {
        self.is_fn_like() || self.is_class_static_block_decl()
    }

    pub fn is_paren_type_node(&self) -> bool {
        // matches!(self, Node::ParenTy(_))
        false
    }

    pub fn is_ty_alias(&self) -> bool {
        self.is_type_decl()
    }

    pub fn is_ty_node(&self) -> bool {
        self.is_rest_ty()
            || self.is_tuple_ty()
            || self.is_indexed_access_ty()
            || self.is_cond_ty()
            || self.is_refer_ty()
            || self.is_ty_param()
    }

    pub fn is_ty_refer_ty(&self) -> bool {
        // TODO: is_expr_with_ty_args
        self.is_refer_ty()
    }
}

macro_rules! as_node {
    ($( ($kind:ident, $ty: ty, $as_kind: ident, $is_kind: ident)),* $(,)?) => {
        impl<'cx> Node<'cx> {
            pub fn id(&self) -> NodeID {
                match self {
                    $(Node::$kind(n) => n.id,)*
                }
            }

            pub fn span(&self) -> Span {
                match self {
                    $(Node::$kind(n) => n.span,)*
                }
            }

            $(
                #[inline(always)]
                pub fn $as_kind(&self) -> Option<$ty> {
                    if let Node::$kind(n) = self {
                        Some(n)
                    } else {
                        None
                    }
                }

                #[inline(always)]
                pub fn $is_kind(&self) -> bool {
                    self.$as_kind().is_some()
                }
            )*
        }
    };
}

as_node!(
    (Program, &'cx ast::Program<'cx>, as_program, is_program),
    (VarStmt, &'cx ast::VarStmt<'cx>, as_var_stmt, is_var_stmt),
    (
        ParamDecl,
        &'cx ast::ParamDecl<'cx>,
        as_param_decl,
        is_param_decl
    ),
    (FnDecl, &'cx ast::FnDecl<'cx>, as_fn_decl, is_fn_decl),
    (IfStmt, &'cx ast::IfStmt<'cx>, as_if_stmt, is_if_stmt),
    (RetStmt, &'cx ast::RetStmt<'cx>, as_ret_stmt, is_ret_stmt),
    (EmptyStmt, &'cx ast::EmptyStmt, as_empty_stmt, is_empty_stmt),
    (
        ClassDecl,
        &'cx ast::ClassDecl<'cx>,
        as_class_decl,
        is_class_decl
    ),
    (
        BlockStmt,
        &'cx ast::BlockStmt<'cx>,
        as_block_stmt,
        is_block_stmt
    ),
    (VarDecl, &'cx ast::VarDecl<'cx>, as_var_decl, is_var_decl),
    (BinExpr, &'cx ast::BinExpr<'cx>, as_bin_expr, is_bin_expr),
    (NumLit, &'cx ast::NumLit, as_num_lit, is_num_lit),
    (BoolLit, &'cx ast::BoolLit, as_bool_lit, is_bool_lit),
    (NullLit, &'cx ast::NullLit, as_null_lit, is_null_lit),
    (StringLit, &'cx ast::StringLit, as_string_lit, is_string_lit),
    (
        ArrayLit,
        &'cx ast::ArrayLit<'cx>,
        as_array_lit,
        is_array_lit
    ),
    (Ident, &'cx ast::Ident, as_ident, is_ident),
    (OmitExpr, &'cx ast::OmitExpr, as_omit_expr, is_omit_expr),
    (
        ParenExpr,
        &'cx ast::ParenExpr<'cx>,
        as_paren_expr,
        is_paren_expr
    ),
    (
        CondExpr,
        &'cx ast::CondExpr<'cx>,
        as_cond_expr,
        is_cond_expr
    ),
    (
        ObjectMemberField,
        &'cx ast::ObjectMemberField<'cx>,
        as_object_member_field,
        is_object_member_field
    ),
    (
        ObjectLit,
        &'cx ast::ObjectLit<'cx>,
        as_object_lit,
        is_object_lit
    ),
    (
        CallExpr,
        &'cx ast::CallExpr<'cx>,
        as_call_expr,
        is_call_expr
    ),
    (FnExpr, &'cx ast::FnExpr<'cx>, as_fn_expr, is_fn_expr),
    (NewExpr, &'cx ast::NewExpr<'cx>, as_new_expr, is_new_expr),
    (
        AssignExpr,
        &'cx ast::AssignExpr<'cx>,
        as_assign_expr,
        is_assign_expr
    ),
    (ArrayTy, &'cx ast::ArrayTy<'cx>, as_array_ty, is_array_ty),
    (FnTy, &'cx ast::FnTy<'cx>, as_fn_ty, is_fn_ty),
    (LitTy, &'cx ast::LitTy<'cx>, as_lit_ty, is_lit_ty),
    (TyParam, &'cx ast::TyParam<'cx>, as_ty_param, is_ty_param),
    (Modifier, &'cx ast::Modifier, as_modifier, is_modifier),
    (
        ClassPropEle,
        &'cx ast::ClassPropEle<'cx>,
        as_class_prop_ele,
        is_class_prop_ele
    ),
    (
        ClassMethodEle,
        &'cx ast::ClassMethodEle<'cx>,
        as_class_method_ele,
        is_class_method_ele
    ),
    (
        ArrowFnExpr,
        &'cx ast::ArrowFnExpr<'cx>,
        as_arrow_fn_expr,
        is_arrow_fn_expr
    ),
    (
        PrefixUnaryExpr,
        &'cx ast::PrefixUnaryExpr<'cx>,
        as_prefix_unary_expr,
        is_prefix_unary_expr
    ),
    (
        ClassExpr,
        &'cx ast::ClassExpr<'cx>,
        as_class_expr,
        is_class_expr
    ),
    (
        ClassExtendsClause,
        &'cx ast::ClassExtendsClause<'cx>,
        as_class_extends_clause,
        is_class_extends_clause
    ),
    (
        ImplementsClause,
        &'cx ast::ImplementsClause<'cx>,
        as_implements_clause,
        is_implements_clause
    ),
    (
        InterfaceExtendsClause,
        &'cx ast::InterfaceExtendsClause<'cx>,
        as_interface_extends_clause,
        is_interface_extends_clause
    ),
    (
        IndexSigDecl,
        &'cx ast::IndexSigDecl<'cx>,
        as_index_sig_decl,
        is_index_sig_decl
    ),
    (
        PropAccessExpr,
        &'cx ast::PropAccessExpr<'cx>,
        as_prop_access_expr,
        is_prop_access_expr
    ),
    (
        ClassCtor,
        &'cx ast::ClassCtor<'cx>,
        as_class_ctor,
        is_class_ctor
    ),
    (
        GetterDecl,
        &'cx ast::GetterDecl<'cx>,
        as_getter_decl,
        is_getter_decl
    ),
    (
        SetterDecl,
        &'cx ast::SetterDecl<'cx>,
        as_setter_decl,
        is_setter_decl
    ),
    (
        CallSigDecl,
        &'cx ast::CallSigDecl<'cx>,
        as_call_sig_decl,
        is_call_sig_decl
    ),
    (
        InterfaceDecl,
        &'cx ast::InterfaceDecl<'cx>,
        as_interface_decl,
        is_interface_decl
    ),
    (
        PropSignature,
        &'cx ast::PropSignature<'cx>,
        as_prop_signature,
        is_prop_signature
    ),
    (
        MethodSignature,
        &'cx ast::MethodSignature<'cx>,
        as_method_signature,
        is_method_signature
    ),
    (ThisExpr, &'cx ast::ThisExpr, as_this_expr, is_this_expr),
    (
        TypeDecl,
        &'cx ast::TypeDecl<'cx>,
        as_type_decl,
        is_type_decl
    ),
    (RestTy, &'cx ast::RestTy<'cx>, as_rest_ty, is_rest_ty),
    (TupleTy, &'cx ast::TupleTy<'cx>, as_tuple_ty, is_tuple_ty),
    (
        IndexedAccessTy,
        &'cx ast::IndexedAccessTy<'cx>,
        as_indexed_access_ty,
        is_indexed_access_ty
    ),
    (CondTy, &'cx ast::CondTy<'cx>, as_cond_ty, is_cond_ty),
    (ReferTy, &'cx ast::ReferTy<'cx>, as_refer_ty, is_refer_ty),
);
