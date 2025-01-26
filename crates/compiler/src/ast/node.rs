use bolt_ts_span::{ModuleID, Span};

use super::ModifierKind;

bolt_ts_utils::module_index!(NodeID);

impl NodeID {
    pub fn new(module: ModuleID, index: u32) -> Self {
        Self { module, index }
    }
    pub fn into_root(&self) -> Self {
        Self::new(self.module(), 0)
    }
}

bitflags::bitflags! {
  #[derive(Clone, Copy, Debug)]
  pub struct FnFlags: u32 {
        const NORMAL          = 0;
        const GENERATOR       = 1 << 0;
        const ASYNC           = 1 << 1;
        const INVALID         = 1 << 2;
        const ASYNC_GENERATOR = Self::ASYNC.bits() | Self::GENERATOR.bits();
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Node<'cx> {
    Program(&'cx super::Program<'cx>),

    // stmt
    VarStmt(&'cx super::VarStmt<'cx>),
    ParamDecl(&'cx super::ParamDecl<'cx>),
    FnDecl(&'cx super::FnDecl<'cx>),
    IfStmt(&'cx super::IfStmt<'cx>),
    RetStmt(&'cx super::RetStmt<'cx>),
    EmptyStmt(&'cx super::EmptyStmt),
    ClassDecl(&'cx super::ClassDecl<'cx>),
    NamespaceDecl(&'cx super::NsDecl<'cx>),
    ClassCtor(&'cx super::ClassCtor<'cx>),
    ClassPropElem(&'cx super::ClassPropElem<'cx>),
    ClassMethodElem(&'cx super::ClassMethodElem<'cx>),
    GetterDecl(&'cx super::GetterDecl<'cx>),
    SetterDecl(&'cx super::SetterDecl<'cx>),
    ClassExtendsClause(&'cx super::ClassExtendsClause<'cx>),
    InterfaceDecl(&'cx super::InterfaceDecl<'cx>),
    TypeDecl(&'cx super::TypeDecl<'cx>),
    InterfaceExtendsClause(&'cx super::InterfaceExtendsClause<'cx>),
    ClassImplementsClause(&'cx super::ClassImplementsClause<'cx>),
    BlockStmt(&'cx super::BlockStmt<'cx>),
    ThrowStmt(&'cx super::ThrowStmt<'cx>),
    EnumDecl(&'cx super::EnumDecl<'cx>),
    Modifier(&'cx super::Modifier),
    ShorthandSpec(&'cx super::ShorthandSpec<'cx>),
    NsImport(&'cx super::NsImport<'cx>),
    NsExport(&'cx super::NsExport<'cx>),
    GlobExport(&'cx super::GlobExport<'cx>),
    SpecsExport(&'cx super::SpecsExport<'cx>),
    ImportNamedSpec(&'cx super::ImportNamedSpec<'cx>),
    ExportNamedSpec(&'cx super::ExportNamedSpec<'cx>),
    ImportClause(&'cx super::ImportClause<'cx>),
    ImportDecl(&'cx super::ImportDecl<'cx>),
    ExportDecl(&'cx super::ExportDecl<'cx>),
    ForStmt(&'cx super::ForStmt<'cx>),
    ForInStmt(&'cx super::ForInStmt<'cx>),
    ForOfStmt(&'cx super::ForOfStmt<'cx>),
    WhileStmt(&'cx super::WhileStmt<'cx>),
    DoStmt(&'cx super::DoStmt<'cx>),
    BreakStmt(&'cx super::BreakStmt<'cx>),
    ContinueStmt(&'cx super::ContinueStmt<'cx>),
    TryStmt(&'cx super::TryStmt<'cx>),
    CatchClause(&'cx super::CatchClause<'cx>),
    ObjectBindingElem(&'cx super::ObjectBindingElem<'cx>),
    ObjectPat(&'cx super::ObjectPat<'cx>),

    // expr
    VarDecl(&'cx super::VarDecl<'cx>),
    BinExpr(&'cx super::BinExpr<'cx>),
    NumLit(&'cx super::NumLit),
    BoolLit(&'cx super::BoolLit),
    NullLit(&'cx super::NullLit),
    StringLit(&'cx super::StringLit),
    ArrayLit(&'cx super::ArrayLit<'cx>),
    Ident(&'cx super::Ident),
    OmitExpr(&'cx super::OmitExpr),
    ParenExpr(&'cx super::ParenExpr<'cx>),
    CondExpr(&'cx super::CondExpr<'cx>),
    EnumMember(&'cx super::EnumMember<'cx>),
    ObjectShorthandMember(&'cx super::ObjectShorthandMember<'cx>),
    ObjectPropMember(&'cx super::ObjectPropMember<'cx>),
    ObjectMethodMember(&'cx super::ObjectMethodMember<'cx>),
    ObjectLit(&'cx super::ObjectLit<'cx>),
    CallExpr(&'cx super::CallExpr<'cx>),
    FnExpr(&'cx super::FnExpr<'cx>),
    ClassExpr(&'cx super::ClassExpr<'cx>),
    NewExpr(&'cx super::NewExpr<'cx>),
    AssignExpr(&'cx super::AssignExpr<'cx>),
    ArrowFnExpr(&'cx super::ArrowFnExpr<'cx>),
    PrefixUnaryExpr(&'cx super::PrefixUnaryExpr<'cx>),
    PostfixUnaryExpr(&'cx super::PostfixUnaryExpr<'cx>),
    PropAccessExpr(&'cx super::PropAccessExpr<'cx>),
    EleAccessExpr(&'cx super::EleAccessExpr<'cx>),
    ThisExpr(&'cx super::ThisExpr),
    TypeofExpr(&'cx super::TypeofExpr<'cx>),
    VoidExpr(&'cx super::VoidExpr<'cx>),
    SuperExpr(&'cx super::SuperExpr),
    QualifiedName(&'cx super::QualifiedName<'cx>),

    // ty
    NumLitTy(&'cx super::NumLitTy),
    StringLitTy(&'cx super::StringLitTy),
    ReferTy(&'cx super::ReferTy<'cx>),
    ArrayTy(&'cx super::ArrayTy<'cx>),
    IndexedAccessTy(&'cx super::IndexedAccessTy<'cx>),
    FnTy(&'cx super::FnTy<'cx>),
    CtorTy(&'cx super::CtorTy<'cx>),
    ObjectLitTy(&'cx super::ObjectLitTy<'cx>),
    TyParam(&'cx super::TyParam<'cx>),
    MappedTyParam(&'cx super::MappedTyParam<'cx>),
    IndexSigDecl(&'cx super::IndexSigDecl<'cx>),
    CallSigDecl(&'cx super::CallSigDecl<'cx>),
    CtorSigDecl(&'cx super::CtorSigDecl<'cx>),
    PropSignature(&'cx super::PropSignature<'cx>),
    MethodSignature(&'cx super::MethodSignature<'cx>),
    RestTy(&'cx super::RestTy<'cx>),
    TupleTy(&'cx super::TupleTy<'cx>),
    CondTy(&'cx super::CondTy<'cx>),
    IntersectionTy(&'cx super::IntersectionTy<'cx>),
    UnionTy(&'cx super::UnionTy<'cx>),
    TypeofTy(&'cx super::TypeofTy<'cx>),
    MappedTy(&'cx super::MappedTy<'cx>),
    TyOp(&'cx super::TyOp<'cx>),
    PredTy(&'cx super::PredTy<'cx>),
}

impl<'cx> Node<'cx> {
    pub fn is_class_like(&self) -> bool {
        use Node::*;
        matches!(self, ClassDecl(_) | ClassExpr(_))
    }

    pub fn is_fn_decl_like(&self) -> bool {
        use Node::*;
        matches!(
            self,
            FnDecl(_) | FnExpr(_) | ClassMethodElem(_) | ClassCtor(_) | ArrowFnExpr(_)
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

    pub fn is_ty_refer_ty(&self) -> bool {
        // TODO: is_expr_with_ty_args
        self.is_refer_ty()
    }

    pub fn is_ty(&self) -> bool {
        self.as_ty().is_some()
    }

    pub fn as_ty(&self) -> Option<super::Ty<'cx>> {
        macro_rules! as_ty_node {
            ($( ($node_kind:ident, $ty_node_kind: ident)),* $(,)?) => {
                match self {
                    $(Node::$node_kind(n) => Some(super::Ty {
                        kind: super::TyKind::$ty_node_kind(n)
                    }),)*
                    _ => None,
                }
            };
        }
        as_ty_node!(
            (ReferTy, Refer),
            (ArrayTy, Array),
            (TupleTy, Tuple),
            (IndexedAccessTy, IndexedAccess),
            (FnTy, Fn),
            (ObjectLitTy, ObjectLit),
            (NumLitTy, NumLit),
            (StringLitTy, StringLit),
            (RestTy, Rest),
            (CondTy, Cond),
            (UnionTy, Union),
            (IntersectionTy, Intersection),
        )
    }

    pub fn ident_name(&self) -> Option<&'cx super::Ident> {
        use Node::*;
        match self {
            Ident(n) => Some(n),
            FnDecl(n) => Some(n.name),
            ClassDecl(n) => Some(n.name),
            ClassExpr(n) => n.name,
            ParamDecl(n) => Some(n.name),
            InterfaceDecl(n) => Some(n.name),
            ClassPropElem(n) => match n.name.kind {
                super::PropNameKind::Ident(ident) => Some(ident),
                _ => None,
            },
            ClassMethodElem(n) => match n.name.kind {
                super::PropNameKind::Ident(ident) => Some(ident),
                _ => None,
            },
            PropSignature(n) => match n.name.kind {
                super::PropNameKind::Ident(ident) => Some(ident),
                _ => None,
            },
            ObjectPropMember(n) => match n.name.kind {
                super::PropNameKind::Ident(ident) => Some(ident),
                _ => None,
            },
            ObjectMethodMember(n) => match n.name.kind {
                super::PropNameKind::Ident(ident) => Some(ident),
                _ => None,
            },
            ObjectShorthandMember(n) => Some(n.name),
            _ => None,
        }
    }

    pub fn ty_args(&self) -> Option<&'cx super::Tys<'cx>> {
        macro_rules! ty_args {
            ($($node_kind:ident),* $(,)?) => {
                match self {
                    $(Node::$node_kind(n) => n.ty_args,)*
                    _ => None,
                }
            };
        }
        ty_args!(ReferTy)
    }

    pub fn ty_params(&self) -> Option<super::TyParams<'cx>> {
        macro_rules! ty_params {
            ($($node_kind:ident),* $(,)?) => {
                match self {
                    $(Node::$node_kind(n) => n.ty_params,)*
                    _ => None,
                }
            };
        }
        ty_params!(
            FnDecl,
            FnExpr,
            ArrowFnExpr,
            ClassDecl,
            ClassCtor,
            CtorSigDecl,
            ClassMethodElem,
            TypeDecl,
            MethodSignature,
            CallSigDecl,
            InterfaceDecl,
            FnTy,
        )
    }

    pub fn params(&self) -> Option<super::ParamsDecl<'cx>> {
        macro_rules! params {
            ($($node_kind:ident),* $(,)?) => {
                match self {
                    $(Node::$node_kind(n) => Some(n.params),)*
                    _ => None,
                }
            };
        }
        params!(
            FnDecl,
            FnExpr,
            ArrowFnExpr,
            ClassCtor,
            CtorSigDecl,
            ClassMethodElem,
            MethodSignature,
            CallSigDecl,
            FnTy,
            CtorTy,
        )
    }

    pub fn ret_ty(&self) -> Option<&'cx super::Ty<'cx>> {
        macro_rules! dot_ty {
            ($($node_kind:ident),* $(,)?) => {
                match self {
                    $(Node::$node_kind(n) => Some(n.ty),)*
                    _ => None,
                }
            };
        }

        if let Some(ty) = dot_ty!(FnTy, IndexSigDecl) {
            return Some(ty);
        }

        macro_rules! dot_ty_with_option {
            ($($node_kind:ident),* $(,)?) => {
                match self {
                    $(Node::$node_kind(n) => n.ty,)*
                    _ => None,
                }
            };
        }

        dot_ty_with_option!(
            CallSigDecl,
            CtorSigDecl,
            FnDecl,
            MethodSignature,
            ClassMethodElem,
            FnExpr,
            ArrowFnExpr,
            VarDecl,
            ParamDecl,
            PropSignature,
            ClassPropElem,
            // TypePredicate,
            // ParenTy
            // TypeOp,
            // Mapped,
            // AssertionExpr
        )
    }

    pub fn ty_anno(&self) -> Option<&'cx super::Ty<'cx>> {
        if self.is_fn_decl() || self.is_ty_alias() {
            return None;
        }
        self.ret_ty()
    }

    pub fn is_decl(&self) -> bool {
        use super::Node::*;
        matches!(
            self,
            VarDecl(_)
                | ObjectShorthandMember(_)
                | ObjectPropMember(_)
                | ObjectMethodMember(_)
                | ClassDecl(_)
                | ClassExpr(_)
                | ClassPropElem(_)
                | ClassMethodElem(_)
                | ArrowFnExpr(_)
                | FnExpr(_)
                | ClassCtor(_)
                | FnDecl(_)
                | InterfaceDecl(_)
                | ParamDecl(_)
                | TyParam(_)
                | ShorthandSpec(_)
        )
    }

    pub fn fn_body(&self) -> Option<super::ArrowFnExprBody<'cx>> {
        if let Some(f) = self.as_arrow_fn_expr() {
            return Some(f.body);
        }
        use super::ArrowFnExprBody::Block;
        macro_rules! fn_body {
            ($($node_kind:ident),* $(,)?) => {
                match self {
                    $(Node::$node_kind(n) => Some(Block(&n.body)),)*
                    _ => None,
                }
            };
        }

        if let Some(body) = fn_body!(FnExpr) {
            return Some(body);
        }

        macro_rules! fn_body_with_option {
            ($( $node_kind:ident),* $(,)?) => {
                match self {
                    $(Node::$node_kind(n) if n.body.is_some() => Some(Block(n.body.unwrap())),)*
                    _ => None,
                }
            };
        }
        fn_body_with_option!(FnDecl, ClassMethodElem, ClassCtor)
    }

    pub fn fn_flags(&self) -> FnFlags {
        if !self.is_fn_like() {
            return FnFlags::INVALID;
        }
        let mut flags = FnFlags::NORMAL;
        if self.is_fn_decl() || self.is_fn_expr() || self.is_class_method_ele() {
            // todo: check aster token
        } else if self.as_arrow_fn_expr().is_some() {
            // todo: check async modifiers
        }

        if self.fn_body().is_none() {
            flags |= FnFlags::INVALID;
        }

        flags
    }

    pub fn module_name(&self) -> Option<&'cx super::StringLit> {
        match self {
            Node::ImportDecl(n) => Some(n.module),
            _ => None,
        }
    }

    #[inline]
    pub fn is_call_or_new_expr(&self) -> bool {
        self.is_call_expr() || self.is_new_expr()
    }

    #[inline]
    pub fn is_fn_expr_or_arrow_fnc_expr(&self) -> bool {
        self.is_fn_expr() || self.is_arrow_fn_expr()
    }

    pub fn is_class_ele(&self) -> bool {
        self.is_class_ctor()
            || self.is_class_prop_ele()
            || self.is_class_method_ele()
            || self.is_getter_decl()
            || self.is_setter_decl()
            || self.is_index_sig_decl()
    }

    pub fn is_static(&self) -> bool {
        (self.is_class_ele() && self.has_static_modifier()) || self.is_class_static_block_decl()
    }

    pub fn has_static_modifier(&self) -> bool {
        self.has_syntactic_modifier(enumflags2::BitFlags::from(ModifierKind::Static))
    }

    pub fn has_syntactic_modifier(&self, flags: enumflags2::BitFlags<ModifierKind>) -> bool {
        self.modifiers()
            .map_or(false, |ms| ms.flags.intersects(flags))
    }

    pub fn modifiers(&self) -> Option<&'cx super::Modifiers> {
        macro_rules! modifiers {
            ($( $node_kind:ident),* $(,)?) => {
                match self {
                    $(Node::$node_kind(n) => n.modifiers,)*
                    _ => None,
                }
            };
        }
        modifiers!(ClassMethodElem, ClassPropElem)
    }

    pub fn is_block_scope(&self, parent: Option<&Self>) -> bool {
        if self.is_program()
            || self.is_namespace_decl()
            || self.is_for_stmt()
            || self.is_for_in_stmt()
            || self.is_for_of_stmt()
            || self.is_class_ctor()
            || self.is_class_method_ele()
            || self.is_fn_decl()
            || self.is_fn_expr()
            || self.is_class_prop_ele()
        {
            true
        } else if self.is_block_stmt() {
            if let Some(parent) = parent {
                !parent.is_fn_like_or_class_static_block_decl()
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn node_flags(&self) -> super::NodeFlags {
        macro_rules! node_flags {
            ($( $node_kind:ident),* $(,)?) => {
                match self {
                    $(Node::$node_kind(n) => n.flags,)*
                    _ => Default::default(),
                }
            };
        }
        node_flags!(FnDecl, ClassMethodElem)
    }
}

macro_rules! as_node {
    ($( ($kind:ident, $ty: ty, $as_kind: ident, $expect_kind: ident, $is_kind: ident)),* $(,)?) => {
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
                #[track_caller]
                #[inline(always)]
                pub fn $as_kind(&self) -> Option<$ty> {
                    if let Node::$kind(n) = self {
                        Some(n)
                    } else {
                        None
                    }
                }
                #[track_caller]
                #[inline(always)]
                pub fn $is_kind(&self) -> bool {
                    self.$as_kind().is_some()
                }
                #[track_caller]
                #[inline(always)]
                pub fn $expect_kind(&self) -> $ty {
                    self.$as_kind().unwrap()
                }
            )*
        }
    };
}

as_node!(
    (
        Program,
        &'cx super::Program<'cx>,
        as_program,
        expect_program,
        is_program
    ),
    (
        VarStmt,
        &'cx super::VarStmt<'cx>,
        as_var_stmt,
        expect_var_stmt,
        is_var_stmt
    ),
    (
        ParamDecl,
        &'cx super::ParamDecl<'cx>,
        as_param_decl,
        expect_param_decl,
        is_param_decl
    ),
    (
        FnDecl,
        &'cx super::FnDecl<'cx>,
        as_fn_decl,
        expect_fn_decl,
        is_fn_decl
    ),
    (
        IfStmt,
        &'cx super::IfStmt<'cx>,
        as_if_stmt,
        expect_if_stmt,
        is_if_stmt
    ),
    (
        RetStmt,
        &'cx super::RetStmt<'cx>,
        as_ret_stmt,
        expect_ret_stmt,
        is_ret_stmt
    ),
    (
        EmptyStmt,
        &'cx super::EmptyStmt,
        as_empty_stmt,
        expect_empty_stmt,
        is_empty_stmt
    ),
    (
        ClassDecl,
        &'cx super::ClassDecl<'cx>,
        as_class_decl,
        expect_class_decl,
        is_class_decl
    ),
    (
        EnumDecl,
        &'cx super::EnumDecl<'cx>,
        as_enum_decl,
        expect_enum_decl,
        is_enum_decl
    ),
    (
        NamespaceDecl,
        &'cx super::NsDecl<'cx>,
        as_namespace_decl,
        expect_namespace_decl,
        is_namespace_decl
    ),
    (
        BlockStmt,
        &'cx super::BlockStmt<'cx>,
        as_block_stmt,
        expect_block_stmt,
        is_block_stmt
    ),
    (
        VarDecl,
        &'cx super::VarDecl<'cx>,
        as_var_decl,
        expect_var_decl,
        is_var_decl
    ),
    (
        BinExpr,
        &'cx super::BinExpr<'cx>,
        as_bin_expr,
        expect_bin_expr,
        is_bin_expr
    ),
    (
        NumLit,
        &'cx super::NumLit,
        as_num_lit,
        expect_num_lit,
        is_num_lit
    ),
    (
        BoolLit,
        &'cx super::BoolLit,
        as_bool_lit,
        expect_bool_lit,
        is_bool_lit
    ),
    (
        NullLit,
        &'cx super::NullLit,
        as_null_lit,
        expect_null_lit,
        is_null_lit
    ),
    (
        StringLit,
        &'cx super::StringLit,
        as_string_lit,
        expect_string_lit,
        is_string_lit
    ),
    (
        ArrayLit,
        &'cx super::ArrayLit<'cx>,
        as_array_lit,
        expect_array_lit,
        is_array_lit
    ),
    (Ident, &'cx super::Ident, as_ident, expect_ident, is_ident),
    (
        OmitExpr,
        &'cx super::OmitExpr,
        as_omit_expr,
        expect_omit_expr,
        is_omit_expr
    ),
    (
        ParenExpr,
        &'cx super::ParenExpr<'cx>,
        as_paren_expr,
        expect_paren_expr,
        is_paren_expr
    ),
    (
        CondExpr,
        &'cx super::CondExpr<'cx>,
        as_cond_expr,
        expect_cond_expr,
        is_cond_expr
    ),
    (
        EnumMember,
        &'cx super::EnumMember<'cx>,
        as_enum_member,
        expect_enum_member,
        is_enum_member
    ),
    (
        ObjectShorthandMember,
        &'cx super::ObjectShorthandMember<'cx>,
        as_object_shorthand_member,
        expect_object_shorthand_member,
        is_object_shorthand_member
    ),
    (
        ObjectPropMember,
        &'cx super::ObjectPropMember<'cx>,
        as_object_prop_member,
        expect_object_prop_member,
        is_object_prop_member
    ),
    (
        ObjectMethodMember,
        &'cx super::ObjectMethodMember<'cx>,
        as_object_method_member,
        expect_object_method_member,
        is_object_method_member
    ),
    (
        ObjectLit,
        &'cx super::ObjectLit<'cx>,
        as_object_lit,
        expect_object_lit,
        is_object_lit
    ),
    (
        CallExpr,
        &'cx super::CallExpr<'cx>,
        as_call_expr,
        expect_call_expr,
        is_call_expr
    ),
    (
        FnExpr,
        &'cx super::FnExpr<'cx>,
        as_fn_expr,
        expect_fn_expr,
        is_fn_expr
    ),
    (
        NewExpr,
        &'cx super::NewExpr<'cx>,
        as_new_expr,
        expect_new_expr,
        is_new_expr
    ),
    (
        AssignExpr,
        &'cx super::AssignExpr<'cx>,
        as_assign_expr,
        expect_assign_expr,
        is_assign_expr
    ),
    (
        ArrayTy,
        &'cx super::ArrayTy<'cx>,
        as_array_ty,
        expect_array_ty,
        is_array_ty
    ),
    (
        FnTy,
        &'cx super::FnTy<'cx>,
        as_fn_ty,
        expect_fn_ty,
        is_fn_ty
    ),
    (
        CtorTy,
        &'cx super::CtorTy<'cx>,
        as_ctor_ty,
        expect_ctor_ty,
        is_ctor_ty
    ),
    (
        StringLitTy,
        &'cx super::StringLitTy,
        as_string_lit_ty,
        expect_string_lit_ty,
        is_string_lit_ty
    ),
    (
        NumLitTy,
        &'cx super::NumLitTy,
        as_num_lit_ty,
        expect_num_lit_ty,
        is_num_lit_ty
    ),
    (
        ObjectLitTy,
        &'cx super::ObjectLitTy<'cx>,
        as_object_lit_ty,
        expect_object_lit_ty,
        is_object_lit_ty
    ),
    (
        TyParam,
        &'cx super::TyParam<'cx>,
        as_ty_param,
        expect_ty_param,
        is_ty_param
    ),
    (
        MappedTyParam,
        &'cx super::MappedTyParam<'cx>,
        as_mapped_ty_param,
        expect_mapped_ty_param,
        is_mapped_ty_param
    ),
    (
        Modifier,
        &'cx super::Modifier,
        as_modifier,
        expect_modifier,
        is_modifier
    ),
    (
        ClassPropElem,
        &'cx super::ClassPropElem<'cx>,
        as_class_prop_ele,
        expect_class_prop_ele,
        is_class_prop_ele
    ),
    (
        ClassMethodElem,
        &'cx super::ClassMethodElem<'cx>,
        as_class_method_ele,
        expect_class_method_ele,
        is_class_method_ele
    ),
    (
        ArrowFnExpr,
        &'cx super::ArrowFnExpr<'cx>,
        as_arrow_fn_expr,
        expect_arrow_fn_expr,
        is_arrow_fn_expr
    ),
    (
        PrefixUnaryExpr,
        &'cx super::PrefixUnaryExpr<'cx>,
        as_prefix_unary_expr,
        expect_prefix_unary_expr,
        is_prefix_unary_expr
    ),
    (
        PostfixUnaryExpr,
        &'cx super::PostfixUnaryExpr<'cx>,
        as_postfix_unary_expr,
        expect_postfix_unary_expr,
        is_postfix_unary_expr
    ),
    (
        ClassExpr,
        &'cx super::ClassExpr<'cx>,
        as_class_expr,
        expect_class_expr,
        is_class_expr
    ),
    (
        ClassExtendsClause,
        &'cx super::ClassExtendsClause<'cx>,
        as_class_extends_clause,
        expect_class_extends_clause,
        is_class_extends_clause
    ),
    (
        ClassImplementsClause,
        &'cx super::ClassImplementsClause<'cx>,
        as_class_implements_clause,
        expect_class_implements_clause,
        is_class_implements_clause
    ),
    (
        InterfaceExtendsClause,
        &'cx super::InterfaceExtendsClause<'cx>,
        as_interface_extends_clause,
        expect_interface_extends_clause,
        is_interface_extends_clause
    ),
    (
        IndexSigDecl,
        &'cx super::IndexSigDecl<'cx>,
        as_index_sig_decl,
        expect_index_sig_decl,
        is_index_sig_decl
    ),
    (
        PropAccessExpr,
        &'cx super::PropAccessExpr<'cx>,
        as_prop_access_expr,
        expect_prop_access_expr,
        is_prop_access_expr
    ),
    (
        TypeofExpr,
        &'cx super::TypeofExpr<'cx>,
        as_typeof_expr,
        expect_typeof_expr,
        is_typeof_expr
    ),
    (
        VoidExpr,
        &'cx super::VoidExpr<'cx>,
        as_void_expr,
        expect_void_expr,
        is_void_expr
    ),
    (
        EleAccessExpr,
        &'cx super::EleAccessExpr<'cx>,
        as_ele_access_expr,
        expect_ele_access_expr,
        is_ele_access_expr
    ),
    (
        ClassCtor,
        &'cx super::ClassCtor<'cx>,
        as_class_ctor,
        expect_class_ctor,
        is_class_ctor
    ),
    (
        GetterDecl,
        &'cx super::GetterDecl<'cx>,
        as_getter_decl,
        expect_getter_decl,
        is_getter_decl
    ),
    (
        SetterDecl,
        &'cx super::SetterDecl<'cx>,
        as_setter_decl,
        expect_setter_decl,
        is_setter_decl
    ),
    (
        CtorSigDecl,
        &'cx super::CtorSigDecl<'cx>,
        as_ctor_sig_decl,
        expect_ctor_sig_decl,
        is_ctor_sig_decl
    ),
    (
        CallSigDecl,
        &'cx super::CallSigDecl<'cx>,
        as_call_sig_decl,
        expect_call_sig_decl,
        is_call_sig_decl
    ),
    (
        InterfaceDecl,
        &'cx super::InterfaceDecl<'cx>,
        as_interface_decl,
        expect_interface_decl,
        is_interface_decl
    ),
    (
        PropSignature,
        &'cx super::PropSignature<'cx>,
        as_prop_signature,
        expect_prop_signature,
        is_prop_signature
    ),
    (
        MethodSignature,
        &'cx super::MethodSignature<'cx>,
        as_method_signature,
        expect_method_signature,
        is_method_signature
    ),
    (
        ThisExpr,
        &'cx super::ThisExpr,
        as_this_expr,
        expect_this_expr,
        is_this_expr
    ),
    (
        TypeDecl,
        &'cx super::TypeDecl<'cx>,
        as_type_decl,
        expect_type_decl,
        is_type_decl
    ),
    (
        RestTy,
        &'cx super::RestTy<'cx>,
        as_rest_ty,
        expect_rest_ty,
        is_rest_ty
    ),
    (
        TupleTy,
        &'cx super::TupleTy<'cx>,
        as_tuple_ty,
        expect_tuple_ty,
        is_tuple_ty
    ),
    (
        IndexedAccessTy,
        &'cx super::IndexedAccessTy<'cx>,
        as_indexed_access_ty,
        expect_indexed_access_ty,
        is_indexed_access_ty
    ),
    (
        CondTy,
        &'cx super::CondTy<'cx>,
        as_cond_ty,
        expect_cond_ty,
        is_cond_ty
    ),
    (
        ReferTy,
        &'cx super::ReferTy<'cx>,
        as_refer_ty,
        expect_refer_ty,
        is_refer_ty
    ),
    (
        IntersectionTy,
        &'cx super::IntersectionTy<'cx>,
        as_intersection_ty,
        expect_intersection_ty,
        is_intersection_ty
    ),
    (
        UnionTy,
        &'cx super::UnionTy<'cx>,
        as_union_ty,
        expect_union_ty,
        is_union_ty
    ),
    (
        TypeofTy,
        &'cx super::TypeofTy<'cx>,
        as_typeof_ty,
        expect_typeof_ty,
        is_typeof_ty
    ),
    (
        ThrowStmt,
        &'cx super::ThrowStmt<'cx>,
        as_throw_stmt,
        expect_throw_stmt,
        is_throw_stmt
    ),
    (
        ShorthandSpec,
        &'cx super::ShorthandSpec<'cx>,
        as_shorthand_spec,
        expect_shorthand_spec,
        is_shorthand_spec
    ),
    (
        NsImport,
        &'cx super::NsImport<'cx>,
        as_ns_import,
        expect_ns_import,
        is_ns_import
    ),
    (
        ImportNamedSpec,
        &'cx super::ImportNamedSpec<'cx>,
        as_import_named_spec,
        expect_import_named_spec,
        is_import_named_spec
    ),
    (
        ImportClause,
        &'cx super::ImportClause<'cx>,
        as_import_clause,
        expect_import_clause,
        is_import_clause
    ),
    (
        ImportDecl,
        &'cx super::ImportDecl<'cx>,
        as_import_decl,
        expect_import_decl,
        is_import_decl
    ),
    (
        NsExport,
        &'cx super::NsExport<'cx>,
        as_ns_export,
        expect_ns_export,
        is_ns_export
    ),
    (
        ExportNamedSpec,
        &'cx super::ExportNamedSpec<'cx>,
        as_export_named_spec,
        expect_export_named_spec,
        is_export_named_spec
    ),
    (
        ExportDecl,
        &'cx super::ExportDecl<'cx>,
        as_export_decl,
        expect_export_decl,
        is_export_decl
    ),
    (
        GlobExport,
        &'cx super::GlobExport<'cx>,
        as_glob_export,
        expect_glob_export,
        is_glob_export
    ),
    (
        SpecsExport,
        &'cx super::SpecsExport<'cx>,
        as_specs_export,
        expect_specs_export,
        is_specs_export
    ),
    (
        ForStmt,
        &'cx super::ForStmt<'cx>,
        as_for_stmt,
        expect_for_stmt,
        is_for_stmt
    ),
    (
        ForInStmt,
        &'cx super::ForInStmt<'cx>,
        as_for_in_stmt,
        expect_for_in_stmt,
        is_for_in_stmt
    ),
    (
        ForOfStmt,
        &'cx super::ForOfStmt<'cx>,
        as_for_of_stmt,
        expect_for_of_stmt,
        is_for_of_stmt
    ),
    (
        BreakStmt,
        &'cx super::BreakStmt<'cx>,
        as_break_stmt,
        expect_break_stmt,
        is_break_stmt
    ),
    (
        ContinueStmt,
        &'cx super::ContinueStmt<'cx>,
        as_continue_stmt,
        expect_continue_stmt,
        is_continue_stmt
    ),
    (
        SuperExpr,
        &'cx super::SuperExpr,
        as_super_expr,
        expect_super_expr,
        is_super_expr
    ),
    (
        MappedTy,
        &'cx super::MappedTy<'cx>,
        as_mapped_ty,
        expect_mapped_ty,
        is_mapped_ty
    ),
    (
        TyOp,
        &'cx super::TyOp<'cx>,
        as_ty_op,
        expect_ty_op,
        is_ty_op
    ),
    (
        TryStmt,
        &'cx super::TryStmt<'cx>,
        as_try_stmt,
        expect_try_stmt,
        is_try_stmt
    ),
    (
        CatchClause,
        &'cx super::CatchClause<'cx>,
        as_catch_clause,
        expect_catch_clause,
        is_catch_clause
    ),
    (
        DoStmt,
        &'cx super::DoStmt<'cx>,
        as_do_stmt,
        expect_do_stmt,
        is_do_stmt
    ),
    (
        WhileStmt,
        &'cx super::WhileStmt<'cx>,
        as_while_stmt,
        expect_while_stmt,
        is_while_stmt
    ),
    (
        QualifiedName,
        &'cx super::QualifiedName<'cx>,
        as_qualified_name,
        expect_qualified_name,
        is_qualified_name
    ),
    (
        ObjectPat,
        &'cx super::ObjectPat<'cx>,
        as_object_pat,
        expect_object_pat,
        is_object_pat
    ),
    (
        ObjectBindingElem,
        &'cx super::ObjectBindingElem<'cx>,
        as_object_binding_elem,
        expect_object_binding_elem,
        is_object_binding_elem
    ),
    (
        PredTy,
        &'cx super::PredTy<'cx>,
        as_pred_ty,
        expect_pred_ty,
        is_pred_ty
    )
);
