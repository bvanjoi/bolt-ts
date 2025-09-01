use bolt_ts_span::ModuleID;

use super::{ExprKind, ModifierKind};

bolt_ts_utils::module_index!(NodeID);

impl NodeID {
    pub const fn new(module: ModuleID, index: u32) -> Self {
        Self { module, index }
    }
    pub const fn into_root(&self) -> Self {
        Self::new(self.module(), 0)
    }
}

bitflags::bitflags! {
  #[derive(Clone, Copy, Debug)]
  pub struct FnFlags: u8 {
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

    Modifier(&'cx super::Modifier),
    VarDecl(&'cx super::VarDecl<'cx>),
    ParamDecl(&'cx super::ParamDecl<'cx>),
    ClassExtendsClause(&'cx super::ClassExtendsClause<'cx>),
    ShorthandSpec(&'cx super::ShorthandSpec<'cx>),
    NsImport(&'cx super::NsImport<'cx>),
    NsExport(&'cx super::NsExport<'cx>),
    GlobExport(&'cx super::GlobExport<'cx>),
    SpecsExport(&'cx super::SpecsExport<'cx>),
    ExportNamedSpec(&'cx super::ExportNamedSpec<'cx>),
    ImportNamedSpec(&'cx super::ImportNamedSpec<'cx>),
    ImportClause(&'cx super::ImportClause<'cx>),
    ObjectPat(&'cx super::ObjectPat<'cx>),
    ObjectBindingElem(&'cx super::ObjectBindingElem<'cx>),
    ArrayPat(&'cx super::ArrayPat<'cx>),
    ArrayBinding(&'cx super::ArrayBinding<'cx>),
    EnumMember(&'cx super::EnumMember<'cx>),
    ObjectShorthandMember(&'cx super::ObjectShorthandMember<'cx>),
    ObjectPropAssignment(&'cx super::ObjectPropAssignment<'cx>),
    ObjectMethodMember(&'cx super::ObjectMethodMember<'cx>),
    SpreadAssignment(&'cx super::SpreadAssignment<'cx>),
    SpreadElement(&'cx super::SpreadElement<'cx>),
    // template
    TemplateHead(&'cx super::TemplateHead),
    TemplateSpan(&'cx super::TemplateSpan<'cx>),
    // switch
    CaseClause(&'cx super::CaseClause<'cx>),
    DefaultClause(&'cx super::DefaultClause<'cx>),
    CaseBlock(&'cx super::CaseBlock<'cx>),

    // stmt
    VarStmt(&'cx super::VarStmt<'cx>),
    FnDecl(&'cx super::FnDecl<'cx>),
    IfStmt(&'cx super::IfStmt<'cx>),
    RetStmt(&'cx super::RetStmt<'cx>),
    EmptyStmt(&'cx super::EmptyStmt),
    ClassDecl(&'cx super::ClassDecl<'cx>),
    ModuleDecl(&'cx super::ModuleDecl<'cx>),
    ClassCtor(&'cx super::ClassCtor<'cx>),
    ClassPropElem(&'cx super::ClassPropElem<'cx>),
    ClassMethodElem(&'cx super::ClassMethodElem<'cx>),
    ClassStaticBlockDecl(&'cx super::ClassStaticBlockDecl<'cx>),
    GetterDecl(&'cx super::GetterDecl<'cx>),
    SetterDecl(&'cx super::SetterDecl<'cx>),
    InterfaceDecl(&'cx super::InterfaceDecl<'cx>),
    TypeAliasDecl(&'cx super::TypeAliasDecl<'cx>),
    InterfaceExtendsClause(&'cx super::InterfaceExtendsClause<'cx>),
    ClassImplementsClause(&'cx super::ClassImplementsClause<'cx>),
    BlockStmt(&'cx super::BlockStmt<'cx>),
    ModuleBlock(&'cx super::ModuleBlock<'cx>),
    ThrowStmt(&'cx super::ThrowStmt<'cx>),
    EnumDecl(&'cx super::EnumDecl<'cx>),
    ImportDecl(&'cx super::ImportDecl<'cx>),
    ExportDecl(&'cx super::ExportDecl<'cx>),
    ExportAssign(&'cx super::ExportAssign<'cx>),
    ForStmt(&'cx super::ForStmt<'cx>),
    ForInStmt(&'cx super::ForInStmt<'cx>),
    ForOfStmt(&'cx super::ForOfStmt<'cx>),
    WhileStmt(&'cx super::WhileStmt<'cx>),
    DoStmt(&'cx super::DoStmt<'cx>),
    BreakStmt(&'cx super::BreakStmt<'cx>),
    ContinueStmt(&'cx super::ContinueStmt<'cx>),
    TryStmt(&'cx super::TryStmt<'cx>),
    CatchClause(&'cx super::CatchClause<'cx>),
    DebuggerStmt(&'cx super::DebuggerStmt),
    LabeledStmt(&'cx super::LabeledStmt<'cx>),
    SwitchStmt(&'cx super::SwitchStmt<'cx>),
    ExprStmt(&'cx super::ExprStmt<'cx>),

    // expr
    BinExpr(&'cx super::BinExpr<'cx>),
    OmitExpr(&'cx super::OmitExpr),
    ParenExpr(&'cx super::ParenExpr<'cx>),
    CondExpr(&'cx super::CondExpr<'cx>),
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
    AsExpr(&'cx super::AsExpr<'cx>),
    TyAssertionExpr(&'cx super::TyAssertion<'cx>),
    SatisfiesExpr(&'cx super::SatisfiesExpr<'cx>),
    NonNullExpr(&'cx super::NonNullExpr<'cx>),
    TemplateExpr(&'cx super::TemplateExpr<'cx>),
    TaggedTemplateExpr(&'cx super::TaggedTemplateExpr<'cx>),
    NumLit(&'cx super::NumLit),
    BigIntLit(&'cx super::BigIntLit),
    BoolLit(&'cx super::BoolLit),
    NullLit(&'cx super::NullLit),
    RegExpLit(&'cx super::RegExpLit),
    StringLit(&'cx super::StringLit),
    NoSubstitutionTemplateLit(&'cx super::NoSubstitutionTemplateLit),
    ArrayLit(&'cx super::ArrayLit<'cx>),
    ObjectLit(&'cx super::ObjectLit<'cx>),
    Ident(&'cx super::Ident),
    Binding(&'cx super::Binding<'cx>),
    ComputedPropName(&'cx super::ComputedPropName<'cx>),
    ExprWithTyArgs(&'cx super::ExprWithTyArgs<'cx>),

    // ty
    LitTy(&'cx super::LitTy),
    ReferTy(&'cx super::ReferTy<'cx>),
    ArrayTy(&'cx super::ArrayTy<'cx>),
    IndexedAccessTy(&'cx super::IndexedAccessTy<'cx>),
    FnTy(&'cx super::FnTy<'cx>),
    CtorTy(&'cx super::CtorTy<'cx>),
    ObjectLitTy(&'cx super::ObjectLitTy<'cx>),
    TyParam(&'cx super::TyParam<'cx>),
    IndexSigDecl(&'cx super::IndexSigDecl<'cx>),
    CallSigDecl(&'cx super::CallSigDecl<'cx>),
    CtorSigDecl(&'cx super::CtorSigDecl<'cx>),
    PropSignature(&'cx super::PropSignature<'cx>),
    MethodSignature(&'cx super::MethodSignature<'cx>),
    RestTy(&'cx super::RestTy<'cx>),
    NamedTupleTy(&'cx super::NamedTupleTy<'cx>),
    TupleTy(&'cx super::TupleTy<'cx>),
    CondTy(&'cx super::CondTy<'cx>),
    IntersectionTy(&'cx super::IntersectionTy<'cx>),
    UnionTy(&'cx super::UnionTy<'cx>),
    TypeofTy(&'cx super::TypeofTy<'cx>),
    MappedTy(&'cx super::MappedTy<'cx>),
    TyOp(&'cx super::TyOp<'cx>),
    PredTy(&'cx super::PredTy<'cx>),
    ParenTy(&'cx super::ParenTy<'cx>),
    InferTy(&'cx super::InferTy<'cx>),
    NullableTy(&'cx super::NullableTy<'cx>),
    TemplateLitTy(&'cx super::TemplateLitTy<'cx>),
    TemplateSpanTy(&'cx super::TemplateSpanTy<'cx>),
    IntrinsicTy(&'cx super::IntrinsicTy),
    ThisTy(&'cx super::ThisTy),
    QualifiedName(&'cx super::QualifiedName<'cx>),

    JsxText(&'cx super::JsxText),
    JsxOpeningFrag(&'cx super::JsxOpeningFrag),
    JsxClosingFrag(&'cx super::JsxClosingFrag),
    JsxOpeningElem(&'cx super::JsxOpeningElem<'cx>),
    JsxClosingElem(&'cx super::JsxClosingElem<'cx>),
    JsxSelfClosingElem(&'cx super::JsxSelfClosingElem<'cx>),
    JsxSpreadAttr(&'cx super::JsxSpreadAttr<'cx>),
    JsxNsName(&'cx super::JsxNsName<'cx>),
    JsxNamedAttr(&'cx super::JsxNamedAttr<'cx>),
    JsxExpr(&'cx super::JsxExpr<'cx>),
    JsxFrag(&'cx super::JsxFrag<'cx>),
    JsxElem(&'cx super::JsxElem<'cx>),
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
            FnDecl(_)
                | ClassMethodElem(_)
                | ObjectMethodMember(_)
                | ClassCtor(_)
                | GetterDecl(_)
                | SetterDecl(_)
                | FnExpr(_)
                | ArrowFnExpr(_)
        )
    }

    pub fn is_fn_like(&self) -> bool {
        use Node::*;
        matches!(
            self,
            MethodSignature(_)
                | CallSigDecl(_)
                | CtorSigDecl(_)
                | IndexSigDecl(_)
                | FnTy(_)
                | CtorTy(_)
        ) || self.is_fn_decl_like()
    }

    pub fn is_fn_like_and_has_asterisk(&self) -> bool {
        // TODO: handle asterisk
        false
    }

    pub fn is_fn_like_or_class_static_block_decl(&self) -> bool {
        self.is_fn_like() || self.is_class_static_block_decl()
    }

    pub fn is_paren_type_node(&self) -> bool {
        // matches!(self, Node::ParenTy(_))
        false
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
            ($( $ty:ident ),* $(,)?) => {
                ::paste::paste! {
                    match self {
                        $(Node::[<$ty Ty>](n) => Some(super::Ty {
                            kind: super::TyKind::$ty(n)
                        }),)*
                        _ => None,
                    }
                }
            };
        }
        as_ty_node!(
            Refer,
            Array,
            Tuple,
            IndexedAccess,
            Fn,
            ObjectLit,
            Lit,
            Rest,
            Cond,
            Union,
            Intersection,
            Mapped,
            Infer,
        )
    }

    pub fn name(&self) -> Option<super::DeclarationName<'cx>> {
        use super::DeclarationName;
        use Node::*;

        match self {
            PropSignature(n) => Some(DeclarationName::from_prop_name(n.name)),
            ClassPropElem(prop) => Some(DeclarationName::from_prop_name(prop.name)),
            SetterDecl(n) => Some(DeclarationName::from_prop_name(n.name)),
            GetterDecl(n) => Some(DeclarationName::from_prop_name(n.name)),
            MethodSignature(n) => Some(DeclarationName::from_prop_name(n.name)),
            ObjectPropAssignment(prop) => Some(DeclarationName::from_prop_name(prop.name)),
            ClassMethodElem(prop) => Some(DeclarationName::from_prop_name(prop.name)),
            ObjectMethodMember(prop) => Some(DeclarationName::from_prop_name(prop.name)),
            ObjectShorthandMember(prop) => Some(DeclarationName::Ident(prop.name)),
            FnDecl(n) => Some(DeclarationName::Ident(n.name)),
            _ => None,
        }
    }

    pub fn ident_name(&self) -> Option<&'cx super::Ident> {
        use Node::*;
        match self {
            Ident(n) => Some(n),
            FnDecl(n) => Some(n.name),
            ClassDecl(n) => n.name,
            ClassExpr(n) => n.name,
            ParamDecl(n) => match n.name.kind {
                super::BindingKind::Ident(n) => Some(n),
                super::BindingKind::ObjectPat(_) => None,
                super::BindingKind::ArrayPat(_) => None,
            },
            InterfaceDecl(n) => Some(n.name),
            TypeAliasDecl(n) => Some(n.name),
            ClassPropElem(n) => match n.name.kind {
                super::PropNameKind::Ident(ident) => Some(ident),
                _ => None,
            },
            ClassMethodElem(n) => match n.name.kind {
                super::PropNameKind::Ident(ident) => Some(ident),
                _ => None,
            },
            GetterDecl(n) => match n.name.kind {
                super::PropNameKind::Ident(ident) => Some(ident),
                _ => None,
            },
            PropSignature(n) => match n.name.kind {
                super::PropNameKind::Ident(ident) => Some(ident),
                _ => None,
            },
            ObjectPropAssignment(n) => match n.name.kind {
                super::PropNameKind::Ident(ident) => Some(ident),
                _ => None,
            },
            ObjectMethodMember(n) => match n.name.kind {
                super::PropNameKind::Ident(ident) => Some(ident),
                _ => None,
            },
            ObjectShorthandMember(n) => Some(n.name),
            TyParam(n) => Some(n.name),
            ModuleDecl(n) => match n.name {
                crate::ModuleName::Ident(ident) => Some(ident),
                crate::ModuleName::StringLit(_) => None,
            },
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
            TypeAliasDecl,
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
            CtorTy,
            ClassMethodElem,
            MethodSignature,
            ObjectMethodMember,
            CallSigDecl,
            FnTy,
            SetterDecl,
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

        if let Some(ty) = dot_ty!(FnTy, IndexSigDecl, CtorTy) {
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
            GetterDecl,
        )
    }

    pub fn ty_anno(&self) -> Option<&'cx super::Ty<'cx>> {
        // TODO !self.is_in_js_file
        if self.is_fn_decl() {
            return None;
        }
        if self.is_type_alias_decl() {
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
                | ObjectPropAssignment(_)
                | PropSignature(_)
                | ObjectMethodMember(_)
                | MethodSignature(_)
                | ComputedPropName(_)
                | ClassDecl(_)
                | ClassExpr(_)
                | ClassPropElem(_)
                | ClassMethodElem(_)
                | EnumDecl(_)
                | EnumMember(_)
                | ArrowFnExpr(_)
                | FnExpr(_)
                | ClassCtor(_)
                | FnDecl(_)
                | InterfaceDecl(_)
                | ParamDecl(_)
                | TyParam(_)
                | NsImport(_)
                | ShorthandSpec(_)
                | ExportNamedSpec(_)
                | GetterDecl(_)
                | SetterDecl(_)
                | ObjectLit(_)
                | Program(_)
                | ModuleDecl(_)
                | Binding(_)
        )
    }

    pub fn is_stmt_but_not_decl(&self) -> bool {
        use super::Node::*;
        matches!(
            self,
            BreakStmt(_)
                | ContinueStmt(_)
                | DoStmt(_)
                | EmptyStmt(_)
                | ForStmt(_)
                | ForInStmt(_)
                | ForOfStmt(_)
                | IfStmt(_)
                | RetStmt(_)
                | ThrowStmt(_)
                | VarStmt(_)
                | WhileStmt(_)
        )
    }

    pub fn fn_body(&self) -> Option<super::ArrowFnExprBody<'cx>> {
        if let Some(f) = self.as_arrow_fn_expr() {
            return Some(f.body);
        }
        macro_rules! fn_body {
            ($($node_kind:ident),* $(,)?) => {
                match self {
                    $(Node::$node_kind(n) => Some(super::ArrowFnExprBody::Block(&n.body)),)*
                    _ => None,
                }
            };
        }

        if let Some(body) = fn_body!(FnExpr, ObjectMethodMember) {
            return Some(body);
        }

        macro_rules! fn_body_with_option {
            ($( $node_kind:ident),* $(,)?) => {
                match self {
                    $(Node::$node_kind(n) if n.body.is_some() => Some(super::ArrowFnExprBody::Block(n.body.unwrap())),)*
                    _ => None,
                }
            };
        }
        fn_body_with_option!(FnDecl, ClassMethodElem, ClassCtor, GetterDecl)
    }

    pub fn fn_flags(&self) -> FnFlags {
        if !self.is_fn_like() {
            return FnFlags::INVALID;
        }
        let mut flags = FnFlags::NORMAL;
        if self.is_fn_decl() || self.is_fn_expr() || self.is_class_method_elem() {
            // todo: check aster token
        } else if self.as_arrow_fn_expr().is_some()
            && self.has_syntactic_modifier(self::ModifierKind::Async.into())
        {
            flags |= FnFlags::GENERATOR;
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
            || self.is_class_prop_elem()
            || self.is_class_method_elem()
            || self.is_getter_decl()
            || self.is_setter_decl()
            || self.is_index_sig_decl()
    }

    pub fn is_static(&self) -> bool {
        (self.is_class_ele() && self.has_static_modifier()) || self.is_class_static_block_decl()
    }

    pub fn has_static_modifier(&self) -> bool {
        self.has_syntactic_modifier(ModifierKind::Static.into())
    }

    pub fn has_effective_readonly_modifier(&self) -> bool {
        self.has_syntactic_modifier(ModifierKind::Readonly.into())
    }

    pub fn has_effective_modifier(&self, kind: ModifierKind) -> bool {
        self.has_syntactic_modifier(kind.into())
    }

    pub fn has_syntactic_modifier(&self, flags: enumflags2::BitFlags<ModifierKind>) -> bool {
        self.modifiers()
            .is_some_and(|ms| ms.flags.intersects(flags))
    }

    pub fn modifiers(&self) -> Option<&super::Modifiers<'cx>> {
        macro_rules! modifiers {
            ($( $node_kind:ident),* $(,)?) => {
                match self {
                    $(Node::$node_kind(n) => n.modifiers,)*
                    _ => None,
                }
            };
        }
        modifiers!(
            ClassCtor,
            ClassMethodElem,
            ClassPropElem,
            GetterDecl,
            SetterDecl,
            PropSignature,
            FnDecl,
            VarStmt,
            ClassDecl,
            ModuleDecl,
            TypeAliasDecl,
            InterfaceDecl,
            ParamDecl,
            IndexSigDecl,
        )
    }

    pub fn can_have_modifiers(&self) -> bool {
        self.modifiers().is_some()
    }

    pub fn is_block_scope(&self, parent: Option<&Self>) -> bool {
        if self.is_program()
            || self.is_module_decl()
            || self.is_for_stmt()
            || self.is_for_in_stmt()
            || self.is_for_of_stmt()
            || self.is_class_ctor()
            || self.is_class_method_elem()
            || self.is_fn_decl()
            || self.is_fn_expr()
            || self.is_arrow_fn_expr()
            || self.is_class_prop_elem()
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

    pub fn is_super_call(&self) -> bool {
        self.as_call_expr()
            .is_some_and(|expr| matches!(expr.expr.kind, ExprKind::Super(_)))
    }

    pub fn is_optional_decl(&self) -> bool {
        match self {
            Node::PropSignature(n) => n.question.is_some(),
            _ => false,
        }
    }

    pub fn is_assertion_expr(&self) -> bool {
        // TODO: is_type_assertion_expr
        self.is_as_expr()
    }

    pub fn has_flow_node(&self) -> bool {
        macro_rules! has_flow_node {
            ($( $node_kind:ident),* $(,)?) => {
                match self {
                    $(Node::$node_kind(_) => true,)*
                    _ => false,
                }
            };
        }
        has_flow_node!(Ident, ThisExpr, SuperExpr)
    }

    pub fn is_stmt(&self) -> bool {
        self.is_block_stmt() || self.is_decl() || self.is_stmt_but_not_decl()
    }

    pub fn ge_first_stmt_and_le_last_stmt(&self) -> bool {
        use self::Node::*;
        // same as `n >= FirstStatement && n <= LastStatement`
        matches!(
            self,
            VarStmt(_)
                | IfStmt(_)
                | DoStmt(_)
                | WhileStmt(_)
                | ExprStmt(_)
                | ForStmt(_)
                | ForInStmt(_)
                | ForOfStmt(_)
                | ContinueStmt(_)
                | BreakStmt(_)
                | RetStmt(_)
                // | WithStmt(_)
                // | SwitchStmt(_)
                // | LabeledStmt(_)
                | ThrowStmt(_)
                | TryStmt(_)
                | DebuggerStmt(_)
        )
    }

    pub fn has_locals(&self) -> bool {
        use Node::*;

        matches!(
            self,
            ArrowFnExpr(_)
                | BlockStmt(_)
                | CallSigDecl(_)
                | CatchClause(_)
                | ClassCtor(_)
                | CondTy(_)
                | CtorTy(_)
                | CtorSigDecl(_)
                | ClassMethodElem(_)
                | ForStmt(_)
                | ForInStmt(_)
                | ForOfStmt(_)
                | FnDecl(_)
                | FnExpr(_)
                | FnTy(_)
                | GetterDecl(_)
                | IndexSigDecl(_)
                | MappedTy(_)
                | MethodSignature(_)
                | ModuleDecl(_)
                | ObjectMethodMember(_)
                | Program(_)
                | SetterDecl(_)
                | TypeAliasDecl(_)
                | ClassStaticBlockDecl(_)
        )
    }

    pub fn is_readonly_ty_op(&self) -> bool {
        self.as_ty_op()
            .is_some_and(|n| n.op == super::TyOpKind::Readonly)
    }

    pub fn is_access_expr(&self) -> bool {
        self.is_prop_access_expr() || self.is_ele_access_expr()
    }

    pub fn is_same_kind(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }

    pub fn is_effective_module_decl(&self) -> bool {
        self.is_ident() || self.is_module_decl()
    }

    pub fn is_ambient_module(&self) -> bool {
        self.as_module_decl().is_some_and(|n| n.is_ambient())
    }

    pub fn get_external_module_name(&self) -> Option<&'cx super::StringLit> {
        use Node::*;
        match self {
            ImportDecl(n) => Some(n.module),
            ExportDecl(n) => n.module_spec(),
            ModuleDecl(n) => match n.name {
                crate::ModuleName::StringLit(n) => Some(n),
                _ => None,
            },
            _ => unreachable!(),
        }
    }

    pub fn import_export_spec_name(&self) -> Option<bolt_ts_atom::Atom> {
        use Node::*;
        match self {
            ShorthandSpec(n) => Some(n.name.name),
            ImportNamedSpec(n) => match n.prop_name.kind {
                crate::ModuleExportNameKind::Ident(ident) => Some(ident.name),
                crate::ModuleExportNameKind::StringLit(lit) => Some(lit.val),
            },
            ExportNamedSpec(n) => match n.prop_name.kind {
                crate::ModuleExportNameKind::Ident(ident) => Some(ident.name),
                crate::ModuleExportNameKind::StringLit(lit) => Some(lit.val),
            },
            _ => None,
        }
    }

    pub fn initializer(&self) -> Option<&'cx super::Expr<'cx>> {
        macro_rules! initializer  {
            ($( $node_kind:ident ),* $(,)?) => {
                match self {
                    $(Node::$node_kind(n) => Some(n.init),)*
                    _ => None,
                }
            };
        }

        if let Some(init) = initializer!(ObjectPropAssignment) {
            return Some(init);
        }

        macro_rules! opt_initializer  {
            ($( $node_kind:ident ),* $(,)?) => {
                match self {
                    $(Node::$node_kind(n) => n.init,)*
                    _ => None,
                }
            };
        }

        opt_initializer!(VarDecl, ClassPropElem, ParamDecl)
    }

    pub fn is_this_less_var_like_decl(&self) -> bool {
        let ty_node = self.ty_anno();
        if let Some(ty_node) = ty_node {
            ty_node.is_this_less()
        } else {
            self.initializer().is_none()
        }
    }

    pub fn error_span(&self) -> bolt_ts_span::Span {
        match self {
            Node::VarDecl(n) => n.name.span,
            Node::ModuleDecl(n) => n.name.span(),
            _ => self.span(),
        }
    }
}

macro_rules! as_node {
    ($( ($kind:ident, $ty: ty, $name: ident)),* $(,)?) => {
        ::paste::paste! {
            impl<'cx> Node<'cx> {
                pub fn id(&self) -> NodeID {
                    match self {
                        $(Node::$kind(n) => n.id,)*
                    }
                }

                pub fn span(&self) -> bolt_ts_span::Span {
                    match self {
                        $(Node::$kind(n) => n.span,)*
                    }
                }

                $(
                    #[track_caller]
                    #[inline(always)]
                    pub fn [<as_ $name>](&self) -> Option<&'cx $ty> {
                        if let Node::$kind(n) = self {
                            Some(n)
                        } else {
                            None
                        }
                    }

                    #[track_caller]
                    #[inline(always)]
                    pub fn [<is_ $name>](&self) -> bool {
                        self.[<as_ $name>]().is_some()
                    }

                    #[track_caller]
                    #[inline(always)]
                    pub fn [<expect_ $name>](&self) -> &'cx $ty {
                        self.[<as_ $name>]().unwrap()
                    }
                )*
            }
        }
    };
}

as_node!(
    (Program, super::Program<'cx>, program),
    (VarStmt, super::VarStmt<'cx>, var_stmt),
    (ParamDecl, super::ParamDecl<'cx>, param_decl),
    (FnDecl, super::FnDecl<'cx>, fn_decl),
    (IfStmt, super::IfStmt<'cx>, if_stmt),
    (RetStmt, super::RetStmt<'cx>, ret_stmt),
    (EmptyStmt, super::EmptyStmt, empty_stmt),
    (ClassDecl, super::ClassDecl<'cx>, class_decl),
    (EnumDecl, super::EnumDecl<'cx>, enum_decl),
    (ModuleDecl, super::ModuleDecl<'cx>, module_decl),
    (BlockStmt, super::BlockStmt<'cx>, block_stmt),
    (ModuleBlock, super::ModuleBlock<'cx>, module_block),
    (VarDecl, super::VarDecl<'cx>, var_decl),
    (BinExpr, super::BinExpr<'cx>, bin_expr),
    (NonNullExpr, super::NonNullExpr<'cx>, non_null_expr),
    (TemplateExpr, super::TemplateExpr<'cx>, template_expr),
    (
        TaggedTemplateExpr,
        super::TaggedTemplateExpr<'cx>,
        tagged_template_expr
    ),
    (TemplateHead, super::TemplateHead, template_head),
    (TemplateSpan, super::TemplateSpan<'cx>, template_span),
    (CaseClause, super::CaseClause<'cx>, case_clause),
    (DefaultClause, super::DefaultClause<'cx>, default_clause),
    (CaseBlock, super::CaseBlock<'cx>, case_block),
    (
        ComputedPropName,
        super::ComputedPropName<'cx>,
        computed_prop_name
    ),
    (NumLit, super::NumLit, num_lit),
    (BigIntLit, super::BigIntLit, big_int_lit),
    (BoolLit, super::BoolLit, bool_lit),
    (NullLit, super::NullLit, null_lit),
    (RegExpLit, super::RegExpLit, reg_exp_lit),
    (StringLit, super::StringLit, string_lit),
    (
        NoSubstitutionTemplateLit,
        super::NoSubstitutionTemplateLit,
        no_substitution_template_lit
    ),
    (ArrayLit, super::ArrayLit<'cx>, array_lit),
    (Ident, super::Ident, ident),
    (Binding, super::Binding, binding),
    (OmitExpr, super::OmitExpr, omit_expr),
    (ParenExpr, super::ParenExpr<'cx>, paren_expr),
    (CondExpr, super::CondExpr<'cx>, cond_expr),
    (EnumMember, super::EnumMember<'cx>, enum_member),
    (
        ObjectShorthandMember,
        super::ObjectShorthandMember<'cx>,
        object_shorthand_member
    ),
    (
        ObjectPropAssignment,
        super::ObjectPropAssignment<'cx>,
        object_prop_member
    ),
    (
        ObjectMethodMember,
        super::ObjectMethodMember<'cx>,
        object_method_member
    ),
    (
        SpreadAssignment,
        super::SpreadAssignment<'cx>,
        spread_assignment
    ),
    (SpreadElement, super::SpreadElement<'cx>, spread_element),
    (ObjectLit, super::ObjectLit<'cx>, object_lit),
    (CallExpr, super::CallExpr<'cx>, call_expr),
    (FnExpr, super::FnExpr<'cx>, fn_expr),
    (NewExpr, super::NewExpr<'cx>, new_expr),
    (AssignExpr, super::AssignExpr<'cx>, assign_expr),
    (ArrayTy, super::ArrayTy<'cx>, array_ty),
    (FnTy, super::FnTy<'cx>, fn_ty),
    (CtorTy, super::CtorTy<'cx>, ctor_ty),
    (LitTy, super::LitTy, lit_ty),
    (ObjectLitTy, super::ObjectLitTy<'cx>, object_lit_ty),
    (TyParam, super::TyParam<'cx>, ty_param),
    (Modifier, super::Modifier, modifier),
    (ClassPropElem, super::ClassPropElem<'cx>, class_prop_elem),
    (
        ClassStaticBlockDecl,
        super::ClassStaticBlockDecl<'cx>,
        class_static_block_decl
    ),
    (
        ClassMethodElem,
        super::ClassMethodElem<'cx>,
        class_method_elem
    ),
    (ArrowFnExpr, super::ArrowFnExpr<'cx>, arrow_fn_expr),
    (
        PrefixUnaryExpr,
        super::PrefixUnaryExpr<'cx>,
        prefix_unary_expr
    ),
    (
        PostfixUnaryExpr,
        super::PostfixUnaryExpr<'cx>,
        postfix_unary_expr
    ),
    (ClassExpr, super::ClassExpr<'cx>, class_expr),
    (
        ClassExtendsClause,
        super::ClassExtendsClause<'cx>,
        class_extends_clause
    ),
    (
        ClassImplementsClause,
        super::ClassImplementsClause<'cx>,
        class_implements_clause
    ),
    (
        InterfaceExtendsClause,
        super::InterfaceExtendsClause<'cx>,
        interface_extends_clause
    ),
    (
        ExprWithTyArgs,
        super::ExprWithTyArgs<'cx>,
        expr_with_ty_args
    ),
    (IndexSigDecl, super::IndexSigDecl<'cx>, index_sig_decl),
    (PropAccessExpr, super::PropAccessExpr<'cx>, prop_access_expr),
    (TypeofExpr, super::TypeofExpr<'cx>, typeof_expr),
    (VoidExpr, super::VoidExpr<'cx>, void_expr),
    (EleAccessExpr, super::EleAccessExpr<'cx>, ele_access_expr),
    (ClassCtor, super::ClassCtor<'cx>, class_ctor),
    (GetterDecl, super::GetterDecl<'cx>, getter_decl),
    (SetterDecl, super::SetterDecl<'cx>, setter_decl),
    (CtorSigDecl, super::CtorSigDecl<'cx>, ctor_sig_decl),
    (CallSigDecl, super::CallSigDecl<'cx>, call_sig_decl),
    (InterfaceDecl, super::InterfaceDecl<'cx>, interface_decl),
    (DebuggerStmt, super::DebuggerStmt, debugger_stmt),
    (PropSignature, super::PropSignature<'cx>, prop_signature),
    (
        MethodSignature,
        super::MethodSignature<'cx>,
        method_signature
    ),
    (ThisExpr, super::ThisExpr, this_expr),
    (AsExpr, super::AsExpr<'cx>, as_expr),
    (TyAssertionExpr, super::TyAssertion<'cx>, ty_assertion),
    (SatisfiesExpr, super::SatisfiesExpr<'cx>, satisfies_expr),
    (TypeAliasDecl, super::TypeAliasDecl<'cx>, type_alias_decl),
    (RestTy, super::RestTy<'cx>, rest_ty),
    (NamedTupleTy, super::NamedTupleTy<'cx>, named_tuple_ty),
    (TupleTy, super::TupleTy<'cx>, tuple_ty),
    (
        IndexedAccessTy,
        super::IndexedAccessTy<'cx>,
        indexed_access_ty
    ),
    (CondTy, super::CondTy<'cx>, cond_ty),
    (ReferTy, super::ReferTy<'cx>, refer_ty),
    (IntersectionTy, super::IntersectionTy<'cx>, intersection_ty),
    (UnionTy, super::UnionTy<'cx>, union_ty),
    (TypeofTy, super::TypeofTy<'cx>, typeof_ty),
    (ThrowStmt, super::ThrowStmt<'cx>, throw_stmt),
    (ShorthandSpec, super::ShorthandSpec<'cx>, shorthand_spec),
    (NsImport, super::NsImport<'cx>, ns_import),
    (
        ImportNamedSpec,
        super::ImportNamedSpec<'cx>,
        import_named_spec
    ),
    (ImportClause, super::ImportClause<'cx>, import_clause),
    (ImportDecl, super::ImportDecl<'cx>, import_decl),
    (NsExport, super::NsExport<'cx>, ns_export),
    (
        ExportNamedSpec,
        super::ExportNamedSpec<'cx>,
        export_named_spec
    ),
    (ExportDecl, super::ExportDecl<'cx>, export_decl),
    (ExportAssign, super::ExportAssign<'cx>, export_assign),
    (GlobExport, super::GlobExport<'cx>, glob_export),
    (SpecsExport, super::SpecsExport<'cx>, specs_export),
    (ForStmt, super::ForStmt<'cx>, for_stmt),
    (ForInStmt, super::ForInStmt<'cx>, for_in_stmt),
    (ForOfStmt, super::ForOfStmt<'cx>, for_of_stmt),
    (BreakStmt, super::BreakStmt<'cx>, break_stmt),
    (ContinueStmt, super::ContinueStmt<'cx>, continue_stmt),
    (SuperExpr, super::SuperExpr, super_expr),
    (MappedTy, super::MappedTy<'cx>, mapped_ty),
    (TyOp, super::TyOp<'cx>, ty_op),
    (TryStmt, super::TryStmt<'cx>, try_stmt),
    (CatchClause, super::CatchClause<'cx>, catch_clause),
    (DoStmt, super::DoStmt<'cx>, do_stmt),
    (WhileStmt, super::WhileStmt<'cx>, while_stmt),
    (ExprStmt, super::ExprStmt<'cx>, expr_stmt),
    (SwitchStmt, super::SwitchStmt<'cx>, switch_stmt),
    (LabeledStmt, super::LabeledStmt<'cx>, labeled_stmt),
    (QualifiedName, super::QualifiedName<'cx>, qualified_name),
    (ObjectPat, super::ObjectPat<'cx>, object_pat),
    (
        ObjectBindingElem,
        super::ObjectBindingElem<'cx>,
        object_binding_elem
    ),
    (ArrayPat, super::ArrayPat<'cx>, array_pat),
    (ArrayBinding, super::ArrayBinding<'cx>, array_binding),
    (PredTy, super::PredTy<'cx>, pred_ty),
    (ParenTy, super::ParenTy<'cx>, paren_ty),
    (InferTy, super::InferTy<'cx>, infer_ty),
    (NullableTy, super::NullableTy<'cx>, nullable_ty),
    (TemplateLitTy, super::TemplateLitTy<'cx>, template_lit_ty),
    (TemplateSpanTy, super::TemplateSpanTy<'cx>, template_span_ty),
    (IntrinsicTy, super::IntrinsicTy, intrinsic_ty),
    (ThisTy, super::ThisTy, this_ty),
    // jsx
    (JsxText, super::JsxText, jsx_text),
    (JsxOpeningFrag, super::JsxOpeningFrag, jsx_opening_frag),
    (JsxClosingFrag, super::JsxClosingFrag, jsx_closing_frag),
    (JsxOpeningElem, super::JsxOpeningElem<'cx>, jsx_opening_elem),
    (JsxClosingElem, super::JsxClosingElem<'cx>, jsx_closing_elem),
    (
        JsxSelfClosingElem,
        super::JsxSelfClosingElem<'cx>,
        jsx_self_closing_elem
    ),
    (JsxSpreadAttr, super::JsxSpreadAttr<'cx>, jsx_spread_attr),
    (JsxNsName, super::JsxNsName<'cx>, jsx_ns_name),
    (JsxNamedAttr, super::JsxNamedAttr<'cx>, jsx_named_attr),
    (JsxExpr, super::JsxExpr<'cx>, jsx_expr),
    (JsxFrag, super::JsxFrag<'cx>, jsx_frag),
    (JsxElem, super::JsxElem<'cx>, jsx_elem),
);
