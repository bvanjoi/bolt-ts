use rts_span::Span;

use crate::ast;

rts_span::new_index!(NodeID);

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
    ClassPropEle(&'cx ast::ClassPropEle<'cx>),
    ClassMethodEle(&'cx ast::ClassMethodEle<'cx>),
    ClassExtendsClause(&'cx ast::ClassExtendsClause<'cx>),
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

    // ty
    ArrayTy(&'cx ast::ArrayTy<'cx>),
    FnTy(&'cx ast::FnTy<'cx>),
    LitTy(&'cx ast::LitTy<'cx>),
    TyParam(&'cx ast::TyParam<'cx>),
}

impl Node<'_> {
    pub fn id(&self) -> NodeID {
        use Node::*;
        match self {
            Program(n) => n.id,
            VarStmt(n) => n.id,
            ParamDecl(n) => n.id,
            FnDecl(n) => n.id,
            IfStmt(n) => n.id,
            RetStmt(n) => n.id,
            EmptyStmt(n) => n.id,
            VarDecl(n) => n.id,
            BinExpr(n) => n.id,
            NumLit(n) => n.id,
            BoolLit(n) => n.id,
            NullLit(n) => n.id,
            StringLit(n) => n.id,
            ArrayLit(n) => n.id,
            Ident(n) => n.id,
            OmitExpr(n) => n.id,
            ParenExpr(n) => n.id,
            CondExpr(n) => n.id,
            ObjectMemberField(n) => n.id,
            ObjectLit(n) => n.id,
            CallExpr(n) => n.id,
            ArrayTy(n) => n.id,
            FnTy(n) => n.id,
            ClassDecl(n) => n.id,
            BlockStmt(n) => n.id,
            FnExpr(n) => n.id,
            NewExpr(n) => n.id,
            TyParam(n) => n.id,
            AssignExpr(n) => n.id,
            LitTy(n) => n.id,
            Modifier(n) => n.id,
            ClassPropEle(n) => n.id,
            ClassMethodEle(n) => n.id,
            ArrowFnExpr(n) => n.id,
            PrefixUnaryExpr(n) => n.id,
            ClassExpr(n) => n.id,
            ClassExtendsClause(n) => n.id,
            ImplementsClause(n) => n.id,
            InterfaceExtendsClause(n) => n.id,
        }
    }

    pub fn span(&self) -> Span {
        use Node::*;
        match self {
            Program(n) => n.span,
            VarStmt(n) => n.span,
            ParamDecl(n) => n.span,
            FnDecl(n) => n.span,
            IfStmt(n) => n.span,
            RetStmt(n) => n.span,
            EmptyStmt(n) => n.span,
            ClassDecl(n) => n.span,
            BlockStmt(n) => n.span,
            VarDecl(n) => n.span,
            BinExpr(n) => n.span,
            NumLit(n) => n.span,
            BoolLit(n) => n.span,
            NullLit(n) => n.span,
            StringLit(n) => n.span,
            ArrayLit(n) => n.span,
            Ident(n) => n.span,
            OmitExpr(n) => n.span,
            ParenExpr(n) => n.span,
            CondExpr(n) => n.span,
            ObjectMemberField(n) => n.span,
            ObjectLit(n) => n.span,
            CallExpr(n) => n.span,
            FnExpr(n) => n.span,
            NewExpr(n) => n.span,
            AssignExpr(n) => n.span,
            ArrayTy(n) => n.span,
            FnTy(n) => n.span,
            LitTy(n) => n.span,
            TyParam(n) => n.span,
            Modifier(n) => n.span,
            ClassPropEle(n) => n.span,
            ClassMethodEle(n) => n.span,
            ArrowFnExpr(n) => n.span,
            PrefixUnaryExpr(n) => n.span,
            ClassExpr(n) => n.span,
            ClassExtendsClause(n) => n.span,
            ImplementsClause(n) => n.span,
            InterfaceExtendsClause(n) => n.span,
        }
    }

    pub fn is_class_like(&self) -> bool {
        use Node::*;
        matches!(self, ClassDecl(_) | ClassExpr(_))
    }
}
