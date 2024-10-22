use crate::ast;

rts_span::new_index!(NodeID);

#[derive(Debug, Clone, Copy)]
pub enum Node<'cx> {
    Program(&'cx ast::Program<'cx>),

    Stmt(&'cx ast::Stmt<'cx>),
    VarStmt(&'cx ast::VarStmt<'cx>),
    ParamDecl(&'cx ast::ParamDecl<'cx>),
    FnDecl(&'cx ast::FnDecl<'cx>),
    IfStmt(&'cx ast::IfStmt<'cx>),
    RetStmt(&'cx ast::RetStmt<'cx>),
    EmptyStmt(&'cx ast::EmptyStmt),
    ClassDecl(&'cx ast::ClassDecl<'cx>),
    BlockStmt(&'cx ast::BlockStmt<'cx>),
    HeritageClauses(&'cx ast::HeritageClauses<'cx>),
    HeritageClause(&'cx ast::HeritageClause<'cx>),

    Expr(&'cx ast::Expr<'cx>),
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
    PropName(&'cx ast::PropName<'cx>),
    ObjectMemberField(&'cx ast::ObjectMemberField<'cx>),
    ObjectLit(&'cx ast::ObjectLit<'cx>),
    CallExpr(&'cx ast::CallExpr<'cx>),
    FnExpr(&'cx ast::FnExpr<'cx>),
    NewExpr(&'cx ast::NewExpr<'cx>),
    AssignExpr(&'cx ast::AssignExpr<'cx>),

    Ty(&'cx ast::Ty<'cx>),
    ArrayTy(&'cx ast::ArrayTy<'cx>),
    FnTy(&'cx ast::FnTy<'cx>),

    TyParam(&'cx ast::TyParam<'cx>),
}

impl Node<'_> {
    pub fn id(&self) -> NodeID {
        use Node::*;
        match self {
            Program(n) => n.id,
            Stmt(n) => n.id,
            VarStmt(n) => n.id,
            ParamDecl(n) => n.id,
            FnDecl(n) => n.id,
            IfStmt(n) => n.id,
            RetStmt(n) => n.id,
            EmptyStmt(n) => n.id,
            Expr(n) => n.id,
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
            PropName(n) => n.id,
            ObjectMemberField(n) => n.id,
            ObjectLit(n) => n.id,
            CallExpr(n) => n.id,
            Ty(n) => n.id,
            ArrayTy(n) => n.id,
            FnTy(n) => n.id,
            ClassDecl(n) => n.id,
            BlockStmt(n) => n.id,
            HeritageClauses(n) => n.id,
            HeritageClause(n) => n.id,
            FnExpr(f) => f.id,
            NewExpr(new) => new.id,
            TyParam(param) => param.id,
            AssignExpr(assign) => assign.id,
        }
    }
}
