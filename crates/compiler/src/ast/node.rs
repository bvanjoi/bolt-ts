use crate::ast;

rts_span::new_index!(NodeID);

#[derive(Debug, Clone, Copy)]
pub enum Node<'cx> {
    Program(&'cx ast::Program<'cx>),

    Stmt(&'cx ast::Stmt<'cx>),
    VarStmt(&'cx ast::VarStmt<'cx>),

    Expr(&'cx ast::Expr<'cx>),
    VarDecl(&'cx ast::VarDecl<'cx>),
    BinExpr(&'cx ast::BinExpr<'cx>),
    NumLit(&'cx ast::NumLit),
    BoolLit(&'cx ast::BoolLit),
    NullLit(&'cx ast::NullLit),
    Ident(&'cx ast::Ident),
}
