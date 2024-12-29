use super::TyChecker;
use crate::ast;
use crate::bind::SymbolID;

impl TyChecker<'_> {
    pub(super) fn get_symbol_of_decl(&mut self, id: ast::NodeID) -> SymbolID {
        use ast::Node::*;
        let node = self.p.node(id);
        let id = match node {
            VarDecl(n) => n.id,
            ObjectMemberField(n) => n.id,
            ClassDecl(n) => n.id,
            ClassExpr(n) => n.id,
            ClassPropEle(n) => n.id,
            ClassMethodEle(n) => n.id,
            ArrowFnExpr(n) => n.id,
            FnExpr(n) => n.id,
            ClassCtor(n) => n.id,
            FnDecl(n) => n.id,
            InterfaceDecl(n) => n.id,
            TyParam(n) => n.id,
            _ => {
                todo!("{:#?}", node)
            }
        };
        self.binder.final_res(id)
    }
}
