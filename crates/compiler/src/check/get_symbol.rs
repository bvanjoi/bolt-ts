use super::TyChecker;
use crate::ast;
use crate::bind::SymbolID;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_symbol_of_decl(&mut self, id: ast::NodeID) -> SymbolID {
        use ast::Node::*;
        let id = match self.nodes.get(id) {
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
            _ => {
                todo!("{:#?}", self.nodes.get(id))
            }
        };
        *self.final_res.get(&id).unwrap()
    }
}
