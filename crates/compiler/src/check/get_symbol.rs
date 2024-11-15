use super::TyChecker;
use crate::ast;
use crate::bind::SymbolID;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_symbol_of_decl(&mut self, id: ast::NodeID) -> SymbolID {
        use ast::Node::*;
        match self.nodes.get(id) {
            VarDecl(decl) => *self.final_res.get(&decl.id).unwrap(),
            ObjectMemberField(field) => *self.final_res.get(&field.id).unwrap(),
            ClassDecl(decl) => *self.final_res.get(&decl.id).unwrap(),
            ClassExpr(expr) => *self.final_res.get(&expr.id).unwrap(),
            ClassPropEle(prop) => *self.final_res.get(&prop.id).unwrap(),
            ArrowFnExpr(f) => *self.final_res.get(&f.id).unwrap(),
            FnExpr(f) => *self.final_res.get(&f.id).unwrap(),
            _ => todo!("{:#?}", self.nodes.get(id)),
        }
    }
}
