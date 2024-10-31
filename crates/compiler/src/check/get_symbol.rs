use super::TyChecker;
use crate::ast;
use crate::bind::SymbolID;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_symbol_of_decl(&mut self, id: ast::NodeID) -> SymbolID {
        match self.nodes.get(id) {
            ast::Node::VarDecl(decl) => *self.final_res.get(&decl.id).unwrap(),
            ast::Node::ObjectMemberField(field) => *self.final_res.get(&field.id).unwrap(),
            _ => todo!(),
        }
    }
}
