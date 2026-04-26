use bolt_ts_ast as ast;

use super::check::EnumMemberValue;
use super::check::TyChecker;
use super::ty;

pub struct EmitResolver<'cx, 'a> {
    checker: &'a mut TyChecker<'cx>,
}

impl<'cx, 'a> EmitResolver<'cx, 'a> {
    pub fn new(checker: &'a mut TyChecker<'cx>) -> Self {
        Self { checker }
    }

    pub fn atoms(&self) -> &bolt_ts_atom::AtomIntern {
        &self.checker.atoms
    }

    pub fn get_enum_member_value(&self, n: &'cx ast::EnumMember<'cx>) -> EnumMemberValue {
        let Some(node_links) = self.checker.node_links(n.id) else {
            unreachable!()
        };
        node_links.expect_enum_member_value()
    }

    pub fn program(&self, module_id: bolt_ts_span::ModuleID) -> &'cx ast::Program<'cx> {
        self.checker.p.root(module_id)
    }

    pub fn node_flags(&self, id: ast::NodeID) -> bolt_ts_ast::NodeFlags {
        self.checker.p.node_flags(id)
    }

    pub fn ensure_type_for_variable_declaration(
        &mut self,
        n: &'cx ast::VarDecl<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        debug_assert!(matches!(n.name.kind, ast::BindingKind::Ident(_)));
        let symbol = self.checker.get_symbol_of_decl(n.id);
        let Some(links) = self.checker.symbol_links(symbol) else {
            unreachable!()
        };
        links.expect_ty()
    }

    pub fn ensure_type_for_function_declaration(
        &mut self,
        n: &'cx ast::FnDecl<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let sig = self.checker.get_sig_from_decl(n.id);
        self.checker.get_ret_ty_of_sig(sig)
    }

    pub fn print_type(&mut self, ty: &'cx ty::Ty<'cx>) -> String {
        self.checker.print_ty(ty, None).to_string()
    }
}
