use bolt_ts_ast as ast;
use bolt_ts_config::NormalizedTsConfig;

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

    pub fn config(&self) -> &NormalizedTsConfig {
        &self.checker.config
    }

    pub fn program(&self, module_id: bolt_ts_span::ModuleID) -> &'cx ast::Program<'cx> {
        self.checker.p.root(module_id)
    }

    pub fn leading_comment(
        &self,
        module_id: bolt_ts_span::ModuleID,
        start: usize,
    ) -> Option<&[bolt_ts_scanner::CommentId]> {
        self.checker
            .p
            .get(module_id)
            .leading_trailing_comments
            .get_leading_comments(start as u32)
    }

    pub fn get_comment(
        &self,
        module_id: bolt_ts_span::ModuleID,
        comment_id: bolt_ts_scanner::CommentId,
    ) -> Option<&bolt_ts_scanner::Comment> {
        self.checker.p.get(module_id).comments.get(comment_id)
    }

    pub fn module_content(&self, module: bolt_ts_span::ModuleID) -> &str {
        self.checker.module_arena.get_content(module)
    }

    pub fn is_module_instantiated(
        &self,
        module: bolt_ts_span::ModuleID,
        block: Option<&'cx ast::ModuleBlock<'cx>>,
        _id: ast::NodeID,
    ) -> bool {
        let Some(block) = block else {
            return true;
        };
        self.checker
            .node_query(module)
            .get_module_instance_state_worker(block, |n, _| self.checker.binder.parent(n))
            != bolt_ts_binder::ModuleInstanceState::NonInstantiated
    }

    pub fn is_import_equals_namespace_module(&self, node: &'cx ast::ImportEqualsDecl<'cx>) -> bool {
        match node.module_reference {
            ast::ModuleReferenceKind::EntityName(n) => {
                let most_left = n.get_first_identifier();
                let symbol = self.checker.final_res(most_left.id);
                self.checker
                    .symbol(symbol)
                    .flags
                    .contains(bolt_ts_binder::SymbolFlags::NAMESPACE_MODULE)
            }
            ast::ModuleReferenceKind::ExternalModuleReference(_) => false,
        }
    }

    pub fn node_flags(&self, id: ast::NodeID) -> bolt_ts_ast::NodeFlags {
        self.checker.p.node_flags(id)
    }

    pub fn ensure_type_for_variable_declaration(
        &mut self,
        n: &'cx ast::VarDecl<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        debug_assert!(matches!(n.name.kind, ast::BindingKind::Ident(_)));
        let symbol = self.checker.get_symbol_of_declaration(n.id);
        let Some(links) = self.checker.symbol_links(symbol) else {
            unreachable!()
        };
        links.expect_ty()
    }

    pub fn ensure_type_for_identifier_in_binding(
        &mut self,
        binding: ast::NodeID,
    ) -> &'cx ty::Ty<'cx> {
        debug_assert!(matches!(
            self.checker.p.node(binding),
            ast::Node::ArrayBinding(_) | ast::Node::ObjectBindingElem(_)
        ));
        let symbol = self.checker.get_symbol_of_declaration(binding);
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
        self.checker.get_return_type_of_signature(sig)
    }

    pub fn ensure_type_for_class_method_element(
        &mut self,
        n: &'cx ast::ClassMethodElem<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let sig = self.checker.get_sig_from_decl(n.id);
        self.checker.get_return_type_of_signature(sig)
    }

    pub fn ensure_type_for_parameter_declaration<const IGNORE_PRIVATE: bool>(
        &mut self,
        n: &'cx ast::ParamDecl<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let symbol = self.checker.get_symbol_of_declaration(n.id);
        let Some(links) = self.checker.symbol_links(symbol) else {
            return self.checker.any_ty;
        };
        links.expect_ty()
    }
    pub fn print_type(&mut self, ty: &'cx ty::Ty<'cx>) -> String {
        self.checker.print_ty(ty, None).to_string()
    }
}
