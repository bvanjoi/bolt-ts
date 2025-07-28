use bolt_ts_span::ModuleID;

use super::Parser;
use super::ast;

impl<'cx> Parser<'cx> {
    #[inline(always)]
    pub fn root(&self, id: ModuleID) -> &ast::Program<'cx> {
        self.get(id).root()
    }

    #[inline(always)]
    pub fn node(&self, id: ast::NodeID) -> ast::Node<'cx> {
        self.get(id.module()).node(id)
    }

    pub fn is_object_lit_method(&self, id: ast::NodeID) -> bool {
        // TODO: handle this after parse method in object
        self.node(id).is_object_method_member()
    }

    pub fn is_import_or_export_spec(&self, id: ast::NodeID) -> bool {
        let n = self.node(id);
        n.is_import_named_spec() || n.is_export_named_spec()
    }

    pub fn index_of_node(&self, elements: &[&'cx ast::Expr<'cx>], id: ast::NodeID) -> usize {
        debug_assert!(elements.is_sorted_by_key(|probe| probe.span().lo()));
        elements
            .binary_search_by_key(&self.node(id).span().lo(), |probe| probe.span().lo())
            .unwrap()
    }

    pub fn get_annotated_accessor_ty_node(&self, node: ast::NodeID) -> Option<&'cx ast::Ty<'cx>> {
        let node = self.node(node);
        match node {
            ast::Node::GetterDecl(n) => n.ty,
            ast::Node::SetterDecl(n) => n.get_effective_ty_annotation_node(),
            ast::Node::PropSignature(_) => todo!(),
            _ => None,
        }
    }

    pub fn is_alias_symbol_decl(&self, id: ast::NodeID) -> bool {
        self.get(id.module()).nodes.is_alias_symbol_decl(id)
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum AccessKind {
    Read,
    Write,
    ReadWrite,
}
