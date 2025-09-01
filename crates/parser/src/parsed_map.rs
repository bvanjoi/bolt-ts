use bolt_ts_ast as ast;
use bolt_ts_span::ModuleID;

use crate::ParseResultForGraph;

#[derive(Default)]
pub struct ParsedMap<'cx> {
    map: Vec<ParseResultForGraph<'cx>>,
}

impl<'cx> ParsedMap<'cx> {
    pub fn new() -> Self {
        Self {
            map: Vec::with_capacity(512),
        }
    }

    pub fn from_map(map: Vec<ParseResultForGraph<'cx>>) -> Self {
        Self { map }
    }

    pub fn into_map(self) -> Vec<ParseResultForGraph<'cx>> {
        self.map
    }

    pub fn get_map(&self) -> &Vec<ParseResultForGraph<'cx>> {
        &self.map
    }

    #[inline(always)]
    pub fn insert(&mut self, id: ModuleID, result: ParseResultForGraph<'cx>) {
        assert_eq!(id.as_usize(), self.map.len());
        self.map.push(result);
    }

    #[inline(always)]
    pub fn get(&self, id: ModuleID) -> &ParseResultForGraph<'cx> {
        let idx = id.as_usize();
        debug_assert!(idx < self.map.len());
        unsafe { self.map.get_unchecked(idx) }
    }

    pub fn steal_errors(&mut self) -> Vec<bolt_ts_errors::Diag> {
        self.map
            .iter_mut()
            .flat_map(|result| std::mem::take(&mut result.diags))
            .collect()
    }

    #[inline(always)]
    pub fn module_count(&self) -> usize {
        self.map.len()
    }

    pub fn node_flags(&self, node: ast::NodeID) -> ast::NodeFlags {
        let idx = node.module().as_usize();
        debug_assert!(idx < self.map.len());
        unsafe { self.map.get_unchecked(idx).node_flags(node) }
    }

    #[inline(always)]
    pub fn root(&self, id: ModuleID) -> &'cx ast::Program<'cx> {
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
