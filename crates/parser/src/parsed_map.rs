use std::mem::MaybeUninit;

use bolt_ts_ast as ast;
use bolt_ts_span::ModuleID;

use super::ParseResultForGraph;

#[derive(Default)]
pub struct ParsedMap<'cx> {
    map: Vec<MaybeUninit<ParseResultForGraph<'cx>>>,
}

impl<'cx> ParsedMap<'cx> {
    #[inline(always)]
    pub fn preserve(cap: usize) -> Self {
        let mut map: Vec<MaybeUninit<ParseResultForGraph<'cx>>> = Vec::with_capacity(cap * 4);
        unsafe {
            map.set_len(cap);
        }
        Self { map }
    }

    #[inline(always)]
    pub fn from_map(map: Vec<ParseResultForGraph<'cx>>) -> Self {
        let map = map.into_iter().map(MaybeUninit::new).collect();
        Self { map }
    }

    #[inline(always)]
    pub fn into_map(self) -> Vec<ParseResultForGraph<'cx>> {
        debug_assert!(self.map.iter().all(|item| !item.as_ptr().is_null()));
        let this = std::mem::ManuallyDrop::new(self);
        let (ptr, len, cap) = (this.map.as_ptr(), this.map.len(), this.map.capacity());
        unsafe { Vec::from_raw_parts(ptr as *mut ParseResultForGraph<'cx>, len, cap) }
    }

    #[inline(always)]
    pub fn get_map(&self) -> &[ParseResultForGraph<'cx>] {
        debug_assert!(self.map.iter().all(|item| !item.as_ptr().is_null()));
        let ptr = self.map.as_ptr() as *const ParseResultForGraph<'cx>;
        unsafe { std::slice::from_raw_parts(ptr, self.map.len()) }
    }

    #[inline(always)]
    pub fn insert(&mut self, id: ModuleID, result: ParseResultForGraph<'cx>) {
        assert_eq!(id.as_usize(), self.map.len());
        self.map.push(MaybeUninit::new(result));
    }

    #[inline(always)]
    pub fn insert_within_preserve(&mut self, index: ModuleID, result: ParseResultForGraph<'cx>) {
        debug_assert!(index.as_usize() < self.map.len());
        self.map[index.as_usize()] = MaybeUninit::new(result);
    }

    #[inline(always)]
    pub fn get(&self, id: ModuleID) -> &ParseResultForGraph<'cx> {
        debug_assert!(self.map.iter().all(|item| !item.as_ptr().is_null()));
        let idx = id.as_usize();
        debug_assert!(idx < self.map.len());
        unsafe { &*self.map.get_unchecked(idx).as_ptr() }
    }

    pub fn steal_errors(&mut self) -> Vec<bolt_ts_errors::Diag> {
        debug_assert!(self.map.iter().all(|item| !item.as_ptr().is_null()));
        self.map
            .iter_mut()
            .flat_map(|result| {
                let ptr = unsafe { &mut *result.as_mut_ptr() };
                std::mem::take(&mut ptr.diags)
            })
            .collect()
    }

    #[inline(always)]
    pub fn module_count(&self) -> usize {
        self.map.len()
    }

    pub fn node_flags(&self, node: ast::NodeID) -> ast::NodeFlags {
        debug_assert!(self.map.iter().all(|item| !item.as_ptr().is_null()));
        let idx = node.module().as_usize();
        debug_assert!(idx < self.map.len());
        unsafe { (&*self.map.get_unchecked(idx).as_ptr()).node_flags(node) }
    }

    #[inline(always)]
    pub fn root(&self, id: ModuleID) -> &'cx ast::Program<'cx> {
        debug_assert!(self.map.iter().all(|item| !item.as_ptr().is_null()));
        self.get(id).root()
    }

    #[inline(always)]
    pub fn node(&self, id: ast::NodeID) -> ast::Node<'cx> {
        debug_assert!(self.map.iter().all(|item| !item.as_ptr().is_null()));
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

    pub fn is_call_chain(&self, id: ast::NodeID) -> bool {
        self.node(id).is_call_expr() && self.node_flags(id).contains(ast::NodeFlags::OPTIONAL_CHAIN)
    }
}
