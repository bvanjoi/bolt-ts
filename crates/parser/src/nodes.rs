use super::Node;
use super::ast;
use super::ast::Node::*;

#[derive(Debug, Default)]
pub struct Nodes<'cx>(pub(super) Vec<Node<'cx>>);

impl<'cx> Nodes<'cx> {
    pub fn get(&self, id: ast::NodeID) -> Node<'cx> {
        let idx = id.index_as_usize();
        debug_assert!(idx < self.0.len(), "idx: {idx}, len: {}", self.0.len());
        *unsafe { self.0.get_unchecked(idx) }
    }

    pub(crate) fn insert(&mut self, id: ast::NodeID, node: Node<'cx>) {
        debug_assert_eq!(id.index_as_usize(), self.0.len());
        self.0.push(node);
    }

    pub fn root(&self) -> &'cx ast::Program<'cx> {
        let idx = self.0.len() - 1;
        let node = unsafe { self.0.get_unchecked(idx) };
        node.expect_program()
    }

    pub fn get_non_assigned_name_of_decl(
        &self,
        id: ast::NodeID,
    ) -> Option<ast::DeclarationName<'cx>> {
        let n = self.get(id);
        match n {
            Ident(n) => Some(ast::DeclarationName::Ident(n)),
            CallExpr(_) | BinExpr(_) => {
                // TODO:
                None
            }
            ExportAssign(_) => {
                // TODO:
                None
            }
            _ => n.name(),
        }
    }

    pub fn is_param_prop_decl(&self, id: ast::NodeID, parent: ast::NodeID) -> bool {
        let n = self.get(id);
        n.as_param_decl()
            .is_some_and(|param| self.param_is_prop_decl(param, parent))
    }

    pub fn param_is_prop_decl(&self, param: &'cx ast::ParamDecl<'cx>, parent: ast::NodeID) -> bool {
        // TODO: has_syntactic_modifier
        param
            .modifiers
            .is_some_and(|mods| mods.flags.intersects(ast::ModifierKind::PARAMETER_PROPERTY))
            && self.get(parent).is_class_ctor()
    }

    pub fn is_alias_symbol_decl(&self, id: ast::NodeID) -> bool {
        let node = self.get(id);
        use ast::Node::*;
        match node {
            ImportNamedSpec(_) |  // `import { a as b } from 'xxx'`
            ShorthandSpec(_) |    // `export { spec }` or `import { spec } from 'xxx'`
            ExportNamedSpec(_) |  // `export { a as b }`
            NsImport(_)           // `import * as ns from 'xxx'`
            => true,
            ImportClause(n) => n.name.is_some(), // `import a from 'xxx'`
            ExportAssign(n) => n.is_aliasable(),
            _ => false
        }
    }
}
