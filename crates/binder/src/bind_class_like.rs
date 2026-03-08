use super::errors;
use super::symbol::SymbolFlags;
use super::{BinderState, SymbolID, SymbolName, prop_name};

use bolt_ts_ast as ast;
use bolt_ts_ast::r#trait;

impl<'cx> BinderState<'cx, '_, '_> {
    pub(super) fn bind_prop_or_method_or_access<const BIND_FLOW_NODE: bool>(
        &mut self,
        decl_id: ast::NodeID,
        name: &'cx ast::PropName<'cx>,
        includes: SymbolFlags,
        excludes: SymbolFlags,
    ) -> SymbolID {
        if BIND_FLOW_NODE && let Some(current_flow) = self.current_flow {
            self.flow_nodes.insert_flow_of_node(decl_id, current_flow);
        }
        if self.node_query().has_dynamic_name(decl_id) {
            self.bind_anonymous_decl(decl_id, includes, SymbolName::Computed)
        } else {
            let name = prop_name(name);
            self.declare_symbol_and_add_to_symbol_table(name, decl_id, includes, excludes)
        }
    }

    pub(super) fn bind_class_like_decl<const IS_EXPR: bool>(
        &mut self,
        c: &'cx impl r#trait::ClassLike<'cx>,
    ) {
        let name = c
            .name()
            .map_or(SymbolName::ClassExpr, |name| SymbolName::Atom(name.name));
        let node_id = c.id();
        let symbol = if !IS_EXPR {
            self.bind_block_scoped_decl(
                node_id,
                name,
                SymbolFlags::CLASS,
                SymbolFlags::CLASS_EXCLUDES,
            )
        } else {
            self.bind_anonymous_decl(node_id, SymbolFlags::CLASS, name)
        };
        self.create_final_res(node_id, symbol);

        let name = SymbolName::Atom(ast::keyword::IDENT_PROTOTYPE);
        let prototype_symbol =
            self.create_symbol(name, SymbolFlags::PROPERTY.union(SymbolFlags::PROTOTYPE));
        let s = self.symbols.get(symbol);
        let Some(exports) = s.exports.as_ref() else {
            unreachable!()
        };
        if let Some(symbol_export) = exports.0.get(&name) {
            let error = errors::DuplicateIdentifier {
                span: self
                    .p
                    .node(self.symbols.get(*symbol_export).decls.as_ref().unwrap()[0])
                    .span(),
                name: "prototype".to_string(),
                original_span: c.name().map(|name| name.span).unwrap_or(c.span()),
            };
            self.push_error(Box::new(error));
        }

        let s = self.symbols.get_mut(symbol);
        let Some(exports) = s.exports.as_mut() else {
            unreachable!()
        };
        exports.0.insert(name, prototype_symbol);
        self.symbols.get_mut(prototype_symbol).parent = Some(symbol);
    }
}
