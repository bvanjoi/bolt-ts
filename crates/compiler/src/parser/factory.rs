use bolt_ts_ast::{self as ast};
use bolt_ts_span::Span;

use super::ParserState;

impl<'cx> ParserState<'cx, '_> {
    pub fn create_numeric_literal(&mut self, val: f64, span: Span) -> &'cx ast::NumLit {
        let id = self.next_node_id();
        let n = self.alloc(ast::NumLit { id, val, span });
        self.nodes.insert(id, ast::Node::NumLit(n));
        self.node_flags_map.insert(id, ast::NodeFlags::empty());
        n
    }
}
