use bolt_ts_ast as ast;

use super::Nodes;

pub fn get_touching_property_name<'cx>(
    root: &ast::Program<'cx>,
    pos: u32,
    nodes: &Nodes<'cx>,
) -> Option<ast::NodeID> {
    get_node_at_position_worker::<true, true>(root, pos, nodes, |node| {
        node.is_property_name_literal() || node.is_private_ident()
    })
}

pub fn get_touching_node<'cx>(
    root: &ast::Program<'cx>,
    pos: u32,
    nodes: &Nodes<'cx>,
    include_preceding_token_at_position: impl Fn(ast::Node<'cx>) -> bool + Copy,
) -> Option<ast::NodeID> {
    get_node_at_position_worker::<false, false>(
        root,
        pos,
        nodes,
        include_preceding_token_at_position,
    )
}

fn get_node_at_position_worker<
    'cx,
    const ALLOW_POSITION_IN_LEADING_TRIVIA: bool,
    const INCLUDE_END_POSITION: bool,
>(
    root: &ast::Program<'cx>,
    pos: u32,
    nodes: &Nodes<'cx>,
    include_preceding_token_at_position: impl Fn(ast::Node<'cx>) -> bool + Copy,
) -> Option<ast::NodeID> {
    debug_assert!(pos < root.span().hi());
    let mut current = root.id();
    loop {
        let node = nodes.get(current);
        // TODO: get_children
        let next = match node {
            ast::Node::Ident(_) => {
                return include_preceding_token_at_position(node).then_some(node.id());
            }
            ast::Node::Program(n) => binary_search_node_by_position_in_stmts(n.stmts(), pos),
            ast::Node::EnumDecl(n) => {
                let node = nodes.get(n.name.id);
                return include_preceding_token_at_position(node).then_some(n.name.id);
            }
            ast::Node::ExprStmt(n) => {
                if position_in_node(pos, n.span) {
                    Some(n.expr.id())
                } else {
                    None
                }
            }
            _ => {
                // TODO: handle others
                return None;
            }
        };

        if let Some(next) = next {
            current = next;
        } else {
            return Some(current);
        }
    }
}

fn position_in_node(pos: u32, span: bolt_ts_span::Span) -> bool {
    span.lo() <= pos && pos < span.hi()
}

fn binary_search_node_by_position_in_stmts(
    stmts: &[&ast::Stmt<'_>],
    pos: u32,
) -> Option<ast::NodeID> {
    let mut left = 0;
    let mut right = stmts.len();
    while left < right {
        let mid = left + (right - left) / 2;
        let stmt = stmts[mid];
        let span = stmt.span();
        let end = span.hi();
        if end < pos {
            left = mid + 1;
            continue;
        }

        // TODO: ALLOW_POSITION_IN_LEADING_TRIVIA
        let start = span.lo();
        if start > pos {
            right = mid;
            continue;
        }

        // TODO: node_contains_position
        return Some(stmt.id());
    }
    None
}
