mod ast;
mod node;

use ast::Program;
use node::{Node, NodeID};
use rustc_hash::FxHashMap;

trait Parser<'parser> {
    type AST;
    fn parse(&mut self, input: &str);
    fn root(&self) -> &'parser Self::AST;
}

pub struct TsParser<'p> {
    arena: bumpalo::Bump,
    node_map: FxHashMap<NodeID, Node<'p>>,
    parent_map: FxHashMap<NodeID, NodeID>,
    next_node_id: NodeID,
}

pub struct NodeMap<'p> {
    node_map: FxHashMap<NodeID, Node<'p>>,
    parent_map: FxHashMap<NodeID, NodeID>,
}

impl<'p> NodeMap<'p> {
    pub fn root(&self) -> &'p ast::Program<'p> {
        let id = NodeID::root();
        let Node::Program(p) = self.node_map[&id] else {
            unreachable!()
        };
        p
    }

    pub fn parent(&self, id: NodeID) -> Option<NodeID> {
        self.parent_map.get(&id).copied()
    }
}

impl<'p> TsParser<'p> {
    pub fn new() -> Self {
        Self {
            arena: bumpalo::Bump::new(),
            node_map: Default::default(),
            parent_map: Default::default(),
            next_node_id: NodeID::root(),
        }
    }

    fn next_node_id(&mut self) -> NodeID {
        let old = self.next_node_id;
        self.next_node_id = self.next_node_id.next();
        old
    }
}

impl<'p> TsParser<'p> {
    pub fn parse(&'p mut self, input: &str) -> NodeMap<'p> {
        let id = self.next_node_id();
        let stmts = self.arena.alloc(vec![]);
        let p = self.arena.alloc(Program { id, stmts });
        self.node_map.insert(id, Node::Program(p));

        NodeMap {
            node_map: std::mem::take(&mut self.node_map),
            parent_map: std::mem::take(&mut self.parent_map),
        }
    }
}
