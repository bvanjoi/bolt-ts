use bolt_ts_arena::la_arena;

use crate::ir;

#[derive(Default)]
pub struct GraphArena(la_arena::Arena<Graph>);

impl GraphArena {
    pub fn alloc_empty_graph(&mut self) -> GraphID {
        let graph = Graph {
            basic_block_arena: BasicBlockArena::default(),
            entry: BasicBlockID::PLACEHOLDER,
        };
        GraphID(self.0.alloc(graph))
    }

    #[inline(always)]
    pub fn get(&self, id: GraphID) -> &Graph {
        &self.0[id.0]
    }

    #[inline(always)]
    pub fn get_mut(&mut self, id: GraphID) -> &mut Graph {
        &mut self.0[id.0]
    }
}

pub struct Graph {
    basic_block_arena: BasicBlockArena,

    entry: BasicBlockID,
}

impl Graph {
    #[inline(always)]
    pub(crate) fn get_basic_block(&self, id: BasicBlockID) -> &BasicBlock {
        &self.basic_block_arena.0[id.0]
    }

    pub fn entry(&self) -> BasicBlockID {
        debug_assert!(
            self.entry != BasicBlockID::PLACEHOLDER,
            "Graph entry is not set"
        );
        self.entry
    }

    pub(crate) fn feed_entry(&mut self, entry: BasicBlockID) {
        debug_assert!(
            self.entry == BasicBlockID::PLACEHOLDER,
            "Graph entry is already set: {:?}",
            self.entry
        );
        self.entry = entry;
    }

    pub(crate) fn alloc_basic_block(&mut self, stmts: Vec<ir::Stmt>) -> BasicBlockID {
        let bb = self.basic_block_arena.0.alloc(BasicBlock {
            stmts,
            predecessors: Vec::with_capacity(4),
            successors: Vec::with_capacity(4),
        });
        BasicBlockID(bb)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct GraphID(la_arena::Idx<Graph>);

#[derive(Default)]
pub struct BasicBlockArena(la_arena::Arena<BasicBlock>);

pub struct BasicBlock {
    stmts: Vec<ir::Stmt>,
    predecessors: Vec<BasicBlockID>,
    successors: Vec<BasicBlockID>,
}

impl BasicBlock {
    #[inline(always)]
    pub fn stmts(&self) -> &Vec<ir::Stmt> {
        &self.stmts
    }

    #[inline(always)]
    pub(super) fn add_predecessor(&mut self, pred: BasicBlockID) {
        self.predecessors.push(pred);
    }

    #[inline(always)]
    pub(super) fn add_successor(&mut self, succ: BasicBlockID) {
        self.successors.push(succ);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BasicBlockID(la_arena::Idx<BasicBlock>);

impl BasicBlockID {
    const PLACEHOLDER: BasicBlockID = BasicBlockID(la_arena::Idx::from_raw(
        la_arena::RawIdx::from_u32(u32::MAX),
    ));
}
