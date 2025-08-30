use bolt_ts_arena::la_arena;

use crate::ir;

#[derive(Default)]
pub struct GraphArena(la_arena::Arena<Graph>);

impl GraphArena {
    pub fn alloc_empty_graph(&mut self) -> GraphID {
        let graph = Graph {
            basic_block_arena: BasicBlockArena::default(),
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
}

impl Graph {
    #[inline(always)]
    pub(crate) fn get_basic_block(&self, id: BasicBlockID) -> &BasicBlock {
        &self.basic_block_arena.0[id.0]
    }

    #[inline(always)]
    pub(crate) fn get_mut_basic_block(&mut self, id: BasicBlockID) -> &mut BasicBlock {
        &mut self.basic_block_arena.0[id.0]
    }

    pub(crate) fn alloc_empty_basic_block(&mut self) -> BasicBlockID {
        let bb = self.basic_block_arena.0.alloc(BasicBlock {
            stmts: Vec::with_capacity(8),
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

    pub(crate) fn add_stmt(&mut self, stmt: ir::Stmt) {
        self.stmts.push(stmt);
    }

    #[inline(always)]
    pub(crate) fn add_predecessor(&mut self, pred: BasicBlockID) {
        self.predecessors.push(pred);
    }

    #[inline(always)]
    pub(crate) fn add_successor(&mut self, succ: BasicBlockID) {
        self.successors.push(succ);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BasicBlockID(la_arena::Idx<BasicBlock>);

impl BasicBlockID {
    pub const ENTRY: BasicBlockID =
        BasicBlockID(la_arena::Idx::from_raw(la_arena::RawIdx::from_u32(0)));
    const PLACEHOLDER: BasicBlockID = BasicBlockID(la_arena::Idx::from_raw(
        la_arena::RawIdx::from_u32(u32::MAX),
    ));
}
