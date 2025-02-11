use bolt_ts_utils::fx_hashmap_with_capacity;
use rustc_hash::FxHashMap;

use crate::ast;

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, Default)]
    pub struct FlowFlags: u32 {
        const UNREACHABLE    = 1 << 0;  // Unreachable code
        const START          = 1 << 1;  // Start of flow graph
        const BRANCH_LABEL   = 1 << 2;  // Non-looping junction
        const LOOP_LABEL     = 1 << 3;  // Looping junction
        const ASSIGNMENT     = 1 << 4;  // Assignment
        const TRUE_CONDITION = 1 << 5;  // Condition known to be true
        const FALSE_CONDITION= 1 << 6;  // Condition known to be false
        const SWITCH_CLAUSE  = 1 << 7;  // Switch statement clause
        const ARRAY_MUTATION = 1 << 8;  // Potential array mutation
        const CALL           = 1 << 9;  // Potential assertion call
        const REDUCE_LABEL   = 1 << 10; // Temporarily reduce antecedents of label
        const REFERENCED     = 1 << 11; // Referenced as antecedent once
        const SHARED         = 1 << 12; // Referenced as antecedent more than once

        const LABEL      = Self::BRANCH_LABEL.bits() | Self::LOOP_LABEL.bits();
        const CONDITION  = Self::TRUE_CONDITION.bits() | Self::FALSE_CONDITION.bits();
    }
}

bolt_ts_utils::module_index!(FlowID);

pub struct FlowNode<'cx> {
    pub flags: FlowFlags,
    pub kind: FlowNodeKind<'cx>,
}

pub enum FlowNodeKind<'cx> {
    Start(FlowStart),
    Cond(FlowCond<'cx>),
    Label(FlowLabel),
    Unreachable(FlowUnreachable),
}

#[derive(Clone, Copy)]
pub struct FlowCond<'cx> {
    pub node: &'cx ast::Expr<'cx>,
    pub antecedent: FlowID,
}

pub struct FlowLabel {
    pub antecedent: Option<Vec<FlowID>>,
}

pub struct FlowUnreachable;

pub struct FlowStart {
    pub node: Option<ast::NodeID>,
}

#[derive(Default)]
pub struct FlowNodes<'cx> {
    module_id: bolt_ts_span::ModuleID,
    data: Vec<FlowNode<'cx>>,
    container_map: FxHashMap<u32, u32>,
    cond_expr_map: FxHashMap<u32, (FlowID, FlowID)>,
}

impl<'cx> FlowNodes<'cx> {
    pub(super) fn new(module_id: bolt_ts_span::ModuleID) -> Self {
        Self {
            module_id,
            data: Vec::with_capacity(512),
            container_map: fx_hashmap_with_capacity(512),
            cond_expr_map: fx_hashmap_with_capacity(256),
        }
    }

    #[inline]
    pub fn get_flow_node(&self, id: FlowID) -> &FlowNode<'cx> {
        &self.data[id.index as usize]
    }

    #[inline]
    pub fn get_mut_flow_node(&mut self, id: FlowID) -> &mut FlowNode<'cx> {
        &mut self.data[id.index as usize]
    }

    pub(super) fn insert_flow_node(&mut self, node: FlowNode<'cx>) -> FlowID {
        let id = self.data.len() as u32;
        self.data.push(node);
        FlowID {
            module: self.module_id,
            index: id,
        }
    }

    pub(super) fn create_flow_unreachable(&mut self) -> FlowID {
        let node = FlowNode {
            flags: FlowFlags::UNREACHABLE,
            kind: FlowNodeKind::Unreachable(FlowUnreachable),
        };
        self.insert_flow_node(node)
    }

    pub(super) fn create_branch_label(&mut self) -> FlowID {
        let node = FlowNode {
            flags: FlowFlags::BRANCH_LABEL,
            kind: FlowNodeKind::Label(FlowLabel { antecedent: None }),
        };
        self.insert_flow_node(node)
    }

    pub(super) fn add_antecedent(&mut self, label: FlowID, antecedent: FlowID) {
        let node = self.get_mut_flow_node(label);
        if node.flags.intersects(FlowFlags::UNREACHABLE) {
            return;
        }
        let FlowNodeKind::Label(label) = &mut node.kind else {
            unreachable!()
        };
        let contain = label
            .antecedent
            .as_ref()
            .is_some_and(|list| list.contains(&antecedent));
        if contain {
            return;
        }
        label
            .antecedent
            .get_or_insert_with(|| vec![])
            .push(antecedent);
        self.set_flow_node_referenced(antecedent);
    }

    pub fn set_flow_node_referenced(&mut self, id: FlowID) {
        let node = self.get_mut_flow_node(id);
        node.flags |= if node.flags.intersects(FlowFlags::REFERENCED) {
            FlowFlags::SHARED
        } else {
            FlowFlags::REFERENCED
        }
    }

    pub fn insert_cond_expr_flow(
        &mut self,
        cond: &'cx ast::CondExpr<'cx>,
        when_true: FlowID,
        when_false: FlowID,
    ) {
        let prev = self
            .cond_expr_map
            .insert(cond.id.index_as_u32(), (when_true, when_false));
        assert!(prev.is_none());
    }

    pub(super) fn create_start(&mut self, node: Option<ast::NodeID>) -> FlowID {
        let node = FlowNode {
            flags: FlowFlags::START,
            kind: FlowNodeKind::Start(FlowStart { node }),
        };
        self.insert_flow_node(node)
    }

    pub(super) fn insert_container_map(&mut self, node: ast::NodeID, flow: FlowID) {
        let prev = self
            .container_map
            .insert(node.index_as_u32(), flow.index as u32);
        assert!(prev.is_none());
    }

    pub(crate) fn get_flow_node_of_node(&self, node: ast::NodeID) -> Option<FlowID> {
        self.container_map
            .get(&node.index_as_u32())
            .map(|&index| FlowID {
                module: self.module_id,
                index,
            })
    }

    pub(crate) fn create_loop_label(&mut self) -> FlowID {
        let node = FlowNode {
            flags: FlowFlags::LOOP_LABEL,
            kind: FlowNodeKind::Label(FlowLabel { antecedent: None }),
        };
        self.insert_flow_node(node)
    }
}
