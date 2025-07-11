#[derive(Default)]
pub struct ParentMap {
    finished: bool,
    inner: Vec<u32>,
}

impl ParentMap {
    const PLACEHOLDER: u32 = u32::MAX;
    pub fn new(cap: usize) -> Self {
        Self {
            finished: false,
            inner: vec![Self::PLACEHOLDER; cap],
        }
    }

    pub fn parent(&self, node_id: bolt_ts_ast::NodeID) -> Option<bolt_ts_ast::NodeID> {
        assert!(self.finished);
        let id = node_id.index_as_usize();
        let p = self.inner[id];
        if p == Self::PLACEHOLDER {
            assert_eq!(id, self.inner.len() - 1);
            None
        } else {
            Some(bolt_ts_ast::NodeID::new(node_id.module(), p))
        }
    }

    pub fn parent_unfinished(&self, node_id: bolt_ts_ast::NodeID) -> Option<bolt_ts_ast::NodeID> {
        assert!(!self.finished);
        let id = node_id.index_as_usize();
        self.inner
            .get(id)
            .map(|&index| bolt_ts_ast::NodeID::new(node_id.module(), index))
    }

    pub fn insert(&mut self, node: bolt_ts_ast::NodeID, parent: bolt_ts_ast::NodeID) {
        assert!(!self.finished);
        let id = node.index_as_usize();
        assert_eq!(
            self.inner[id],
            Self::PLACEHOLDER,
            "node({id}) already has a parent"
        );
        self.inner[id] = parent.index_as_u32();
    }

    pub fn finish(&mut self) {
        assert!(!self.finished);
        assert_eq!(self.inner[self.inner.len() - 1], Self::PLACEHOLDER);
        // TODO: enable this? How can we eliminate these invalid nodes?
        // debug_assert!(self.inner.iter().skip(1).all(|&p| p != Self::PLACEHOLDER));
        self.finished = true;
    }
}
