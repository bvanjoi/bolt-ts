#[derive(Default)]
pub struct ParentMap {
    finished: bool,
    inner: Vec<u32>,
}

impl ParentMap {
    const PLACEHOLDER: u32 = u32::MAX;
    pub(super) fn new(cap: usize) -> Self {
        Self {
            finished: false,
            inner: vec![Self::PLACEHOLDER; cap],
        }
    }

    pub fn parent(&self, node_id: bolt_ts_ast::NodeID) -> Option<bolt_ts_ast::NodeID> {
        let id = node_id.index_as_usize();
        debug_assert!(id < self.inner.len());
        let p = unsafe { *self.inner.get_unchecked(id) };
        if p == Self::PLACEHOLDER {
            assert_eq!(id, self.inner.len() - 1);
            None
        } else {
            Some(bolt_ts_ast::NodeID::new(node_id.module(), p))
        }
    }

    pub(super) fn insert(&mut self, node: bolt_ts_ast::NodeID, parent: bolt_ts_ast::NodeID) {
        let id = node.index_as_usize();
        debug_assert_eq!(
            self.inner[id],
            Self::PLACEHOLDER,
            "node({node:#?}) already has a parent"
        );
        self.inner[id] = parent.index_as_u32();
    }

    pub(super) fn finish(&mut self) {
        assert!(!self.finished);
        assert_eq!(self.inner[self.inner.len() - 1], Self::PLACEHOLDER);
        // debug_assert!(
        //     self.inner
        //         .iter()
        //         .rev()
        //         .skip(1)
        //         .all(|&p| p != Self::PLACEHOLDER)
        // );
        self.finished = true;
    }
}
