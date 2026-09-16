use bolt_ts_ast as ast;
use bolt_ts_utils::FxIndexSet;

use super::TyChecker;

#[derive(Debug, Clone)]
pub enum PotentiallyUnusedIdentifier<'cx> {
    InferTy(&'cx ast::InferTy<'cx>),
}

impl PotentiallyUnusedIdentifier<'_> {
    fn id(&self) -> ast::NodeID {
        match self {
            PotentiallyUnusedIdentifier::InferTy(n) => n.id,
        }
    }
}

impl std::hash::Hash for PotentiallyUnusedIdentifier<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id().hash(state);
    }
}

impl std::cmp::PartialEq for PotentiallyUnusedIdentifier<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl std::cmp::Eq for PotentiallyUnusedIdentifier<'_> {}

#[derive(Debug, Clone)]
pub struct PotentiallyUnusedIdentifiers<'cx>(FxIndexSet<PotentiallyUnusedIdentifier<'cx>>);

pub struct AllPotentiallyUnusedIdentifiers<'cx> {
    map: Vec<PotentiallyUnusedIdentifiers<'cx>>,
}

impl<'cx> AllPotentiallyUnusedIdentifiers<'cx> {
    pub(super) fn new(cap: usize) -> Self {
        let map = vec![PotentiallyUnusedIdentifiers(FxIndexSet::default()); cap];
        Self { map }
    }

    fn insert(&mut self, n: PotentiallyUnusedIdentifier<'cx>) {
        let id = n.id();
        let idx = id.module().as_usize();
        debug_assert!(idx < self.map.len());
        let item = unsafe { self.map.get_unchecked_mut(idx) };
        item.0.insert(n);
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn register_potentially_unused_infer_type_node(
        &mut self,
        n: &'cx ast::InferTy<'cx>,
    ) {
        let n = PotentiallyUnusedIdentifier::InferTy(n);
        self.all_potentially_unused_identifiers.insert(n);
    }
}
