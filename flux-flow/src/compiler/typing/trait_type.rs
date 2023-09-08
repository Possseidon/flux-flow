use std::{collections::BTreeSet, sync::Arc};

/// A type that can represent a set of types that each fully implement a certain set of traits.
#[derive(Default)]
pub(crate) struct TraitTypes {
    types: Option<Arc<BTreeSet<TraitSet>>>,
}

struct TraitSet {
    traits: Arc<BTreeSet<TraitRef>>,
}

/// Index into a list of traits.
struct TraitRef {
    index: usize,
}
