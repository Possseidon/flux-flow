use std::{cmp::Ordering, collections::BTreeSet, sync::Arc};

use crate::static_type::StaticType;

use super::OrderedRuntimeValue;

#[derive(Clone, Debug, Default, Eq)]
pub(super) enum Impl {
    #[default]
    Empty,
    NonEmpty(Arc<BTreeSet<OrderedRuntimeValue>>),
}

impl Impl {
    fn as_set(&self) -> &BTreeSet<OrderedRuntimeValue> {
        const EMPTY: &BTreeSet<OrderedRuntimeValue> = &BTreeSet::new();
        match self {
            Impl::Empty => EMPTY,
            Impl::NonEmpty(values) => values,
        }
    }
}

impl std::hash::Hash for Impl {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // ignore list_type so that Impl hashes to the same value
        self.as_set().hash(state);
    }
}

impl PartialEq for Impl {
    fn eq(&self, other: &Self) -> bool {
        self.as_set() == other.as_set()
    }
}

impl PartialEq<OrderedImpl> for Impl {
    fn eq(&self, other: &OrderedImpl) -> bool {
        self.as_set() == other.as_set()
    }
}

#[derive(Clone, Debug, Default, Eq)]
pub(super) enum OrderedImpl {
    #[default]
    Empty,
    NonEmpty {
        values: Arc<BTreeSet<OrderedRuntimeValue>>,
        /// Only used to determine how to compute [`Ord`].
        ///
        /// Completely ignored by [`Hash`] and [`Eq`].
        set_type: StaticType,
    },
}

impl OrderedImpl {
    fn as_set(&self) -> &BTreeSet<OrderedRuntimeValue> {
        const EMPTY: &BTreeSet<OrderedRuntimeValue> = &BTreeSet::new();
        match self {
            OrderedImpl::Empty => EMPTY,
            OrderedImpl::NonEmpty { values, .. } => values,
        }
    }

    fn set_type(&self) -> Option<&StaticType> {
        match self {
            OrderedImpl::Empty => None,
            OrderedImpl::NonEmpty { set_type, .. } => Some(set_type),
        }
    }
}

impl std::hash::Hash for OrderedImpl {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // ignore list_type so that Impl hashes to the same value
        self.as_set().hash(state);
    }
}

impl PartialEq for OrderedImpl {
    fn eq(&self, other: &Self) -> bool {
        self.as_set() == other.as_set()
    }
}

impl PartialEq<Impl> for OrderedImpl {
    fn eq(&self, other: &Impl) -> bool {
        other == self
    }
}

impl PartialOrd for OrderedImpl {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for OrderedImpl {
    fn cmp(&self, other: &Self) -> Ordering {
        todo!("compare using set_type")
    }
}
