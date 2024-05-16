use std::{cmp::Ordering, collections::BTreeMap, sync::Arc};

use itertools::equal;

use crate::static_type::StaticType;

use super::{OrderedRuntimeValue, RuntimeValue};

#[derive(Clone, Debug, Default, Eq)]
pub(super) enum Impl {
    #[default]
    Empty,
    NonEmpty(Arc<BTreeMap<OrderedRuntimeValue, RuntimeValue>>),
}

impl Impl {
    fn as_map(&self) -> &BTreeMap<OrderedRuntimeValue, RuntimeValue> {
        const EMPTY: &BTreeMap<OrderedRuntimeValue, RuntimeValue> = &BTreeMap::new();
        match self {
            Impl::Empty => EMPTY,
            Impl::NonEmpty(map) => map,
        }
    }
}

impl std::hash::Hash for Impl {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_map().hash(state);
    }
}

impl PartialEq for Impl {
    fn eq(&self, other: &Self) -> bool {
        self.as_map() == other.as_map()
    }
}

impl PartialEq<OrderedImpl> for Impl {
    fn eq(&self, other: &OrderedImpl) -> bool {
        let self_map = self.as_map();
        let other_map = other.as_map();
        equal(self_map.keys(), other_map.keys()) && equal(self_map.values(), other_map.values())
    }
}

#[derive(Clone, Debug, Default, Eq)]
pub(super) enum OrderedImpl {
    #[default]
    Empty,
    NonEmpty {
        values: Arc<BTreeMap<OrderedRuntimeValue, OrderedRuntimeValue>>,
        /// Only used to determine how to compute [`Ord`].
        ///
        /// Completely ignored by [`Hash`] and [`Eq`].
        map_type: StaticType,
    },
}

impl OrderedImpl {
    fn as_map(&self) -> &BTreeMap<OrderedRuntimeValue, OrderedRuntimeValue> {
        const EMPTY: &BTreeMap<OrderedRuntimeValue, OrderedRuntimeValue> = &BTreeMap::new();
        match self {
            OrderedImpl::Empty => EMPTY,
            OrderedImpl::NonEmpty { values, .. } => values,
        }
    }

    fn map_type(&self) -> Option<&StaticType> {
        match self {
            OrderedImpl::Empty => None,
            OrderedImpl::NonEmpty { map_type, .. } => Some(map_type),
        }
    }
}

impl std::hash::Hash for OrderedImpl {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // ignore map_type so that Impl hashes to the same value
        self.as_map().hash(state);
    }
}

impl PartialEq for OrderedImpl {
    fn eq(&self, other: &Self) -> bool {
        self.as_map() == other.as_map()
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
        todo!("compare using list_type")
    }
}
