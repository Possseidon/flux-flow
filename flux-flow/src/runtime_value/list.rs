use std::{cmp::Ordering, sync::Arc};

use crate::static_type::StaticType;

use super::{OrderedRuntimeValue, RuntimeValue};

#[derive(Clone, Debug, Default, Eq)]
pub(super) enum Impl {
    #[default]
    Empty,
    NonEmpty(Arc<Vec<RuntimeValue>>),
}

impl Impl {
    fn as_slice(&self) -> &[RuntimeValue] {
        match self {
            Impl::Empty => &[],
            Impl::NonEmpty(values) => values.as_slice(),
        }
    }
}

impl std::hash::Hash for Impl {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_slice().hash(state);
    }
}

impl PartialEq for Impl {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl PartialEq<OrderedImpl> for Impl {
    fn eq(&self, other: &OrderedImpl) -> bool {
        self.as_slice() == other.as_slice()
    }
}

#[derive(Clone, Debug, Default, Eq)]
pub(super) enum OrderedImpl {
    #[default]
    Empty,
    NonEmpty {
        values: Arc<Vec<OrderedRuntimeValue>>,
        /// Only used to determine how to compute [`Ord`].
        ///
        /// Completely ignored by [`Hash`] and [`Eq`].
        list_type: StaticType,
    },
}

impl OrderedImpl {
    fn as_slice(&self) -> &[OrderedRuntimeValue] {
        match self {
            OrderedImpl::Empty => &[],
            OrderedImpl::NonEmpty { values, .. } => values,
        }
    }

    fn list_type(&self) -> Option<&StaticType> {
        match self {
            OrderedImpl::Empty => None,
            OrderedImpl::NonEmpty { list_type, .. } => Some(list_type),
        }
    }
}

impl std::hash::Hash for OrderedImpl {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // ignore list_type so that Impl hashes to the same value
        self.as_slice().hash(state);
    }
}

impl PartialEq for OrderedImpl {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
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
