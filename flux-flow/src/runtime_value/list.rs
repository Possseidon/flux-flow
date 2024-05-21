use std::{
    any::Any, cmp::Ordering, collections::VecDeque, hash::Hasher, num::NonZeroUsize, sync::Arc,
};

use crate::static_type::StaticType;

use super::{OrderedRuntimeValue, RuntimeValue};

#[derive(Clone, Debug, Default, Eq)]
pub(super) enum Impl {
    #[default]
    Empty,

    ZstLen(NonZeroUsize),

    InlineBytes([u8; 23]), // TODO: how big can I get this?
    ArrayBytes(Arc<[u8]>),
    VecBytes(Arc<Vec<u8>>),
    VecDequeBytes(Arc<VecDeque<u8>>),

    ArrayValues(Arc<[RuntimeValue]>),
    VecValues(Arc<Vec<RuntimeValue>>),
    VecDequeValues(Arc<VecDeque<RuntimeValue>>),
}

impl Impl {
    fn as_slice(&self) -> &[RuntimeValue] {
        match self {
            Impl::Empty => &[],
            // Impl::Vec(values) => values.as_slice(),
            _ => todo!(),
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
    Vec {
        values: Arc<Vec<OrderedRuntimeValue>>,
        /// Only used to determine how to compute [`Ord`].
        ///
        /// Completely ignored by [`Hash`] and [`Eq`].
        list_type: StaticType,
    },
    VecDeque {
        values: Arc<VecDeque<OrderedRuntimeValue>>,
        /// Only used to determine how to compute [`Ord`].
        ///
        /// Completely ignored by [`Hash`] and [`Eq`].
        list_type: StaticType,
    },
    VecBytes {
        bytes: Arc<Vec<u8>>,
        /// Only used to determine how to compute [`Ord`].
        ///
        /// Completely ignored by [`Hash`] and [`Eq`].
        list_type: StaticType,
    },
    Stack {
        stack_index: usize,
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
            OrderedImpl::Vec { values, .. } => values,
            _ => todo!(),
        }
    }

    fn list_type(&self) -> Option<&StaticType> {
        match self {
            OrderedImpl::Empty => None,
            OrderedImpl::Vec { list_type, .. } => Some(list_type),
            _ => todo!(),
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
