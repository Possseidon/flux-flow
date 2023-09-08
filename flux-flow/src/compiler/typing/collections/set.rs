use std::{collections::BTreeSet, num::NonZeroUsize, sync::Arc};

use crate::compiler::typing::{Type, Value};

/// A type that can represent a set of distinct values.
///
/// The empty set shall be represented by [`SetType::Empty`].
#[derive(Default)]
pub(crate) enum SetType {
    #[default]
    Never,
    Empty,
    ByValues(SetTypeByValues),
    ByLen(SetTypeByLen),
}

/// A set that can only represent a fixed set of values.
///
/// Its element type can be deduced from its values.
pub(crate) struct SetTypeByValues {
    values: Arc<BTreeSet<Value>>,
}

/// A set of elements with a potentially constrained length.
pub(crate) struct SetTypeByLen {
    element_type: Arc<Type>,
    min_len: usize,
    max_len: NonZeroUsize,
}
