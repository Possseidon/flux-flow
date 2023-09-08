use std::{collections::BTreeSet, num::NonZeroUsize, sync::Arc};

use crate::compiler::typing::{Type, Value};

/// A type that can represent a list of values.
///
/// The empty list shall be represented by [`ListType::Empty`].
#[derive(Default)]
pub(crate) enum ListType {
    #[default]
    Never,
    Empty,
    ByValues(ListTypeByValues),
    ByLen(ListTypeByLen),
}

/// A list that can only represent a fixed set of values.
///
/// Its element type can be deduced from its values.
pub(crate) struct ListTypeByValues {
    values: Arc<BTreeSet<Value>>,
}

/// A list of elements with a potentially constrained length.
pub(crate) struct ListTypeByLen {
    element_type: Arc<Type>,
    min_len: usize,
    max_len: NonZeroUsize,
}
